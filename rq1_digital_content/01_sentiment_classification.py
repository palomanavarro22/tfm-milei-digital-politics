"""
rq1_digital_content/01_sentiment_classification.py

Pipeline completo de Python para RQ1: parte de milei_tweets_combinado.csv
(ya recolectado con Zeeschuimer + API de terceros -- ese paso de
recolección/combinación NO esta incluido en este repo, ver README) y
llega hasta las tablas del Appendix que dependen de los clasificadores
de pysentimiento (A.2, A.4, A.7, A.8, A.9).

RUTAS (correr siempre con la raíz del repo como working directory):
  Lee:    data/raw/milei_tweets_combinado.csv
          data/raw/muestra_codificacion_manual.csv
  Escribe: data/processed/milei_tweets_con_sentimiento.csv
           data/processed/predicciones_pysentimiento_muestra.csv
           appendix_tables/tableA2_*.png, tableA4_*.png, tableA7_*.png,
           tableA8_*.png, tableA9_*.png

Siguiente paso: rq1_digital_content/02_engagement_model.R (Tablas A.3,
A.5, A.6 -- lado de R, necesita el CSV que este script genera).

Consolida en un solo archivo, en orden de ejecución:
  PART 1  -- Clasificación base (sentiment, emotion, irony, hate_speech)
             [antes: RQ1_03_analisis_pysentimiento_completo.py]
  PART 2  -- context_hate_speech, 9 categorías de grupo-blanco
             [antes: RQ1_12_contexths.py y RQ1_13.py -- eran archivos
             IDÉNTICOS, se consolidan en un solo paso acá]
  PART 3  -- Predicciones de ironía sobre la muestra de validación manual
             [antes: RQ1_08_predicciones_ironia_muestra.py]
  PART 4  -- Helper apa_table_png() (estilo APA con matplotlib)
  PART 5  -- Table A.2 (distribución corpus-level)
  PART 6  -- Table A.4 (cruce "others" vs. suficiencia de texto)
             *** CORREGIDO: ver nota en Part 6 ***
  PART 7  -- Table A.7 (sub-análisis "la casta")
  PART 8  -- Validación de palabras clave (inmigración/clasismo/tribalismo)
             [antes: RQ1_11.py, folded in como diagnóstico]
  PART 9  -- Table A.8 (odio extendido, 9 categorías)
  PART 10 -- Table A.9 + chequeo profundo de estigmatización grupal
             [antes: RQ1_15_verificar_estigmatizacion_grupal.py, folded in]

NO INCLUIDOS (son exploratorios/cualitativos, no generan ningún número
citado en el borrador; quedan como scripts sueltos si los querés correr
aparte):
  - RQ1_04_revisar_others.py   (inspección manual top-15 + muestra azar)
  - RQ1_06_revisar_anger.py    (inspección manual top-25 por prob_anger)

Requiere: pandas, matplotlib, pysentimiento (pip install pysentimiento)
"""

import os
import pandas as pd
import matplotlib.pyplot as plt

# Rutas relativas a la raiz del repo -- correr este script desde ahi.
RAW_DIR = os.path.join("data", "raw")
PROCESSED_DIR = os.path.join("data", "processed")
APPENDIX_DIR = "appendix_tables"

os.makedirs(PROCESSED_DIR, exist_ok=True)
os.makedirs(APPENDIX_DIR, exist_ok=True)
plt.rcParams["font.family"] = "serif"
plt.rcParams["font.serif"] = ["Times New Roman", "Times", "DejaVu Serif"]


# =============================================================================
# PART 1 -- Clasificación base: sentiment, emotion, irony, hate_speech
# Corre sobre milei_tweets_combinado.csv (Zeeschuimer + API), genera
# milei_tweets_con_sentimiento.csv.
# =============================================================================

def part1_clasificacion_base():
    from pysentimiento import create_analyzer

    print("\n" + "=" * 70)
    print("PART 1 -- Clasificacion base (sentiment/emotion/irony/hate_speech)")
    print("=" * 70)

    print("Cargando dataset...")
    df = pd.read_csv(os.path.join(RAW_DIR, "milei_tweets_combinado.csv"), dtype={"tweet_id": str})
    print(f"{len(df)} tweets cargados")

    # Nos quedamos solo con los que tienen texto (algunos son solo imagen,
    # full_text vacío)
    df_con_texto = df[df["full_text"].notna() & (df["full_text"].str.strip() != "")].copy()
    print(f"{len(df_con_texto)} tweets con texto para analizar (de {len(df)} totales)")

    print("\nCargando modelos (puede tardar la primera vez si no estan cacheados)...")
    analizador_sentimiento = create_analyzer(task="sentiment", lang="es")
    analizador_emocion = create_analyzer(task="emotion", lang="es")
    analizador_ironia = create_analyzer(task="irony", lang="es")
    analizador_odio = create_analyzer(task="hate_speech", lang="es")

    print("\nCorriendo analisis sobre cada tweet...")

    resultados_sentimiento, resultados_emocion, resultados_ironia, resultados_odio = [], [], [], []
    prob_pos, prob_neg, prob_neu = [], [], []
    prob_ironic = []
    prob_hateful, prob_targeted, prob_aggressive = [], [], []
    # Probabilidades completas de las 7 categorias de emocion (no solo la ganadora)
    prob_joy, prob_anger, prob_disgust, prob_fear = [], [], [], []
    prob_sadness, prob_surprise, prob_others = [], [], []

    total = len(df_con_texto)
    for i, texto in enumerate(df_con_texto["full_text"], start=1):
        sent = analizador_sentimiento.predict(texto)
        emo = analizador_emocion.predict(texto)
        iro = analizador_ironia.predict(texto)
        odio = analizador_odio.predict(texto)  # odio.output es una LISTA, no un string unico

        resultados_sentimiento.append(sent.output)
        resultados_emocion.append(emo.output)  # etiqueta ganadora, se mantiene por referencia
        resultados_ironia.append(iro.output)
        # Si la lista esta vacia (no detecto nada), guardamos "none"
        resultados_odio.append(", ".join(odio.output) if odio.output else "none")

        prob_pos.append(sent.probas.get("POS"))
        prob_neg.append(sent.probas.get("NEG"))
        prob_neu.append(sent.probas.get("NEU"))
        prob_ironic.append(iro.probas.get("ironic"))
        prob_hateful.append(odio.probas.get("hateful"))
        prob_targeted.append(odio.probas.get("targeted"))
        prob_aggressive.append(odio.probas.get("aggressive"))

        prob_joy.append(emo.probas.get("joy"))
        prob_anger.append(emo.probas.get("anger"))
        prob_disgust.append(emo.probas.get("disgust"))
        prob_fear.append(emo.probas.get("fear"))
        prob_sadness.append(emo.probas.get("sadness"))
        prob_surprise.append(emo.probas.get("surprise"))
        prob_others.append(emo.probas.get("others"))

        if i % 50 == 0 or i == total:
            print(f"  {i}/{total} procesados")

    df_con_texto["sentimiento"] = resultados_sentimiento
    df_con_texto["emocion"] = resultados_emocion
    df_con_texto["ironia"] = resultados_ironia
    df_con_texto["discurso_odio"] = resultados_odio
    df_con_texto["prob_pos"] = prob_pos
    df_con_texto["prob_neg"] = prob_neg
    df_con_texto["prob_neu"] = prob_neu
    df_con_texto["prob_ironic"] = prob_ironic
    df_con_texto["prob_hateful"] = prob_hateful
    df_con_texto["prob_targeted"] = prob_targeted
    df_con_texto["prob_aggressive"] = prob_aggressive
    df_con_texto["prob_joy"] = prob_joy
    df_con_texto["prob_anger"] = prob_anger
    df_con_texto["prob_disgust"] = prob_disgust
    df_con_texto["prob_fear"] = prob_fear
    df_con_texto["prob_sadness"] = prob_sadness
    df_con_texto["prob_surprise"] = prob_surprise
    df_con_texto["prob_others"] = prob_others

    df_con_texto.to_csv(os.path.join(PROCESSED_DIR, "milei_tweets_con_sentimiento.csv"), index=False, encoding="utf-8")
    print(f"\nGuardado: milei_tweets_con_sentimiento.csv ({len(df_con_texto)} tweets)")

    print("\n--- Distribucion de sentimiento ---")
    print(df_con_texto["sentimiento"].value_counts())
    print("\n--- Distribucion de emocion (etiqueta ganadora, solo referencia) ---")
    print(df_con_texto["emocion"].value_counts())
    print("\n--- Distribucion de ironia ---")
    print(df_con_texto["ironia"].value_counts())
    print("\n--- Distribucion de discurso de odio ---")
    print(df_con_texto["discurso_odio"].value_counts())

    return df_con_texto


# =============================================================================
# PART 2 -- context_hate_speech, 9 categorias de grupo-blanco
# CONSOLIDADO: RQ1_12_contexths.py y RQ1_13.py eran archivos identicos,
# se corre una sola vez acá.
#
# DECISION METODOLOGICA (documentada en el original): se corre SIN
# contexto (predict(texto) solo). El modelo fue entrenado con pares
# comentario+noticia; nuestros tweets son posteos originales sin un
# "contexto" real y especifico al que responder. En la prueba con 3
# tweets, pasar un contexto generico apenas cambio los resultados -- se
# documenta como limitacion exploratoria.
# =============================================================================

def part2_context_hate_speech(df_con_texto):
    from pysentimiento import create_analyzer

    print("\n" + "=" * 70)
    print("PART 2 -- context_hate_speech (9 categorias)")
    print("=" * 70)

    print("Cargando modelo context_hate_speech (puede tardar la primera vez)...")
    analizador = create_analyzer(task="context_hate_speech", lang="es")

    categorias = ["CALLS", "WOMEN", "LGBTI", "RACISM", "CLASS", "POLITICS",
                  "DISABLED", "APPEARANCE", "CRIMINAL"]

    resultados_output = []
    probas_por_categoria = {c: [] for c in categorias}

    total = len(df_con_texto)
    for i, texto in enumerate(df_con_texto["full_text"], start=1):
        pred = analizador.predict(texto)
        resultados_output.append(", ".join(pred.output) if pred.output else "none")
        for c in categorias:
            probas_por_categoria[c].append(pred.probas.get(c, None))
        if i % 50 == 0 or i == total:
            print(f"  {i}/{total} procesados")

    df_con_texto["odio_contextual_categorias"] = resultados_output
    for c in categorias:
        df_con_texto[f"prob_odio_{c.lower()}"] = probas_por_categoria[c]

    df_con_texto.to_csv(os.path.join(PROCESSED_DIR, "milei_tweets_con_sentimiento.csv"), index=False, encoding="utf-8")
    print("\nActualizado: milei_tweets_con_sentimiento.csv (agregadas 10 columnas nuevas)")

    print("\n--- Medias de probabilidad por categoria ---")
    for c in categorias:
        col = f"prob_odio_{c.lower()}"
        print(f"  {col:20s}: media={df_con_texto[col].mean():.4f}, max={df_con_texto[col].max():.4f}")

    return df_con_texto


# =============================================================================
# PART 3 -- Prediccion de ironia sobre la muestra de validacion manual
# [antes: RQ1_08_predicciones_ironia_muestra.py]
#
# VERIFICADO (corrida real, 13-sep-2026): esta prediccion separada y la
# columna `ironia` ya calculada en el corpus principal (Part 1) coinciden
# 80/80 (100%), diferencia maxima en prob_ironic = 0.000000. Son
# identicas -- da igual cual alimente Table A.3 (lado R). Este paso se
# mantiene solo como respaldo/chequeo cruzado, no por necesidad.
# =============================================================================

def part3_prediccion_ironia_muestra_manual():
    import csv
    from pysentimiento import create_analyzer

    print("\n" + "=" * 70)
    print("PART 3 -- Prediccion de ironia sobre muestra de validacion manual")
    print("=" * 70)

    analizador_ironia = create_analyzer(task="irony", lang="es")
    filas_salida = []

    with open(os.path.join(RAW_DIR, "muestra_codificacion_manual.csv"), "r", encoding="utf-8") as f:
        lector = csv.DictReader(f, delimiter=';')
        for fila in lector:
            texto = fila["full_text"]
            pred = analizador_ironia.predict(texto)
            filas_salida.append({
                "tweet_id": fila["tweet_id"],
                "prediccion_pysentimiento": pred.output,
                "prob_ironic": pred.probas.get("ironic", None),
            })

    with open(os.path.join(PROCESSED_DIR, "predicciones_pysentimiento_muestra.csv"), "w", encoding="utf-8", newline="") as f:
        escritor = csv.DictWriter(f, fieldnames=["tweet_id", "prediccion_pysentimiento", "prob_ironic"])
        escritor.writeheader()
        escritor.writerows(filas_salida)

    print(f"Predicciones guardadas para {len(filas_salida)} tweets en "
          f"{os.path.join(PROCESSED_DIR, 'predicciones_pysentimiento_muestra.csv')}")


# =============================================================================
# PART 4 -- Helper: tabla con estilo APA usando matplotlib
# =============================================================================

def apa_table_png(tabla_df, titulo, archivo, nota="Source: own elaboration.", ancho=9):
    """Dibuja tabla_df como imagen con estilo APA: lineas horizontales
    solamente, titulo en negrita arriba, nota de fuente en italica abajo."""

    n_filas = len(tabla_df) + 1
    alto = 0.45 * n_filas + 1.0

    fig, ax = plt.subplots(figsize=(ancho, alto))
    ax.axis("off")

    tabla = ax.table(
        cellText=tabla_df.values,
        colLabels=tabla_df.columns,
        cellLoc="left", loc="center", colLoc="left",
    )
    tabla.auto_set_font_size(False)
    tabla.set_fontsize(11)

    n_cols = len(tabla_df.columns)
    tabla.auto_set_column_width(col=list(range(n_cols)))
    tabla.scale(1, 1.6)

    for (row, col), celda in tabla.get_celld().items():
        celda.set_linewidth(0)
        celda.PAD = 0.03
        if row == 0:
            celda.set_text_props(fontweight="bold")

    for col in range(n_cols):
        tabla[0, col].visible_edges = "TB"
        tabla[0, col].set_linewidth(1.5)
    ultima_fila = len(tabla_df)
    for col in range(n_cols):
        tabla[ultima_fila, col].visible_edges = "B"
        tabla[ultima_fila, col].set_linewidth(1.5)

    ax.set_title(titulo, fontsize=13, fontweight="bold", loc="left", pad=14)
    fig.text(0.06, 0.02, nota, fontsize=9, style="italic", ha="left")

    plt.tight_layout(rect=[0, 0.05, 1, 1])
    ruta = os.path.join(APPENDIX_DIR, archivo)
    plt.savefig(ruta, dpi=200, bbox_inches="tight")
    plt.close(fig)
    print(f"Guardado: {ruta}")


# =============================================================================
# PART 5 -- Table A.2: distribucion corpus-level
# =============================================================================

def part5_tabla_A2(df_texto):
    print("\n" + "=" * 70)
    print("PART 5 -- Table A.2")
    print("=" * 70)

    dist_sentimiento = df_texto["sentimiento"].value_counts(normalize=True).mul(100).round(0).astype(int)
    dist_emocion = df_texto["emocion"].value_counts(normalize=True).mul(100).round(0).astype(int)
    dist_ironia = df_texto["ironia"].value_counts(normalize=True).mul(100).round(0).astype(int)
    dist_odio_generico = df_texto["discurso_odio"].value_counts()

    tabla_A2 = pd.DataFrame({
        "Measure": ["Sentiment", "Emotion (discrete label)", "Irony (binary label)", "Hate speech (generic classifier)"],
        "Distribution": [
            ", ".join(f"{k}: {v}%" for k, v in dist_sentimiento.items()),
            ", ".join(f"{k}: {v}%" for k, v in dist_emocion.items()),
            ", ".join(f"{k}: {v}%" for k, v in dist_ironia.items()),
            ", ".join(f"{k}: {v}" for k, v in dist_odio_generico.items()),
        ]
    })
    apa_table_png(tabla_A2, "Table A.2. Sentiment, emotion, and irony classification — corpus-level distribution",
                  "tableA2_distribucion.png", ancho=11)


# =============================================================================
# PART 6 -- Table A.4: cruce "others" vs. suficiencia de texto
#
# *** CORREGIDO ***
# La version anterior (RQ_Appendix_Tablas_Python.py) media el texto CON
# la URL de Twitter incluida (~23 caracteres), asi que casi ningun tweet
# quedaba marcado como "insuficiente" -- reproducia el 3/233 (1.3%/98.7%)
# que hoy esta desactualizado en el borrador. La version verificada
# (RQ1_05_cruce_others_media.py, ya corrida y confirmada por Paloma)
# saca la URL ANTES de medir el largo, dando 20/216 (8.5%/91.5%), que es
# lo que ya esta citado en el cuerpo del texto. Esta version usa esa
# misma logica para que la tabla quede sincronizada con el cuerpo.
# =============================================================================

def part6_tabla_A4(df_texto):
    print("\n" + "=" * 70)
    print("PART 6 -- Table A.4 (FIXED: largo de texto SIN la URL)")
    print("=" * 70)

    others = df_texto[df_texto["emocion"] == "others"].copy()

    others["largo_texto_sin_url"] = (
        others["full_text"]
        .str.replace(r"https://t\.co/\S+", "", regex=True)
        .str.strip()
        .str.len()
    )
    others["texto_insuficiente"] = others["largo_texto_sin_url"] < 15

    tabla_A4 = pd.DataFrame({
        "": ["Insufficient text (<15 characters)", "Substantive text present"],
        "n": [int(others["texto_insuficiente"].sum()), int((~others["texto_insuficiente"]).sum())],
        '% of "others"': [
            f"{100*others['texto_insuficiente'].mean():.1f}%",
            f"{100*(~others['texto_insuficiente']).mean():.1f}%"
        ]
    })
    print(tabla_A4.to_string(index=False))
    apa_table_png(tabla_A4, f'Table A.4. Cross-tabulation of the "others" emotion category (N = {len(others)})',
                  "tableA4_others.png")


# =============================================================================
# PART 7 -- Table A.7: sub-analisis "la casta"
# =============================================================================

def part7_tabla_A7(df_texto):
    print("\n" + "=" * 70)
    print("PART 7 -- Table A.7")
    print("=" * 70)

    df_texto["menciona_casta"] = df_texto["full_text"].str.contains(r"\bcasta\b|castas", case=False, na=False, regex=True)
    df_texto["engagement"] = df_texto["likes"] + df_texto["retweets"] + df_texto["replies"] + df_texto["quotes"]

    casta = df_texto[df_texto["menciona_casta"]]
    resto = df_texto[~df_texto["menciona_casta"]]

    tabla_A7 = pd.DataFrame({
        "": ["Mean prob_anger", "Mean engagement"],
        '"Casta" tweets': [f"{casta['prob_anger'].mean():.3f}", f"{casta['engagement'].mean():,.0f}"],
        "Rest of corpus": [f"{resto['prob_anger'].mean():.3f}", f"{resto['engagement'].mean():,.0f}"]
    })
    apa_table_png(tabla_A7, f'Table A.7. "La casta" content sub-analysis (N = {len(casta)} of {len(df_texto)} tweets)',
                  "tableA7_casta.png")

    return df_texto  # ya trae menciona_casta y engagement para partes siguientes


# =============================================================================
# PART 8 -- Validacion de palabras clave (inmigracion/clasismo/tribalismo)
# [antes: RQ1_11.py] -- diagnostico citado en el parrafo de Table A.8
# =============================================================================

def part8_validacion_keywords(df_texto):
    print("\n" + "=" * 70)
    print("PART 8 -- Validacion de palabras clave")
    print("=" * 70)

    terminos_inmigracion = r"inmigra|migrante|extranjer|boliviano|paraguayo|peruano|venezolano|frontera|bolita|paragua"
    terminos_clasismo = r"\bnegro\b|negro de m|villero|cabecita"
    terminos_tribalismo = r"\bkuka\b|kukas"

    df_texto["menciona_inmigracion"] = df_texto["full_text"].str.contains(terminos_inmigracion, case=False, na=False, regex=True)
    df_texto["menciona_clasismo"] = df_texto["full_text"].str.contains(terminos_clasismo, case=False, na=False, regex=True)
    df_texto["menciona_tribalismo"] = df_texto["full_text"].str.contains(terminos_tribalismo, case=False, na=False, regex=True)

    for grupo, col in [("INMIGRACION/NACIONALIDAD", "menciona_inmigracion"),
                        ("CLASISMO (revisar a mano)", "menciona_clasismo"),
                        ("TRIBALISMO POLITICO ('kuka')", "menciona_tribalismo")]:
        n = int(df_texto[col].sum())
        print(f"  {grupo:35s}: {n} de {len(df_texto)} tweets")


# =============================================================================
# PART 9 -- Table A.8: odio extendido (context_hate_speech, 9 categorias)
# =============================================================================

def part9_tabla_A8(df_texto):
    print("\n" + "=" * 70)
    print("PART 9 -- Table A.8")
    print("=" * 70)

    categorias_odio = ["calls", "women", "lgbti", "racism", "class", "politics", "disabled", "appearance", "criminal"]
    filas_A8 = []
    for cat in categorias_odio:
        col = f"prob_odio_{cat}"
        if col in df_texto.columns:
            filas_A8.append({
                "Category": cat.upper(),
                "Corpus mean probability": f"{df_texto[col].mean():.4f}",
                "Max": f"{df_texto[col].max():.3f}"
            })

    if filas_A8:
        tabla_A8 = pd.DataFrame(filas_A8)
        apa_table_png(tabla_A8, "Table A.8. Extended hate-speech probing (context_hate_speech, 9 categories)",
                      "tableA8_odio_extendido.png")
    else:
        print("Table A.8: columnas prob_odio_* no encontradas -- correr PART 2 primero.")

    # Cruce especifico con los tweets de "casta" (POLITICS/CLASS son las
    # relevantes) -- sustenta el parrafo narrativo de Table A.8: el
    # registro combativo de Milei contra "la casta" no se detecta como
    # odio dirigido a un grupo protegido. [restaurado desde RQ1_13.py --
    # faltaba en esta consolidacion]
    if "prob_odio_politics" in df_texto.columns and "prob_odio_class" in df_texto.columns:
        if "menciona_casta" not in df_texto.columns:
            df_texto["menciona_casta"] = df_texto["full_text"].str.contains(
                r"\bcasta\b|castas", case=False, na=False, regex=True
            )
        casta = df_texto[df_texto["menciona_casta"]]
        resto = df_texto[~df_texto["menciona_casta"]]
        print(f"\n--- Entre los {len(casta)} tweets con 'casta': prob_odio_politics y prob_odio_class ---")
        print(f"  prob_odio_politics: media={casta['prob_odio_politics'].mean():.4f} "
              f"(vs. resto: {resto['prob_odio_politics'].mean():.4f})")
        print(f"  prob_odio_class:    media={casta['prob_odio_class'].mean():.4f} "
              f"(vs. resto: {resto['prob_odio_class'].mean():.4f})")


# =============================================================================
# PART 10 -- Table A.9 + chequeo profundo de estigmatizacion grupal
# [antes: Table A.9 de RQ_Appendix_Tablas_Python.py + RQ1_15_verificar_
# estigmatizacion_grupal.py, consolidados]
# =============================================================================

def part10_tabla_A9_y_estigmatizacion(df_texto):
    print("\n" + "=" * 70)
    print("PART 10 -- Table A.9 + validacion de estigmatizacion grupal")
    print("=" * 70)

    patrones = {
        "banda de inmorales": r"banda de inmoral",
        "casta roja": r"casta roja",
        "periodistas pauteros": r"pauter[oa]",
        "políticos rastreros": r"rastrer[oa]",
        "chorros de la política": r"chorro",
        "gobierno de delincuentes": r"gobierno.{0,20}delincuente|delincuente.{0,20}gobierno",
        "banda de corruptos": r"banda de corrupto",
    }

    filas_A9 = []
    casos_encontrados = []
    for etiqueta, patron in patrones.items():
        coincidencias = df_texto[df_texto["full_text"].str.contains(patron, case=False, regex=True, na=False)]
        print(f"  {etiqueta:30s}: {len(coincidencias)} tweets")
        if len(coincidencias) > 0:
            casos_encontrados.append((etiqueta, coincidencias))
        for _, fila in coincidencias.iterrows():
            if "prob_hateful" in df_texto.columns and "prob_odio_politics" in df_texto.columns:
                filas_A9.append({
                    "Phrase": etiqueta,
                    'Generic: "hateful" prob.': f"{fila.get('prob_hateful', float('nan')):.2f}",
                    "POLITICS": f"{fila.get('prob_odio_politics', float('nan')):.4f}",
                    "CLASS": f"{fila.get('prob_odio_class', float('nan')):.4f}",
                })

    if filas_A9:
        tabla_A9 = pd.DataFrame(filas_A9).sort_values('Generic: "hateful" prob.', ascending=False)
        apa_table_png(tabla_A9, "Table A.9. Hate speech classifier output on group-stigmatizing tweets",
                      "tableA9_validacion_dirigida.png", ancho=10)
    else:
        print("Table A.9: no se encontraron coincidencias o faltan columnas prob_odio_* --",
              "correr PART 2 primero.")

    # Chequeo profundo adicional (Perez-Diaz & Arroyas Langa 2025): confirma
    # si el problema es de deteccion (frase aparece, prob baja) o de
    # cobertura (la frase directamente no aparece en nuestra ventana).
    if casos_encontrados:
        from pysentimiento import create_analyzer

        print("\n--- Chequeo profundo: clasificadores sobre los casos encontrados ---")
        analizador_generico = create_analyzer(task="hate_speech", lang="es")
        analizador_contexto = create_analyzer(task="context_hate_speech", lang="es")
        for etiqueta, coincidencias in casos_encontrados:
            print(f"\n{etiqueta} ({len(coincidencias)} tweets)")
            for _, fila in coincidencias.iterrows():
                texto = fila["full_text"]
                pred_generico = analizador_generico.predict(texto)
                pred_contexto = analizador_contexto.predict(texto)
                print(f"  Texto: {texto[:100]}")
                print(f"    hate_speech generico: {pred_generico.probas}")
                print(f"    POLITICS={pred_contexto.probas.get('POLITICS', float('nan')):.4f}, "
                      f"CLASS={pred_contexto.probas.get('CLASS', float('nan')):.4f}")
    else:
        print("\n>>> Ninguna de las frases textuales especificas del paper aparece en el corpus.")
        print(">>> El paper y esta muestra, aunque se solapan en fecha, no son exactamente")
        print(">>> los mismos tweets -- posible diferencia en el metodo de scraping.")


# =============================================================================
# MAIN -- correr todo en orden
# =============================================================================

if __name__ == "__main__":
    df_texto = part1_clasificacion_base()
    df_texto = part2_context_hate_speech(df_texto)
    part3_prediccion_ironia_muestra_manual()
    part5_tabla_A2(df_texto)
    part6_tabla_A4(df_texto)
    df_texto = part7_tabla_A7(df_texto)
    part8_validacion_keywords(df_texto)
    part9_tabla_A8(df_texto)
    part10_tabla_A9_y_estigmatizacion(df_texto)

    print("\n" + "=" * 70)
    print("01_sentiment_classification.py -- listo.")
    print(f"Tablas generadas en {APPENDIX_DIR}/: A.2, A.4 (corregida), A.7, A.8, A.9")
    print("Tabla A.3, A.5, A.6 se generan del lado de R (02_engagement_model.R)")
    print("=" * 70)
