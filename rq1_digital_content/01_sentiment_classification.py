"""
Classification pipeline for RQ1: runs pysentimiento's classifiers over
Milei's tweets and builds the classifier-dependent appendix tables
(A.2, A.4, A.7, A.8, A.9).

Run from the repo root. Reads:
    data/raw/milei_tweets_combinado.csv
    data/raw/muestra_codificacion_manual.csv
Writes:
    data/processed/milei_tweets_con_sentimiento.csv
    data/processed/predicciones_pysentimiento_muestra.csv
    appendix_tables/tableA{2,4,7,8,9}_*.png

Tables A.3, A.5, A.6 are generated separately by
rq1_digital_content/02_engagement_model.R, once this script has produced
milei_tweets_con_sentimiento.csv.

Requires: pandas, matplotlib, pysentimiento
"""

import os
import pandas as pd
import matplotlib.pyplot as plt

RAW_DIR = os.path.join("data", "raw")
PROCESSED_DIR = os.path.join("data", "processed")
APPENDIX_DIR = "appendix_tables"

os.makedirs(PROCESSED_DIR, exist_ok=True)
os.makedirs(APPENDIX_DIR, exist_ok=True)
plt.rcParams["font.family"] = "serif"
plt.rcParams["font.serif"] = ["Times New Roman", "Times", "DejaVu Serif"]


def clasificacion_base():
    """Sentiment, emotion, irony and generic hate-speech classifiers."""
    from pysentimiento import create_analyzer

    print("Cargando dataset...")
    df = pd.read_csv(os.path.join(RAW_DIR, "milei_tweets_combinado.csv"), dtype={"tweet_id": str})
    df_con_texto = df[df["full_text"].notna() & (df["full_text"].str.strip() != "")].copy()
    print(f"{len(df_con_texto)} de {len(df)} tweets tienen texto para analizar")

    analizador_sentimiento = create_analyzer(task="sentiment", lang="es")
    analizador_emocion = create_analyzer(task="emotion", lang="es")
    analizador_ironia = create_analyzer(task="irony", lang="es")
    analizador_odio = create_analyzer(task="hate_speech", lang="es")

    columnas = {c: [] for c in [
        "sentimiento", "emocion", "ironia", "discurso_odio",
        "prob_pos", "prob_neg", "prob_neu", "prob_ironic",
        "prob_hateful", "prob_targeted", "prob_aggressive",
        "prob_joy", "prob_anger", "prob_disgust", "prob_fear",
        "prob_sadness", "prob_surprise", "prob_others",
    ]}

    total = len(df_con_texto)
    for i, texto in enumerate(df_con_texto["full_text"], start=1):
        sent = analizador_sentimiento.predict(texto)
        emo = analizador_emocion.predict(texto)
        iro = analizador_ironia.predict(texto)
        odio = analizador_odio.predict(texto)

        columnas["sentimiento"].append(sent.output)
        columnas["emocion"].append(emo.output)
        columnas["ironia"].append(iro.output)
        columnas["discurso_odio"].append(", ".join(odio.output) if odio.output else "none")

        columnas["prob_pos"].append(sent.probas.get("POS"))
        columnas["prob_neg"].append(sent.probas.get("NEG"))
        columnas["prob_neu"].append(sent.probas.get("NEU"))
        columnas["prob_ironic"].append(iro.probas.get("ironic"))
        columnas["prob_hateful"].append(odio.probas.get("hateful"))
        columnas["prob_targeted"].append(odio.probas.get("targeted"))
        columnas["prob_aggressive"].append(odio.probas.get("aggressive"))
        for emocion in ["joy", "anger", "disgust", "fear", "sadness", "surprise", "others"]:
            columnas[f"prob_{emocion}"].append(emo.probas.get(emocion))

        if i % 50 == 0 or i == total:
            print(f"  {i}/{total} procesados")

    for col, valores in columnas.items():
        df_con_texto[col] = valores

    df_con_texto.to_csv(os.path.join(PROCESSED_DIR, "milei_tweets_con_sentimiento.csv"), index=False, encoding="utf-8")
    print(f"Guardado: milei_tweets_con_sentimiento.csv ({len(df_con_texto)} tweets)")
    return df_con_texto


def context_hate_speech(df_con_texto):
    """9-category targeted hate-speech model. Run without context: these
    are original posts, not replies to a specific comment/article pair
    the model was trained on."""
    from pysentimiento import create_analyzer

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
    return df_con_texto


def prediccion_ironia_muestra_manual():
    """Irony predictions on the 80-tweet manual validation sample, used
    for Table A.3 (cross-check against the main corpus's `ironia` column)."""
    import csv
    from pysentimiento import create_analyzer

    analizador_ironia = create_analyzer(task="irony", lang="es")
    filas_salida = []

    with open(os.path.join(RAW_DIR, "muestra_codificacion_manual.csv"), "r", encoding="utf-8") as f:
        for fila in csv.DictReader(f, delimiter=';'):
            pred = analizador_ironia.predict(fila["full_text"])
            filas_salida.append({
                "tweet_id": fila["tweet_id"],
                "prediccion_pysentimiento": pred.output,
                "prob_ironic": pred.probas.get("ironic", None),
            })

    ruta = os.path.join(PROCESSED_DIR, "predicciones_pysentimiento_muestra.csv")
    with open(ruta, "w", encoding="utf-8", newline="") as f:
        escritor = csv.DictWriter(f, fieldnames=["tweet_id", "prediccion_pysentimiento", "prob_ironic"])
        escritor.writeheader()
        escritor.writerows(filas_salida)
    print(f"Guardado: {ruta} ({len(filas_salida)} tweets)")


def apa_table_png(tabla_df, titulo, archivo, nota="Source: own elaboration.", ancho=9):
    """Renders tabla_df as an APA-style table image: horizontal rules
    only, bold title, italic source note."""
    n_filas = len(tabla_df) + 1
    alto = 0.45 * n_filas + 1.0

    fig, ax = plt.subplots(figsize=(ancho, alto))
    ax.axis("off")

    tabla = ax.table(cellText=tabla_df.values, colLabels=tabla_df.columns,
                      cellLoc="left", loc="center", colLoc="left")
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


def tabla_A2(df_texto):
    dist_sentimiento = df_texto["sentimiento"].value_counts(normalize=True).mul(100).round(0).astype(int)
    dist_emocion = df_texto["emocion"].value_counts(normalize=True).mul(100).round(0).astype(int)
    dist_ironia = df_texto["ironia"].value_counts(normalize=True).mul(100).round(0).astype(int)
    dist_odio = df_texto["discurso_odio"].value_counts()

    tabla = pd.DataFrame({
        "Measure": ["Sentiment", "Emotion (discrete label)", "Irony (binary label)", "Hate speech (generic classifier)"],
        "Distribution": [
            ", ".join(f"{k}: {v}%" for k, v in dist_sentimiento.items()),
            ", ".join(f"{k}: {v}%" for k, v in dist_emocion.items()),
            ", ".join(f"{k}: {v}%" for k, v in dist_ironia.items()),
            ", ".join(f"{k}: {v}" for k, v in dist_odio.items()),
        ]
    })
    print(tabla.to_string(index=False))
    apa_table_png(tabla, "Table A.2. Sentiment, emotion, and irony classification — corpus-level distribution",
                  "tableA2_distribucion.png", ancho=11)


def tabla_A4(df_texto):
    """"Others"-labeled tweets vs. text sufficiency. Text length is
    measured AFTER stripping the auto-appended t.co URL -- otherwise
    almost every tweet clears the 15-character threshold on the URL
    alone."""
    others = df_texto[df_texto["emocion"] == "others"].copy()
    others["largo_sin_url"] = (
        others["full_text"].str.replace(r"https://t\.co/\S+", "", regex=True).str.strip().str.len()
    )
    others["texto_insuficiente"] = others["largo_sin_url"] < 15

    tabla = pd.DataFrame({
        "": ["Insufficient text (<15 characters)", "Substantive text present"],
        "n": [int(others["texto_insuficiente"].sum()), int((~others["texto_insuficiente"]).sum())],
        '% of "others"': [
            f"{100*others['texto_insuficiente'].mean():.1f}%",
            f"{100*(~others['texto_insuficiente']).mean():.1f}%",
        ]
    })
    print(tabla.to_string(index=False))
    apa_table_png(tabla, f'Table A.4. Cross-tabulation of the "others" emotion category (N = {len(others)})',
                  "tableA4_others.png")


def tabla_A7(df_texto):
    df_texto["menciona_casta"] = df_texto["full_text"].str.contains(r"\bcasta\b|castas", case=False, na=False, regex=True)
    df_texto["engagement"] = df_texto["likes"] + df_texto["retweets"] + df_texto["replies"] + df_texto["quotes"]

    casta = df_texto[df_texto["menciona_casta"]]
    resto = df_texto[~df_texto["menciona_casta"]]

    tabla = pd.DataFrame({
        "": ["Mean prob_anger", "Mean engagement"],
        '"Casta" tweets': [f"{casta['prob_anger'].mean():.3f}", f"{casta['engagement'].mean():,.0f}"],
        "Rest of corpus": [f"{resto['prob_anger'].mean():.3f}", f"{resto['engagement'].mean():,.0f}"],
    })
    apa_table_png(tabla, f'Table A.7. "La casta" content sub-analysis (N = {len(casta)} of {len(df_texto)} tweets)',
                  "tableA7_casta.png")
    return df_texto


def validacion_keywords(df_texto):
    """Keyword checks for immigration/class/tribalist slurs -- referenced
    in the Table A.8 discussion."""
    patrones = {
        "INMIGRACION/NACIONALIDAD": r"inmigra|migrante|extranjer|boliviano|paraguayo|peruano|venezolano|frontera|bolita|paragua",
        "CLASISMO (revisar a mano)": r"\bnegro\b|negro de m|villero|cabecita",
        "TRIBALISMO POLITICO ('kuka')": r"\bkuka\b|kukas",
    }
    for etiqueta, patron in patrones.items():
        n = df_texto["full_text"].str.contains(patron, case=False, na=False, regex=True).sum()
        print(f"  {etiqueta:35s}: {n} de {len(df_texto)} tweets")


def tabla_A8(df_texto):
    categorias = ["calls", "women", "lgbti", "racism", "class", "politics", "disabled", "appearance", "criminal"]
    filas = []
    for cat in categorias:
        col = f"prob_odio_{cat}"
        if col in df_texto.columns:
            filas.append({
                "Category": cat.upper(),
                "Corpus mean probability": f"{df_texto[col].mean():.4f}",
                "Max": f"{df_texto[col].max():.3f}",
            })

    if not filas:
        print("Table A.8: faltan columnas prob_odio_* -- correr context_hate_speech() primero.")
        return

    apa_table_png(pd.DataFrame(filas), "Table A.8. Extended hate-speech probing (context_hate_speech, 9 categories)",
                  "tableA8_odio_extendido.png")

    if "menciona_casta" not in df_texto.columns:
        df_texto["menciona_casta"] = df_texto["full_text"].str.contains(r"\bcasta\b|castas", case=False, na=False, regex=True)
    casta = df_texto[df_texto["menciona_casta"]]
    resto = df_texto[~df_texto["menciona_casta"]]
    print(f"Entre los {len(casta)} tweets con 'casta': "
          f"POLITICS={casta['prob_odio_politics'].mean():.4f} (resto: {resto['prob_odio_politics'].mean():.4f}), "
          f"CLASS={casta['prob_odio_class'].mean():.4f} (resto: {resto['prob_odio_class'].mean():.4f})")


def tabla_A9_y_estigmatizacion(df_texto):
    """Targeted validation against stigmatizing phrases identified by
    Pérez-Díaz & Arroyas Langa (2025), plus a deeper check on whether low
    hate-speech scores reflect under-detection or simple absence of the
    phrase in this corpus."""
    patrones = {
        "banda de inmorales": r"banda de inmoral",
        "casta roja": r"casta roja",
        "periodistas pauteros": r"pauter[oa]",
        "políticos rastreros": r"rastrer[oa]",
        "chorros de la política": r"chorro",
        "gobierno de delincuentes": r"gobierno.{0,20}delincuente|delincuente.{0,20}gobierno",
        "banda de corruptos": r"banda de corrupto",
    }

    filas, casos_encontrados = [], []
    for etiqueta, patron in patrones.items():
        coincidencias = df_texto[df_texto["full_text"].str.contains(patron, case=False, regex=True, na=False)]
        print(f"  {etiqueta:30s}: {len(coincidencias)} tweets")
        if len(coincidencias) > 0:
            casos_encontrados.append((etiqueta, coincidencias))
        for _, fila in coincidencias.iterrows():
            if "prob_hateful" in df_texto.columns and "prob_odio_politics" in df_texto.columns:
                filas.append({
                    "Phrase": etiqueta,
                    'Generic: "hateful" prob.': f"{fila.get('prob_hateful', float('nan')):.2f}",
                    "POLITICS": f"{fila.get('prob_odio_politics', float('nan')):.4f}",
                    "CLASS": f"{fila.get('prob_odio_class', float('nan')):.4f}",
                })

    if filas:
        tabla = pd.DataFrame(filas).sort_values('Generic: "hateful" prob.', ascending=False)
        apa_table_png(tabla, "Table A.9. Hate speech classifier output on group-stigmatizing tweets",
                      "tableA9_validacion_dirigida.png", ancho=10)
    else:
        print("Table A.9: no se encontraron coincidencias o faltan columnas prob_odio_*.")

    if not casos_encontrados:
        print("Ninguna de las frases del paper aparece en este corpus.")
        return

    from pysentimiento import create_analyzer

    analizador_generico = create_analyzer(task="hate_speech", lang="es")
    analizador_contexto = create_analyzer(task="context_hate_speech", lang="es")
    for etiqueta, coincidencias in casos_encontrados:
        print(f"\n{etiqueta} ({len(coincidencias)} tweets)")
        for _, fila in coincidencias.iterrows():
            texto = fila["full_text"]
            pred_generico = analizador_generico.predict(texto)
            pred_contexto = analizador_contexto.predict(texto)
            print(f"  {texto[:100]}")
            print(f"    hate_speech: {pred_generico.probas}")
            print(f"    POLITICS={pred_contexto.probas.get('POLITICS', float('nan')):.4f}, "
                  f"CLASS={pred_contexto.probas.get('CLASS', float('nan')):.4f}")


if __name__ == "__main__":
    df = clasificacion_base()
    df = context_hate_speech(df)
    prediccion_ironia_muestra_manual()

    tabla_A2(df)
    tabla_A4(df)
    df = tabla_A7(df)
    validacion_keywords(df)
    tabla_A8(df)
    tabla_A9_y_estigmatizacion(df)

    print(f"\nListo. Tablas en {APPENDIX_DIR}/. Tablas A.3/A.5/A.6: correr 02_engagement_model.R.")
