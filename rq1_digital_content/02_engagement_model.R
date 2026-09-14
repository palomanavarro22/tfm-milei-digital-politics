# =============================================================================
# rq1_digital_content/02_engagement_model.R
#
# Lado de R de RQ1: parte de data/processed/milei_tweets_con_sentimiento.csv
# (ya generado por 01_sentiment_classification.py) y produce:
#   PART 1 -- Carga y construccion de variables (engagement, controles)
#   PART 2 -- Table A.3: validacion de ironia (codificacion manual vs. modelo)
#   PART 3 -- Table A.5: regresion Binomial Negativa (IRR, sin estandarizar)
#   PART 4 -- Table A.6: ranking estandarizado de las 5 variables de contenido
#             -- produce `ranking_rq1` y `etiquetas_rq1`, que alimentan
#             Table 1 en rq3_congruence/table1_and_appendix.R
#
# Tablas A.2, A.4, A.7, A.8, A.9 se generan del lado de Python
# (01_sentiment_classification.py) -- no se duplican aca.
#
# Correr con la raiz del repo como working directory.
#
# Requiere: dplyr, MASS, broom, gt (gt::gtsave necesita el paquete webshot2
# instalado una vez: install.packages("webshot2"))
# =============================================================================

library(dplyr)
library(MASS)     # glm.nb()
library(broom)

select <- dplyr::select   # MASS::select() enmascara dplyr::select() al cargar MASS

source("R/table_helpers.R")   # apa_table(), save_png(), formatear_con_estrellas()

# =============================================================================
# PART 1 -- Cargar corpus clasificado y construir variables
# =============================================================================

cat("\n\n############################################################\n")
cat("PART 1 -- Cargar corpus y construir variables\n")
cat("############################################################\n")

df_completo <- read.csv("data/processed/milei_tweets_con_sentimiento.csv", stringsAsFactors = FALSE,
                        colClasses = c(tweet_id = "character"))
df_completo$created_at <- as.POSIXct(df_completo$created_at, tz = "UTC")

df_completo <- df_completo |>
  mutate(
    engagement = likes + retweets + replies + quotes,
    tiene_imagen = as.integer(n_media > 0),
    es_reply = as.integer(is_reply == "True"),
    largo_texto = nchar(full_text),
    dias_desde_inicio = as.numeric(difftime(created_at, min(created_at, na.rm = TRUE), units = "days")),
    menciona_casta = grepl("\\bcasta\\b|castas", full_text, ignore.case = TRUE)
  )

cat(sprintf("Corpus completo: %d tweets\n", nrow(df_completo)))
cat(sprintf("Tweets con n_media faltante (NA): %d\n", sum(is.na(df_completo$n_media))))

# Muestra para el modelo de engagement (excluye los que no tienen metadata de imagen)
df <- df_completo |> filter(!is.na(tiene_imagen))
cat(sprintf("Tweets tras excluir metadata faltante de imagen: %d\n\n", nrow(df)))

etiquetas_rq1 <- c(
  prob_anger = "Anger probability", prob_fear = "Fear probability",
  prob_pos = "Positive sentiment", prob_neg = "Negative sentiment",
  prob_ironic = "Irony probability", tiene_imagen = "Has image",
  es_reply = "Is reply", largo_texto = "Text length",
  dias_desde_inicio = "Days since window start", `(Intercept)` = "Intercept"
)

# =============================================================================
# PART 2 -- Table A.3: validacion de ironia (codificacion manual vs. modelo)
#
# Cruza muestra_codificacion_manual.csv (codificacion humana) contra la
# columna `ironia` YA CALCULADA en df_completo (Part 1 del script de
# Python), por tweet_id.
#
# VERIFICADO (13-sep-2026): esta columna y la prediccion separada de
# 01_sentiment_classification.py Part 3 (predicciones_pysentimiento_muestra.csv)
# coinciden 80/80, diferencia maxima = 0.000000 -- son identicas, no hay
# ambiguedad sobre cual fuente usar aca.
# =============================================================================

cat("\n\n############################################################\n")
cat("PART 2 -- Table A.3 (validacion de ironia)\n")
cat("############################################################\n")

manual_ironia <- read.csv2("data/raw/muestra_codificacion_manual.csv", stringsAsFactors = FALSE,
                           colClasses = c(tweet_id = "character"), sep = ";")
manual_ironia$codigo_manual_norm <- tolower(trimws(manual_ironia$codigo_manual))

fusion_ironia <- manual_ironia |>
  left_join(df_completo |> dplyr::select(tweet_id, ironia), by = "tweet_id") |>
  mutate(ironia_norm = tolower(trimws(ironia)))

n_validacion <- nrow(fusion_ironia)
tabla_confusion <- table(fusion_ironia$codigo_manual_norm, fusion_ironia$ironia_norm)

po <- sum(diag(tabla_confusion)) / sum(tabla_confusion)
p_manual_ironic <- mean(fusion_ironia$codigo_manual_norm == "ironic")
p_model_ironic <- mean(fusion_ironia$ironia_norm == "ironic")
pe <- p_manual_ironic * p_model_ironic + (1 - p_manual_ironic) * (1 - p_model_ironic)
kappa <- (po - pe) / (1 - pe)

tabla_A3_df <- tibble::tibble(
  ` ` = c("Manual: Not ironic", "Manual: Ironic"),
  `Model: Not ironic` = c(tabla_confusion["not ironic", "not ironic"], tabla_confusion["ironic", "not ironic"]),
  `Model: Ironic` = c(tabla_confusion["not ironic", "ironic"], tabla_confusion["ironic", "ironic"])
)

cat(sprintf("N=%d, Po=%.4f, Pe=%.4f, Kappa=%.4f\n", n_validacion, po, pe, kappa))
print(tabla_A3_df)

apa_table(
  tabla_A3_df,
  sprintf("Table A.3. Irony validation — manual coding vs. model classification (N = %d)", n_validacion),
  sprintf("Source: own elaboration. Po = %.4f, Pe = %.4f, Cohen's Kappa = %.4f.", po, pe, kappa)
) |> save_png("tableA3_validacion_ironia.png")

# =============================================================================
# PART 3 -- Table A.5: regresion Binomial Negativa (IRR, sin estandarizar)
# =============================================================================

cat("\n\n############################################################\n")
cat("PART 3 -- Table A.5 (modelo Binomial Negativa)\n")
cat("############################################################\n")

modelo_nb <- glm.nb(
  engagement ~ prob_anger + prob_fear + prob_pos + prob_neg + prob_ironic +
    tiene_imagen + es_reply + largo_texto + dias_desde_inicio,
  data = df
)

cat(sprintf("N del modelo (glm.nb excluye NA automaticamente): %d\n\n", nobs(modelo_nb)))
print(summary(modelo_nb))

tabla_A5_df <- tidy(modelo_nb, exponentiate = TRUE) |>
  mutate(Predictor = etiquetas_rq1[term],
         IRR = formatear_con_estrellas(estimate, p.value)) |>
  transmute(Predictor, IRR)

apa_table(
  tabla_A5_df, "Table A.5. Negative Binomial regression of engagement (N = 360)",
  "Source: own elaboration. IRR = incidence rate ratio. *** p<.001, ** p<.01, * p<.05, \u2020 p<.1"
) |> save_png("tableA5_nb_engagement.png")

# =============================================================================
# PART 4 -- Table A.6: ranking estandarizado de las 5 variables de contenido
#
# Usa scale() simple (NO svymean/svyvar como en RQ2) porque el corpus de
# 450 tweets no tiene diseno muestral/pesos de encuesta -- es la muestra
# completa (o su recorte por missingness), no una muestra probabilistica
# de una poblacion mas grande. Esta asimetria con RQ2 (que SI usa pesos
# svy) es intencional, no un error -- documentada tambien en
# rq2_electoral_grievances/02_grievance_model.R.
# =============================================================================

cat("\n\n############################################################\n")
cat("PART 4 -- Table A.6 (ranking estandarizado)\n")
cat("############################################################\n")

vars_contenido_rq1 <- c("prob_anger", "prob_fear", "prob_pos", "prob_neg", "prob_ironic")

df_z <- df |>
  mutate(across(all_of(vars_contenido_rq1), ~ as.numeric(scale(.x)), .names = "{.col}_z"))

modelo_nb_z <- glm.nb(
  engagement ~ prob_anger_z + prob_fear_z + prob_pos_z + prob_neg_z + prob_ironic_z +
    tiene_imagen + es_reply + largo_texto + dias_desde_inicio,
  data = df_z
)

vars_contenido_z <- paste0(vars_contenido_rq1, "_z")
coefs_contenido_z <- summary(modelo_nb_z)$coefficients[vars_contenido_z, ]

ranking_rq1 <- data.frame(
  variable = vars_contenido_rq1,
  beta_estandarizado = coefs_contenido_z[, "Estimate"],
  magnitud_efecto = abs(coefs_contenido_z[, "Estimate"]),
  p_valor = coefs_contenido_z[, "Pr(>|z|)"]
) |>
  arrange(desc(magnitud_efecto)) |>
  mutate(rank = row_number())

cat("=== Standardized ranking (Table A.6 / Table 1 order) ===\n")
print(ranking_rq1, row.names = FALSE, digits = 3)

irr_sin_estandarizar <- tidy(modelo_nb, exponentiate = TRUE) |>
  filter(term %in% vars_contenido_rq1) |>
  transmute(variable = term, IRR = sprintf("%.2f", estimate), p = sprintf("%.4f", p.value))

tabla_A6_df <- ranking_rq1 |>
  left_join(irr_sin_estandarizar, by = "variable") |>
  arrange(rank) |>
  transmute(Rank = rank, Predictor = unname(etiquetas_rq1[variable]), IRR, p)

apa_table(
  tabla_A6_df, "Table A.6. Ranking of content variables by effect magnitude",
  "Source: own elaboration. Ranked by standardized effect magnitude; IRR shown is from the original (unstandardized) model."
) |> save_png("tableA6_ranking_rq1.png")

cat("\n\n############################################################\n")
cat("02_engagement_model.R complete.\n")
cat("`ranking_rq1` y `etiquetas_rq1` quedan en el entorno -- para Table 1,\n")
cat("segui con rq2_electoral_grievances/02_grievance_model.R y despues\n")
cat("rq3_congruence/table1_and_appendix.R, EN LA MISMA SESION de R.\n")
cat("############################################################\n")
