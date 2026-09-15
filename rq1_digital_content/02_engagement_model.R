# rq1_digital_content/02_engagement_model.R
#
# Negative Binomial model of tweet engagement (RQ1). Reads the corpus
# classified by 01_sentiment_classification.py and produces Tables
# A.3, A.5, A.6. Leaves `ranking_rq1`/`etiquetas_rq1` in the session,
# needed by rq3_congruence/table1_and_appendix.R.
#
# Run from the repo root.
# Requires: dplyr, MASS, broom, gt (gt::gtsave needs webshot2 installed
# once: install.packages("webshot2"))

library(dplyr)
library(MASS)     # glm.nb()
library(broom)

select <- dplyr::select   # MASS::select() masks dplyr::select()

source("R/table_helpers.R")   # apa_table(), save_png(), formatear_con_estrellas()

etiquetas_rq1 <- c(
  prob_anger = "Anger probability", prob_fear = "Fear probability",
  prob_pos = "Positive sentiment", prob_neg = "Negative sentiment",
  prob_ironic = "Irony probability", tiene_imagen = "Has image",
  es_reply = "Is reply", largo_texto = "Text length",
  dias_desde_inicio = "Days since window start", `(Intercept)` = "Intercept"
)

cargar_datos <- function() {
  # Loads the classified corpus and builds the model's variables
  # (image flag, reply flag, text length, days since window start).
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

  cat(sprintf("Corpus: %d tweets (%d sin metadata de imagen)\n",
              nrow(df_completo), sum(is.na(df_completo$n_media))))
  df_completo
}

tabla_A3 <- function(df_completo) {
  # Irony validation: manual coding vs. model classification (Cohen's kappa).
  manual_ironia <- read.csv2("data/raw/muestra_codificacion_manual.csv", stringsAsFactors = FALSE,
                             colClasses = c(tweet_id = "character"), sep = ";")
  manual_ironia$codigo_manual_norm <- tolower(trimws(manual_ironia$codigo_manual))

  fusion <- manual_ironia |>
    left_join(df_completo |> select(tweet_id, ironia), by = "tweet_id") |>
    mutate(ironia_norm = tolower(trimws(ironia)))

  tabla_confusion <- table(fusion$codigo_manual_norm, fusion$ironia_norm)
  po <- sum(diag(tabla_confusion)) / sum(tabla_confusion)
  p_manual <- mean(fusion$codigo_manual_norm == "ironic")
  p_modelo <- mean(fusion$ironia_norm == "ironic")
  pe <- p_manual * p_modelo + (1 - p_manual) * (1 - p_modelo)
  kappa <- (po - pe) / (1 - pe)

  tabla <- tibble::tibble(
    ` ` = c("Manual: Not ironic", "Manual: Ironic"),
    `Model: Not ironic` = c(tabla_confusion["not ironic", "not ironic"], tabla_confusion["ironic", "not ironic"]),
    `Model: Ironic` = c(tabla_confusion["not ironic", "ironic"], tabla_confusion["ironic", "ironic"])
  )

  cat(sprintf("Table A.3: N=%d, Kappa=%.4f\n", nrow(fusion), kappa))
  apa_table(
    tabla,
    sprintf("Table A.3. Irony validation — manual coding vs. model classification (N = %d)", nrow(fusion)),
    sprintf("Source: own elaboration. Po = %.4f, Pe = %.4f, Cohen's Kappa = %.4f.", po, pe, kappa)
  ) |> save_png("tableA3_validacion_ironia.png")
}

modelo_engagement <- function(df) {
  # Negative Binomial regression of engagement on emotional/rhetorical
  # content plus formatting controls (image, reply, length, timing).
  glm.nb(
    engagement ~ prob_anger + prob_fear + prob_pos + prob_neg + prob_ironic +
      tiene_imagen + es_reply + largo_texto + dias_desde_inicio,
    data = df
  )
}

tabla_A5 <- function(modelo_nb) {
  # Unstandardized IRRs, for substantive interpretation.
  tabla <- tidy(modelo_nb, exponentiate = TRUE) |>
    mutate(Predictor = etiquetas_rq1[term], IRR = formatear_con_estrellas(estimate, p.value)) |>
    transmute(Predictor, IRR)

  apa_table(
    tabla, sprintf("Table A.5. Negative Binomial regression of engagement (N = %d)", nobs(modelo_nb)),
    "Source: own elaboration. IRR = incidence rate ratio. *** p<.001, ** p<.01, * p<.05, \u2020 p<.1"
  ) |> save_png("tableA5_nb_engagement.png")
}

ranking_estandarizado <- function(df, modelo_nb) {
  # Standardizes the 5 content variables with scale(): plain mean/SD is
  # correct here since this corpus has no survey design or weights
  # (unlike RQ2's CNEP model, which uses svymean()/svyvar() instead).
  # Returns the ranking used for Table A.6 and for Table 1 in RQ3.
  vars_contenido <- c("prob_anger", "prob_fear", "prob_pos", "prob_neg", "prob_ironic")

  df_z <- df |> mutate(across(all_of(vars_contenido), ~ as.numeric(scale(.x)), .names = "{.col}_z"))

  modelo_z <- glm.nb(
    engagement ~ prob_anger_z + prob_fear_z + prob_pos_z + prob_neg_z + prob_ironic_z +
      tiene_imagen + es_reply + largo_texto + dias_desde_inicio,
    data = df_z
  )

  vars_z <- paste0(vars_contenido, "_z")
  coefs_z <- summary(modelo_z)$coefficients[vars_z, ]

  ranking <- data.frame(
    variable = vars_contenido,
    beta_estandarizado = coefs_z[, "Estimate"],
    magnitud_efecto = abs(coefs_z[, "Estimate"]),
    p_valor = coefs_z[, "Pr(>|z|)"]
  ) |> arrange(desc(magnitud_efecto)) |> mutate(rank = row_number())

  irr_crudo <- tidy(modelo_nb, exponentiate = TRUE) |>
    filter(term %in% vars_contenido) |>
    transmute(variable = term, IRR = sprintf("%.2f", estimate), p = sprintf("%.4f", p.value))

  tabla_A6_df <- ranking |>
    left_join(irr_crudo, by = "variable") |>
    arrange(rank) |>
    transmute(Rank = rank, Predictor = unname(etiquetas_rq1[variable]), IRR, p)

  apa_table(
    tabla_A6_df, "Table A.6. Ranking of content variables by effect magnitude",
    "Source: own elaboration. Ranked by standardized effect magnitude; IRR shown is from the original (unstandardized) model."
  ) |> save_png("tableA6_ranking_rq1.png")

  ranking
}

df_completo <- cargar_datos()
df <- df_completo |> filter(!is.na(tiene_imagen))

tabla_A3(df_completo)

modelo_nb <- modelo_engagement(df)
print(summary(modelo_nb))
tabla_A5(modelo_nb)

ranking_rq1 <- ranking_estandarizado(df, modelo_nb)
print(ranking_rq1, row.names = FALSE, digits = 3)

cat("\n02_engagement_model.R listo. `ranking_rq1`/`etiquetas_rq1` quedan en el entorno --",
    "\nseguir con 02_grievance_model.R y despues table1_and_appendix.R, en la misma sesion.\n")
