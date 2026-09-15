# rq3_congruence/table1_and_appendix.R
#
# Builds Table 1: the theory-guided comparison between RQ1 (digital
# content) and RQ2 (electoral grievances), grouped into 6 dimensions
# with a congruence verdict per row. Ends with a completeness check of
# the full appendix (A.2-A.14 + Table 1).
#
# Doesn't fit any model. Requires, in this order, in the SAME R session:
#   1. rq1_digital_content/02_engagement_model.R  -> ranking_rq1, etiquetas_rq1
#   2. rq2_electoral_grievances/02_grievance_model.R -> ranking_rq2, etiquetas_rq2
#
# Run from the repo root.

library(dplyr)
library(tibble)

source("R/table_helpers.R")   # apa_table(), save_png(), formatear_con_estrellas()

if (!exists("ranking_rq1") || !exists("etiquetas_rq1")) {
  stop("Falta ranking_rq1/etiquetas_rq1 -- correr rq1_digital_content/02_engagement_model.R primero, en esta misma sesion.")
}
if (!exists("ranking_rq2") || !exists("etiquetas_rq2")) {
  stop("Falta ranking_rq2/etiquetas_rq2 -- correr rq2_electoral_grievances/02_grievance_model.R primero, en esta misma sesion.")
}

formatear_variable_rq1 <- function(var) {
  fila <- ranking_rq1[ranking_rq1$variable == var, ]
  sprintf("**%s** — Rank %d; β_std = %.3f; p = %s",
          etiquetas_rq1[var], fila$rank, fila$beta_estandarizado,
          ifelse(fila$p_valor < .001, "< .001", sprintf("%.3f", fila$p_valor)))
}

formatear_variable_rq2 <- function(var) {
  fila <- ranking_rq2[ranking_rq2$variable == var, ]
  sprintf("**%s** — Rank %d; β_std = %.3f; p = %s",
          etiquetas_rq2[var], fila$rank, fila$beta_estandarizado,
          ifelse(fila$p_valor < .001, "< .001", sprintf("%.3f", fila$p_valor)))
}

# Thematic grouping and verdict per row are editorial decisions, not a
# calculation -- edit here. Only the numbers (rank, beta_std, p) are
# pulled live from the models, so re-estimating them updates the table
# without touching this text. Irony has no RQ2 counterpart by design.
definicion_tabla1 <- list(
  list(
    dimension = "Institutional / antisystem grievance",
    rq1_vars = c("prob_anger"),
    rq2_vars = c("desc_gob_saliente", "antipluralismo", "compromiso_democratico"),
    rq2_extra_texto = "Political indifference and perceptions that politicians favor the rich rank lower and are not significant.",
    veredicto = "**Partial correspondence.** The clearest cross-domain pattern is concentrated around institutional and antisystem attitudes."
  ),
  list(
    dimension = "Economic insecurity",
    rq1_vars = c("prob_fear"),
    rq2_vars = c("econ_pais", "econ_personal"),
    rq2_extra_texto = NULL,
    veredicto = "**No positive evidence of correspondence.** Neither side emerges as substantively prominent."
  ),
  list(
    dimension = "Broader negative affect",
    rq1_vars = c("prob_neg"),
    rq2_vars = character(0),
    rq2_extra_texto = NULL,
    rq2_texto_manual = "No unique equivalent survey construct.",
    veredicto = "**Secondary indicator only; no one-to-one test.**"
  ),
  list(
    dimension = "Rhetorical style",
    rq1_vars = c("prob_ironic"),
    rq2_vars = character(0),
    rq2_extra_texto = NULL,
    rq2_texto_manual = "No directly equivalent CNEP construct.",
    veredicto = "**Unmatched on the electoral side.**"
  ),
  list(
    dimension = "Economic ideology",
    rq1_vars = character(0),
    rq1_texto_manual = "No direct RQ1 counterpart",
    rq2_vars = c("privado_vs_pub"),
    rq2_extra_texto = NULL,
    veredicto = "**Unmatched on the communication side.**"
  ),
  list(
    dimension = "Political socialization",
    rq1_vars = character(0),
    rq1_texto_manual = "No direct RQ1 counterpart",
    rq2_vars = c("familia_pol"),
    rq2_extra_texto = NULL,
    veredicto = "**Unmatched on the communication side.**"
  )
)

armar_fila_tabla1 <- function(fila_def) {
  texto_rq1 <- if (length(fila_def$rq1_vars) > 0) {
    paste(sapply(fila_def$rq1_vars, formatear_variable_rq1), collapse = ". ")
  } else {
    fila_def$rq1_texto_manual
  }

  texto_rq2 <- if (length(fila_def$rq2_vars) > 0) {
    partes <- sapply(fila_def$rq2_vars, formatear_variable_rq2)
    if (!is.null(fila_def$rq2_extra_texto)) partes <- c(partes, fila_def$rq2_extra_texto)
    paste(partes, collapse = ". ")
  } else {
    fila_def$rq2_texto_manual
  }

  tibble(
    `Cross-domain dimension` = fila_def$dimension,
    `RQ1: digital communication` = texto_rq1,
    `RQ2: electoral attitudes` = texto_rq2,
    `RQ3 assessment` = fila_def$veredicto
  )
}

generar_tabla1 <- function() {
  tabla <- bind_rows(lapply(definicion_tabla1, armar_fila_tabla1))
  print(tabla, width = Inf)

  apa_table(
    tabla, "Table 1. Theory-guided cross-domain comparison for RQ3",
    "Source: own elaboration. Both effects are standardized coefficients (beta_std). RQ1: N = 360 tweets; RQ2: N = 851 social-media users."
  ) |>
    fmt_markdown(columns = c(`RQ1: digital communication`, `RQ2: electoral attitudes`, `RQ3 assessment`)) |>
    save_png("table1_congruencia_rq1_rq2.png")

  tabla
}

resumen_apendice <- function() {
  # Confirms appendix_tables/ has all 13 images -- if any is missing, an
  # earlier script didn't run to completion.
  tablas_esperadas <- c(
    "tableA2_distribucion.png"         = "Table A.2 (01_sentiment_classification.py)",
    "tableA3_validacion_ironia.png"    = "Table A.3 (02_engagement_model.R)",
    "tableA4_others.png"               = "Table A.4 (01_sentiment_classification.py)",
    "tableA5_nb_engagement.png"        = "Table A.5 (02_engagement_model.R)",
    "tableA6_ranking_rq1.png"          = "Table A.6 (02_engagement_model.R)",
    "tableA7_casta.png"                = "Table A.7 (01_sentiment_classification.py)",
    "tableA8_odio_extendido.png"       = "Table A.8 (01_sentiment_classification.py)",
    "tableA9_validacion_dirigida.png"  = "Table A.9 (01_sentiment_classification.py)",
    "tableA11_modelos_ABC.png"         = "Table A.11 (02_grievance_model.R)",
    "tableA12_h2_robustez.png"         = "Table A.12 (02_grievance_model.R)",
    "tableA13_modelo_c_solo_redes.png" = "Table A.13 (02_grievance_model.R)",
    "tableA14_perfil_4_grupos.png"     = "Table A.14 (02_grievance_model.R)",
    "table1_congruencia_rq1_rq2.png"   = "Table 1 (este script)"
  )

  for (archivo in names(tablas_esperadas)) {
    marca <- if (file.exists(file.path("appendix_tables", archivo))) "OK   " else "FALTA"
    cat(sprintf("[%s] %-35s -- %s\n", marca, archivo, tablas_esperadas[archivo]))
  }

  faltantes <- names(tablas_esperadas)[!file.exists(file.path("appendix_tables", names(tablas_esperadas)))]
  if (length(faltantes) > 0) {
    cat(sprintf("\nFaltan %d tabla(s) -- revisar que los tres scripts previos hayan corrido completos.\n", length(faltantes)))
  } else {
    cat("\nLas 13 tablas del apendice estan completas.\n")
  }
}

tabla_1_df <- generar_tabla1()
resumen_apendice()
