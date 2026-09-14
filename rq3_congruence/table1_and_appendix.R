# =============================================================================
# rq3_congruence/table1_and_appendix.R
#
# Arma la Table 1 real del cuerpo de la tesis: comparacion narrativa entre
# RQ1 (contenido digital) y RQ2 (agravios electorales), agrupada en 6
# dimensiones teoricas, cada una con su propio veredicto de congruencia.
# Termina con un resumen de todo el apendice de tablas (A.2-A.14 + Table 1).
#
# Esto NO recalcula ningun modelo. Requiere, en este orden, EN LA MISMA
# sesion de R:
#   1. rq1_digital_content/02_engagement_model.R  -> ranking_rq1, etiquetas_rq1
#   2. rq2_electoral_grievances/02_grievance_model.R -> ranking_rq2, etiquetas_rq2
#
# Correr con la raiz del repo como working directory.
#
# TITULO: "Theory-guided cross-domain comparison for RQ3" (pedido de
# Javi, reemplaza el titulo original "Congruence between RQ1 content and
# RQ2 grievance dimensions" -- confirmar que el documento final de la
# tesis tambien refleje este cambio).
#
# NOTA IMPORTANTE: la agrupacion tematica (que variable de RQ2 va con
# que variable de RQ1) y el veredicto de cada fila son DECISIONES
# EDITORIALES DE PALOMA, no un calculo automatico -- se definen abajo en
# `definicion_tabla1`, a mano, y son editables. Lo unico que se toma en
# vivo de los modelos son los numeros (Rank, beta_std, p): si se
# reestima algun modelo, la tabla se actualiza sola sin reescribir el
# texto de interpretacion.
#
# CONFIRMADO: prob_ironic NO tiene contraparte en RQ2 (fila "Rhetorical
# style" -> "No directly equivalent CNEP construct"). Un mapeo anterior
# que emparejaba ironia con confianza_medios (media trust) fue
# descartado -- no corresponde a la version final de la tesis.
# =============================================================================

library(dplyr)
library(tibble)

source("R/table_helpers.R")   # apa_table(), save_png(), formatear_con_estrellas()

if (!exists("ranking_rq1") || !exists("etiquetas_rq1")) {
  stop("Falta ranking_rq1/etiquetas_rq1 -- correr rq1_digital_content/02_engagement_model.R primero, en esta misma sesion.")
}
if (!exists("ranking_rq2") || !exists("etiquetas_rq2")) {
  stop("Falta ranking_rq2/etiquetas_rq2 -- correr rq2_electoral_grievances/02_grievance_model.R primero, en esta misma sesion.")
}

# =============================================================================
# Helpers para formatear una variable individual como texto de celda
# =============================================================================

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

# =============================================================================
# Definicion de las 6 filas (agrupacion tematica + veredicto = decision
# editorial de Paloma, no calculo automatico)
# =============================================================================

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
    if (!is.null(fila_def$rq2_extra_texto)) {
      partes <- c(partes, fila_def$rq2_extra_texto)
    }
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

tabla_1_df <- bind_rows(lapply(definicion_tabla1, armar_fila_tabla1))

cat("=== Table 1 (data frame) ===\n")
print(tabla_1_df, width = Inf)

# =============================================================================
# TITULO ACTUALIZADO (pedido de Javi): "Congruence" presuponia el
# resultado; "Theory-guided cross-domain comparison" describe lo que la
# tabla efectivamente evalua, sin presuponerlo.
# =============================================================================

apa_table(
  tabla_1_df, "Table 1. Theory-guided cross-domain comparison for RQ3",
  "Source: own elaboration. Both effects are standardized coefficients (beta_std). RQ1: N = 360 tweets; RQ2: N = 851 social-media users."
) |>
  fmt_markdown(columns = c(`RQ1: digital communication`, `RQ2: electoral attitudes`, `RQ3 assessment`)) |>
  save_png("table1_congruencia_rq1_rq2.png")

# =============================================================================
# Resumen del apendice completo -- chequea que appendix_tables/ tenga
# todas las imagenes esperadas, generadas por los tres scripts previos.
# Util como ultimo paso antes de armar el documento final: si algo falta
# ahi, significa que algun script anterior no se corrio o fallo antes de
# llegar al save_png() correspondiente.
# =============================================================================

tablas_esperadas <- c(
  "tableA2_distribucion.png"          = "Table A.2 (01_sentiment_classification.py)",
  "tableA3_validacion_ironia.png"     = "Table A.3 (02_engagement_model.R)",
  "tableA4_others.png"                = "Table A.4 (01_sentiment_classification.py)",
  "tableA5_nb_engagement.png"         = "Table A.5 (02_engagement_model.R)",
  "tableA6_ranking_rq1.png"           = "Table A.6 (02_engagement_model.R)",
  "tableA7_casta.png"                 = "Table A.7 (01_sentiment_classification.py)",
  "tableA8_odio_extendido.png"        = "Table A.8 (01_sentiment_classification.py)",
  "tableA9_validacion_dirigida.png"   = "Table A.9 (01_sentiment_classification.py)",
  "tableA11_modelos_ABC.png"          = "Table A.11 (02_grievance_model.R)",
  "tableA12_h2_robustez.png"          = "Table A.12 (02_grievance_model.R)",
  "tableA13_modelo_c_solo_redes.png"  = "Table A.13 (02_grievance_model.R)",
  "tableA14_perfil_4_grupos.png"      = "Table A.14 (02_grievance_model.R)",
  "table1_congruencia_rq1_rq2.png"    = "Table 1   (este script)"
)

cat("\n\n############################################################\n")
cat("RESUMEN DEL APENDICE -- appendix_tables/\n")
cat("############################################################\n")

for (archivo in names(tablas_esperadas)) {
  existe <- file.exists(file.path("appendix_tables", archivo))
  marca <- if (existe) "OK  " else "FALTA"
  cat(sprintf("[%s] %-35s -- %s\n", marca, archivo, tablas_esperadas[archivo]))
}

faltantes <- names(tablas_esperadas)[!file.exists(file.path("appendix_tables", names(tablas_esperadas)))]
if (length(faltantes) > 0) {
  cat("\n>>> Faltan", length(faltantes), "tabla(s). Revisar que los tres scripts",
      "\n>>> (01_sentiment_classification.py, 02_engagement_model.R,",
      "\n>>> 02_grievance_model.R) hayan corrido completos, sin errores,",
      "\n>>> antes de este.\n")
} else {
  cat("\n>>> Las 13 tablas del apendice (A.2-A.14 + Table 1) estan completas.\n")
}

cat("\n\n############################################################\n")
cat("table1_and_appendix.R complete.\n")
cat("Recordatorio: actualizar el titulo de Table 1 en el documento final\n")
cat("de la tesis a 'Theory-guided cross-domain comparison for RQ3'.\n")
cat("############################################################\n")
