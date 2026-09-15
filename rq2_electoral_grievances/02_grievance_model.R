# rq2_electoral_grievances/02_grievance_model.R
#
# RQ2: does social media use for campaign information predict Milei's
# vote beyond classical predictors (H2/H3)? Which grievances predict
# support among social media users (for RQ3)?
#
# Run from the repo root. Requires the cleaned dataset
# (data/processed/cnep_argentina2023_limpio.rds), produced by
# 01_cnep_exploration.qmd. The raw Excel is only read for the code-90
# exclusion diagnostic.

library(dplyr)
library(survey)

source("R/table_helpers.R")   # apa_table(), save_png(), formatear_con_estrellas()

vars_agravio_rq2 <- c("econ_pais", "econ_personal", "desc_gob_saliente", "confianza_gob",
                      "confianza_medios", "pols_defienden_ricos", "desc_democracia",
                      "antipluralismo", "compromiso_democratico", "comprension_pol",
                      "resentimiento_gob", "familia_pol", "amigos_pol",
                      "equidad_vs_desig", "orden_vs_libertad", "privado_vs_pub")

vars_control <- c("ideologia", "edad", "genero", "ingreso", "educacion", "clase_subjetiva",
                  "interes_politico", "conocimiento", "diarios", "radio", "tv", "region", "zona")

etiquetas_rq2 <- c(
  ideologia = "Ideology", edad = "Age", genero = "Gender", ingreso = "Income",
  educacion = "Education", clase_subjetiva = "Subjective class",
  interes_politico = "Political interest", conocimiento = "Political knowledge",
  diarios = "Newspapers", radio = "Radio", tv = "TV", region = "Region", zona = "Zone",
  indice_redes_sociales = "Social media index", uso_twitter = "Twitter/X use",
  econ_pais = "National economy", econ_personal = "Personal economy",
  desc_gob_saliente = "Outgoing govt. dissatisfaction", confianza_gob = "Government trust",
  confianza_medios = "Media trust", pols_defienden_ricos = "Politicians favor rich",
  desc_democracia = "Democracy dissatisfaction", antipluralismo = "Antipluralism",
  compromiso_democratico = "Democratic commitment", comprension_pol = "Political understanding",
  resentimiento_gob = "Political indifference", familia_pol = "Family political talk",
  amigos_pol = "Friends political talk", equidad_vs_desig = "Equality vs. initiative",
  orden_vs_libertad = "Order vs. liberties", privado_vs_pub = "Privatization preference",
  `(Intercept)` = "Intercept"
)

recodificar_plataforma <- function(x) {
  # 995 = did not use this platform.
  x <- if_else(x == 995, 0, x)
  as.numeric(x > 0)
}

cargar_datos <- function() {
  # Loads the cleaned CNEP dataset, builds the outcome, the social-media
  # index, and renames the grievance + control variables by theme.
  cnep <- readRDS("data/processed/cnep_argentina2023_limpio.rds")
  cat(sprintf("Dataset: %d rows x %d columns\n", nrow(cnep), ncol(cnep)))

  cnep <- cnep |>
    mutate(voto_milei = case_when(
      `H.VoteWhichRecent2.Z` == 11 ~ 1,
      `H.VoteWhichRecent2.Z` %in% c(1, 10, 12, 13) ~ 0,
      TRUE ~ NA_real_  # 993=DK and any other code left as NA
    ))

  variables_redes_sociales <- c("D.Platform_2_3", "D.Platform_2_4", "D.Platform_2_5",
                                "D.Platform_2_6", "D.Platform_2_7")  # Facebook/Twitter/WhatsApp/Instagram/TikTok

  cnep <- cnep |>
    mutate(across(all_of(variables_redes_sociales), recodificar_plataforma, .names = "{.col}_bin")) |>
    mutate(
      indice_redes_sociales = rowSums(across(ends_with("_bin")), na.rm = FALSE),
      uso_alguna_red = as.integer(indice_redes_sociales > 0),
      uso_twitter = recodificar_plataforma(`D.Platform_2_4`)
    )

  cnep |>
    mutate(
      ideologia = `C.LRSelf_2`, edad = EDAD, genero = GÉNERO_NUMÉRICO,
      ingreso = `L.Income.Z`, educacion = L.Education, clase_subjetiva = L.SubjClass,
      interes_politico = `H.Interest_2`, conocimiento = H.InfoTest,
      diarios = `D.CamPaper_2`, radio = `D.CamRadio_2`, tv = `D.CamTV_2`,
      region = regiones, zona = tipozona,
      econ_pais = `A.EconSit_2`, econ_personal = `A.Respsit_2`,
      desc_gob_saliente = `A.GovPerf_2`, confianza_gob = `B.TrustGov_2`,
      confianza_medios = `B.TrustMedia_2`, pols_defienden_ricos = `B.PolsDefendRich_2`,
      desc_democracia = `B.DemSat_2`, antipluralismo = `B.OneParty_2`,
      compromiso_democratico = `B.DemAuth_2`, comprension_pol = `B.PolCompl_2`,
      resentimiento_gob = `B.DontCare_2`, familia_pol = `E.FamTalk_2`,
      amigos_pol = `E.FriendTalk_2`, equidad_vs_desig = `J.EqualInd_2`,
      orden_vs_libertad = `J.OrderLib_2`, privado_vs_pub = `J.PrivPub2_2`
    )
}

chequear_variables <- function(cnep) {
  # No grievance variable should still have the undocumented missing
  # code 90 (or 93-99, 998, 999) as its max after cleaning.
  vars_check <- c("voto_milei", "indice_redes_sociales", vars_control, vars_agravio_rq2, "L.WtWithin")
  for (v in vars_check) {
    cat(sprintf("%-22s NA: %5d of %d\n", v, sum(is.na(cnep[[v]])), nrow(cnep)))
  }
  for (v in vars_agravio_rq2) {
    maximo <- max(cnep[[v]], na.rm = TRUE)
    alerta <- if (maximo %in% c(90, 93:99, 998, 999)) " <<< NOT FIXED" else ""
    cat(sprintf("%-22s max=%.1f%s\n", v, maximo, alerta))
  }
}

construir_muestra_comun <- function(cnep) {
  # Common analytical sample (listwise deletion) across all Model A/B/C
  # predictors, so AIC comparisons aren't confounded by sample changes.
  variables_modelo <- c("voto_milei", vars_control, "indice_redes_sociales", vars_agravio_rq2)
  cnep |>
    filter(!is.na(`L.WtWithin`)) |>
    filter(if_all(all_of(variables_modelo), ~ !is.na(.)))
}

modelos_abc <- function(disenio) {
  # Model A: classical predictors. B: + social media index (H2/H3).
  # C: + 16 grievances (for RQ3).
  modelo_A <- svyglm(
    voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
      interes_politico + conocimiento + diarios + radio + tv + region + zona,
    design = disenio, family = quasibinomial()
  )
  modelo_B <- svyglm(
    voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
      interes_politico + conocimiento + diarios + radio + tv + region + zona +
      indice_redes_sociales,
    design = disenio, family = quasibinomial()
  )
  modelo_C <- svyglm(
    voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
      interes_politico + conocimiento + diarios + radio + tv + region + zona +
      indice_redes_sociales + econ_pais + econ_personal +
      desc_gob_saliente + confianza_gob + confianza_medios + pols_defienden_ricos +
      desc_democracia + antipluralismo + compromiso_democratico +
      comprension_pol + resentimiento_gob + familia_pol + amigos_pol +
      equidad_vs_desig + orden_vs_libertad + privado_vs_pub,
    design = disenio, family = quasibinomial()
  )

  cat(sprintf("N: A=%d B=%d C=%d\n", nobs(modelo_A), nobs(modelo_B), nobs(modelo_C)))
  print(summary(modelo_C))
  cat(sprintf("AIC: A=%.1f B=%.1f C=%.1f (lower is better; a drop from A to B would support H3)\n",
              AIC(modelo_A)[2], AIC(modelo_B)[2], AIC(modelo_C)[2]))

  list(A = modelo_A, B = modelo_B, C = modelo_C)
}

verificacion_bivariada_h2 <- function(muestra, disenio) {
  # H2 with no controls, and with exposure collapsed to yes/no -- checks
  # the null result doesn't hinge on model specification.
  cat("% Milei vote by level of social media index:\n")
  print(muestra |> group_by(indice_redes_sociales) |>
          summarise(n = n(), pct_milei = mean(voto_milei, na.rm = TRUE) * 100, .groups = "drop"))

  modelo_bivariado <- svyglm(voto_milei ~ indice_redes_sociales, design = disenio, family = quasibinomial())
  print(summary(modelo_bivariado))

  modelo_binario <- svyglm(voto_milei ~ uso_alguna_red, design = disenio, family = quasibinomial())
  print(summary(modelo_binario))

  list(bivariado = modelo_bivariado, binario = modelo_binario)
}

modelo_C_social_media <- function(muestra_comun) {
  # Model C restricted to respondents who use at least one social media
  # platform for campaign information -- the relevant subgroup for RQ3.
  cnep_solo_redes <- muestra_comun |> filter(indice_redes_sociales > 0)
  disenio_solo_redes <- svydesign(ids = ~1, weights = ~`L.WtWithin`, data = cnep_solo_redes)

  modelo <- svyglm(
    voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
      interes_politico + conocimiento + diarios + radio + tv + region + zona +
      econ_pais + econ_personal +
      desc_gob_saliente + confianza_gob + confianza_medios + pols_defienden_ricos +
      desc_democracia + antipluralismo + compromiso_democratico +
      comprension_pol + resentimiento_gob + familia_pol + amigos_pol +
      equidad_vs_desig + orden_vs_libertad + privado_vs_pub,
    design = disenio_solo_redes, family = quasibinomial()
  )

  cat(sprintf("N=%d of %d (%.1f%%)\n", nrow(cnep_solo_redes), nrow(muestra_comun),
              100 * nrow(cnep_solo_redes) / nrow(muestra_comun)))
  print(summary(modelo))

  list(muestra = cnep_solo_redes, disenio = disenio_solo_redes, modelo = modelo)
}

ranking_estandarizado_rq2 <- function(cnep_solo_redes, disenio_solo_redes) {
  # Survey-weighted standardization (svymean()/svyvar() with L.WtWithin)
  # of the 16 grievances. Unlike RQ1's plain scale(), CNEP has a complex
  # survey design -- unweighted mean/SD would bias beta_std toward the
  # raw sample instead of the population the design represents.
  cnep_z <- cnep_solo_redes
  for (v in vars_agravio_rq2) {
    f <- as.formula(paste0("~", v))
    media <- as.numeric(coef(svymean(f, disenio_solo_redes, na.rm = TRUE)))
    sd_pond <- sqrt(as.numeric(coef(svyvar(f, disenio_solo_redes, na.rm = TRUE))))
    cnep_z[[paste0(v, "_z")]] <- (cnep_z[[v]] - media) / sd_pond
  }

  disenio_z <- svydesign(ids = ~1, weights = ~`L.WtWithin`, data = cnep_z)
  vars_z <- paste0(vars_agravio_rq2, "_z")
  modelo_z <- svyglm(
    as.formula(paste("voto_milei ~", paste(c(vars_control, vars_z), collapse = " + "))),
    design = disenio_z, family = quasibinomial()
  )

  coefs_z <- coef(summary(modelo_z))[vars_z, ]
  data.frame(
    variable = vars_agravio_rq2,
    beta_estandarizado = coefs_z[, "Estimate"],
    magnitud_efecto = abs(coefs_z[, "Estimate"]),
    p_valor = coefs_z[, "Pr(>|t|)"]
  ) |> arrange(desc(magnitud_efecto)) |> mutate(rank = row_number())
}

comparar_completo_vs_solo_redes <- function(modelo_completo, modelo_solo_redes) {
  # Do Model C's grievances hold up within the social-media subgroup,
  # not just in the full sample?
  vars <- c("desc_gob_saliente", "orden_vs_libertad", "privado_vs_pub", "confianza_medios", "pols_defienden_ricos")
  data.frame(
    variable = vars,
    coef_completo = coef(modelo_completo)[vars],
    p_completo = coef(summary(modelo_completo))[vars, "Pr(>|t|)"],
    coef_solo_redes = coef(modelo_solo_redes)[vars],
    p_solo_redes = coef(summary(modelo_solo_redes))[vars, "Pr(>|t|)"]
  )
}

robustez_twitter_vs_indice <- function(cnep) {
  # H2 with Twitter alone instead of the composite index, on the same
  # sample -- checks the null result doesn't depend on how exposure is
  # operationalized.
  cnep <- cnep |> mutate(uso_twitter = as.integer(if_else(`D.Platform_2_4` == 995, 0, `D.Platform_2_4`) > 0))
  vars <- c("voto_milei", vars_control, "indice_redes_sociales", "uso_twitter")
  muestra <- cnep |> filter(!is.na(`L.WtWithin`)) |> filter(if_all(all_of(vars), ~ !is.na(.)))
  disenio <- svydesign(ids = ~1, weights = ~`L.WtWithin`, data = muestra)

  modelo_indice <- svyglm(
    voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
      interes_politico + conocimiento + diarios + radio + tv + region + zona + indice_redes_sociales,
    design = disenio, family = quasibinomial()
  )
  modelo_twitter <- svyglm(
    voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
      interes_politico + conocimiento + diarios + radio + tv + region + zona + uso_twitter,
    design = disenio, family = quasibinomial()
  )

  cat(sprintf("N=%d (same for both)\n", nrow(muestra)))
  cat("Composite index:\n"); print(coef(summary(modelo_indice))["indice_redes_sociales", ])
  cat("Twitter alone:\n"); print(coef(summary(modelo_twitter))["uso_twitter", ])

  list(indice = modelo_indice, twitter = modelo_twitter)
}

diagnostico_codigo_90 <- function() {
  # How many respondents are lost specifically to the undocumented code
  # 90, and whether they look systematically different. Needs the raw
  # Excel, since the cleaned .rds already has 90 recoded to NA.
  variables_codigo_90 <- c("A.EconSit_2", "A.Respsit_2", "A.GovPerf_2", "B.TrustGov_2",
                           "B.TrustMedia_2", "B.DemSat_2", "B.OneParty_2", "B.DemAuth_2",
                           "B.PolsDefendRich_2", "B.PolCompl_2", "B.DontCare_2",
                           "E.FamTalk_2", "E.FriendTalk_2",
                           "J.EqualInd_2", "J.OrderLib_2", "J.PrivPub2_2", "C.LRSelf_2")

  cnep_crudo <- readxl::read_excel("data/raw/Argentina2023_Fin.xlsx") |>
    mutate(tiene_algun_90 = rowSums(across(all_of(variables_codigo_90), ~ . == 90), na.rm = TRUE) > 0)

  cat(sprintf("Cases with at least one 90: %d of %d (%.1f%%)\n",
              sum(cnep_crudo$tiene_algun_90), nrow(cnep_crudo), 100 * mean(cnep_crudo$tiene_algun_90)))

  print(cnep_crudo |> group_by(tiene_algun_90) |>
          summarise(n = n(), edad_media = mean(EDAD, na.rm = TRUE),
                    pct_genero_masc = mean(GÉNERO_NUMÉRICO == 2, na.rm = TRUE) * 100,
                    educacion_media = mean(L.Education, na.rm = TRUE), .groups = "drop"))
}

perfil_4_grupos <- function(muestra_comun) {
  # Weighted means of ideology + the 16 grievances across 4 groups (vote
  # x social media use), on the same common sample as Models A-C.
  muestra_comun <- muestra_comun |>
    mutate(grupo = case_when(
      voto_milei == 1 & uso_alguna_red == 1 ~ "Milei + social media",
      voto_milei == 1 & uso_alguna_red == 0 ~ "Milei, no social media",
      voto_milei == 0 & uso_alguna_red == 1 ~ "No Milei + social media",
      voto_milei == 0 & uso_alguna_red == 0 ~ "No Milei, no social media",
      TRUE ~ NA_character_
    ))

  cnep_perfil <- muestra_comun |> filter(!is.na(grupo))
  disenio_perfil <- svydesign(ids = ~1, weights = ~`L.WtWithin`, data = cnep_perfil)
  vars_perfil <- c("ideologia", vars_agravio_rq2)

  tabla <- data.frame(variable = vars_perfil)
  for (g in c("Milei + social media", "Milei, no social media",
              "No Milei + social media", "No Milei, no social media")) {
    medias <- sapply(vars_perfil, function(v) {
      svymean(as.formula(paste0("~", v)), subset(disenio_perfil, grupo == g), na.rm = TRUE)[1]
    })
    tabla[[g]] <- round(medias, 2)
  }

  cat(sprintf("N=%d of %d\n", nrow(cnep_perfil), nrow(muestra_comun)))
  print(cnep_perfil |> count(grupo))
  print(tabla, row.names = FALSE)
  tabla
}

tabla_A11 <- function(modelos) {
  extraer_col <- function(modelo, vars_orden) {
    cs <- coef(summary(modelo))
    sapply(vars_orden, function(v) {
      if (!v %in% rownames(cs)) return("\u2014")
      p <- cs[v, "Pr(>|t|)"]
      estrellas <- if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else if (p < .1) "\u2020" else ""
      sprintf("%.3f%s", cs[v, "Estimate"], estrellas)
    })
  }

  orden <- c("(Intercept)", vars_control, "indice_redes_sociales", vars_agravio_rq2)
  tabla <- data.frame(
    Predictor = unname(etiquetas_rq2[orden]),
    `Model A` = extraer_col(modelos$A, orden), `Model B` = extraer_col(modelos$B, orden),
    `Model C` = extraer_col(modelos$C, orden), check.names = FALSE
  )
  tabla <- rbind(tabla,
    data.frame(Predictor = "N", `Model A` = as.character(nobs(modelos$A)),
               `Model B` = as.character(nobs(modelos$B)), `Model C` = as.character(nobs(modelos$C)), check.names = FALSE),
    data.frame(Predictor = "AIC", `Model A` = sprintf("%.1f", AIC(modelos$A)[2]),
               `Model B` = sprintf("%.1f", AIC(modelos$B)[2]), `Model C` = sprintf("%.1f", AIC(modelos$C)[2]), check.names = FALSE)
  )

  print(tabla, row.names = FALSE)
  apa_table(tabla, "Table A.11. Weighted logistic regression -- Models A, B, C (N = 1,421)",
            "Source: own elaboration. Survey-weighted (svyglm). *** p<.001, ** p<.01, * p<.05, \u2020 p<.1") |>
    save_png("tableA11_modelos_ABC.png")
}

tabla_A12 <- function(bivariados, robustez) {
  fila <- function(modelo, termino, etiqueta) {
    cs <- coef(summary(modelo))
    data.frame(Operationalization = etiqueta, Coefficient = sprintf("%.3f", cs[termino, "Estimate"]),
               p = sprintf("%.3f", cs[termino, "Pr(>|t|)"]))
  }

  tabla <- rbind(
    fila(robustez$indice, "indice_redes_sociales", "Composite index, with controls"),
    fila(robustez$twitter, "uso_twitter", "Twitter/X only, with controls"),
    fila(bivariados$bivariado, "indice_redes_sociales", "Composite index, bivariate"),
    fila(bivariados$binario, "uso_alguna_red", "Any social media use, bivariate")
  )

  print(tabla, row.names = FALSE)
  apa_table(tabla, "Table A.12. H2 robustness -- all operationalizations tested") |>
    save_png("tableA12_h2_robustez.png")
}

tabla_A13 <- function(ranking, modelo_solo_redes) {
  # Same coefficients as modelo_solo_redes, on their original scale, but
  # ordered by the standardized ranking (not by raw coefficient size).
  coefs <- coef(summary(modelo_solo_redes))
  tabla <- ranking |>
    arrange(rank) |>
    mutate(Coefficient = sprintf("%.3f", coefs[variable, "Estimate"]),
           p = sprintf("%.3f", coefs[variable, "Pr(>|t|)"])) |>
    transmute(Rank = rank, Predictor = unname(etiquetas_rq2[variable]), Coefficient, p)

  print(tabla, row.names = FALSE)
  apa_table(tabla, "Table A.13. Model C, social media users only (N = 851), ranked",
            "Source: own elaboration. Ranked by standardized effect magnitude (see Table 1); coefficients shown are on each predictor's original response scale.") |>
    save_png("tableA13_modelo_c_solo_redes.png")
}

tabla_A14 <- function(tabla_perfil) {
  tabla <- tabla_perfil |>
    mutate(Predictor = unname(etiquetas_rq2[variable])) |>
    select(Predictor, `Milei + social media`, `Milei, no social media`,
           `No Milei + social media`, `No Milei, no social media`)

  print(tabla, row.names = FALSE)
  apa_table(tabla, "Table A.14. Descriptive profile -- vote choice x social media use (weighted means)") |>
    save_png("tableA14_perfil_4_grupos.png")
}

# --- Run ---

cnep <- cargar_datos()
chequear_variables(cnep)

cnep_muestra_comun <- construir_muestra_comun(cnep)
disenio_comun <- svydesign(ids = ~1, weights = ~`L.WtWithin`, data = cnep_muestra_comun)
cat(sprintf("Common sample: %d\n", nrow(cnep_muestra_comun)))

modelos <- modelos_abc(disenio_comun)
bivariados <- verificacion_bivariada_h2(cnep_muestra_comun, disenio_comun)

resultado_solo_redes <- modelo_C_social_media(cnep_muestra_comun)
ranking_rq2 <- ranking_estandarizado_rq2(resultado_solo_redes$muestra, resultado_solo_redes$disenio)
print(ranking_rq2, row.names = FALSE, digits = 3)
print(comparar_completo_vs_solo_redes(modelos$C, resultado_solo_redes$modelo), digits = 3)

robustez <- robustez_twitter_vs_indice(cnep)
diagnostico_codigo_90()

tabla_perfil <- perfil_4_grupos(cnep_muestra_comun)

tabla_A11(modelos)
tabla_A12(bivariados, robustez)
tabla_A13(ranking_rq2, resultado_solo_redes$modelo)
tabla_A14(tabla_perfil)

cat("\n02_grievance_model.R listo. `ranking_rq2`/`etiquetas_rq2` quedan en el entorno --",
    "\npara Table 1, correr table1_and_appendix.R despues, en la misma sesion.\n")
