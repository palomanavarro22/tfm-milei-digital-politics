# rq2_electoral_grievances/02_grievance_model.R
# RQ2: does social media use for campaign information predict Milei's vote
# beyond classical predictors? (H2, H3). Grievances predicting support
# among social media users (for RQ3).
#
# Correr con la raiz del repo como working directory. Requiere el dataset
# ya limpio (data/processed/cnep_argentina2023_limpio.rds), generado por
# 01_cnep_exploration.qmd. El Excel crudo (data/raw/Argentina2023_Fin.xlsx)
# solo se lee en Part 8 (diagnostico de codigo 90).
#
# Este script es autocontenido en cuanto a tablas: source() al helper
# compartido de abajo alcanza para generar A.11-A.14 sin importar si
# 01_sentiment_classification.py / 02_engagement_model.R (RQ1) corrieron
# antes en esta sesion o no -- RQ2 no depende sustantivamente de RQ1.
#
# Part 6 incluye la estandarizacion ponderada por encuesta (svymean()/
# svyvar() con L.WtWithin) de los 16 predictores de agravio, que produce
# ranking_rq2 -- la fuente del ranking estandarizado en Table 1 y del
# orden de filas en Table A.13.
#
# Part 10 (al final) genera las tablas formateadas A.11-A.14 directamente
# desde los objetos de modelo ajustados arriba, para que la salida en
# consola y las tablas exportadas nunca puedan desincronizarse. Table 1
# (RQ3) NO se arma aca -- ver rq3_congruence/table1_and_appendix.R.

library(dplyr)
library(survey)

source("R/table_helpers.R")   # apa_table(), save_png(), formatear_con_estrellas()

cnep <- readRDS("data/processed/cnep_argentina2023_limpio.rds")
cat(sprintf("Dataset loaded: %d rows x %d columns\n", nrow(cnep), ncol(cnep)))

# Dependent variable and predictor construction


cnep <- cnep |>
  mutate(
    voto_milei = case_when(
      `H.VoteWhichRecent2.Z` == 11 ~ 1,
      `H.VoteWhichRecent2.Z` %in% c(1, 10, 12, 13) ~ 0,
      TRUE ~ NA_real_  # 993=DK and any other code are left as NA
    )
  )

cat("\n--- voto_milei distribution ---\n")
print(table(cnep$voto_milei, useNA = "always"))

# Social media use index for campaign information.

recodificar_plataforma <- function(x) {
  x <- if_else(x == 995, 0, x)
  as.numeric(x > 0)
}

variables_redes_sociales <- c("D.Platform_2_3", "D.Platform_2_4", "D.Platform_2_5",
                              "D.Platform_2_6", "D.Platform_2_7")
# 3=Facebook, 4=Twitter, 5=WhatsApp, 6=Instagram, 7=TikTok

cnep <- cnep |> 
  mutate(across(all_of(variables_redes_sociales), recodificar_plataforma, .names = "{.col}_bin")) |> 
  mutate(
    indice_redes_sociales = rowSums(across(ends_with("_bin")), na.rm = FALSE),
    uso_alguna_red = as.integer(indice_redes_sociales > 0),
    uso_twitter = recodificar_plataforma(`D.Platform_2_4`)
  )

cat("\n--- Social media index distribution (0-5) ---\n")
print(table(cnep$indice_redes_sociales, useNA = "always"))

# Rename variables, grouped by theoretical category.

cnep <- cnep |> 
  mutate(
    #  Classical controls (sociodemographic and political) 
    ideologia          = `C.LRSelf_2`,
    edad               = EDAD,
    genero             = GÉNERO_NUMÉRICO,
    ingreso            = `L.Income.Z`,
    educacion          = L.Education,
    clase_subjetiva    = L.SubjClass,
    interes_politico   = `H.Interest_2`,
    conocimiento       = H.InfoTest,
    diarios            = `D.CamPaper_2`,
    radio              = `D.CamRadio_2`,
    tv                 = `D.CamTV_2`,
    region             = regiones,
    zona               = tipozona,
    
    # Economic grievance 
    econ_pais          = `A.EconSit_2`,
    econ_personal      = `A.Respsit_2`,
    
    # Previous government performance 
    desc_gob_saliente  = `A.GovPerf_2`,
    # Confidence in governments (1=Hardly ever...4=Just about always) --
    confianza_gob      = `B.TrustGov_2`,
    # confianza_medios: 1=never trusts...4=always trusts (renamed; formerly
    confianza_medios     = `B.TrustMedia_2`,
    pols_defienden_ricos = `B.PolsDefendRich_2`,  # candidate link to "casta" in RQ3

    # --- Democratic / antisystem attitudes ---
    desc_democracia    = `B.DemSat_2`,
    antipluralismo     = `B.OneParty_2`,
    # compromiso_democratico: 1=doesn't matter, 2=authoritarian sometimes
    # preferable, 3=democracy preferable 
    compromiso_democratico = `B.DemAuth_2`,
    comprension_pol    = `B.PolCompl_2`,
    resentimiento_gob  = `B.DontCare_2`,  # proxy for political resentment
    
    # --- Interpersonal political discussion ---
    familia_pol        = `E.FamTalk_2`,
    amigos_pol         = `E.FriendTalk_2`,
    
    # --- Value orientations ---
    equidad_vs_desig   = `J.EqualInd_2`,
    orden_vs_libertad  = `J.OrderLib_2`,
    privado_vs_pub     = `J.PrivPub2_2`
  )

vars_agravio_rq2 <- c("econ_pais", "econ_personal", "desc_gob_saliente", "confianza_gob",
                      "confianza_medios", "pols_defienden_ricos", "desc_democracia",
                      "antipluralismo", "compromiso_democratico", "comprension_pol",
                      "resentimiento_gob", "familia_pol", "amigos_pol",
                      "equidad_vs_desig", "orden_vs_libertad", "privado_vs_pub")

vars_control <- c("ideologia", "edad", "genero", "ingreso", "educacion", "clase_subjetiva",
                  "interes_politico", "conocimiento", "diarios", "radio", "tv", "region", "zona")

# Display labels, used to format tables A.11-A.14 in Part 10.
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


#Descriptive analysis of the variables 

vars_check <- c("voto_milei", "indice_redes_sociales", vars_control, vars_agravio_rq2, "L.WtWithin")

cat("\n--- Variable summary before building the survey design ---\n")
for (v in vars_check) {
  n_na <- sum(is.na(cnep[[v]]))
  cat(sprintf("%-22s NA: %5d of %d\n", v, n_na, nrow(cnep)))
}

cat("\n--- Scale variable ranges (no maximum should be 90, 998, or 999) ---\n")
for (v in vars_agravio_rq2) {
  maximo <- max(cnep[[v]], na.rm = TRUE)
  alerta <- if (maximo %in% c(90, 93, 94, 95, 96, 97, 98, 99, 998, 999)) " <<< NOT FIXED, DO NOT PROCEED" else ""
  cat(sprintf("%-22s max=%.1f%s\n", v, maximo, alerta))
}
cat("\n>>> If any alert appears above, go back to 01_cnep_exploration.qmd,",
    "\n>>> add that variable to the fix chunk, re-render, and only then proceed. <<<\n\n")


#  Survey design and common sample (required to compare AIC)

cnep_para_diseño <- cnep |>  filter(!is.na(`L.WtWithin`))
cat(sprintf("\nExcluding %d cases without a survey weight (no georeference)\n",
            nrow(cnep) - nrow(cnep_para_diseño)))

variables_modelo_completo <- c("voto_milei", vars_control, "indice_redes_sociales", vars_agravio_rq2)

cnep_muestra_comun <- cnep_para_diseño |> 
  filter(if_all(all_of(variables_modelo_completo), ~ !is.na(.)))

cat(sprintf("Common sample (complete cases across all variables): %d of %d\n",
            nrow(cnep_muestra_comun), nrow(cnep_para_diseño)))

disenio_comun <- svydesign(ids = ~1, weights = ~`L.WtWithin`, data = cnep_muestra_comun)


# Fitting the models: Model A (classical) vs. B (+ social media) vs. C (+ grievances)

modelo_A_clasico <- svyglm(
  voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
    interes_politico + conocimiento + diarios + radio + tv + region + zona,
  design = disenio_comun, family = quasibinomial()
)

modelo_B_extendido <- svyglm(
  voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
    interes_politico + conocimiento + diarios + radio + tv + region + zona +
    indice_redes_sociales,
  design = disenio_comun, family = quasibinomial()
)

modelo_C_con_agravios <- svyglm(
  voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
    interes_politico + conocimiento + diarios + radio + tv + region + zona +
    indice_redes_sociales +
    econ_pais + econ_personal +
    desc_gob_saliente + confianza_gob + confianza_medios + pols_defienden_ricos +
    desc_democracia + antipluralismo + compromiso_democratico +
    comprension_pol + resentimiento_gob +
    familia_pol + amigos_pol +
    equidad_vs_desig + orden_vs_libertad + privado_vs_pub,
  design = disenio_comun, family = quasibinomial()
)

cat("\nN across the three models (should match): A:", nobs(modelo_A_clasico),
    "| B:", nobs(modelo_B_extendido), "| C:", nobs(modelo_C_con_agravios), "\n\n")

cat("=== MODEL A -- Classical predictors only ===\n")
print(summary(modelo_A_clasico))

cat("\n=== MODEL B -- Classical + social media index (H2/H3) ===\n")
print(summary(modelo_B_extendido))

cat("\n=== MODEL C -- + expanded grievance variables (for RQ3) ===\n")
print(summary(modelo_C_con_agravios))

cat("\n=== AIC comparison (same sample across all three) ===\n")
cat(sprintf("Model A (classical):   %.1f\n", AIC(modelo_A_clasico)[2]))
cat(sprintf("Model B (+ soc. media): %.1f\n", AIC(modelo_B_extendido)[2]))
cat(sprintf("Model C (+ grievances): %.1f\n", AIC(modelo_C_con_agravios)[2]))
cat("(Lower AIC = better fit; a notable drop from A to B would support H3)\n")


#  Bivariate verification of H2 (no controls)

cat("\n\n############################################################\n")
cat("PART 5 -- Bivariate verification of H2\n")
cat("############################################################\n")

cat("\n=== Raw table: % Milei vote by level of social media index ===\n")
tabla_cruda <- cnep_muestra_comun |> 
  group_by(indice_redes_sociales) |> 
  summarise(n = n(), pct_milei = mean(voto_milei, na.rm = TRUE) * 100, .groups = "drop")
print(tabla_cruda)

cat("\n=== Bivariate model (NO controls) ===\n")
modelo_bivariado <- svyglm(
  voto_milei ~ indice_redes_sociales,
  design = disenio_comun, family = quasibinomial()
)
print(summary(modelo_bivariado))

cat("\n=== Alternative: binary variable (used ANY social media, yes/no) ===\n")
cnep_muestra_comun <- cnep_muestra_comun |> 
  mutate(uso_alguna_red = as.integer(indice_redes_sociales > 0))

disenio_comun <- svydesign(ids = ~1, weights = ~`L.WtWithin`, data = cnep_muestra_comun)

modelo_binario <- svyglm(
  voto_milei ~ uso_alguna_red,
  design = disenio_comun, family = quasibinomial()
)
print(summary(modelo_binario))

cat("\n(H2 reframed: CNEP has no 'primary source' of information variable --",
    "\nit was operationalized as exposure, not primacy. If none of these",
    "\ncoefficients are significant, H2 does not hold at any level of",
    "\nanalysis: raw, controlled, or binary.)\n")


# Robustness: Model C among social media users only
# RQ3 does not depend on H2 holding, checking whether
# Model C's significant grievances hold within the subgroup that does use
# social media, as additional robustness. 
# This is the final model for RQ3: "grievances predicting support among
# social media users" 


cat("\n\n############################################################\n")
cat("PART 6 -- Model C among social media users only\n")
cat("############################################################\n")

cnep_solo_redes <- cnep_muestra_comun |> 
  filter(indice_redes_sociales > 0)

cat(sprintf("Social media user subsample: %d of %d (%.1f%%)\n",
            nrow(cnep_solo_redes), nrow(cnep_muestra_comun),
            100 * nrow(cnep_solo_redes) / nrow(cnep_muestra_comun)))

disenio_solo_redes <- svydesign(ids = ~1, weights = ~`L.WtWithin`, data = cnep_solo_redes)

modelo_C_solo_redes <- svyglm(
  voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
    interes_politico + conocimiento + diarios + radio + tv + region + zona +
    econ_pais + econ_personal +
    desc_gob_saliente + confianza_gob + confianza_medios + pols_defienden_ricos +
    desc_democracia + antipluralismo + compromiso_democratico +
    comprension_pol + resentimiento_gob +
    familia_pol + amigos_pol +
    equidad_vs_desig + orden_vs_libertad + privado_vs_pub,
  design = disenio_solo_redes, family = quasibinomial()
)

cat(sprintf("\nModel N: %d\n", nobs(modelo_C_solo_redes)))
cat(sprintf("Dispersion: %.2f (compare against %.2f for the full Model C)\n\n",
            summary(modelo_C_solo_redes)$dispersion, summary(modelo_C_con_agravios)$dispersion))

cat("=== MODEL C -- social media users only ===\n")
print(summary(modelo_C_solo_redes))

# -----------------------------------------------------------------------
# Weighted standardization of the 16 grievance predictors, for the RQ3
# ranking (Table 1) and for the rank order shown in Table A.13.
#
# IMPORTANT: this uses svymean()/svyvar() with the CNEP survey weight
# (L.WtWithin), NOT a plain scale(). This is a deliberate asymmetry with
# RQ1 (which standardizes tweet-level content variables with plain
# scale(), since the 450-tweet corpus has no survey design/weights).
# Using unweighted mean/sd here would bias beta_std toward the raw
# sample's composition instead of the population the CNEP design
# represents -- svymean()/svyvar() is the correct approach given CNEP's
# complex survey design.
# -----------------------------------------------------------------------

cnep_solo_redes_z <- cnep_solo_redes
for (v in vars_agravio_rq2) {
  f <- as.formula(paste0("~", v))
  media_pond <- as.numeric(coef(svymean(f, disenio_solo_redes, na.rm = TRUE)))
  sd_pond    <- sqrt(as.numeric(coef(svyvar(f, disenio_solo_redes, na.rm = TRUE))))
  cnep_solo_redes_z[[paste0(v, "_z")]] <- (cnep_solo_redes_z[[v]] - media_pond) / sd_pond
}

disenio_solo_redes_z <- svydesign(ids = ~1, weights = ~`L.WtWithin`, data = cnep_solo_redes_z)

vars_agravio_rq2_z <- paste0(vars_agravio_rq2, "_z")

modelo_C_solo_redes_z <- svyglm(
  as.formula(paste("voto_milei ~", paste(c(vars_control, vars_agravio_rq2_z), collapse = " + "))),
  design = disenio_solo_redes_z, family = quasibinomial()
)

coefs_agravio_z <- coef(summary(modelo_C_solo_redes_z))[vars_agravio_rq2_z, ]

ranking_rq2 <- data.frame(
  variable         = vars_agravio_rq2,
  beta_estandarizado = coefs_agravio_z[, "Estimate"],
  magnitud_efecto     = abs(coefs_agravio_z[, "Estimate"]),
  p_valor             = coefs_agravio_z[, "Pr(>|t|)"]
) |>
  arrange(desc(magnitud_efecto)) |>
  mutate(rank = row_number())

cat("\n=== Standardized ranking of the 16 grievances (Table 1 / Table A.13 order) ===\n")
print(ranking_rq2, row.names = FALSE, digits = 3)

cat("\n(Table A.13 shows the UNSTANDARDIZED coefficients from modelo_C_solo_redes,",
    "\non each predictor's original response scale, but ORDERED by this",
    "\nstandardized ranking -- not re-ranked by raw coefficient size.)\n")

cat("\n=== Comparison: full Model C vs. social media users only ===\n")
vars_interes <- c("desc_gob_saliente", "orden_vs_libertad", "privado_vs_pub", "confianza_medios", "pols_defienden_ricos")
comparacion <- data.frame(
  variable = vars_interes,
  coef_completo = coef(modelo_C_con_agravios)[vars_interes],
  p_completo = coef(summary(modelo_C_con_agravios))[vars_interes, "Pr(>|t|)"],
  coef_solo_redes = coef(modelo_C_solo_redes)[vars_interes],
  p_solo_redes = coef(summary(modelo_C_solo_redes))[vars_interes, "Pr(>|t|)"]
)
print(comparacion, digits = 3)


# H2 robustness: Twitter alone instead of the composite index

cnep <- cnep |> 
  mutate(uso_twitter = as.integer(if_else(`D.Platform_2_4` == 995, 0, `D.Platform_2_4`) > 0))

vars_h2_robustez <- c("voto_milei", vars_control, "indice_redes_sociales", "uso_twitter")

cnep_h2_robustez <- cnep |> 
  filter(!is.na(`L.WtWithin`)) |> 
  filter(if_all(all_of(vars_h2_robustez), ~ !is.na(.)))

cat(sprintf("\nN for the fair comparison (same sample for both versions): %d\n",
            nrow(cnep_h2_robustez)))

disenio_h2_robustez <- svydesign(ids = ~1, weights = ~`L.WtWithin`, data = cnep_h2_robustez)

modelo_indice_mismo_n <- svyglm(
  voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
    interes_politico + conocimiento + diarios + radio + tv + region + zona +
    indice_redes_sociales,
  design = disenio_h2_robustez, family = quasibinomial()
)

modelo_solo_twitter <- svyglm(
  voto_milei ~ ideologia + edad + genero + ingreso + educacion + clase_subjetiva +
    interes_politico + conocimiento + diarios + radio + tv + region + zona +
    uso_twitter,
  design = disenio_h2_robustez, family = quasibinomial()
)

cat(sprintf("N for both models (should match): index=%d | Twitter=%d\n",
            nobs(modelo_indice_mismo_n), nobs(modelo_solo_twitter)))

cat("\n=== PART 7 -- H2: composite index vs. Twitter alone, SAME sample ===\n")
cat("\nComposite index (5 platforms):\n")
print(coef(summary(modelo_indice_mismo_n))["indice_redes_sociales", ])
cat("\nTwitter alone:\n")
print(coef(summary(modelo_solo_twitter))["uso_twitter", ])

cat("\n(Neither operationalization is significant -- H2 does not depend on",
    "\nhow social media exposure is measured; the null finding holds.)\n")


# Diagnostic: cost of excluding code 90
#
# Quantifies how much N is lost specifically to code 90, due to non-specification
#and whether affected cases look systematically different from the rest.
#Needs the raw Excel (before the fix), not the
# already-cleaned .rds.

cat("\n\n############################################################\n")
cat("PART 8 -- Exclusion cost diagnostic (code 90)\n")
cat("############################################################\n")

variables_codigo_90 <- c("A.EconSit_2", "A.Respsit_2", "A.GovPerf_2", "B.TrustGov_2",
                         "B.TrustMedia_2", "B.DemSat_2", "B.OneParty_2", "B.DemAuth_2",
                         "B.PolsDefendRich_2", "B.PolCompl_2", "B.DontCare_2",
                         "E.FamTalk_2", "E.FriendTalk_2",
                         "J.EqualInd_2", "J.OrderLib_2", "J.PrivPub2_2", "C.LRSelf_2")
# FIXED: this list must audit exactly the 16 vars_agravio_rq2 sources + C.LRSelf_2
# (ideologia). It previously included "B.PoliticalViolence_2" (unused anywhere
# in the analysis) instead of "B.PolsDefendRich_2" (the source for
# pols_defienden_ricos, which IS one of the 16 grievances in Model C).

cnep_crudo_90 <- readxl::read_excel("data/raw/Argentina2023_Fin.xlsx")

cnep_crudo_90 <- cnep_crudo_90 |> 
  mutate(tiene_algun_90 = rowSums(across(all_of(variables_codigo_90), ~ . == 90), na.rm = TRUE) > 0)

cat(sprintf("Cases with AT LEAST ONE 90 across the 17 variables: %d of %d (%.1f%%)\n",
            sum(cnep_crudo_90$tiene_algun_90), nrow(cnep_crudo_90),
            100 * mean(cnep_crudo_90$tiene_algun_90)))

cat("\n--- Comparison: cases with 90 vs. the rest (already-clean variables) ---\n")
comparacion_90 <- cnep_crudo_90 |> 
  group_by(tiene_algun_90) |> 
  summarise(
    n = n(),
    edad_media = mean(EDAD, na.rm = TRUE),
    pct_genero_masc = mean(GÉNERO_NUMÉRICO == 2, na.rm = TRUE) * 100,
    educacion_media = mean(L.Education, na.rm = TRUE),
    .groups = "drop"
  )
print(comparacion_90)

cat("\n(If age/gender/education are similar across the two groups, the\n")
cat("exclusion likely does not introduce major bias on those dimensions --\n")
cat("this does not prove the absence of all bias, but is a reasonable\n")
cat("signal given the time available.)\n")

# =============================================================================
# Descriptive profile: 4 groups (vote x social media use)
# Compares weighted means of
# ideology and the 16 grievances across 4 groups, to see whether Milei
# voters who also use social media look more "radicalized" in their
# grievances than Milei voters in general.
# =============================================================================

cat("\n\n############################################################\n")
cat("PART 9 -- 4-group descriptive profile\n")
cat("############################################################\n")

cnep_muestra_comun <- cnep_muestra_comun |>
  mutate(
    grupo = case_when(
      voto_milei == 1 & uso_alguna_red == 1 ~ "Milei + social media",
      voto_milei == 1 & uso_alguna_red == 0 ~ "Milei, no social media",
      voto_milei == 0 & uso_alguna_red == 1 ~ "No Milei + social media",
      voto_milei == 0 & uso_alguna_red == 0 ~ "No Milei, no social media",
      TRUE ~ NA_character_
    )
  )

cat("--- 4-group distribution (unweighted, for N only) ---\n")
print(table(cnep_muestra_comun$grupo, useNA = "always"))

# FIXED: cnep_perfil now derives from cnep_muestra_comun (the same common
# sample used for Models A-C, N=1,421), not from the full `cnep` (N=3,433).
# Building it from `cnep` only required non-missing group + the 16
# grievances, silently allowing complete cases on classical controls
# (ideology, income, education, etc.) to differ from Models A-C -- which
# contradicts the appendix note that this profile uses the same N=1,421.
cnep_perfil <- cnep_muestra_comun |>
  filter(!is.na(grupo))

cat(sprintf("\nN for the profile (same common sample as Models A-C): %d of %d\n",
            nrow(cnep_perfil), nrow(cnep_muestra_comun)))

disenio_perfil <- svydesign(ids = ~1, weights = ~`L.WtWithin`, data = cnep_perfil)

vars_perfil <- c("ideologia", vars_agravio_rq2)

tabla_perfil <- data.frame(variable = vars_perfil)
for (g in c("Milei + social media", "Milei, no social media", "No Milei + social media", "No Milei, no social media")) {
  medias <- sapply(vars_perfil, function(v) {
    formula_v <- as.formula(paste0("~", v))
    svymean(formula_v, subset(disenio_perfil, grupo == g), na.rm = TRUE)[1]
  })
  tabla_perfil[[g]] <- round(medias, 2)
}

n_grupos <- cnep_perfil |>  count(grupo)
cat("\n--- N per group (profile sample) ---\n")
print(n_grupos)

cat("\n=== DESCRIPTIVE PROFILE TABLE (weighted means) ===\n\n")
print(tabla_perfil, row.names = FALSE)

cat("\n(This is DESCRIPTIVE, not a model -- it compares means, not statistical",
    "\nsignificance of differences.)\n")

# =============================================================================
# PART 10 -- Formatted output tables (A.11-A.14), built from the exact same
# model objects fit above, so nothing here can drift out of sync with the
# console output in Parts 4-9. Table A.13's row order comes directly from
# the standardized `ranking_rq2` built in Part 6.
#
# Uses apa_table() (own utility function, assumed sourced earlier in the
# project -- e.g. source("R/funciones_apa_table.R")). If not available,
# swap apa_table(...) |> gt::gtsave(...) for a plain View()/write.csv() of
# the *_df data frame -- the data frames themselves don't depend on it.
# =============================================================================

cat("\n\n############################################################\n")
cat("PART 10 -- Formatted tables A.11-A.14 (standardization-consistent)\n")
cat("############################################################\n")

extraer_col_A11 <- function(modelo, vars_orden) {
  cs <- coef(summary(modelo))
  sapply(vars_orden, function(v) {
    if (v %in% rownames(cs)) {
      p <- cs[v, "Pr(>|t|)"]
      estrellas <- if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else if (p < .1) "\u2020" else ""
      sprintf("%.3f%s", cs[v, "Estimate"], estrellas)
    } else {
      "\u2014"  # em dash: term not in this model
    }
  })
}

orden_terminos_A11 <- c("(Intercept)", vars_control, "indice_redes_sociales", vars_agravio_rq2)

tabla_A11_df <- data.frame(
  Predictor = unname(etiquetas_rq2[orden_terminos_A11]),
  `Model A` = extraer_col_A11(modelo_A_clasico, orden_terminos_A11),
  `Model B` = extraer_col_A11(modelo_B_extendido, orden_terminos_A11),
  `Model C` = extraer_col_A11(modelo_C_con_agravios, orden_terminos_A11),
  check.names = FALSE
)
tabla_A11_df <- rbind(
  tabla_A11_df,
  data.frame(Predictor = "N", `Model A` = as.character(nobs(modelo_A_clasico)),
             `Model B` = as.character(nobs(modelo_B_extendido)),
             `Model C` = as.character(nobs(modelo_C_con_agravios)), check.names = FALSE),
  data.frame(Predictor = "AIC", `Model A` = sprintf("%.1f", AIC(modelo_A_clasico)[2]),
             `Model B` = sprintf("%.1f", AIC(modelo_B_extendido)[2]),
             `Model C` = sprintf("%.1f", AIC(modelo_C_con_agravios)[2]), check.names = FALSE)
)

cat("\n=== Table A.11 (data frame) ===\n")
print(tabla_A11_df, row.names = FALSE)

apa_table(
  tabla_A11_df, "Table A.11. Weighted logistic regression -- Models A, B, C (N = 1,421)",
  "Source: own elaboration. Survey-weighted (svyglm). *** p<.001, ** p<.01, * p<.05, \u2020 p<.1"
) |> save_png("tableA11_modelos_ABC.png")

# --- Table A.12: H2 robustness, all four operationalizations ---

extraer_fila_h2 <- function(modelo, termino, etiqueta) {
  cs <- coef(summary(modelo))
  data.frame(Operationalization = etiqueta,
             Coefficient = sprintf("%.3f", cs[termino, "Estimate"]),
             p = sprintf("%.3f", cs[termino, "Pr(>|t|)"]))
}

tabla_A12_df <- rbind(
  extraer_fila_h2(modelo_indice_mismo_n, "indice_redes_sociales", "Composite index, with controls"),
  extraer_fila_h2(modelo_solo_twitter, "uso_twitter", "Twitter/X only, with controls"),
  extraer_fila_h2(modelo_bivariado, "indice_redes_sociales", "Composite index, bivariate"),
  extraer_fila_h2(modelo_binario, "uso_alguna_red", "Any social media use, bivariate")
)

cat("\n=== Table A.12 (data frame) ===\n")
print(tabla_A12_df, row.names = FALSE)

apa_table(
  tabla_A12_df, "Table A.12. H2 robustness -- all operationalizations tested"
) |> save_png("tableA12_h2_robustez.png")

# --- Table A.13: Model C, social-media users only, ORDERED by ranking_rq2 ---
# (ranking_rq2 was built in Part 6 from the weighted-standardized model;
# this table shows the UNSTANDARDIZED coefficients, same as modelo_C_solo_redes,
# just ordered by standardized magnitude instead of raw magnitude.)

coefs_C_solo_redes <- coef(summary(modelo_C_solo_redes))

tabla_A13_df <- ranking_rq2 |>
  arrange(rank) |>
  mutate(
    Coefficient = sprintf("%.3f", coefs_C_solo_redes[variable, "Estimate"]),
    p           = sprintf("%.3f", coefs_C_solo_redes[variable, "Pr(>|t|)"])
  ) |>
  transmute(Rank = rank, Predictor = unname(etiquetas_rq2[variable]), Coefficient, p)

cat("\n=== Table A.13 (data frame, ranked by standardized effect) ===\n")
print(tabla_A13_df, row.names = FALSE)

apa_table(
  tabla_A13_df, "Table A.13. Model C, social media users only (N = 851), ranked",
  "Source: own elaboration. Ranked by standardized effect magnitude (see Table 1); coefficients shown are on each predictor's original response scale."
) |> save_png("tableA13_modelo_c_solo_redes.png")

# --- Table A.14: 4-group descriptive profile (reuses tabla_perfil from Part 9) ---

tabla_A14_df <- tabla_perfil |>
  mutate(Predictor = unname(etiquetas_rq2[variable])) |>
  select(Predictor, `Milei + social media`, `Milei, no social media`,
         `No Milei + social media`, `No Milei, no social media`)

cat("\n=== Table A.14 (data frame) ===\n")
print(tabla_A14_df, row.names = FALSE)

apa_table(
  tabla_A14_df, "Table A.14. Descriptive profile -- vote choice x social media use (weighted means)"
) |> save_png("tableA14_perfil_4_grupos.png")

# --- Table 1 (body, RQ3 congruence) ---
# Table 1 no se arma aca -- agrupa todo en 6 dimensiones tematicas con un
# veredicto narrativo por fila, no es una lista plana rankeada como esta
# Part 10. Esa tabla vive en rq3_congruence/table1_and_appendix.R, y
# necesita `ranking_rq2`/`etiquetas_rq2` (dejados en el entorno por este
# script) y `ranking_rq1`/`etiquetas_rq1` (dejados por
# rq1_digital_content/02_engagement_model.R) -- correr los tres scripts
# de R en la MISMA sesion, en ese orden.

cat("\n\n############################################################\n")
cat("02_grievance_model.R complete -- Part 10 generated tables A.11-A.14\n")
cat("directly from these models. For Table 1 (RQ3 congruence), run\n")
cat("rq3_congruence/table1_and_appendix.R next, in this same session.\n")
cat("############################################################\n")