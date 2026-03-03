source("procesamiento/07.3_KM_Cox_af.R", encoding = "UTF-8")

# ==========================================
# 1) META-ANÁLISIS: AF (MODELO COMPLETO)
# ==========================================

# Función corregida para extraer HR e IC95% de AF por encuesta
get_af <- function(df, estudio_lab,
                   term_af = "af_cancer_binariaYes", # Nombre exacto de tu tabla
                   modelo_keep = "Modelo completo") { # Nombre exacto de tu tabla
  df %>%
    filter(modelo == modelo_keep, term == term_af) %>%
    transmute(
      estudio = estudio_lab,
      HR = HR, 
      lo = IC_inf, 
      hi = IC_sup
    )
}

# Extraer AF de cada ENS y unir
af_2009  <- get_af(resumen,        "ENS 2009-2010")
af_2016  <- get_af(resumen_2016,   "ENS 2016-2017")
af_both <- bind_rows(af_2009, af_2016)

# Transformar a log(HR) y SE desde IC95%
af_both <- af_both %>%
  mutate(
    TE   = log(HR),
    seTE = (log(hi) - log(lo)) / (2*1.96)
  )

# Meta-análisis: efectos aleatorios (REML)
m_af <- metagen(
  TE = af_both$TE,
  seTE = af_both$seTE,
  studlab = af_both$estudio,
  sm = "HR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML"
)

# Resumen y forest plot
summary(m_af)
forest(m_af, backtransf = TRUE,
       xlab = "Hazard Ratio",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")


# Guardar gráfico final (descomentar para guardar físicamente)
# 1. Abrimos el archivo PNG indicando la ruta y el tamaño
png("output/graphs/Metaanalisis_AF_ModeloCompleto.png", 
    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico (aquí pones tu código exacto del forest)
forest(m_af, backtransf = TRUE,
       xlab = "Hazard Ratio",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# 3. Cerramos el archivo para que se guarde físicamente
dev.off()

# ==========================================
# 2) META-ANÁLISIS: SEXO (MODELO COMPLETO)
# ==========================================

get_sexo <- function(df, estudio_lab, modelo_keep = "Modelo completo") {
  df %>%
    filter(modelo == modelo_keep,
           term == "sexoFemale") %>% # Nombre exacto de tu tabla
    transmute(estudio = estudio_lab, HR = HR, lo = IC_inf, hi = IC_sup)
}

sx_2009 <- get_sexo(resumen, "ENS 2009-2010")
sx_2016 <- get_sexo(resumen_2016, "ENS 2016-2017")

sx_both <- bind_rows(sx_2009, sx_2016) %>%
  mutate(TE = log(HR), seTE = (log(hi)-log(lo))/(2*1.96))

m_sexo <- metagen(TE = sx_both$TE, seTE = sx_both$seTE,
                  studlab = sx_both$estudio, sm = "HR",
                  comb.fixed = FALSE, comb.random = TRUE, method.tau = "REML")

summary(m_sexo)
forest(m_sexo, backtransf = TRUE, xlab = "Hazard Ratio",
       main = "Sexo (Mujer vs Hombre) — Modelo completo",
       leftlabs = "Estudio", rightlabs = "HR (IC95%)",
       smlab = "Modelo de efectos aleatorios")

# 1. Abrimos el archivo PNG indicando la ruta y el tamaño
png("output/graphs/Metaanalisis_AF_solo_sexo.png", 
    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico (aquí pones tu código exacto del forest)
forest(m_sexo, backtransf = TRUE,
       xlab = "Hazard Ratio",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# 3. Cerramos el archivo para que se guarde físicamente
dev.off()

# ==========================================
# 3) META-ANÁLISIS: AF (MODELO CRUDO)
# ==========================================

get_af_crudo <- function(df, estudio_lab,
                   term_af = "af_cancer_binariaYes",
                   modelo_keep = "Crudo") {
  df %>%
    filter(modelo == modelo_keep, term == term_af) %>%
    transmute(
      estudio = estudio_lab,
      HR = HR, 
      lo = IC_inf, 
      hi = IC_sup
    )
}

af_crudo_2009  <- get_af_crudo(resumen,        "ENS 2009-2010")
af_crudo_2016  <- get_af_crudo(resumen_2016,   "ENS 2016-2017")

af_both_crudo <- bind_rows(af_crudo_2009, af_crudo_2016) %>%
  mutate(
    TE   = log(HR),
    seTE = (log(hi) - log(lo)) / (2*1.96)
  )

m_af_crudo <- metagen(
  TE = af_both_crudo$TE,
  seTE = af_both_crudo$seTE,
  studlab = af_both_crudo$estudio,
  sm = "HR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML"
)

summary(m_af_crudo)
forest(m_af_crudo, backtransf = TRUE,
       xlab = "Hazard Ratio",
       main = "AF Cáncer — Modelo Crudo",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")


# Guardar gráfico final (descomentar para guardar físicamente)
# 1. Abrimos el archivo PNG indicando la ruta y el tamaño
png("output/graphs/Metaanalisis_AF_Modelocrudo.png", 
    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico (aquí pones tu código exacto del forest)
forest(m_af_crudo, backtransf = TRUE,
       xlab = "Hazard Ratio",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# 3. Cerramos el archivo para que se guarde físicamente
dev.off()

# ==========================================
# 4) META-ANÁLISIS: AF (MODELO EDAD+SEXO)
# ==========================================

get_edadsexo <- function(df, estudio_lab,
                   term_af = "af_cancer_binariaYes",
                   modelo_keep = "Edad+Sexo") {
  df %>%
    filter(modelo == modelo_keep, term == term_af) %>%
    transmute(
      estudio = estudio_lab,
      HR = HR, 
      lo = IC_inf, 
      hi = IC_sup
    )
}

af_edadsexo_2009  <- get_edadsexo(resumen,        "ENS 2009-2010")
af_edadsexo_2016  <- get_edadsexo(resumen_2016,   "ENS 2016-2017")

af_both_edadsexo <- bind_rows(af_edadsexo_2009, af_edadsexo_2016) %>%
  mutate(
    TE   = log(HR),
    seTE = (log(hi) - log(lo)) / (2*1.96)
  )

m_af_edadsexo <- metagen(
  TE = af_both_edadsexo$TE,
  seTE = af_both_edadsexo$seTE,
  studlab = af_both_edadsexo$estudio,
  sm = "HR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML"
)

summary(m_af_edadsexo)
forest(m_af_edadsexo, backtransf = TRUE,
       xlab = "Hazard Ratio",
       main = "AF Cáncer — Modelo Edad+Sexo",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# Guardar gráfico final (descomentar para guardar físicamente)
# 1. Abrimos el archivo PNG indicando la ruta y el tamaño
png("output/graphs/Metaanalisis_AF_Modeloedadsexo.png", 
    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico (aquí pones tu código exacto del forest)
forest(m_af_edadsexo, backtransf = TRUE,
       xlab = "Hazard Ratio",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# 3. Cerramos el archivo para que se guarde físicamente
dev.off()
