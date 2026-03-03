source("procesamiento/07.3_KM_Cox_af.R", encoding = "UTF-8")

# ==========================================
# 1) META-ANÁLISIS: Depresion (MODELO COMPLETO)
# ==========================================

# Función corregida para extraer HR e IC95% de  por encuesta
get_af <- function(df, estudio_lab,
                   term_depresion = "ictaumentado1", # Nombre exacto de tu tabla
                   modelo_keep = "Modelo completo") { # Nombre exacto de tu tabla
  df %>%
    filter(modelo == modelo_keep, term == term_depresion) %>%
    transmute(
      estudio = estudio_lab,
      HR = HR, 
      lo = IC_inf, 
      hi = IC_sup
    )
}

# Extraer AF de cada ENS y unir
depresion_2003_incidencia  <- get_af(resumen2003,        "ENS 2003")
depresion_2009_incidencia  <- get_af(resumen2009,   "ENS 2009-2010")
depresion_2016_incidencia  <- get_af(resumen2009,   "ENS 2009-2010")
depresion_both_incidencia <- bind_rows(depresion_2003_incidencia , depresion_2009_incidencia, depresion_2016_incidencia)

# Transformar a log(HR) y SE desde IC95%
depresion_both_incidencia<- depresion_both_incidencia %>%
  mutate(
    TE   = log(HR),
    seTE = (log(hi) - log(lo)) / (2*1.96)
  )

# Meta-análisis: efectos aleatorios (REML)
m_depresion <- metagen(
  TE = depresion_both_incidencia$TE,
  seTE = depresion_both_incidencia$seTE,
  studlab = depresion_both_incidencia$estudio,
  sm = "HR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML"
)

# Resumen y forest plot
summary(m_depresion)
forest(m_depresion, backtransf = TRUE,
       xlab = "Hazard Ratio",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")


# Guardar gráfico final (descomentar para guardar físicamente)
# 1. Abrimos el archivo PNG indicando la ruta y el tamaño
png("output/graphs/incidencia/Metaanalisis_ICT_ModeloCompleto.png", 
    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico (aquí pones tu código exacto del forest)
forest(m_depresion, backtransf = TRUE,
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

sx_2003 <- get_sexo(resumen2003, "ENS 2003")
sx_2009 <- get_sexo(resumen2009, "ENS 2009-2010")
sx_2016 <- get_sexo(resumen2016, "ENS 2009-2010")

sx_both <- bind_rows(sx_2003, sx_2009, sx_2016) %>%
  mutate(TE = log(HR), seTE = (log(hi)-log(lo))/(2*1.96))

m_sexo_depresion <- metagen(TE = sx_both$TE, seTE = sx_both$seTE,
                  studlab = sx_both$estudio, sm = "HR",
                  comb.fixed = FALSE, comb.random = TRUE, method.tau = "REML")

summary(m_sexo_depresion)
forest(m_sexo_depresion, backtransf = TRUE, xlab = "Hazard Ratio",
       main = "Sexo (Mujer vs Hombre) — Modelo completo",
       leftlabs = "Estudio", rightlabs = "HR (IC95%)",
       smlab = "Modelo de efectos aleatorios")

# 1. Abrimos el archivo PNG indicando la ruta y el tamaño
png("output/graphs/incidencia/Metaanalisis_ICTsolo_sexo.png", 
    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico (aquí pones tu código exacto del forest)
forest(m_sexo_depresion, backtransf = TRUE,
       xlab = "Hazard Ratio",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# 3. Cerramos el archivo para que se guarde físicamente
dev.off()

# ==========================================
# 3) META-ANÁLISIS: Depresion (MODELO CRUDO)
# ==========================================

get_depresion_crudo <- function(df, estudio_lab,
                   term_af = "ictaumentado1",
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

Depresion_crudo_2003  <- get_depresion_crudo(resumen2003,        "ENS 2003")
Depresion_crudo_2009  <- get_depresion_crudo(resumen2009,   "ENS 2009-2010")
Depresion_crudo_2016  <- get_depresion_crudo(resumen2016,   "ENS 2009-2010")
Depresion_both_crudo <- bind_rows(Depresion_crudo_2003, Depresion_crudo_2009, Depresion_crudo_2016) %>%
  mutate(
    TE   = log(HR),
    seTE = (log(hi) - log(lo)) / (2*1.96)
  )

m_depresion_crudo <- metagen(
  TE = Depresion_both_crudo$TE,
  seTE = Depresion_both_crudo$seTE,
  studlab = Depresion_both_crudo$estudio,
  sm = "HR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML"
)

summary(m_depresion_crudo)
forest(m_depresion_crudo, backtransf = TRUE,
       xlab = "Hazard Ratio",
       main = "Sintomas depresivos Cáncer — Modelo Crudo",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")


# Guardar gráfico final (descomentar para guardar físicamente)
# 1. Abrimos el archivo PNG indicando la ruta y el tamaño
png("output/graphs/incidencia/Metaanalisis_ICT_Modelocrudo.png", 
    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico (aquí pones tu código exacto del forest)
forest(m_depresion_crudo, backtransf = TRUE,
       xlab = "Hazard Ratio",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# 3. Cerramos el archivo para que se guarde físicamente
dev.off()

# ==========================================
# 4) META-ANÁLISIS: Sintomas depresivos (MODELO EDAD+SEXO)
# ==========================================

get_edadsexo_depresion <- function(df, estudio_lab,
                   term_af = "ictaumentado1",
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

ict_edadsexo_2003  <- get_edadsexo_depresion(resumen2003,        "ENS 2003")
ict_edadsexo_2009  <- get_edadsexo_depresion(resumen2009,   "ENS 2009-2010")

ict_both_edadsexo <- bind_rows(ict_edadsexo_2003, ict_edadsexo_2009) %>%
  mutate(
    TE   = log(HR),
    seTE = (log(hi) - log(lo)) / (2*1.96)
  )

m_ict_edadsexo <- metagen(
  TE = ict_both_edadsexo$TE,
  seTE = ict_both_edadsexo$seTE,
  studlab = ict_both_edadsexo$estudio,
  sm = "HR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML"
)

summary(m_ict_edadsexo)

forest(m_ict_edadsexo, backtransf = TRUE,
       xlab = "Hazard Ratio",
       main = "Sintomas depresivos Cáncer — Modelo Edad+Sexo",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# Guardar gráfico final (descomentar para guardar físicamente)
# 1. Abrimos el archivo PNG indicando la ruta y el tamaño
png("output/graphs/incidencia/Metaanalisis_ICT_Modeloedadsexo.png", 
    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico (aquí pones tu código exacto del forest)
forest(m_depresion_edadsexo, backtransf = TRUE,
       xlab = "Hazard Ratio",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# 3. Cerramos el archivo para que se guarde físicamente
dev.off()
