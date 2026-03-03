source("procesamiento/07.2_KM_Cox_ICT.R", encoding = "UTF-8")

# ==========================================
# 1) META-ANÁLISIS: ICT (MODELO COMPLETO)
# ==========================================

# Función para extraer HR e IC95% de ICT por encuesta
get_ict <- function(df, estudio_lab,
                    term_ict = "ictaumentado1", 
                    modelo_keep = "Modelo completo") { 
  df %>%
    filter(modelo == modelo_keep, term == term_ict) %>%
    transmute(
      estudio = estudio_lab,
      HR = HR, 
      lo = IC_inf, 
      hi = IC_sup
    )
}

# Extraer ICT de cada ENS y unir
ict_2003  <- get_ict(resumen_2003,        "ENS 2003")
ict_2009  <- get_ict(resumen_2009,        "ENS 2009-2010")
ict_2016  <- get_ict(resumen_2016,   "ENS 2016-2017")
ict_both <- bind_rows(ict_2003, ict_2009, ict_2016)

# Transformar a log(HR) y SE desde IC95%
ict_both <- ict_both %>%
  mutate(
    TE   = log(HR),
    seTE = (log(hi) - log(lo)) / (2*1.96)
  )

# Meta-análisis: efectos aleatorios (REML)
m_ict <- metagen(
  TE = ict_both$TE,
  seTE = ict_both$seTE,
  studlab = ict_both$estudio,
  sm = "HR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML"
)

# Resumen y forest plot
summary(m_ict)
forest(m_ict, backtransf = TRUE,
       xlab = "Hazard Ratio",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# Guardar gráfico final (descomentar para guardar físicamente)
# 1. Abrimos el archivo PNG indicando la ruta y el tamaño
#png("output/graphs/Metaanalisis_ICT_ModeloCompleto.png", 
#    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico
#forest(m_ict, backtransf = TRUE,
#       xlab = "Hazard Ratio",
#       leftlabs = c("Estudio"),
#       rightlabs = c("HR (IC95%)"),
#       smlab = "Modelo de efectos aleatorios")

# 3. Cerramos el archivo para que se guarde físicamente
#dev.off()


# ==========================================
# 2) META-ANÁLISIS: SEXO (MODELO COMPLETO)
# ==========================================

get_sexo <- function(df, estudio_lab, modelo_keep = "Modelo completo") {
  df %>%
    filter(modelo == modelo_keep,
           term == "sexoFemale") %>% 
    transmute(estudio = estudio_lab, HR = HR, lo = IC_inf, hi = IC_sup)
}

sx_2003 <- get_sexo(resumen_2003, "ENS 2003")
sx_2009 <- get_sexo(resumen_2009, "ENS 2009-2010")
sx_2016 <- get_sexo(resumen_2016, "ENS 2016-2017")

sx_both <- bind_rows(sx_2003, sx_2009, sx_2016) %>%
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
#png("output/graphs/Metaanalisis_ICT_solo_sexo.png", 
#    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico
#forest(m_sexo, backtransf = TRUE,
#       xlab = "Hazard Ratio",
#       leftlabs = c("Estudio"),
#       rightlabs = c("HR (IC95%)"),
#       smlab = "Modelo de efectos aleatorios")

# 3. Cerramos el archivo para que se guarde físicamente
#dev.off()


# ==========================================
# 3) META-ANÁLISIS: ICT (MODELO CRUDO)
# ==========================================

get_ict_crudo <- function(df, estudio_lab,
                          term_ict = "ictaumentado1",
                          modelo_keep = "Crudo") {
  df %>%
    filter(modelo == modelo_keep, term == term_ict) %>%
    transmute(
      estudio = estudio_lab,
      HR = HR, 
      lo = IC_inf, 
      hi = IC_sup
    )
}

ict_crudo_2003  <- get_ict_crudo(resumen_2003,        "ENS 2003")
ict_crudo_2009  <- get_ict_crudo(resumen_2009,        "ENS 2009-2010")
ict_crudo_2016  <- get_ict_crudo(resumen_2016,   "ENS 2016-2017")

ict_both_crudo <- bind_rows(ict_crudo_2003, ict_crudo_2009, ict_crudo_2016) %>%
  mutate(
    TE   = log(HR),
    seTE = (log(hi) - log(lo)) / (2*1.96)
  )

m_ict_crudo <- metagen(
  TE = ict_both_crudo$TE,
  seTE = ict_both_crudo$seTE,
  studlab = ict_both_crudo$estudio,
  sm = "HR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "REML"
)

summary(m_ict_crudo)
forest(m_ict_crudo, backtransf = TRUE,
       xlab = "Hazard Ratio",
       main = "ICT Aumentado — Modelo Crudo",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")


# Guardar gráfico final (descomentar para guardar físicamente)
# 1. Abrimos el archivo PNG indicando la ruta y el tamaño
#png("output/graphs/Metaanalisis_ICT_Modelocrudo.png", 
#    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico
#forest(m_ict_crudo, backtransf = TRUE,
#       xlab = "Hazard Ratio",
#       leftlabs = c("Estudio"),
#       rightlabs = c("HR (IC95%)"),
#       smlab = "Modelo de efectos aleatorios")

# 3. Cerramos el archivo para que se guarde físicamente
#dev.off()


# ==========================================
# 4) META-ANÁLISIS: ICT (MODELO EDAD+SEXO)
# ==========================================

get_ict_edadsexo <- function(df, estudio_lab,
                             term_ict = "ictaumentado1",
                             modelo_keep = "Edad+Sexo") {
  df %>%
    filter(modelo == modelo_keep, term == term_ict) %>%
    transmute(
      estudio = estudio_lab,
      HR = HR, 
      lo = IC_inf, 
      hi = IC_sup
    )
}

ict_edadsexo_2003  <- get_ict_edadsexo(resumen_2003,        "ENS 2003")
ict_edadsexo_2009  <- get_ict_edadsexo(resumen_2009,        "ENS 2009-2010")
ict_edadsexo_2016  <- get_ict_edadsexo(resumen_2016,   "ENS 2016-2017")

ict_both_edadsexo <- bind_rows(ict_edadsexo_2003, ict_edadsexo_2009, ict_edadsexo_2016) %>%
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
       main = "ICT Aumentado — Modelo Edad+Sexo",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# Guardar gráfico final (descomentar para guardar físicamente)
# 1. Abrimos el archivo PNG indicando la ruta y el tamaño
#png("output/graphs/Metaanalisis_ICT_Modeloedadsexo.png", 
    width = 10, height = 5, units = "in", res = 300)

# 2. Dibujamos el gráfico
#forest(m_ict_edadsexo, backtransf = TRUE,
       xlab = "Hazard Ratio",
       leftlabs = c("Estudio"),
       rightlabs = c("HR (IC95%)"),
       smlab = "Modelo de efectos aleatorios")

# 3. Cerramos el archivo para que se guarde físicamente
#dev.off()
