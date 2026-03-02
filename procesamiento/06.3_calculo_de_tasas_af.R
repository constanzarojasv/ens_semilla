# 0.Cargar librerías
source("procesamiento/04_etiquetado_variables.R", encoding = "UTF-8")


#Calculo de tasa de mortalidad general según af_cancer_binaria
analizar_tasa_5y_af_2009 <- function(datos, variable_grupo, pesos, conglomerado, estrato) {
    
  # 1. Definir límite de tiempo (5 años)
  limite <- 1826.25

  # 2. Preparar datos (Crear variable de evento y filtrar NAs)
  datos_prep <- datos %>%
    filter(!is.na(!!sym(variable_grupo))) %>% 
    mutate(
      muerte_5y = if_else(!is.na(FECHA_DEF) & dias_transcurridos <= limite, 1, 0)
    )
  
  # 3. Crear el diseño muestral
  diseno <- svydesign(
    id = as.formula(paste0("~", conglomerado)),
    strata = as.formula(paste0("~", estrato)),
    weights = as.formula(paste0("~", pesos)),
    data = datos_prep,
    nest = TRUE
  )
  
  options(survey.lonely.psu = "adjust")
  
  # 4. Calcular tasa (Proporción)
  tabla_tasa <- svyby(
    formula = ~muerte_5y,
    by = as.formula(paste0("~", variable_grupo)),
    design = diseno,
    svymean,
    na.rm = TRUE
  )
  
  # 5. Formatear resultados (Escalar a 1.000)
  # Usamos pmax(0, ...) para evitar que el límite inferior sea negativo
  tabla_final <- tabla_tasa %>%
    mutate(
      incidencia_1000 = muerte_5y * 1000,
      lim_inf_1000 = pmax(0, (muerte_5y - 1.96 * se) * 1000),
      lim_sup_1000 = (muerte_5y + 1.96 * se) * 1000
    )
  
  # 6. Test de Independencia (Rao-Scott)
  test_rao <- svychisq(
    formula = as.formula(paste0("~", variable_grupo, " + muerte_5y")),
    design = diseno
  )
  
  # Retornar una lista con ambos resultados
  return(list(
    resultados_tabla = tabla_final,
    test_estadistico = test_rao
  ))
}

# Ejemplo para ENS 2003
analisis_af_5y_2009 <- analizar_tasa_5y_af_2009(
  datos = ens2009_final,
  variable_grupo = "af_cancer_binaria",
  pesos = "FEXP1",
  conglomerado = "conglomerado",
  estrato = "estrato"
)

# Para ver la tabla:
print(analisis_af_5y_2009$resultados_tabla)

# Para ver el p-valor:
print(analisis_af_5y_2009$test_estadistico)

#LO ANTERIOR ES MORTALIDAD GENERAL. AHORA ES ESPECIFICA POR CANCER
# 1. Definición de la función con nombre específico para cáncer
analizar_tasa_cancer_5y_af_2009 <- function(datos, variable_grupo, pesos, conglomerado, estrato, limite = 1826.25) {
  
  # 2. Preparar datos (Evento: Cáncer + Tiempo <= 5 años)
  datos_prep <- datos %>%
    filter(!is.na(!!sym(variable_grupo))) %>% 
    mutate(
      # El evento ocurre solo si es muerte por cáncer Y está dentro del límite
      muerte_5y = if_else(muerte_cancer == "Cancer death" & dias_transcurridos <= limite, 1, 0),
      # Tratamos NAs como 0 (no evento/censura)
      muerte_5y = coalesce(muerte_5y, 0)
    )
  
  # 3. Crear el diseño muestral
  diseno <- svydesign(
    id = as.formula(paste0("~", conglomerado)),
    strata = as.formula(paste0("~", estrato)),
    weights = as.formula(paste0("~", pesos)),
    data = datos_prep,
    nest = TRUE
  )
  
  options(survey.lonely.psu = "adjust")
  
  # 4. Calcular tasa (Proporción)
  tabla_tasa <- svyby(
    formula = ~muerte_5y,
    by = as.formula(paste0("~", variable_grupo)),
    design = diseno,
    svymean,
    na.rm = TRUE
  )
  
  # 5. Formatear resultados (Escalar a 1.000)
  tabla_final <- tabla_tasa %>%
    mutate(
      incidencia_cancer_1000 = muerte_5y * 1000,
      lim_inf_1000 = pmax(0, (muerte_5y - 1.96 * se) * 1000),
      lim_sup_1000 = (muerte_5y + 1.96 * se) * 1000
    )
  
  # 6. Test de Independencia (Rao-Scott)
  test_rao <- svychisq(
    formula = as.formula(paste0("~", variable_grupo, " + muerte_5y")),
    design = diseno
  )
  
  return(list(
    resultados_tabla = tabla_final,
    test_estadistico = test_rao
  ))
}

# --- Ejecución del análisis ---
analisis_cancer_5y_af_2009 <- analizar_tasa_cancer_5y_af_2009(
  datos = ens2009_final,
  variable_grupo = "af_cancer_binaria",
  pesos = "FEXP1",
  conglomerado = "conglomerado",
  estrato = "estrato"
)

# Para ver los resultados:
print(analisis_cancer_5y_af_2009$resultados_tabla)


# ==============================================================================
# 1. MORTALIDAD GENERAL - ENS 2016
# ==============================================================================

analizar_tasa_5y_af_2016 <- function(datos, variable_grupo, pesos, conglomerado, estrato) {
    
  # 1. Definir límite de tiempo (5 años)
  limite <- 1826.25

  # 2. Preparar datos (Evento general basado en FECHA_DEF)
  datos_prep <- datos %>%
    filter(!is.na(!!sym(variable_grupo))) %>% 
    mutate(
      muerte_5y = if_else(!is.na(FECHA_DEF) & dias_transcurridos <= limite, 1, 0)
    )
  
  # 3. Diseño muestral
  diseno <- svydesign(
    id = as.formula(paste0("~", conglomerado)),
    strata = as.formula(paste0("~", estrato)),
    weights = as.formula(paste0("~", pesos)),
    data = datos_prep,
    nest = TRUE
  )
  
  options(survey.lonely.psu = "adjust")
  
  # 4. Calcular tasa
  tabla_tasa <- svyby(
    formula = ~muerte_5y,
    by = as.formula(paste0("~", variable_grupo)),
    design = diseno,
    svymean,
    na.rm = TRUE
  )
  
  # 5. Formatear resultados
  tabla_final <- tabla_tasa %>%
    mutate(
      incidencia_1000 = muerte_5y * 1000,
      lim_inf_1000 = pmax(0, (muerte_5y - 1.96 * se) * 1000),
      lim_sup_1000 = (muerte_5y + 1.96 * se) * 1000
    )
  
  # 6. Test Rao-Scott
  test_rao <- svychisq(
    formula = as.formula(paste0("~", variable_grupo, " + muerte_5y")),
    design = diseno
  )
  
  return(list(resultados_tabla = tabla_final, test_estadistico = test_rao))
}

# Ejecución Mortalidad General 2016
analisis_af_5y_2016 <- analizar_tasa_5y_af_2016(
  datos = ens2016_final,
  variable_grupo = "af_cancer_binaria",
  pesos = "Fexp_F1p_Corr",
  conglomerado = "conglomerado",
  estrato = "estrato"
)

# ==============================================================================
# 2. MORTALIDAD ESPECÍFICA POR CÁNCER - ENS 2016
# ==============================================================================

analizar_tasa_cancer_5y_af_2016 <- function(datos, variable_grupo, pesos, conglomerado, estrato, limite = 1826.25) {
  
  # 2. Preparar datos (Evento: Causa Cáncer + Tiempo <= 5 años)
  datos_prep <- datos %>%
    filter(!is.na(!!sym(variable_grupo))) %>% 
    mutate(
      # Evento si es muerte por cáncer Y dentro del tiempo límite
      muerte_5y = if_else(muerte_cancer == "Cancer death" & dias_transcurridos <= limite, 1, 0),
      muerte_5y = coalesce(muerte_5y, 0)
    )
  
  # 3. Diseño muestral
  diseno <- svydesign(
    id = as.formula(paste0("~", conglomerado)),
    strata = as.formula(paste0("~", estrato)),
    weights = as.formula(paste0("~", pesos)),
    data = datos_prep,
    nest = TRUE
  )
  
  options(survey.lonely.psu = "adjust")
  
  # 4. Calcular tasa
  tabla_tasa <- svyby(
    formula = ~muerte_5y,
    by = as.formula(paste0("~", variable_grupo)),
    design = diseno,
    svymean,
    na.rm = TRUE
  )
  
  # 5. Formatear resultados
  tabla_final <- tabla_tasa %>%
    mutate(
      incidencia_cancer_1000 = muerte_5y * 1000,
      lim_inf_1000 = pmax(0, (muerte_5y - 1.96 * se) * 1000),
      lim_sup_1000 = (muerte_5y + 1.96 * se) * 1000
    )
  
  # 6. Test Rao-Scott
  test_rao <- svychisq(
    formula = as.formula(paste0("~", variable_grupo, " + muerte_5y")),
    design = diseno
  )
  
  return(list(resultados_tabla = tabla_final, test_estadistico = test_rao))
}

# Ejecución Mortalidad Cáncer 2016
analisis_cancer_5y_af_2016 <- analizar_tasa_cancer_5y_af_2016(
  datos = ens2016_final,
  variable_grupo = "af_cancer_binaria",
  pesos = "Fexp_F1p_Corr",
  conglomerado = "conglomerado",
  estrato = "estrato"
)

# --- Visualización de Resultados ---
print(analisis_af_5y_2016$resultados_tabla)

print(analisis_cancer_5y_af_2016$resultados_tabla)
