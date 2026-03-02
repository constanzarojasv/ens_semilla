# 0.Cargar librerías
source("procesamiento/00_setup.R", encoding = "UTF-8")

ens2003_final$Edad_Codificada
ens2009_final$Edad_Codificada
ens2016_final$Edad_Codificada

#funcion para  calcular tasas a 5 años
analizar_tasa_5y <- function(datos, variable_grupo, pesos, conglomerado, estrato) {
  
  library(survey)
  library(dplyr)
  
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
analisis2003_ict_5y <- analizar_tasa_5y(
  datos = ens2003_final,
  variable_grupo = "ictaumentado",
  pesos = "FEXP_analisis",
  conglomerado = "conglomerado",
  estrato = "estrato"
)

# Para ver la tabla:
print(analisis2003_ict_5y$resultados_tabla)

# Para ver el p-valor:
print(analisis2003_ict_5y$test_estadistico)

#LO ANTERIOR ES MORTALIDAD GENERAL. AHORA ES ESPECIFICA POR CANCER
# 1. Definición de la función con nombre específico para cáncer
analizar_tasa_cancer_5y <- function(datos, variable_grupo, pesos, conglomerado, estrato, limite = 1826.25) {
  
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
# El objeto ahora se llama analisis_cancer_5y_depresion
analisis2003_cancer_5y_ict <- analizar_tasa_cancer_5y(
  datos = ens2003_final,
  variable_grupo = "ictaumentado",
  pesos = "FEXP_analisis",
  conglomerado = "conglomerado",
  estrato = "estrato"
)

# Para ver los resultados:
print(analisis2003_cancer_5y_ict$resultados_tabla)
print(analisis2003_cancer_5y_ict$test_estadistico)

# 2009
analisis2009_cancer_5y_ict <- analizar_tasa_cancer_5y(
  datos = ens2009_final,
  variable_grupo = "ictaumentado",
  pesos = "FEXP1",
  conglomerado = "conglomerado",
  estrato = "estrato"
)

# Para ver los resultados:
print(analisis2009_cancer_5y_ict$resultados_tabla)
print(analisis2009_cancer_5y_ict$test_estadistico)

# 2016
analisis2016_cancer_5y_ict <- analizar_tasa_cancer_5y(
  datos = ens2016_final,
  variable_grupo = "ictaumentado",
  pesos = "Fexp_F1p_Corr",
  conglomerado = "conglomerado",
  estrato = "estrato"
)

# Para ver los resultados:
print(analisis2016_cancer_5y_ict$resultados_tabla)
# Para ver el p-valor:
print(analisis2016_cancer_5y_ict$test_estadistico)
