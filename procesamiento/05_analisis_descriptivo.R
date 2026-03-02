#Cargar setup con librerías, bases y etiquetado de variables
source("procesamiento/04_etiquetado_variables.R", encoding = "UTF-8")

# --- 1️⃣ Preparar variables ---
ens2003_final <- ens2003_final %>%
  mutate(
    tiempo_total = dias_transcurridos / 365.25,       # convertir días a años
    evento_total = muerte_cancer,                     # evento sin censura
    evento_label = factor(evento_total,
                          levels = c(0,1),
                          labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )

ens2003_final$ictaumentado <- as.numeric(ens2003_final$ict >= 0.6)

# --- 2️⃣ Diseño de encuesta ---
# 1. Definir el diseño original con todos los datos
survey_design2003 <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~FEXP_analisis,
  data = ens2003_final,
  nest = TRUE
)

# 2. Crear un subconjunto para la variable específica (esto mantiene la integridad del diseño)
survey_designdepresion2003 <- subset(survey_design2003, !is.na(Depresion_1_AP))
options(survey.lonely.psu="adjust")

#2. realizar tabla
tabla1_depresion_2003 <- survey_designdepresion2003 %>% 
  tbl_svysummary(
    by = Depresion_1_AP, 
    include = c(edad, Edad_Codificada, sexo, NEDU, zona, fuma, estado_nutricional, a17, muerte_cancer, fallecidos),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      # CAMBIO CLAVE: agregamos {n_unweighted} para ver el n real
      all_categorical() ~ "{n_unweighted} ({p}%)" 
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)), # 0 decimales para n, 1 para %
    missing = "no",
    label = list(
      edad ~ "Edad (años)",
      sexo ~ "Sexo",
      NEDU ~ "Nivel educacional",
      zona ~ "Zona",
      fuma ~ "Hábito tabáquico",
      estado_nutricional ~ "Estado nutricional",
      a17 ~ "Realiza actividad física",
      muerte_cancer ~ "Mortalidad por cáncer",
      fallecidos ~ "Estado vital"
    )
  ) %>%
  # El argumento unweighted = TRUE asegura que la columna 'N' sea el conteo real
  add_n(unweighted = TRUE) %>% 
  add_p(test = list(all_continuous() ~ "svy.t.test", all_categorical() ~ "svy.wald.test")) %>%
  add_overall(last = FALSE, col_label = "**General (N={N_unweighted})**") %>%
  modify_header(
    label = "**Variable**",
    stat_1 = "**Sin síntomas (N={n_unweighted})**",
    stat_2 = "**Con síntomas (N={n_unweighted})**"
  ) %>%
  bold_labels()

# 3. Mostrar el resultado
tabla1_depresion_2003

# Guardar el objeto intacto
saveRDS(tabla1_depresion_2003, file = "output/tables/tabla1_depresion_2003.rds")

# (Tus compañeros la podrán abrir en sus computadores usando:)
# tabla_cargada <- readRDS("output/tables/tabla1_depresion_2003.rds")

# Convertir a formato tabla de datos (tibble) y guardar
tabla1_depresion_2003 %>%
  as_tibble() %>%
  export(file = "output/tables/tabla1_depresion_2003.xlsx") # Usando rio

# O si prefieres un CSV:
# tabla1_depresion_2003 %>%
#   as_tibble() %>%
#   write_csv(file = "output/tables/tabla1_depresion_2003.csv") # Usando readr


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#----------------------ICT-------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# 1. Definir el diseño original con todos los datos
survey_design2003 <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~FEXP_analisis,
  data = ens2003_final,
  nest = TRUE
)

#tabla 1 2003 para ICT
# 2. Crear un subconjunto para la variable específica (esto mantiene la integridad del diseño)
survey_design_ICT_2003 <- subset(survey_design2003, !is.na(ict))
options(survey.lonely.psu="adjust")

#2. realizar tabla
tabla1_ict_2003 <- survey_design_ICT_2003 %>% 
  tbl_svysummary(
    by = ictaumentado, 
    include = c(edad, Edad_Codificada, sexo, NEDU, zona, fuma, estado_nutricional, a17, muerte_cancer, fallecidos),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      # CAMBIO CLAVE: agregamos {n_unweighted} para ver el n real
      all_categorical() ~ "{n_unweighted} ({p}%)" 
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)), # 0 decimales para n, 1 para %
    missing = "no",
    label = list(
      edad ~ "Edad (años)",
      sexo ~ "Sexo",
      NEDU ~ "Nivel educacional",
      zona ~ "Zona",
      fuma ~ "Hábito tabáquico",
      estado_nutricional ~ "Estado nutricional",
      a17 ~ "Realiza actividad física",
      muerte_cancer ~ "Mortalidad por cáncer",
      fallecidos ~ "Estado vital"
    )
  ) %>%
  # El argumento unweighted = TRUE asegura que la columna 'N' sea el conteo real
  add_n(unweighted = TRUE) %>% 
  add_p(test = list(all_continuous() ~ "svy.t.test", all_categorical() ~ "svy.wald.test")) %>%
  add_overall(last = FALSE, col_label = "**General (N={N_unweighted})**") %>%
  modify_header(
    label = "**Variable**",
    stat_1 = "**Sin síntomas (N={n_unweighted})**",
    stat_2 = "**Con síntomas (N={n_unweighted})**"
  ) %>%
  bold_labels()

# 3. Mostrar el resultado
tabla1_ict_2003


#Tabla 1 para AF de cáncer (ENS 2009)

#En el factor de expansión del F2 hay missing values (terremoto?)
sum(is.na(ens2009_final$FEXP_analisis)) #Hay 333 MV.

#Los voy a filtrar
ens2009_final_limpia <- ens2009_final %>%
  filter(!is.na(FEXP_analisis))

sum(is.na(ens2009_final_limpia$FEXP_analisis))


#Crear variable de interés
ens2009_final_limpia <- ens2009_final_limpia %>%
  mutate(
    tiempo_total = dias_transcurridos / 365.25,       # convertir días a años
    evento_total = muerte_cancer,                     # evento sin censura
    evento_label = factor(evento_total,
                          levels = c(0,1),
                          labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )

#Cambio clase de variable explicatoria
ens2009_final_limpia <- ens2009_final_limpia %>%
  mutate(af_cancer_binaria = as_factor(af_cancer_binaria))

#Crear diseño muestral con base sin MV. PREGUNTAR A ANGELICA SI SEGUIR CON ESTO O USAR F1
# 1. Definir el diseño original con todos los datos
survey_design2009 <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~FEXP_analisis,
  data = ens2009_final_limpia,
  nest = TRUE
)

# 2. Crear un subconjunto para la variable específica (esto mantiene la integridad del diseño)
survey_design_AF_2009 <- subset(survey_design2009, !is.na(af_cancer_binaria))
options(survey.lonely.psu="adjust")

#2. realizar tabla
tabla1_AF_2009 <- survey_design_AF_2009 %>% 
  tbl_svysummary(
    by = af_cancer_binaria, 
    include = c(edad, Edad_Codificada, sexo, NEDU, zona, fuma, estado_nutricional, GPAQ, AUDIT_RIESGOSO, muerte_cancer, fallecidos),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      # CAMBIO CLAVE: agregamos {n_unweighted} para ver el n real
      all_categorical() ~ "{n_unweighted} ({p}%)" 
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)), # 0 decimales para n, 1 para %
    missing = "no"
  ) %>%
  # El argumento unweighted = TRUE asegura que la columna 'N' sea el conteo real
  add_n(unweighted = TRUE) %>% 
  add_p(test = list(all_continuous() ~ "svy.t.test", all_categorical() ~ "svy.wald.test")) %>%
  add_overall(last = FALSE, col_label = "**General (N={N_unweighted})**") %>%
  modify_header(
    label = "**Variable**",
    stat_1 = "**No family history (N={n_unweighted})**",
    stat_2 = "**≥1 relative with cancer (N={n_unweighted})**"
  ) %>%
  bold_labels()


# 3. Mostrar el resultado
tabla1_AF_2009

# Convertir a formato tabla de datos (tibble) y guardar
tabla1_AF_2009 %>%
  as_tibble() %>%
  export(file = "output/tables/AF_cancer/tabla1_AF_2009.xlsx") 

# Convertir a kable y luego a markdown
tabla1_AF_2009 %>%
  as_kable()

# Guardar el contenido en un objeto
tabla_md_AF_2009 <- tabla1_AF_2009 %>%
  as_kable(format = "markdown")

# Crear el archivo físico
writeLines(tabla_md_AF_2009, "output/tables/AF_cancer/tabla1_AF_2009.md")

#Tabla 1 para AF de cáncer (ENS 2016)

#En el factor de expansión del F2 hay missing values (terremoto?)
sum(is.na(ens2016_final$FEXP_analisis)) #Hay 605 MV.

#Los voy a filtrar
ens2016_final_limpia <- ens2016_final %>%
  filter(!is.na(FEXP_analisis))

sum(is.na(ens2016_final_limpia$FEXP_analisis))


#Crear variable de interés
ens2016_final_limpia <- ens2016_final_limpia %>%
  mutate(
    tiempo_total = dias_transcurridos / 365.25,       # convertir días a años
    evento_total = muerte_cancer,                     # evento sin censura
    evento_label = factor(evento_total,
                          levels = c(0,1),
                          labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )

#Cambio clase de variable explicatoria
ens2016_final_limpia <- ens2016_final_limpia %>%
  mutate(af_cancer_binaria = as_factor(af_cancer_binaria))

#Crear diseño muestral con base sin MV. PREGUNTAR A ANGELICA SI SEGUIR CON ESTO O USAR F1
# 1. Definir el diseño original con todos los datos
survey_design2016 <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~FEXP_analisis,
  data = ens2016_final_limpia,
  nest = TRUE
)

# 2. Crear un subconjunto para la variable específica (esto mantiene la integridad del diseño)
survey_design_AF_2016 <- subset(survey_design2016, !is.na(af_cancer_binaria))
options(survey.lonely.psu="adjust")

#2. realizar tabla
tabla1_AF_2016 <- survey_design_AF_2016 %>% 
  tbl_svysummary(
    by = af_cancer_binaria, 
    include = c(edad, Edad_Codificada, sexo, NEDU, zona, fuma, estado_nutricional, GPAQ, AUDIT_RIESGOSO, muerte_cancer), #quité var fallecidos por mientras
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      # CAMBIO CLAVE: agregamos {n_unweighted} para ver el n real
      all_categorical() ~ "{n_unweighted} ({p}%)" 
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)), # 0 decimales para n, 1 para %
    missing = "no"
  ) %>%
  # El argumento unweighted = TRUE asegura que la columna 'N' sea el conteo real
  add_n(unweighted = TRUE) %>% 
  add_p(test = list(all_continuous() ~ "svy.t.test", all_categorical() ~ "svy.wald.test")) %>%
  add_overall(last = FALSE, col_label = "**General (N={N_unweighted})**") %>%
  modify_header(
    label = "**Variable**",
    stat_1 = "**No family history (N={n_unweighted})**",
    stat_2 = "**≥1 relative with cancer (N={n_unweighted})**"
  ) %>%
  bold_labels()


# 3. Mostrar el resultado
tabla1_AF_2016

# Convertir a formato tabla de datos (tibble) y guardar
tabla1_AF_2016 %>%
  as_tibble() %>%
  export(file = "output/tables/AF_cancer/tabla1_AF_2016.xlsx") 

# Convertir a kable y luego a markdown
tabla1_AF_2016 %>%
  as_kable()

# Guardar el contenido en un objeto
tabla1_AF_2016 <- tabla1_AF_2016 %>%
  as_kable(format = "markdown")

# Crear el archivo físico
writeLines(tabla1_AF_2016, "output/tables/AF_cancer/tabla1_AF_2016.md")