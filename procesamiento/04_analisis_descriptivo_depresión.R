#Cargar setup (librerías)
source("procesamiento/00_setup.R", encoding = "UTF-8")

ens2003_final <- read_rds("input/data-procesada/data-analisis/ens2003_final.rds")
ens2009_final <- read_rds("input/data-procesada/data-analisis/ens2009_final.rds")
ens2016_final <- read_rds("input/data-procesada/data-analisis/ens2016_final.rds")

#primero etiquetar las variables y las categorias de las variables
#etiquetar en ENS 2003

#sexo
class(ens2003_final$sexo)
table(ens2003_final$sexo)
ens2003_final <- ens2003_final %>%
  mutate(sexo = factor(sexo, 
                       levels = c("1", "2"), 
                       labels = c("Male", "Female")))
label(ens2003_final$sexo) <- "Sex"
table(ens2003_final$sexo)

#EDAD
class(ens2003_final$edad)
label(ens2003_final$edad) <- "age"
table(ens2003_final$edad)

#NEDU
class(ens2003_final$NEDU)
table(ens2003_final$NEDU)
ENS_DEF2003$NEDU
ens2003_final <- ens2003_final %>%
  mutate(NEDU = factor(NEDU, 
                       levels = c("1", "2", "3"), 
                       labels = c("<8 years", "8-12 years", ">12 years")))
label(ens2003_final$NEDU) <- "Education"
table(ens2003_final$NEDU)

#zona
class(ens2003_final$zona)
table(ens2003_final$zona)
ens2003_final <- ens2003_final %>%
  mutate(zona = factor(zona, 
                       levels = c("1", "2"), 
                       labels = c("Urban", "Rural")))
label(ens2003_final$zona) <- "Zone"
table(ens2003_final$zona)

#fuma 1= si, ocasionalmente o uno o mas cigarros al dia 0= no fuma o  ha dejado de fumar
class(ens2003_final$fuma)
ens2003_final$fuma<-as.factor(ens2003_final$fuma)
class(ens2003_final$fuma)
table(ens2003_final$fuma)
ens2003_final <- ens2003_final %>%
  mutate(fuma = factor(fuma, 
                       levels = c("0", "1"), 
                       labels = c("Non-smoker", "Smoker")))
label(ens2003_final$fuma) <- "Smoking status"
table(ens2003_final$fuma)

#estado nutricional
class(ens2003_final$estado_nutricional)
ens2003_final$estado_nutricional<-as.factor(ens2003_final$estado_nutricional)
class(ens2003_final$estado_nutricional)
table(ens2003_final$estado_nutricional)
ens2003_final <- ens2003_final %>%
  mutate(estado_nutricional = factor(estado_nutricional, 
                       levels = c("Enflaquecido", "Normal","Sobrepeso","Obesidad"), 
                       labels = c("Underweight", "Normal","Overweight","Obesity")))
label(ens2003_final$estado_nutricional) <- "nutritional status"
table(ens2003_final$estado_nutricional)

#a17 actividad fisica
class(ens2003_final$a17)
ens2003_final$a17<-as.factor(ens2003_final$a17)
class(ens2003_final$a17)
table(ens2003_final$a17)
ens2003_final <- ens2003_final %>%
  mutate(a17 = factor(a17, 
                       levels = c("1", "2", "3", "4"), 
                       labels = c("Yes, 3 or more times per week", "Yes, 1 to 2 times per week", "Less than 4 times per month", "No exercise in the past month")))
label(ens2003_final$a17) <- "Physical activity"
table(ens2003_final$a17)

#muerte cancer
class(ens2003_final$muerte_cancer)
ens2003_final$muerte_cancer<-as.factor(ens2003_final$muerte_cancer)
class(ens2003_final$muerte_cancer)
table(ens2003_final$muerte_cancer)
ens2003_final <- ens2003_final %>%
  mutate(muerte_cancer = factor(muerte_cancer, 
                       levels = c("0", "1"), 
                       labels = c("No cancer death", "Cancer death")))
label(ens2003_final$muerte_cancer) <- "Cancer mortality"
table(ens2003_final$muerte_cancer)

#Fallecimiento
class(ens2003_final$fallecidos)
ens2003_final$fallecidos<-as.factor(ens2003_final$fallecidos)
class(ens2003_final$fallecidos)
table(ens2003_final$fallecidos)
ens2003_final <- ens2003_final %>%
  mutate(fallecidos = factor(fallecidos, 
                       levels = c("1", "2","3"), 
                       labels = c("Cancer death", "Other causes of death", "Alive")))
label(ens2003_final$fallecidos) <- "Vital status"
table(ens2003_final$fallecidos)

#Depresión
class(ens2003_final$Depresion_1_AP)
ens2003_final$Depresion_1_AP<-as.factor(ens2003_final$Depresion_1_AP)
class(ens2003_final$Depresion_1_AP)
table(ens2003_final$Depresion_1_AP)
ens2003_final <- ens2003_final %>%
  mutate(Depresion_1_AP = factor(Depresion_1_AP, 
                                 levels = c("0", "1"), 
                                 labels = c("No symptoms", "With symptoms")))
label(ens2003_final$Depresion_1_AP) <- "Depression"
table(ens2003_final$Depresion_1_AP)

######################################################################################################
#etiquetar variables ENS 2009
#sexo
class(ens2009_final$sexo)
table(ens2009_final$sexo)
ens2009_final <- ens2009_final %>%
  mutate(sexo = factor(sexo, 
                       levels = c("1", "2"), 
                       labels = c("Male", "Female")))
label(ens2009_final$sexo) <- "Sex"
table(ens2009_final$sexo)

#EDAD
class(ens2009_final$edad)
label(ens2009_final$edad) <- "age"
table(ens2009_final$edad)

#NEDU
class(ens2009_final$NEDU)
table(ens2009_final$NEDU)
ens2009_final <- ens2009_final %>%
  mutate(NEDU = factor(NEDU, 
                       levels = c("1", "2", "3"), 
                       labels = c("<8 years", "8-12 years", ">12 years")))
label(ens2009_final$NEDU) <- "Education"
table(ens2009_final$NEDU)

#zona
class(ens2009_final$zona)
table(ens2009_final$zona)
ens2009_final <- ens2009_final %>%
  mutate(zona = factor(zona, 
                       levels = c("1", "2"), 
                       labels = c("Urban", "Rural")))
label(ens2009_final$zona) <- "Zone"
table(ens2009_final$zona)

#fuma 1= si, ocasionalmente o uno o mas cigarros al dia 0= no fuma o  ha dejado de fumar
class(ens2009_final$fuma)
ens2009_final$fuma<-as.factor(ens2009_final$fuma)
class(ens2009_final$fuma)
table(ens2009_final$fuma)
ens2009_final <- ens2009_final %>%
  mutate(fuma = factor(fuma, 
                       levels = c("0", "1"), 
                       labels = c("Non-smoker", "Smoker")))
label(ens2009_final$fuma) <- "Smoking status"
table(ens2009_final$fuma)

#estado nutricional
class(ens2009_final$estado_nutricional)
ens2009_final$estado_nutricional<-as.factor(ens2009_final$estado_nutricional)
class(ens2009_final$estado_nutricional)
table(ens2009_final$estado_nutricional)
ens2009_final <- ens2009_final %>%
  mutate(estado_nutricional = factor(estado_nutricional, 
                       levels = c("Enflaquecido", "Normal","Sobrepeso","Obesidad"), 
                       labels = c("Underweight", "Normal","Overweight","Obesity")))
label(ens2009_final$estado_nutricional) <- "nutritional status"
table(ens2009_final$estado_nutricional)

#a17 actividad fisica
class(ens2009_final$a17)
ens2009_final$a17<-as.factor(ens2009_final$a17)
class(ens2009_final$a17)
table(ens2009_final$a17)
ens2009_final <- ens2009_final %>%
  mutate(a17 = factor(a17, 
                       levels = c("1", "2", "3", "4"), 
                       labels = c("Yes, 3 or more times per week", "Yes, 1 to 2 times per week", "Less than 4 times per month", "No exercise in the past month")))
label(ens2009_final$a17) <- "Physical activity"
table(ens2009_final$a17)

#muerte cancer
class(ens2009_final$muerte_cancer)
ens2009_final$muerte_cancer<-as.factor(ens2009_final$muerte_cancer)
class(ens2009_final$muerte_cancer)
table(ens2009_final$muerte_cancer)
ens2009_final <- ens2009_final %>%
  mutate(muerte_cancer = factor(muerte_cancer, 
                       levels = c("0", "1"), 
                       labels = c("No cancer death", "Cancer death")))
label(ens2009_final$muerte_cancer) <- "Cancer mortality"
table(ens2009_final$muerte_cancer)

#Fallecimiento
class(ens2009_final$fallecidos)
ens2009_final$fallecidos<-as.factor(ens2009_final$fallecidos)
class(ens2009_final$fallecidos)
table(ens2009_final$fallecidos)
ens2009_final <- ens2009_final %>%
  mutate(fallecidos = factor(fallecidos, 
                       levels = c("1", "2","3"), 
                       labels = c("Cancer death", "Other causes of death", "Alive")))
label(ens2009_final$fallecidos) <- "Vital status"
table(ens2009_final$fallecidos)

#Depresión
class(ens2009_final$Depresion_1_AP)
ens2009_final$Depresion_1_AP<-as.factor(ens2009_final$Depresion_1_AP)
class(ens2009_final$Depresion_1_AP)
table(ens2009_final$Depresion_1_AP)
ens2009_final <- ens2009_final %>%
  mutate(Depresion_1_AP = factor(Depresion_1_AP, 
                                 levels = c("0", "1"), 
                                 labels = c("No symptoms", "With symptoms")))
label(ens2009_final$Depresion_1_AP) <- "Depression"
table(ens2009_final$Depresion_1_AP)

##################################################################################################
#etiquetar variabales ens 2016
#sexo
class(ens2016_final$sexo)
table(ens2016_final$sexo)
ens2016_final <- ens2016_final %>%
  mutate(sexo = factor(sexo, 
                       levels = c("1", "2"), 
                       labels = c("Male", "Female")))
label(ens2016_final$sexo) <- "Sex"
table(ens2016_final$sexo)

#EDAD
class(ens2016_final$edad)
label(ens2016_final$edad) <- "age"
table(ens2016_final$edad)

#NEDU
class(ens2016_final$NEDU)
table(ens2016_final$NEDU)
ens2016_final <- ens2016_final %>%
  mutate(NEDU = factor(NEDU, 
                       levels = c("1", "2", "3"), 
                       labels = c("<8 years", "8-12 years", ">12 years")))
label(ens2016_final$NEDU) <- "Education"
table(ens2016_final$NEDU)

#zona
class(ens2016_final$zona)
table(ens2016_final$zona)
ens2016_final <- ens2016_final %>%
  mutate(zona = factor(zona, 
                       levels = c("1", "2"), 
                       labels = c("Urban", "Rural")))
label(ens2016_final$zona) <- "Zone"
table(ens2016_final$zona)

#fuma 1= si, ocasionalmente o uno o mas cigarros al dia 0= no fuma o  ha dejado de fumar
class(ens2016_final$fuma)
ens2016_final$fuma<-as.factor(ens2016_final$fuma)
class(ens2016_final$fuma)
table(ens2016_final$fuma)
ens2016_final <- ens2016_final %>%
  mutate(fuma = factor(fuma, 
                       levels = c("0", "1"), 
                       labels = c("Non-smoker", "Smoker")))
label(ens2016_final$fuma) <- "Smoking status"
table(ens2016_final$fuma)

#estado nutricional
class(ens2016_final$estado_nutricional)
ens2016_final$estado_nutricional<-as.factor(ens2016_final$estado_nutricional)
class(ens2016_final$estado_nutricional)
table(ens2016_final$estado_nutricional)
ens2016_final <- ens2016_final %>%
  mutate(estado_nutricional = factor(estado_nutricional, 
                       levels = c("Enflaquecido", "Normal","Sobrepeso","Obesidad"), 
                       labels = c("Underweight", "Normal","Overweight","Obesity")))
label(ens2016_final$estado_nutricional) <- "nutritional status"
table(ens2016_final$estado_nutricional)

#a17 actividad fisica
class(ens2016_final$a17)
ens2016_final$a17<-as.factor(ens2016_final$a17)
class(ens2016_final$a17)
table(ens2016_final$a17)
ens2016_final <- ens2016_final %>%
  mutate(a17 = factor(a17, 
                       levels = c("1", "2", "3", "4"), 
                       labels = c("Yes, 3 or more times per week", "Yes, 1 to 2 times per week", "Less than 4 times per month", "No exercise in the past month")))
label(ens2016_final$a17) <- "Physical activity"
table(ens2016_final$a17)

#muerte cancer
class(ens2016_final$muerte_cancer)
ens2016_final$muerte_cancer<-as.factor(ens2016_final$muerte_cancer)
class(ens2016_final$muerte_cancer)
table(ens2016_final$muerte_cancer)
ens2016_final <- ens2016_final %>%
  mutate(muerte_cancer = factor(muerte_cancer, 
                       levels = c("0", "1"), 
                       labels = c("No cancer death", "Cancer death")))
label(ens2016_final$muerte_cancer) <- "Cancer mortality"
table(ens2016_final$muerte_cancer)

#Fallecimiento
class(ens2016_final$fallecidos)
ens2016_final$fallecidos<-as.factor(ens2016_final$fallecidos)
class(ens2016_final$fallecidos)
table(ens2016_final$fallecidos)
ens2016_final <- ens2016_final %>%
  mutate(fallecidos = factor(fallecidos, 
                       levels = c("1", "2","3"), 
                       labels = c("Cancer death", "Other causes of death", "Alive")))
label(ens2016_final$fallecidos) <- "Vital status"
table(ens2016_final$fallecidos)

#Depresión
class(ens2016_final$Depresion_1_AP)
ens2016_final$Depresion_1_AP<-as.factor(ens2016_final$Depresion_1_AP)
class(ens2016_final$Depresion_1_AP)
table(ens2016_final$Depresion_1_AP)
ens2016_final <- ens2016_final %>%
  mutate(Depresion_1_AP = factor(Depresion_1_AP, 
                                 levels = c("0", "1"), 
                                 labels = c("No symptoms", "With symptoms")))
label(ens2016_final$Depresion_1_AP) <- "Depression"
table(ens2016_final$Depresion_1_AP)

#falta etiquetar antecedentes fmailiares de cancer e ICT

# --- 1️⃣ Preparar variables ---
ens2003_final <- ens2003_final %>%
  mutate(
    tiempo_total = dias_transcurridos / 365.25,       # convertir días a años
    evento_total = muerte_cancer,                     # evento sin censura
    evento_label = factor(evento_total,
                          levels = c(0,1),
                          labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )

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


#tabla 1 2003 para ICT
# 2. Crear un subconjunto para la variable específica (esto mantiene la integridad del diseño)
survey_design_ICT_2003 <- subset(survey_design2003, !is.na(ict))
options(survey.lonely.psu="adjust")

#2. realizar tabla
tabla1_ict_2003 <- survey_design_ICT_2003 %>% 
  tbl_svysummary(
    by = ict, 
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



