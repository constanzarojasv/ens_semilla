#Cargar setup (librerías)
source("procesamiento/00_setup.R", encoding = "UTF-8")

ens2003_final <- read_rds("input/data-procesada/data-analisis/ens2003_final.rds")
ens2009_final <- read_rds("input/data-procesada/data-analisis/ens2009_final.rds")
ens2016_final <- read_rds("input/data-procesada/data-analisis/ens2016_final.rds")


class(ens2003_final$Depresion_1_AP)
ens2003_final$Depresion_1_AP<-as.factor(ens2003_final$Depresion_1_AP)
class(ens2003_final$Depresion_1_AP)
table(ens2003_final$Depresion_1_AP)

# --- 1️⃣ Preparar variables ---
class(ens2003_final$Depresion_1_AP)
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
survey_designkm2003 <- subset(survey_design2003, !is.na(Depresion_1_AP))
options(survey.lonely.psu="adjust")

#2. realizar tabla
tabla1_2003 <- survey_designkm2003 %>% 
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
tabla1_2003