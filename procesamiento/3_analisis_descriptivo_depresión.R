library(gtsummary)
library(survey)

class(ENS2003conexc_confekm$Depresion_1_AP)
table(ENS2003conexc_confekm$Depresion_1_AP)

# --- 1️⃣ Preparar variables ---
ENS2003conexc_confekm <- ENS2003conexc_confekm %>%
  mutate(
    tiempo_total = dias_transcurridos / 365.25,       # convertir días a años
    evento_total = muerte_cancer,                     # evento sin censura
    evento_label = factor(evento_total,
                          levels = c(0,1),
                          labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )


# --- 2️⃣ Diseño de encuesta ---
survey_designkm2003 <- svydesign(
  id = ~conglomerado_,
  strata = ~estrato_,
  weights = ~FEXP1,
  data = ENS2003conexc_confekm,
  nest = TRUE
)
options(survey.lonely.psu="adjust")

#2. realizar tabla
tabla1_2003 <- survey_designkm2003 %>% 
  tbl_svysummary(
    by = Depresion_1_AP, 
    include = c(edad, sexo, NEDU, zona, fuma, estado_nutricional, a17, muerte_cancer, fallecidos),
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