## 07.4 HR  conjunto con las 3 variables estrella. 
## Notar que solo se usará ENS 2009-10 porque es la 
## común a todas las variables. 

source("procesamiento/04_etiquetado_variables.R", encoding = "UTF-8")



ens2009_final$ictaumentado <- as.numeric(ens2009_final$ict >= 0.6)




####### BASE 2009-10 #####
# Filtrar datos sin NA 
ens2009_final <- ens2009_final %>%
  filter(!is.na(ictaumentado))

ens2009_final$ictaumentado <- as.factor(ens2009_final$ictaumentado)

# Preparar variables
ens2009_final <- ens2009_final %>%
  mutate(
    tiempo_total = dias_transcurridos / 365.25,
    evento_total = if_else(muerte_cancer == "Cancer death", 1, 0),
    evento_label = factor(evento_total,
                          levels = c(0, 1),
                          labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )

# Diseño muestral
survey_designkm <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~FEXP1,
  data = ens2009_final,
  nest = TRUE
)
options(survey.lonely.psu = "certainty")



# Actualizar el diseño de encuesta para incluir las variables necesarias
survey_designkm <- update(survey_designkm,
                          tiempo_anos = ens2009_final$dias_transcurridos / 365.25,
                          Depresion_1_AP = ens2009_final$Depresion_1_AP,
                          ictaumentado = ens2009_final$ictaumentado,
                          af_cancer_binaria = ens2009_final$af_cancer_binaria,
                          edad = ens2009_final$edad,
                          sexo = ens2009_final$sexo,
                          nedu = ens2009_final$NEDU,
                          zona = ens2009_final$zona,
                          AUDIT = ens2009_final$AUDIT_RIESGOSO,
                          fuma = ens2009_final$fuma,
                          imc = ens2009_final$imc,
                          GPAQ = ens2009_final$GPAQ
)


# Modelos de Cox con diseño de encuesta
cox_crudo <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP+ ictaumentado + af_cancer_binaria,
                      design = survey_designkm)

cox_edad <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP + ictaumentado + af_cancer_binaria + edad,
                     design = survey_designkm)

cox_edad_sexo <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP + ictaumentado + af_cancer_binaria +
                            edad + sexo,
                          design = survey_designkm)

cox_edad_sexo_nedu <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP + ictaumentado + af_cancer_binaria +
                               edad + sexo + nedu,
                               design = survey_designkm)

cox_completo <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP+ ictaumentado + af_cancer_binaria + 
                           edad + sexo + nedu + zona + 
                           AUDIT + fuma + imc + GPAQ,
                         design = survey_designkm)


# Tabla resumen de HR, IC y p-value
resumen_2009_conjunto <- bind_rows(
  broom::tidy(cox_crudo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
  broom::tidy(cox_edad_sexo_nedu, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo+NEDU"),
  broom::tidy(cox_completo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Modelo completo")
) %>%
  select(
    modelo, 
    term, 
    HR = estimate, 
    IC_inf = conf.low, 
    IC_sup = conf.high, 
    p.value
  )

# Mostrar tabla en consola
kable(resumen_2009_conjunto, digits = 3, caption = "Modelos de Cox conjunto para muerte por cáncer - ENS 2009")

# Guardar la tabla en formato Markdown
tabla_md_conjunta <- kable(resumen_2009_conjunto, digits = 3, caption = "Modelos de Cox conjunto para muerte por cáncer - ENS 2009")
writeLines(as.character(tabla_md_conjunta), "output/tables/tabla_cox_conjunta_2009.md")
