source("procesamiento/04_etiquetado_variables.R", encoding = "UTF-8")

####### KM expandido  #####
#########ENS 2003############################################################################
# Filtrar datos sin NA en sintomas depresivos
ens2003_final <- ens2003_final %>%
  filter(!is.na(Depresion_1_AP))

ens2003_final$Depresion_1_AP <- as.factor(ens2003_final$Depresion_1_AP)

# Preparar variables
ens2003_final <- ens2003_final %>%
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
  weights = ~FEXP_analisis,
  data = ens2003_final,
  nest = TRUE
)
options(survey.lonely.psu = "certainty")

# Kaplan-Meier ponderado con svykm()
# 1. Recalcular el modelo pidiendo explícitamente los errores estándar (se = TRUE)
km_fit <- svykm(Surv(tiempo_total, evento_total) ~ Depresion_1_AP, 
                design = survey_designkm, 
                se = TRUE)

# 2. Extraer los datos y los intervalos de confianza a un data frame
df_km <- bind_rows(
  lapply(names(km_fit), function(nom) {
    km_obj <- km_fit[[nom]]
    
    # SOLUCIÓN: Indicar explícitamente los tiempos en el argumento 'parm'
    ci <- confint(km_obj, parm = km_obj$time)
    
    df_temp <- data.frame(
      tiempo = km_obj$time,
      supervivencia = km_obj$surv,
      lower = ci[, 1], # Límite inferior
      upper = ci[, 2], # Límite superior
      grupo = nom
    )
    
    # Asegurar que la curva parta exactamente desde el tiempo 0
    df_inicio <- data.frame(
      tiempo = 0, 
      supervivencia = 1, 
      lower = 1, 
      upper = 1, 
      grupo = nom
    )
    
    bind_rows(df_inicio, df_temp)
  })
)

# Limpiar los nombres de los grupos para la leyenda
df_km$grupo <- gsub("Depresion_1_AP=", "", df_km$grupo)

# 3. Generar el gráfico aesthetic con intervalos de confianza
ggplot(df_km, aes(x = tiempo, y = supervivencia, color = grupo, fill = grupo)) +
  
  # Banda de intervalo de confianza (sombreado translúcido)
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  
  # Línea principal de supervivencia
  geom_step(linewidth = 1, direction = "hv") + 
  
  # Forzar el eje Y para que parta desde 0.9 hasta 1.0
  scale_y_continuous(limits = c(0.9, 1.0), 
                     breaks = seq(0.9, 1.0, by = 0.02),
                     labels = scales::percent_format(accuracy = 1)) + 
  
  scale_x_continuous(breaks = seq(0, max(df_km$tiempo, na.rm = TRUE), by = 2)) +
  
  # Colores para líneas (color) y bandas (fill)
  scale_color_manual(values = c("#2980b9", "#c0392b")) + 
  scale_fill_manual(values = c("#2980b9", "#c0392b")) + 
  
  labs(
    title = "Curva de Kaplan-Meier para personas según presencia de síntomas depresivos (Muerte por Cáncer)",
    subtitle = "ENS 2003 - Con Intervalos de Confianza al 95%",
    x = "Años de seguimiento",
    y = "Probabilidad de Supervivencia",
    color = "Síntomas depresivos",
    fill = "Síntomas depresivos" # Asegura que la leyenda combine línea y sombreado
  ) +
  
  # Tema minimalista
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    axis.line.x = element_line(color = "gray50"),
    axis.text = element_text(color = "gray30")
  )

# Test log-rank ponderado
logrank_test <- svyranktest(Surv(tiempo_total, evento_total) ~ Depresion_1_AP, design = survey_designkm)
cat("Valor p log-rank (diseño complejo):", logrank_test$p.value, "\n")

# Ver resumen del tiempo y eventos por grupo
ens2003_final %>%
  group_by(Depresion_1_AP) %>%
  summarise(
    n = n(),
    n_eventos = sum(evento_total == 1, na.rm = TRUE),
    min_tiempo = min(tiempo_total, na.rm = TRUE),
    max_tiempo = max(tiempo_total, na.rm = TRUE)
  )

# Sobrevida a los 5 años (Grupo Con sintomas depresivos)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 5))]

# Sobrevida a los 10 años (Grupo Con sintomas depresivos)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 10))]

#ggsave("output/graphs/KM_depresion_2003.png", width = 12, height = 8, dpi = 300)

#########ENS 2009############################################################################
# Filtrar datos sin NA en sintomas de presivos
ens2009_final <- ens2009_final %>%
  filter(!is.na(Depresion_1_AP))

ens2009_final$Depresion_1_AP <- as.factor(ens2009_final$Depresion_1_AP)

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

# Kaplan-Meier ponderado con svykm()
# 1. Recalcular el modelo pidiendo explícitamente los errores estándar (se = TRUE)
km_fit <- svykm(Surv(tiempo_total, evento_total) ~ Depresion_1_AP, 
                design = survey_designkm, 
                se = TRUE)

# 2. Extraer los datos y los intervalos de confianza a un data frame
df_km <- bind_rows(
  lapply(names(km_fit), function(nom) {
    km_obj <- km_fit[[nom]]
    
    # SOLUCIÓN: Indicar explícitamente los tiempos en el argumento 'parm'
    ci <- confint(km_obj, parm = km_obj$time)
    
    df_temp <- data.frame(
      tiempo = km_obj$time,
      supervivencia = km_obj$surv,
      lower = ci[, 1], # Límite inferior
      upper = ci[, 2], # Límite superior
      grupo = nom
    )
    
    # Asegurar que la curva parta exactamente desde el tiempo 0
    df_inicio <- data.frame(
      tiempo = 0, 
      supervivencia = 1, 
      lower = 1, 
      upper = 1, 
      grupo = nom
    )
    
    bind_rows(df_inicio, df_temp)
  })
)

# Limpiar los nombres de los grupos para la leyenda
df_km$grupo <- gsub("Depresion_1_AP=", "", df_km$grupo)

# 3. Generar el gráfico aesthetic con intervalos de confianza
ggplot(df_km, aes(x = tiempo, y = supervivencia, color = grupo, fill = grupo)) +
  
  # Banda de intervalo de confianza (sombreado translúcido)
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  
  # Línea principal de supervivencia
  geom_step(linewidth = 1, direction = "hv") + 
  
  # Forzar el eje Y para que parta desde 0.9 hasta 1.0
  scale_y_continuous(limits = c(0.9, 1.0), 
                     breaks = seq(0.9, 1.0, by = 0.02),
                     labels = scales::percent_format(accuracy = 1)) + 
  
  scale_x_continuous(breaks = seq(0, max(df_km$tiempo, na.rm = TRUE), by = 2)) +
  
  # Colores para líneas (color) y bandas (fill)
  scale_color_manual(values = c("#2980b9", "#c0392b")) + 
  scale_fill_manual(values = c("#2980b9", "#c0392b")) + 
  
  labs(
    title = "Curva de Kaplan-Meier para personas segun presencia de sintomas depresivos (Muerte por Cáncer)",
    subtitle = "ENS 2009 - Con Intervalos de Confianza al 95%",
    x = "Años de seguimiento",
    y = "Probabilidad de Supervivencia",
    color = "Síntomas depresivos",
    fill = "Síntomas depresivos" # Asegura que la leyenda combine línea y sombreado
  ) +
  
  # Tema minimalista
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    axis.line.x = element_line(color = "gray50"),
    axis.text = element_text(color = "gray30")
  )

# Test log-rank ponderado
logrank_test <- svyranktest(Surv(tiempo_total, evento_total) ~ Depresion_1_AP, design = survey_designkm)
cat("Valor p log-rank (diseño complejo):", logrank_test$p.value, "\n")

# Ver resumen del tiempo y eventos por grupo
ens2009_final %>%
  group_by(Depresion_1_AP) %>%
  summarise(
    n = n(),
    n_eventos = sum(evento_total == 1, na.rm = TRUE),
    min_tiempo = min(tiempo_total, na.rm = TRUE),
    max_tiempo = max(tiempo_total, na.rm = TRUE)
  )

# Sobrevida a los 5 años (Grupo Con sintomas depresivos)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 5))]

# Sobrevida a los 10 años (Grupo Con sintomas depresivos)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 10))]

#ggsave("output/graphs/KM_depresion_2009.png", width = 12, height = 8, dpi = 300)

#################################################################################################
####COX con muestra expandida####
#################################################################################################

################################cox 2003 depresion###############################################
# Actualizar el diseño de encuesta para incluir las variables necesarias

# Preparar variables
ens2003_final <- ens2003_final %>%
  mutate(
    tiempo_total = dias_transcurridos / 365.25,
    evento_total = if_else(muerte_cancer == "Cancer death", 1, 0),
    evento_label = factor(evento_total,
                          levels = c(0, 1),
                          labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )

# Diseño muestral
survey_designkm2003 <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~FEXP_analisis,
  data = ens2003_final,
  nest = TRUE
)
options(survey.lonely.psu = "certainty")

survey_designkm2003 <- update(survey_designkm2003,
  tiempo_anos = ens2003_final$dias_transcurridos / 365.25,
  Depresion_1_AP = ens2003_final$Depresion_1_AP,
  edad = ens2003_final$edad,
  sexo = ens2003_final$sexo,
  nedu = ens2003_final$NEDU,
  zona = ens2003_final$zona,
    fuma = ens2003_final$fuma,
    imc = ens2003_final$imc)

# Modelos de Cox con diseño de encuesta
cox_crudo_depresion_2003 <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP,
                      design = survey_designkm2003)

cox_edad_depresion_2003 <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP + edad,
                     design = survey_designkm2003)

cox_edad_sexo_depresion_2003 <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP + edad + sexo,
                          design = survey_designkm2003)

cox_edad_sexo_nedu_depresion_2003 <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP + edad + sexo + nedu,
                               design = survey_designkm2003)

cox_completo_depresion_2003<- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP + edad + sexo + nedu + zona + fuma + imc + a17,
                       design = survey_designkm2003)

# Tabla resumen de HR, IC y p-value
resumen <- bind_rows(
  broom::tidy(cox_crudo_depresion_2003, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad_depresion_2003, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo_depresion_2003, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
  broom::tidy(cox_completo_depresion_2003, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Modelo completo")
) %>%
  # Como tidy() ya calculó el HR y los IC, solo seleccionamos y renombramos las columnas
  select(
    modelo, 
    term, 
    HR = estimate,        # estimate ya viene exponenciado (es el HR)
    IC_inf = conf.low,    # conf.low es el límite inferior
    IC_sup = conf.high,   # conf.high es el límite superior
    p.value
  )

# Mostrar tabla
kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer segun presencia de sintomas depresivos 2003")

# Generamos la tabla con kable y la guardamos en un objeto
tabla_md <- kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer segun presencia de sintomas depresivos 2003")

# Usamos writeLines para exportar ese objeto a un archivo .md
writeLines(as.character(tabla_md), "output/tables/depresion/tabla_cox_resumen_depresion_2003.md")

# 4️⃣ Forest plot para HR sintomas depresivos
resumen_hr <- resumen %>%
  filter(term == "Depresion_1_APWith symptoms")

ggplot(resumen_hr, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(color = "blue", size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    x = "Modelo",
    y = "HR de sintomas depresivos (IC 95%)",
    title = "Efecto de la presencia de sintomas depresivos sobre muerte por cáncer",
    subtitle = "Forest plot de HR ajustados por distintos modelos"
  ) +
  theme_minimal(base_size = 14)


#Guardar gráfico
#ggsave("output/graphs/FP_HR_AF_2003.png", width = 10, height = 6, dpi = 300)

# Filtrar solo HR de sintomas depresivos
resumen_hr <- resumen %>%
  filter(term == "Depresion_1_APWith symptoms") %>%
  mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo")))

# Forest plot mejorado
ggplot(resumen_hr, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(aes(color = modelo), size = 1.1, fatten = 3) +   # punto más grande
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", size = 0.8) +
  geom_text(aes(label = paste0(round(HR,2), " [", round(IC_inf,2), "-", round(IC_sup,2), "]")), 
            hjust = -0.1, size = 4, color = "black") +  # etiquetas HR+IC
  coord_flip() +
  scale_y_continuous(trans = "log10", breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2), 
                     limits = c(0.25,2.5)) +  # eje logarítmico para HR
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "",
    y = "HR de presencia de sintomas depresivos (IC 95%)",
    title = "Forest plot: Efecto de sintomas depresivos sobre muerte por cáncer",
    subtitle = "Comparación entre modelos ajustados",
    caption = "HR = hazard ratio; IC = intervalo de confianza 95%"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    axis.text = element_text(size = 12)
  )



# --- Forest plot ENS 2003 (objeto: resumen) ---
df_forest_2003 <- resumen %>%
  filter(term %in% c("Depresion_1_APWith symptoms", "sexoFemale")) %>%
  mutate(
    variable = case_when(
      term == "Depresion_1_APWith symptoms" ~ "sintomas depresivos (Con vs Sin)",
      term %in% c("sexoFemale") ~ "Sexo (Mujer vs Hombre)",
      TRUE ~ term
    ),
    modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo"))
  ) %>%
  arrange(variable, modelo) %>%
  mutate(item = paste(modelo, variable, sep = " · "),
         item = factor(item, levels = rev(unique(item))))

p_forest_2003 <- ggplot(df_forest_2003, aes(x = HR, y = item, color = variable)) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey40") +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.15, linewidth = 0.8) +
  geom_point(size = 3) +
  scale_x_log10(
    breaks = c(0.5, 1, 2, 3, 5),
    limits = c(min(0.5, min(df_forest_2003$IC_inf, na.rm=TRUE)*0.9),
               max(5,   max(df_forest_2003$IC_sup, na.rm=TRUE)*1.1))
  ) +
  labs(
    x = "Hazard Ratio (escala log)",
    y = NULL,
    color = NULL,
    title = "Forest plot — HR de antecedentes familiares y sexo (ENS 2003)",
    subtitle = "Modelos de Cox ponderados por diseño muestral",
    caption = "Barras = IC95%. Línea punteada = HR = 1"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

print(p_forest_2003)

#Guardar gráfico
ggsave("output/graphs/FP_HR_sintomas depresivos_2003.png", width = 10, height = 6, dpi = 300)

#############################COX 2009 depresion####################################################
# Actualizar el diseño de encuesta para incluir las variables necesarias
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
survey_designkm2009 <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~FEXP1,
  data = ens2009_final,
  nest = TRUE
)
options(survey.lonely.psu = "certainty")

survey_designkm2009 <- update(survey_designkm2009,
  tiempo_anos = ens2009_final$dias_transcurridos / 365.25,
  Depresion_1_AP = ens2009_final$Depresion_1_AP,
  edad = ens2009_final$edad,
  sexo = ens2009_final$sexo,
  nedu = ens2009_final$NEDU,
  zona = ens2009_final$zona,
    fuma = ens2009_final$fuma,
    imc = ens2009_final$imc)

# Modelos de Cox con diseño de encuesta
cox_crudo_depresion <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP,
                      design = survey_designkm2009)

cox_edad_depresion <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP + edad,
                     design = survey_designkm2009)

cox_edad_sexo_depresion <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP + edad + sexo,
                          design = survey_designkm2009)

cox_edad_sexo_nedu_depresion <- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP + edad + sexo + nedu,
                               design = survey_designkm2009)

cox_completo_depresion<- svycoxph(Surv(tiempo_total, evento_total) ~ Depresion_1_AP + edad + sexo + nedu + zona + fuma + imc + a17,
                       design = survey_designkm2009)

# Tabla resumen de HR, IC y p-value
resumen <- bind_rows(
  broom::tidy(cox_crudo_depresion, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad_depresion, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo_depresion, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
  broom::tidy(cox_completo_depresion, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Modelo completo")
) %>%
  # Como tidy() ya calculó el HR y los IC, solo seleccionamos y renombramos las columnas
  select(
    modelo, 
    term, 
    HR = estimate,        # estimate ya viene exponenciado (es el HR)
    IC_inf = conf.low,    # conf.low es el límite inferior
    IC_sup = conf.high,   # conf.high es el límite superior
    p.value
  )

# Mostrar tabla
kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer segun presencia de sintomas depresivos")

# Generamos la tabla con kable y la guardamos en un objeto
tabla_md <- kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer segun presencia de sintomas depresivos")

# Usamos writeLines para exportar ese objeto a un archivo .md
writeLines(as.character(tabla_md), "output/tables/depresion/tabla_cox_resumen_depresion_2009.md")

# 4️⃣ Forest plot para HR sintomas depresivos
resumen_hr <- resumen %>%
  filter(term == "Depresion_1_APWith symptoms")

ggplot(resumen_hr, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(color = "blue", size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    x = "Modelo",
    y = "HR de sintomas depresivos (IC 95%)",
    title = "Efecto de la presencia de sintomas depresivos sobre muerte por cáncer",
    subtitle = "Forest plot de HR ajustados por distintos modelos"
  ) +
  theme_minimal(base_size = 14)


#Guardar gráfico
#ggsave("output/graphs/FP_HR_AF_2009.png", width = 10, height = 6, dpi = 300)

# Filtrar solo HR de sintomas depresivos
resumen_hr <- resumen %>%
  filter(term == "Depresion_1_APWith symptoms") %>%
  mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo")))

# Forest plot mejorado
ggplot(resumen_hr, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(aes(color = modelo), size = 1.1, fatten = 3) +   # punto más grande
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", size = 0.8) +
  geom_text(aes(label = paste0(round(HR,2), " [", round(IC_inf,2), "-", round(IC_sup,2), "]")), 
            hjust = -0.1, size = 4, color = "black") +  # etiquetas HR+IC
  coord_flip() +
  scale_y_continuous(trans = "log10", breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2), 
                     limits = c(0.25,2.5)) +  # eje logarítmico para HR
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "",
    y = "HR de presencia de sintomas depresivos (IC 95%)",
    title = "Forest plot: Efecto de sintomas depresivos sobre muerte por cáncer",
    subtitle = "Comparación entre modelos ajustados",
    caption = "HR = hazard ratio; IC = intervalo de confianza 95%"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    axis.text = element_text(size = 12)
  )



# --- Forest plot ENS 2009 (objeto: resumen) ---
df_forest_2009 <- resumen %>%
  filter(term %in% c("Depresion_1_APWith symptoms", "sexoFemale")) %>%
  mutate(
    variable = case_when(
      term == "Depresion_1_APWith symptoms" ~ "sintomas depresivos (Con vs Sin)",
      term %in% c("sexoFemale") ~ "Sexo (Mujer vs Hombre)",
      TRUE ~ term
    ),
    modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo"))
  ) %>%
  arrange(variable, modelo) %>%
  mutate(item = paste(modelo, variable, sep = " · "),
         item = factor(item, levels = rev(unique(item))))

p_forest_2009 <- ggplot(df_forest_2009, aes(x = HR, y = item, color = variable)) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey40") +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.15, linewidth = 0.8) +
  geom_point(size = 3) +
  scale_x_log10(
    breaks = c(0.5, 1, 2, 3, 5),
    limits = c(min(0.5, min(df_forest_2009$IC_inf, na.rm=TRUE)*0.9),
               max(5,   max(df_forest_2009$IC_sup, na.rm=TRUE)*1.1))
  ) +
  labs(
    x = "Hazard Ratio (escala log)",
    y = NULL,
    color = NULL,
    title = "Forest plot — HR de antecedentes familiares y sexo (ENS 2009)",
    subtitle = "Modelos de Cox ponderados por diseño muestral",
    caption = "Barras = IC95%. Línea punteada = HR = 1"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

print(p_forest_2009)

#Guardar gráfico
ggsave("output/graphs/FP_HR_sintomas depresivos_2009.png", width = 10, height = 6, dpi = 300)
