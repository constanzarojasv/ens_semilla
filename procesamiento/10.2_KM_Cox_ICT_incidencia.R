source("procesamiento/09_incidencia.R", encoding = "UTF-8")

####### KAPLAN MEIER 2003 EGRESO Y DEFUNCION #####################################################################################
# Filtrar datos sin NA 
ens2003_final <- ens2003_final %>%
  filter(!is.na(ictaumentado))

ens2003_final$ictaumentado <- as.factor(ens2003_final$ictaumentado)


# Preparar variables
ens2003_final <- ens2003_final %>%
  mutate(
    tiempo_total = dias_transcurridosfinal / 365.25,
    # Se ajusta la condición para usar la nueva variable dicotómica
    evento_total = if_else(egresoydefuncion == 1, 1, 0),
    evento_label = factor(evento_total,
                          levels = c(0, 1),
                          labels = c("No muertos o egresados por cáncer", "Muertes o egresos por cáncer"))
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
km_fit <- svykm(Surv(tiempo_total, evento_total) ~ ictaumentado, 
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
df_km$grupo <- gsub("ictaumentado", "", df_km$grupo)

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
    title = "Curva de Kaplan-Meier (Muerte o Egreso por cáncer)",
    subtitle = "ENS 2003 - Con Intervalos de Confianza al 95%",
    x = "Años de seguimiento",
    y = "Probabilidad de Supervivencia",
    color = "ICT aumentado",
    fill = "ICT aumentado" # Asegura que la leyenda combine línea y sombreado
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
logrank_test <- svyranktest(Surv(tiempo_total, evento_total) ~ ictaumentado, design = survey_designkm)
cat("Valor p log-rank (diseño complejo):", logrank_test$p.value, "\n")

# Ver resumen del tiempo y eventos por grupo
ens2003_final %>%
  group_by(ictaumentado) %>%
  summarise(
    n = n(),
    n_eventos = sum(evento_total == 1, na.rm = TRUE),
    min_tiempo = min(tiempo_total, na.rm = TRUE),
    max_tiempo = max(tiempo_total, na.rm = TRUE)
  )

# Sobrevida a los 5 años (Grupo Con ict aumentado)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 5))]

# Sobrevida a los 10 años (Grupo sin)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 10))]




###### KAPLAN MEIER 2009 EGRESO Y DEFUNCION ############################################################
# Filtrar datos sin NA 
ens2009_final <- ens2009_final %>%
  filter(!is.na(ictaumentado))

ens2009_final$ictaumentado <- as.factor(ens2009_final$ictaumentado)

# Preparar variables
ens2009_final <- ens2009_final %>%
  mutate(
    tiempo_total = dias_transcurridosfinal / 365.25,
    evento_total = if_else(egresoydefuncion == 1, 1, 0),
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
km_fit <- svykm(Surv(tiempo_total, evento_total) ~ ictaumentado, 
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
df_km$grupo <- gsub("ictaumentado", "", df_km$grupo)

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
    title = "Curva de Kaplan-Meier (Muerte o egreso por cáncer)",
    subtitle = "ENS 2009 - Con Intervalos de Confianza al 95%",
    x = "Años de seguimiento",
    y = "Probabilidad de Supervivencia",
    color = "ICT aumentado",
    fill = "ICT aumentado" # Asegura que la leyenda combine línea y sombreado
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
logrank_test <- svyranktest(Surv(tiempo_total, evento_total) ~ ictaumentado, design = survey_designkm)
cat("Valor p log-rank (diseño complejo):", logrank_test$p.value, "\n")

# Ver resumen del tiempo y eventos por grupo
ens2009_final %>%
  group_by(ictaumentado) %>%
  summarise(
    n = n(),
    n_eventos = sum(evento_total == 1, na.rm = TRUE),
    min_tiempo = min(tiempo_total, na.rm = TRUE),
    max_tiempo = max(tiempo_total, na.rm = TRUE)
  )

# Sobrevida a los 5 años (Grupo )
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 5))]

# Sobrevida a los 10 años (Grupo sin ict aumentado)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 10))]
