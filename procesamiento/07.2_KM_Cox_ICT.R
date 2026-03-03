source("procesamiento/04_etiquetado_variables.R", encoding = "UTF-8")
ens2003_final$ictaumentado <- as.numeric(ens2003_final$ict >= 0.6)
ens2009_final$ictaumentado <- as.numeric(ens2009_final$ict >= 0.6)
ens2016_final$ictaumentado <- as.numeric(ens2016_final$ict >= 0.6)

####### KAPLAN MEIER 2003 #####
# Filtrar datos sin NA en AF
ens2003_final <- ens2003_final %>%
  filter(!is.na(ictaumentado))

ens2003_final$ictaumentado <- as.factor(ens2003_final$ictaumentado)

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
    title = "Curva de Kaplan-Meier (Muerte por Cáncer)",
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

# Sobrevida a los 5 años 
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 5))]

# Sobrevida a los 10 años 
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 10))]


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





####### KAPLAN MEIER 2009 #####
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
    title = "Curva de Kaplan-Meier (Muerte por Cáncer)",
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

# Sobrevida a los 5 años (Grupo Con ict>)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 5))]

# Sobrevida a los 10 años (Grupo sin)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 10))]



####### KAPLAN MEIER 2009 EGRESO Y DEFUNCION ############################################################
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








####### KAPLAN MEIER 2016 #####
# Filtrar datos sin NA 
ens2016_final <- ens2016_final %>%
  filter(!is.na(ictaumentado))

ens2016_final$ictaumentado <- as.factor(ens2016_final$ictaumentado)

# Preparar variables
ens2016_final <- ens2016_final %>%
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
  weights = ~Fexp_F1p_Corr,
  data = ens2016_final,
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
    title = "Curva de Kaplan-Meier (Muerte por Cáncer)",
    subtitle = "ENS 2016 - Con Intervalos de Confianza al 95%",
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
ens2016_final %>%
  group_by(ictaumentado) %>%
  summarise(
    n = n(),
    n_eventos = sum(evento_total == 1, na.rm = TRUE),
    min_tiempo = min(tiempo_total, na.rm = TRUE),
    max_tiempo = max(tiempo_total, na.rm = TRUE)
  )

# Sobrevida a los 5 años (Grupo Con ict aumentado)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 5))]

# Sobrevida a los 10 años (Grupo sin ict aumentado)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 10))]


####### KAPLAN MEIER 2016 EGRESO Y DEFUNCION #############################################
# Filtrar datos sin NA 
ens2016_final <- ens2016_final %>%
  filter(!is.na(ictaumentado))

ens2016_final$ictaumentado <- as.factor(ens2016_final$ictaumentado)

# Preparar variables
ens2016_final <- ens2016_final %>%
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
  weights = ~Fexp_F1p_Corr,
  data = ens2016_final,
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
    subtitle = "ENS 2016 - Con Intervalos de Confianza al 95%",
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
ens2016_final %>%
  group_by(ictaumentado) %>%
  summarise(
    n = n(),
    n_eventos = sum(evento_total == 1, na.rm = TRUE),
    min_tiempo = min(tiempo_total, na.rm = TRUE),
    max_tiempo = max(tiempo_total, na.rm = TRUE)
  )

# Sobrevida a los 5 años
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 5))]

# Sobrevida a los 10 años
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 10))]






























####COX con muestra expandida####


#=======================================================================================
#### COX con muestra expandida 2003 - Ajustado para ICT Aumentado ####
#=======================================================================================

# Filtrar datos sin NA 
ens2003_final <- ens2003_final %>%
  filter(!is.na(ictaumentado))

ens2003_final$ictaumentado <- as.factor(ens2003_final$ictaumentado)

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

# Asegurarnos de que ictaumentado sea factor para la interpretación correcta del HR
ens2003_final$ictaumentado <- as.factor(ens2003_final$ictaumentado)

# Actualizar el diseño de encuesta para incluir las variables necesarias
survey_designkm <- update(survey_designkm,
                        ictaumentado = ens2003_final$ictaumentado,
                        edad = ens2003_final$edad,
                        sexo = ens2003_final$sexo,
                        nedu = ens2003_final$NEDU,
                        zona = ens2003_final$zona,
                        fuma = ens2003_final$fuma,
                        imc = ens2003_final$imc
                        )

# Modelos de Cox con diseño de encuesta
cox_crudo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado,
                      design = survey_designkm)

cox_edad <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad,
                     design = survey_designkm)

cox_edad_sexo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo,
                          design = survey_designkm)

cox_edad_sexo_nedu <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu,
                               design = survey_designkm)

cox_completo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu + zona + fuma + imc,
                       design = survey_designkm)

# Tabla resumen de HR, IC y p-value
resumen <- bind_rows(
  broom::tidy(cox_crudo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
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

# Mostrar tabla
kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2003)")

# Generamos la tabla con kable y la guardamos en un objeto
tabla_md <- kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2003)")

# Exportar tabla (Asegúrate de tener la carpeta output/tables/ICT_cancer/ creada)
writeLines(as.character(tabla_md), "output/tables/ICT_cancer/tabla_cox_resumen_ict_2003.md")

# 4️⃣ Forest plot para HR de ictaumentado
resumen_hr <- resumen %>%
  filter(term == "ictaumentado1") %>%
  mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo")))

# Forest plot mejorado
ggplot(resumen_hr, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(aes(color = modelo), size = 1.1, fatten = 3) +   
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(HR,2), " [", round(IC_inf,2), "-", round(IC_sup,2), "]")), 
            hjust = -0.1, size = 4, color = "black") +  
  coord_flip() +
  scale_y_continuous(trans = "log10", breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2, 3), 
                     limits = c(0.25, 3.5)) +  
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "",
    y = "HR de ICT aumentado (IC 95%)",
    title = "Forest plot: Efecto de ICT aumentado sobre muerte por cáncer",
    subtitle = "Comparación entre modelos ajustados (ENS 2003)",
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

# Guardar gráfico
# ggsave("output/graphs/FP_HR_ICT_2003.png", width = 10, height = 6, dpi = 300)

# --- Forest plot Comparativo (ICT y Sexo) ---
df_forest_2003 <- resumen %>%
  filter(term %in% c("ictaumentado1", "sexoFemale")) %>%
  mutate(
    variable = case_when(
      term == "ictaumentado1" ~ "ICT (Aumentado vs Normal)",
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
    title = "Forest plot — HR de ICT aumentado y sexo (ENS 2009)",
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


#=======================================================================================
### COX con muestra expandida 2003 - Ajustado para ICT Aumentado  DEFUNCION Y EGRESO####
#=======================================================================================
# Preparar variables
ens2003_final <- ens2003_final %>%
  mutate(
    tiempo_total = dias_transcurridosfinal / 365.25,
    evento_total = if_else(egresoydefuncion == 1, 1, 0),
    evento_label = factor(evento_total,
                          levels = c(0, 1),
                          labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )
survey_designkm <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~FEXP_analisis,
  data = ens2003_final,
  nest = TRUE
)
options(survey.lonely.psu = "certainty")

# Asegurarnos de que ictaumentado sea factor para la interpretación correcta del HR
ens2003_final$ictaumentado <- as.factor(ens2003_final$ictaumentado)

# Actualizar el diseño de encuesta para incluir las variables necesarias
survey_designkm <- update(survey_designkm,
                        ictaumentado = ens2003_final$ictaumentado,
                        edad = ens2003_final$edad,
                        sexo = ens2003_final$sexo,
                        nedu = ens2003_final$NEDU,
                        zona = ens2003_final$zona,
                        fuma = ens2003_final$fuma,
                        imc = ens2003_final$imc
                        )

# Modelos de Cox con diseño de encuesta
cox_crudo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado,
                      design = survey_designkm)

cox_edad <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad,
                     design = survey_designkm)

cox_edad_sexo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo,
                          design = survey_designkm)

cox_edad_sexo_nedu <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu,
                               design = survey_designkm)

cox_completo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu + zona + fuma + imc,
                       design = survey_designkm)

# Tabla resumen de HR, IC y p-value
resumen <- bind_rows(
  broom::tidy(cox_crudo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
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

# Mostrar tabla
kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2003)")

# Generamos la tabla con kable y la guardamos en un objeto
tabla_md <- kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2003)")

# Exportar tabla (Asegúrate de tener la carpeta output/tables/ICT_cancer/ creada)
writeLines(as.character(tabla_md), "output/tables/ICT_cancer/tabla_cox_resumen_ict_2003.md")

# 4️⃣ Forest plot para HR de ictaumentado
resumen_hr <- resumen %>%
  filter(term == "ictaumentado1") %>%
  mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo")))

# Forest plot mejorado
ggplot(resumen_hr, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(aes(color = modelo), size = 1.1, fatten = 3) +   
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(HR,2), " [", round(IC_inf,2), "-", round(IC_sup,2), "]")), 
            hjust = -0.1, size = 4, color = "black") +  
  coord_flip() +
  scale_y_continuous(trans = "log10", breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2, 3), 
                     limits = c(0.25, 3.5)) +  
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "",
    y = "HR de ICT aumentado (IC 95%)",
    title = "Forest plot: Efecto de ICT aumentado sobre muerte por cáncer",
    subtitle = "Comparación entre modelos ajustados (ENS 2003)",
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

# Guardar gráfico
# ggsave("output/graphs/FP_HR_ICT_2003.png", width = 10, height = 6, dpi = 300)

# --- Forest plot Comparativo (ICT y Sexo) ---
df_forest_2003 <- resumen %>%
  filter(term %in% c("ictaumentado1", "sexoFemale")) %>%
  mutate(
    variable = case_when(
      term == "ictaumentado1" ~ "ICT (Aumentado vs Normal)",
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
    title = "Forest plot — HR de ICT aumentado y sexo (ENS 2003)",
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
#=======================================================================================
#### COX con muestra expandida 2009 - Ajustado para ICT Aumentado ####
#=======================================================================================

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

# Asegurarnos de que ictaumentado sea factor para la interpretación correcta del HR
ens2009_final$ictaumentado <- as.factor(ens2009_final$ictaumentado)

# Actualizar el diseño de encuesta para incluir las variables necesarias
survey_designkm <- update(survey_designkm,
                        ictaumentado = ens2009_final$ictaumentado,
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
cox_crudo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado,
                      design = survey_designkm)

cox_edad <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad,
                     design = survey_designkm)

cox_edad_sexo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo,
                          design = survey_designkm)

cox_edad_sexo_nedu <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu,
                               design = survey_designkm)

cox_completo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu + zona + AUDIT + fuma + imc + GPAQ,
                       design = survey_designkm)

# Tabla resumen de HR, IC y p-value
resumen <- bind_rows(
  broom::tidy(cox_crudo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
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

# Mostrar tabla
kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2009)")

# Generamos la tabla con kable y la guardamos en un objeto
tabla_md <- kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2009)")

# Exportar tabla (Asegúrate de tener la carpeta output/tables/ICT_cancer/ creada)
writeLines(as.character(tabla_md), "output/tables/ICT_cancer/tabla_cox_resumen_ict_2009.md")

# 4️⃣ Forest plot para HR de ictaumentado
resumen_hr <- resumen %>%
  filter(term == "ictaumentado1") %>%
  mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo")))

# Forest plot mejorado
ggplot(resumen_hr, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(aes(color = modelo), size = 1.1, fatten = 3) +   
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(HR,2), " [", round(IC_inf,2), "-", round(IC_sup,2), "]")), 
            hjust = -0.1, size = 4, color = "black") +  
  coord_flip() +
  scale_y_continuous(trans = "log10", breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2, 3), 
                     limits = c(0.25, 3.5)) +  
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "",
    y = "HR de ICT aumentado (IC 95%)",
    title = "Forest plot: Efecto de ICT aumentado sobre muerte por cáncer",
    subtitle = "Comparación entre modelos ajustados (ENS 2009)",
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

# Guardar gráfico
# ggsave("output/graphs/FP_HR_ICT_2009.png", width = 10, height = 6, dpi = 300)

# --- Forest plot Comparativo (ICT y Sexo) ---
df_forest_2009 <- resumen %>%
  filter(term %in% c("ictaumentado1", "sexoFemale")) %>%
  mutate(
    variable = case_when(
      term == "ictaumentado1" ~ "ICT (Aumentado vs Normal)",
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
    title = "Forest plot — HR de ICT aumentado y sexo (ENS 2009)",
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


#=======================================================================================
### COX con muestra expandida 2009 - Ajustado para ICT Aumentado  DEFUNCION Y EGRESO####
#=======================================================================================
# Preparar variables
ens2009_final <- ens2009_final %>%
  mutate(
    tiempo_total = dias_transcurridosfinal / 365.25,
    evento_total = if_else(egresoydefuncion == 1, 1, 0),
    evento_label = factor(evento_total,
                          levels = c(0, 1),
                          labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )
survey_designkm <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~FEXP1,
  data = ens2009_final,
  nest = TRUE
)
options(survey.lonely.psu = "certainty")

# Asegurarnos de que ictaumentado sea factor para la interpretación correcta del HR
ens2009_final$ictaumentado <- as.factor(ens2009_final$ictaumentado)

# Actualizar el diseño de encuesta para incluir las variables necesarias
survey_designkm <- update(survey_designkm,
                        ictaumentado = ens2009_final$ictaumentado,
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
cox_crudo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado,
                      design = survey_designkm)

cox_edad <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad,
                     design = survey_designkm)

cox_edad_sexo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo,
                          design = survey_designkm)

cox_edad_sexo_nedu <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu,
                               design = survey_designkm)

cox_completo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu + zona + AUDIT + fuma + imc + GPAQ,
                       design = survey_designkm)

# Tabla resumen de HR, IC y p-value
resumen <- bind_rows(
  broom::tidy(cox_crudo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
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

# Mostrar tabla
kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2009)")

# Generamos la tabla con kable y la guardamos en un objeto
tabla_md <- kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2009)")

# Exportar tabla (Asegúrate de tener la carpeta output/tables/ICT_cancer/ creada)
writeLines(as.character(tabla_md), "output/tables/ICT_cancer/tabla_cox_resumen_ict_2009.md")

# 4️⃣ Forest plot para HR de ictaumentado
resumen_hr <- resumen %>%
  filter(term == "ictaumentado1") %>%
  mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo")))

# Forest plot mejorado
ggplot(resumen_hr, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(aes(color = modelo), size = 1.1, fatten = 3) +   
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(HR,2), " [", round(IC_inf,2), "-", round(IC_sup,2), "]")), 
            hjust = -0.1, size = 4, color = "black") +  
  coord_flip() +
  scale_y_continuous(trans = "log10", breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2, 3), 
                     limits = c(0.25, 3.5)) +  
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "",
    y = "HR de ICT aumentado (IC 95%)",
    title = "Forest plot: Efecto de ICT aumentado sobre muerte por cáncer",
    subtitle = "Comparación entre modelos ajustados (ENS 2009)",
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

# Guardar gráfico
# ggsave("output/graphs/FP_HR_ICT_2009.png", width = 10, height = 6, dpi = 300)

# --- Forest plot Comparativo (ICT y Sexo) ---
df_forest_2009 <- resumen %>%
  filter(term %in% c("ictaumentado1", "sexoFemale")) %>%
  mutate(
    variable = case_when(
      term == "ictaumentado1" ~ "ICT (Aumentado vs Normal)",
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
    title = "Forest plot — HR de ICT aumentado y sexo (ENS 2009)",
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



#=======================================================================================
#### COX con muestra expandida 2016 - Ajustado para ICT Aumentado ####
#=======================================================================================

# Filtrar datos sin NA 
ens2016_final <- ens2016_final %>%
  filter(!is.na(ictaumentado))

ens2016_final$ictaumentado <- as.factor(ens2016_final$ictaumentado)

# Preparar variables
ens2016_final <- ens2016_final %>%
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
  weights = ~Fexp_F1p_Corr,
  data = ens2016_final,
  nest = TRUE
)
options(survey.lonely.psu = "certainty")

# Asegurarnos de que ictaumentado sea factor para la interpretación correcta del HR
ens2016_final$ictaumentado <- as.factor(ens2016_final$ictaumentado)

# Actualizar el diseño de encuesta para incluir las variables necesarias
survey_designkm <- update(survey_designkm,
                        ictaumentado = ens2016_final$ictaumentado,
                        edad = ens2016_final$edad,
                        sexo = ens2016_final$sexo,
                        nedu = ens2016_final$NEDU,
                        zona = ens2016_final$zona,
                        AUDIT = ens2016_final$AUDIT_RIESGOSO,
                        fuma = ens2016_final$fuma,
                        imc = ens2016_final$imc,
                        GPAQ = ens2016_final$GPAQ
                        )

# Modelos de Cox con diseño de encuesta
cox_crudo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado,
                      design = survey_designkm)

cox_edad <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad,
                     design = survey_designkm)

cox_edad_sexo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo,
                          design = survey_designkm)

cox_edad_sexo_nedu <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu,
                               design = survey_designkm)

cox_completo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu + zona + AUDIT + fuma + imc + GPAQ,
                       design = survey_designkm)

# Tabla resumen de HR, IC y p-value
resumen <- bind_rows(
  broom::tidy(cox_crudo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
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

# Mostrar tabla
kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2016)")

# Generamos la tabla con kable y la guardamos en un objeto
tabla_md <- kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2016)")

# Exportar tabla (Asegúrate de tener la carpeta output/tables/ICT_cancer/ creada)
writeLines(as.character(tabla_md), "output/tables/ICT_cancer/tabla_cox_resumen_ict_2016.md")

# 4️⃣ Forest plot para HR de ictaumentado
resumen_hr <- resumen %>%
  filter(term == "ictaumentado1") %>%
  mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo")))

# Forest plot mejorado
ggplot(resumen_hr, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(aes(color = modelo), size = 1.1, fatten = 3) +   
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(HR,2), " [", round(IC_inf,2), "-", round(IC_sup,2), "]")), 
            hjust = -0.1, size = 4, color = "black") +  
  coord_flip() +
  scale_y_continuous(trans = "log10", breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2, 3), 
                     limits = c(0.25, 3.5)) +  
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "",
    y = "HR de ICT aumentado (IC 95%)",
    title = "Forest plot: Efecto de ICT aumentado sobre muerte por cáncer",
    subtitle = "Comparación entre modelos ajustados (ENS 2016)",
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

# Guardar gráfico
# ggsave("output/graphs/FP_HR_ICT_2016.png", width = 10, height = 6, dpi = 300)

# --- Forest plot Comparativo (ICT y Sexo) ---
df_forest_2016 <- resumen %>%
  filter(term %in% c("ictaumentado1", "sexoFemale")) %>%
  mutate(
    variable = case_when(
      term == "ictaumentado1" ~ "ICT (Aumentado vs Normal)",
      term %in% c("sexoFemale") ~ "Sexo (Mujer vs Hombre)",
      TRUE ~ term
    ),
    modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo"))
  ) %>%
  arrange(variable, modelo) %>%
  mutate(item = paste(modelo, variable, sep = " · "),
         item = factor(item, levels = rev(unique(item))))

p_forest_2016 <- ggplot(df_forest_2016, aes(x = HR, y = item, color = variable)) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey40") +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.15, linewidth = 0.8) +
  geom_point(size = 3) +
  scale_x_log10(
    breaks = c(0.5, 1, 2, 3, 5),
    limits = c(min(0.5, min(df_forest_2016$IC_inf, na.rm=TRUE)*0.9),
               max(5,   max(df_forest_2016$IC_sup, na.rm=TRUE)*1.1))
  ) +
  labs(
    x = "Hazard Ratio (escala log)",
    y = NULL,
    color = NULL,
    title = "Forest plot — HR de ICT aumentado y sexo (ENS 2016)",
    subtitle = "Modelos de Cox ponderados por diseño muestral",
    caption = "Barras = IC95%. Línea punteada = HR = 1"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

print(p_forest_2016)


#=======================================================================================
### COX con muestra expandida 2016 - Ajustado para ICT Aumentado  DEFUNCION Y EGRESO####
#=======================================================================================
# Preparar variables
ens2016_final <- ens2016_final %>%
  mutate(
    tiempo_total = dias_transcurridosfinal / 365.25,
    evento_total = if_else(egresoydefuncion == 1, 1, 0),
    evento_label = factor(evento_total,
                          levels = c(0, 1),
                          labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )
survey_designkm <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~Fexp_F1p_Corr,
  data = ens2016_final,
  nest = TRUE
)
options(survey.lonely.psu = "certainty")

# Asegurarnos de que ictaumentado sea factor para la interpretación correcta del HR
ens2016_final$ictaumentado <- as.factor(ens2016_final$ictaumentado)

# Actualizar el diseño de encuesta para incluir las variables necesarias
survey_designkm <- update(survey_designkm,
                        ictaumentado = ens2016_final$ictaumentado,
                        edad = ens2016_final$edad,
                        sexo = ens2016_final$sexo,
                        nedu = ens2016_final$NEDU,
                        zona = ens2016_final$zona,
                        AUDIT = ens2016_final$AUDIT_RIESGOSO,
                        fuma = ens2016_final$fuma,
                        imc = ens2016_final$imc,
                        GPAQ = ens2016_final$GPAQ
                        )

# Modelos de Cox con diseño de encuesta
cox_crudo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado,
                      design = survey_designkm)

cox_edad <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad,
                     design = survey_designkm)

cox_edad_sexo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo,
                          design = survey_designkm)

cox_edad_sexo_nedu <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu,
                               design = survey_designkm)

cox_completo <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu + zona + AUDIT + fuma + imc + GPAQ,
                       design = survey_designkm)

# Tabla resumen de HR, IC y p-value
resumen <- bind_rows(
  broom::tidy(cox_crudo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
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

# Mostrar tabla
kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2016)")

# Generamos la tabla con kable y la guardamos en un objeto
tabla_md <- kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2016)")

# Exportar tabla (Asegúrate de tener la carpeta output/tables/ICT_cancer/ creada)
writeLines(as.character(tabla_md), "output/tables/ICT_cancer/tabla_cox_resumen_ict_2016.md")

# 4️⃣ Forest plot para HR de ictaumentado
resumen_hr <- resumen %>%
  filter(term == "ictaumentado1") %>%
  mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo")))

# Forest plot mejorado
ggplot(resumen_hr, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(aes(color = modelo), size = 1.1, fatten = 3) +   
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(HR,2), " [", round(IC_inf,2), "-", round(IC_sup,2), "]")), 
            hjust = -0.1, size = 4, color = "black") +  
  coord_flip() +
  scale_y_continuous(trans = "log10", breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2, 3), 
                     limits = c(0.25, 3.5)) +  
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "",
    y = "HR de ICT aumentado (IC 95%)",
    title = "Forest plot: Efecto de ICT aumentado sobre muerte por cáncer",
    subtitle = "Comparación entre modelos ajustados (ENS 2016)",
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

# Guardar gráfico
# ggsave("output/graphs/FP_HR_ICT_2016.png", width = 10, height = 6, dpi = 300)

# --- Forest plot Comparativo (ICT y Sexo) ---
df_forest_2016 <- resumen %>%
  filter(term %in% c("ictaumentado1", "sexoFemale")) %>%
  mutate(
    variable = case_when(
      term == "ictaumentado1" ~ "ICT (Aumentado vs Normal)",
      term %in% c("sexoFemale") ~ "Sexo (Mujer vs Hombre)",
      TRUE ~ term
    ),
    modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo"))
  ) %>%
  arrange(variable, modelo) %>%
  mutate(item = paste(modelo, variable, sep = " · "),
         item = factor(item, levels = rev(unique(item))))

p_forest_2016 <- ggplot(df_forest_2016, aes(x = HR, y = item, color = variable)) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey40") +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.15, linewidth = 0.8) +
  geom_point(size = 3) +
  scale_x_log10(
    breaks = c(0.5, 1, 2, 3, 5),
    limits = c(min(0.5, min(df_forest_2016$IC_inf, na.rm=TRUE)*0.9),
               max(5,   max(df_forest_2016$IC_sup, na.rm=TRUE)*1.1))
  ) +
  labs(
    x = "Hazard Ratio (escala log)",
    y = NULL,
    color = NULL,
    title = "Forest plot — HR de ICT aumentado y sexo (ENS 2016)",
    subtitle = "Modelos de Cox ponderados por diseño muestral",
    caption = "Barras = IC95%. Línea punteada = HR = 1"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

print(p_forest_2016)
