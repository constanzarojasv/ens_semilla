source("procesamiento/04_etiquetado_variables.R", encoding = "UTF-8")
ens2003_final$ictaumentado <- as.numeric(ens2003_final$ict >= 0.6)
ens2009_final$ictaumentado <- as.numeric(ens2009_final$ict >= 0.6)
ens2016_final$ictaumentado <- as.numeric(ens2016_final$ict >= 0.6)

#=======================================================================================
####### KAPLAN MEIER 2003 #####
#=======================================================================================
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

# Diseño muestral 2003
survey_designkm_2003 <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~FEXP_analisis,
  data = ens2003_final,
  nest = TRUE
)
options(survey.lonely.psu = "certainty")

# Kaplan-Meier ponderado con svykm()
km_fit_2003 <- svykm(Surv(tiempo_total, evento_total) ~ ictaumentado, 
                design = survey_designkm_2003, 
                se = TRUE)

df_km_2003 <- bind_rows(
  lapply(names(km_fit_2003), function(nom) {
    km_obj <- km_fit_2003[[nom]]
    ci <- confint(km_obj, parm = km_obj$time)
    
    df_temp <- data.frame(
      tiempo = km_obj$time,
      supervivencia = km_obj$surv,
      lower = ci[, 1],
      upper = ci[, 2],
      grupo = nom
    )
    
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

df_km_2003$grupo <- gsub("ictaumentado", "", df_km_2003$grupo)

# Gráfico KM 2003
ggplot(df_km_2003, aes(x = tiempo, y = supervivencia, color = grupo, fill = grupo)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_step(linewidth = 1, direction = "hv") + 
  scale_y_continuous(limits = c(0.9, 1.0), 
                     breaks = seq(0.9, 1.0, by = 0.02),
                     labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks = seq(0, max(df_km_2003$tiempo, na.rm = TRUE), by = 2)) +
  scale_color_manual(values = c("#2980b9", "#c0392b")) + 
  scale_fill_manual(values = c("#2980b9", "#c0392b")) + 
  labs(
    title = "Curva de Kaplan-Meier (Muerte por Cáncer)",
    subtitle = "ENS 2003 - Con Intervalos de Confianza al 95%",
    x = "Años de seguimiento",
    y = "Probabilidad de Supervivencia",
    color = "ICT aumentado",
    fill = "ICT aumentado"
  ) +
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

# Guardar gráfico KM 2003
# ggsave("output/graphs/KM_ICT_2003.png", width = 10, height = 6, dpi = 300)

# Test log-rank 2003
logrank_test_2003 <- svyranktest(Surv(tiempo_total, evento_total) ~ ictaumentado, design = survey_designkm_2003)
cat("Valor p log-rank (diseño complejo) 2003:", logrank_test_2003$p.value, "\n")

# Resumen 2003
ens2003_final %>%
  group_by(ictaumentado) %>%
  summarise(
    n = n(),
    n_eventos = sum(evento_total == 1, na.rm = TRUE),
    min_tiempo = min(tiempo_total, na.rm = TRUE),
    max_tiempo = max(tiempo_total, na.rm = TRUE)
  )

# Sobrevida a los 5 y 10 años
km_fit_2003[[2]]$surv[max(which(km_fit_2003[[2]]$time <= 5))]
km_fit_2003[[2]]$surv[max(which(km_fit_2003[[2]]$time <= 10))]


#=======================================================================================
#### COX con muestra expandida 2003 - Ajustado para ICT Aumentado ####
#=======================================================================================
survey_designkm_2003 <- update(survey_designkm_2003,
                        ictaumentado = ens2003_final$ictaumentado,
                        edad = ens2003_final$edad,
                        sexo = ens2003_final$sexo,
                        nedu = ens2003_final$NEDU,
                        zona = ens2003_final$zona,
                        fuma = ens2003_final$fuma,
                        imc = ens2003_final$imc)

cox_crudo_2003 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado, design = survey_designkm_2003)
cox_edad_2003 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad, design = survey_designkm_2003)
cox_edad_sexo_2003 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo, design = survey_designkm_2003)
cox_edad_sexo_nedu_2003 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu, design = survey_designkm_2003)
cox_completo_2003 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu + zona + fuma + imc, design = survey_designkm_2003)

resumen_2003 <- bind_rows(
  broom::tidy(cox_crudo_2003, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad_2003, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo_2003, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
  broom::tidy(cox_completo_2003, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Modelo completo")
) %>%
  select(modelo, term, HR = estimate, IC_inf = conf.low, IC_sup = conf.high, p.value)

kable(resumen_2003, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2003)")
tabla_md_2003 <- kable(resumen_2003, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2003)")

# Exportar tabla 2003
# writeLines(as.character(tabla_md_2003), "output/tables/ICT/tabla_cox_resumen_ict_2003.md")

# Forest plot HR ictaumentado 2003
resumen_hr_2003 <- resumen_2003 %>%
  filter(term == "ictaumentado1") %>%
  mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo")))

ggplot(resumen_hr_2003, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(aes(color = modelo), size = 1.1, fatten = 3) +   
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(HR,2), " [", round(IC_inf,2), "-", round(IC_sup,2), "]")), 
            hjust = -0.1, size = 4, color = "black") +  
  coord_flip() +
  scale_y_continuous(trans = "log10", breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2, 3), limits = c(0.25, 3.5)) +  
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "", y = "HR de ICT aumentado (IC 95%)",
    title = "Forest plot: Efecto de ICT aumentado sobre muerte por cáncer",
    subtitle = "Comparación entre modelos ajustados (ENS 2003)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())

# Guardar Forest Plot HR 2003
# ggsave("output/graphs/FP_HR_ICT_2003.png", width = 10, height = 6, dpi = 300)

# Forest plot Comparativo 2003
df_forest_2003 <- resumen_2003 %>%
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
  scale_x_log10(breaks = c(0.5, 1, 2, 3, 5), limits = c(min(0.5, min(df_forest_2003$IC_inf, na.rm=TRUE)*0.9), max(5, max(df_forest_2003$IC_sup, na.rm=TRUE)*1.1))) +
  labs(x = "Hazard Ratio (escala log)", y = NULL, color = NULL, title = "Forest plot — HR de ICT aumentado y sexo (ENS 2003)") +
  theme_classic(base_size = 12) + theme(legend.position = "top")
print(p_forest_2003)

# Guardar Forest Plot Comparativo 2003 (Nota: asegúrate de cambiar el nombre si no quieres sobreescribir el anterior)
# ggsave("output/graphs/FP_HR_ICT_Comp_2003.png", width = 10, height = 6, dpi = 300)


#=======================================================================================
####### KAPLAN MEIER 2009 #####
#=======================================================================================
ens2009_final <- ens2009_final %>% filter(!is.na(ictaumentado))
ens2009_final$ictaumentado <- as.factor(ens2009_final$ictaumentado)

ens2009_final <- ens2009_final %>%
  mutate(
    tiempo_total = dias_transcurridos / 365.25,
    evento_total = if_else(muerte_cancer == "Cancer death", 1, 0),
    evento_label = factor(evento_total, levels = c(0, 1), labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )

survey_designkm_2009 <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~FEXP1,
  data = ens2009_final,
  nest = TRUE
)

km_fit_2009 <- svykm(Surv(tiempo_total, evento_total) ~ ictaumentado, design = survey_designkm_2009, se = TRUE)

df_km_2009 <- bind_rows(
  lapply(names(km_fit_2009), function(nom) {
    km_obj <- km_fit_2009[[nom]]
    ci <- confint(km_obj, parm = km_obj$time)
    df_temp <- data.frame(tiempo = km_obj$time, supervivencia = km_obj$surv, lower = ci[, 1], upper = ci[, 2], grupo = nom)
    df_inicio <- data.frame(tiempo = 0, supervivencia = 1, lower = 1, upper = 1, grupo = nom)
    bind_rows(df_inicio, df_temp)
  })
)
df_km_2009$grupo <- gsub("ictaumentado", "", df_km_2009$grupo)

ggplot(df_km_2009, aes(x = tiempo, y = supervivencia, color = grupo, fill = grupo)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_step(linewidth = 1, direction = "hv") + 
  scale_y_continuous(limits = c(0.9, 1.0), breaks = seq(0.9, 1.0, by = 0.02), labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks = seq(0, max(df_km_2009$tiempo, na.rm = TRUE), by = 2)) +
  scale_color_manual(values = c("#2980b9", "#c0392b")) + scale_fill_manual(values = c("#2980b9", "#c0392b")) + 
  labs(title = "Curva de Kaplan-Meier (Muerte por Cáncer)", subtitle = "ENS 2009 - Con Intervalos de Confianza al 95%", x = "Años de seguimiento", y = "Probabilidad de Supervivencia", color = "ICT aumentado", fill = "ICT aumentado") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12), legend.position = "bottom", legend.title = element_text(face = "bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

# Guardar gráfico KM 2009
# ggsave("output/graphs/KM_ICT_2009.png", width = 10, height = 6, dpi = 300)

logrank_test_2009 <- svyranktest(Surv(tiempo_total, evento_total) ~ ictaumentado, design = survey_designkm_2009)
cat("Valor p log-rank (diseño complejo) 2009:", logrank_test_2009$p.value, "\n")

#=======================================================================================
#### COX con muestra expandida 2009 - Ajustado para ICT Aumentado ####
#=======================================================================================
survey_designkm_2009 <- update(survey_designkm_2009,
                        ictaumentado = ens2009_final$ictaumentado,
                        edad = ens2009_final$edad,
                        sexo = ens2009_final$sexo,
                        nedu = ens2009_final$NEDU,
                        zona = ens2009_final$zona,
                        AUDIT = ens2009_final$AUDIT_RIESGOSO,
                        fuma = ens2009_final$fuma,
                        imc = ens2009_final$imc,
                        GPAQ = ens2009_final$GPAQ)

cox_crudo_2009 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado, design = survey_designkm_2009)
cox_edad_2009 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad, design = survey_designkm_2009)
cox_edad_sexo_2009 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo, design = survey_designkm_2009)
cox_edad_sexo_nedu_2009 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu, design = survey_designkm_2009)
cox_completo_2009 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu + zona + AUDIT + fuma + imc + GPAQ, design = survey_designkm_2009)

resumen_2009 <- bind_rows(
  broom::tidy(cox_crudo_2009, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad_2009, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo_2009, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
  broom::tidy(cox_completo_2009, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Modelo completo")
) %>% select(modelo, term, HR = estimate, IC_inf = conf.low, IC_sup = conf.high, p.value)

kable(resumen_2009, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2009)")
tabla_md_2009 <- kable(resumen_2009, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2009)")

# Exportar tabla 2009
# writeLines(as.character(tabla_md_2009), "output/tables/ICT/tabla_cox_resumen_ict_2009.md")

resumen_hr_2009 <- resumen_2009 %>% filter(term == "ictaumentado1") %>% mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo")))

ggplot(resumen_hr_2009, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(aes(color = modelo), size = 1.1, fatten = 3) +   
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(HR,2), " [", round(IC_inf,2), "-", round(IC_sup,2), "]")), hjust = -0.1, size = 4, color = "black") +  
  coord_flip() + scale_y_continuous(trans = "log10", breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2, 3), limits = c(0.25, 3.5)) +  
  scale_color_brewer(palette = "Set2") +
  labs(x = "", y = "HR de ICT aumentado (IC 95%)", title = "Forest plot: Efecto de ICT aumentado sobre muerte por cáncer", subtitle = "Comparación entre modelos ajustados (ENS 2009)") +
  theme_minimal(base_size = 14) + theme(legend.position = "none", panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())

# Guardar Forest Plot HR 2009
# ggsave("output/graphs/FP_HR_ICT_2009.png", width = 10, height = 6, dpi = 300)

df_forest_2009 <- resumen_2009 %>% filter(term %in% c("ictaumentado1", "sexoFemale")) %>%
  mutate(variable = case_when(term == "ictaumentado1" ~ "ICT (Aumentado vs Normal)", term %in% c("sexoFemale") ~ "Sexo (Mujer vs Hombre)", TRUE ~ term),
         modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo"))) %>%
  arrange(variable, modelo) %>% mutate(item = paste(modelo, variable, sep = " · "), item = factor(item, levels = rev(unique(item))))

p_forest_2009 <- ggplot(df_forest_2009, aes(x = HR, y = item, color = variable)) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey40") + geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.15, linewidth = 0.8) + geom_point(size = 3) +
  scale_x_log10(breaks = c(0.5, 1, 2, 3, 5), limits = c(min(0.5, min(df_forest_2009$IC_inf, na.rm=TRUE)*0.9), max(5, max(df_forest_2009$IC_sup, na.rm=TRUE)*1.1))) +
  labs(x = "Hazard Ratio (escala log)", y = NULL, color = NULL, title = "Forest plot — HR de ICT aumentado y sexo (ENS 2009)") +
  theme_classic(base_size = 12) + theme(legend.position = "top")
print(p_forest_2009)

# Guardar Forest Plot Comparativo 2009 
# ggsave("output/graphs/FP_HR_ICT_Comp_2009.png", width = 10, height = 6, dpi = 300)


#=======================================================================================
####### KAPLAN MEIER 2016 #####
#=======================================================================================
ens2016_final <- ens2016_final %>% filter(!is.na(ictaumentado))
ens2016_final$ictaumentado <- as.factor(ens2016_final$ictaumentado)

ens2016_final <- ens2016_final %>%
  mutate(
    tiempo_total = dias_transcurridos / 365.25,
    evento_total = if_else(muerte_cancer == "Cancer death", 1, 0),
    evento_label = factor(evento_total, levels = c(0, 1), labels = c("No muertos por cáncer", "Muertes por cáncer"))
  )

survey_designkm_2016 <- svydesign(
  id = ~conglomerado,
  strata = ~estrato,
  weights = ~Fexp_F1p_Corr,
  data = ens2016_final,
  nest = TRUE
)

km_fit_2016 <- svykm(Surv(tiempo_total, evento_total) ~ ictaumentado, design = survey_designkm_2016, se = TRUE)

df_km_2016 <- bind_rows(
  lapply(names(km_fit_2016), function(nom) {
    km_obj <- km_fit_2016[[nom]]
    ci <- confint(km_obj, parm = km_obj$time)
    df_temp <- data.frame(tiempo = km_obj$time, supervivencia = km_obj$surv, lower = ci[, 1], upper = ci[, 2], grupo = nom)
    df_inicio <- data.frame(tiempo = 0, supervivencia = 1, lower = 1, upper = 1, grupo = nom)
    bind_rows(df_inicio, df_temp)
  })
)
df_km_2016$grupo <- gsub("ictaumentado", "", df_km_2016$grupo)

ggplot(df_km_2016, aes(x = tiempo, y = supervivencia, color = grupo, fill = grupo)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_step(linewidth = 1, direction = "hv") + 
  scale_y_continuous(limits = c(0.9, 1.0), breaks = seq(0.9, 1.0, by = 0.02), labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks = seq(0, max(df_km_2016$tiempo, na.rm = TRUE), by = 2)) +
  scale_color_manual(values = c("#2980b9", "#c0392b")) + scale_fill_manual(values = c("#2980b9", "#c0392b")) + 
  labs(title = "Curva de Kaplan-Meier (Muerte por Cáncer)", subtitle = "ENS 2016 - Con Intervalos de Confianza al 95%", x = "Años de seguimiento", y = "Probabilidad de Supervivencia", color = "ICT aumentado", fill = "ICT aumentado") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), legend.position = "bottom", legend.title = element_text(face = "bold"), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

# Guardar gráfico KM 2016
# ggsave("output/graphs/KM_ICT_2016.png", width = 10, height = 6, dpi = 300)

logrank_test_2016 <- svyranktest(Surv(tiempo_total, evento_total) ~ ictaumentado, design = survey_designkm_2016)
cat("Valor p log-rank (diseño complejo) 2016:", logrank_test_2016$p.value, "\n")


#=======================================================================================
#### COX con muestra expandida 2016 - Ajustado para ICT Aumentado ####
#=======================================================================================
survey_designkm_2016 <- update(survey_designkm_2016,
                        ictaumentado = ens2016_final$ictaumentado,
                        edad = ens2016_final$edad,
                        sexo = ens2016_final$sexo,
                        nedu = ens2016_final$NEDU,
                        zona = ens2016_final$zona,
                        AUDIT = ens2016_final$AUDIT_RIESGOSO,
                        fuma = ens2016_final$fuma,
                        imc = ens2016_final$imc,
                        GPAQ = ens2016_final$GPAQ)

cox_crudo_2016 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado, design = survey_designkm_2016)
cox_edad_2016 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad, design = survey_designkm_2016)
cox_edad_sexo_2016 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo, design = survey_designkm_2016)
cox_edad_sexo_nedu_2016 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu, design = survey_designkm_2016)
cox_completo_2016 <- svycoxph(Surv(tiempo_total, evento_total) ~ ictaumentado + edad + sexo + nedu + zona + AUDIT + fuma + imc + GPAQ, design = survey_designkm_2016)

resumen_2016 <- bind_rows(
  broom::tidy(cox_crudo_2016, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Crudo"),
  broom::tidy(cox_edad_2016, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad"),
  broom::tidy(cox_edad_sexo_2016, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Edad+Sexo"),
  broom::tidy(cox_completo_2016, exponentiate = TRUE, conf.int = TRUE) %>% mutate(modelo = "Modelo completo")
) %>% select(modelo, term, HR = estimate, IC_inf = conf.low, IC_sup = conf.high, p.value)

kable(resumen_2016, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2016)")
tabla_md_2016 <- kable(resumen_2016, digits = 3, caption = "Modelos de Cox para muerte por cáncer - ICT (2016)")

# Exportar tabla 2016
# writeLines(as.character(tabla_md_2016), "output/tables/ICT/tabla_cox_resumen_ict_2016.md")

resumen_hr_2016 <- resumen_2016 %>% filter(term == "ictaumentado1") %>% mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo")))

ggplot(resumen_hr_2016, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(aes(color = modelo), size = 1.1, fatten = 3) +   
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(HR,2), " [", round(IC_inf,2), "-", round(IC_sup,2), "]")), hjust = -0.1, size = 4, color = "black") +  
  coord_flip() + scale_y_continuous(trans = "log10", breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2, 3), limits = c(0.25, 3.5)) +  
  scale_color_brewer(palette = "Set2") +
  labs(x = "", y = "HR de ICT aumentado (IC 95%)", title = "Forest plot: Efecto de ICT aumentado sobre muerte por cáncer", subtitle = "Comparación entre modelos ajustados (ENS 2016)") +
  theme_minimal(base_size = 14) + theme(legend.position = "none", panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())

# Guardar Forest Plot HR 2016
# ggsave("output/graphs/FP_HR_ICT_2016.png", width = 10, height = 6, dpi = 300)

df_forest_2016 <- resumen_2016 %>% filter(term %in% c("ictaumentado1", "sexoFemale")) %>%
  mutate(variable = case_when(term == "ictaumentado1" ~ "ICT (Aumentado vs Normal)", term %in% c("sexoFemale") ~ "Sexo (Mujer vs Hombre)", TRUE ~ term),
         modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Modelo completo"))) %>%
  arrange(variable, modelo) %>% mutate(item = paste(modelo, variable, sep = " · "), item = factor(item, levels = rev(unique(item))))

p_forest_2016 <- ggplot(df_forest_2016, aes(x = HR, y = item, color = variable)) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey40") + geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.15, linewidth = 0.8) + geom_point(size = 3) +
  scale_x_log10(breaks = c(0.5, 1, 2, 3, 5), limits = c(min(0.5, min(df_forest_2016$IC_inf, na.rm=TRUE)*0.9), max(5, max(df_forest_2016$IC_sup, na.rm=TRUE)*1.1))) +
  labs(x = "Hazard Ratio (escala log)", y = NULL, color = NULL, title = "Forest plot — HR de ICT aumentado y sexo (ENS 2016)") +
  theme_classic(base_size = 12) + theme(legend.position = "top")
print(p_forest_2016)

# Guardar Forest Plot Comparativo 2016
# ggsave("output/graphs/FP_HR_ICT_Comp_2016.png", width = 10, height = 6, dpi = 300)
