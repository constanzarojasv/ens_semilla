source("procesamiento/04_etiquetado_variables.R", encoding = "UTF-8")

####### KM expandido  #####
# Filtrar datos sin NA en AF
ens2009_final <- ens2009_final %>%
  filter(!is.na(af_cancer_binaria))

ens2009_final$af_cancer_binaria <- as.factor(ens2009_final$af_cancer_binaria)

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
km_fit <- svykm(Surv(tiempo_total, evento_total) ~ af_cancer_binaria, 
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
df_km$grupo <- gsub("af_cancer_binaria=", "", df_km$grupo)

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
    color = "AF Cáncer",
    fill = "AF Cáncer" # Asegura que la leyenda combine línea y sombreado
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
logrank_test <- svyranktest(Surv(tiempo_total, evento_total) ~ af_cancer_binaria, design = survey_designkm)
cat("Valor p log-rank (diseño complejo):", logrank_test$p.value, "\n")


# Ver resumen del tiempo y eventos por grupo
ens2009_final %>%
  group_by(af_cancer_binaria) %>%
  summarise(
    n = n(),
    n_eventos = sum(evento_total == 1, na.rm = TRUE),
    min_tiempo = min(tiempo_total, na.rm = TRUE),
    max_tiempo = max(tiempo_total, na.rm = TRUE)
  )

# Sobrevida a los 5 años (Grupo Con AF)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 5))]

# Sobrevida a los 10 años (Grupo Con AF)
km_fit[[2]]$surv[max(which(km_fit[[2]]$time <= 10))]

####COX con muestra expandida####

# Librerías

# Actualizar el diseño de encuesta para incluir las variables necesarias

ENS2009conexc_confe <- ENS2009conexc_confe %>%
  mutate(
    sexo = factor(sexo,
                  levels = c(1, 2),
                  labels = c("Hombre", "Mujer"))
  )
survey_designkm <- update(survey_designkm,
                        tiempo_anos = ENS2009conexc_confe$dias_transcurridos / 365.25,
                        af_cancer_binaria = ENS2009conexc_confe$af_cancer_binaria,
                        muerte_cancer = ENS2009conexc_confe$muerte_cancer,
                        edad = ENS2009conexc_confe$edad,
                        sexo = ENS2009conexc_confe$sexo,
                        nedu = ENS2009conexc_confe$nedu,
                        zona = ENS2009conexc_confe$zona,
                        AUDIT = ENS2009conexc_confe$AUDIT_RIESGOSO,
                        fuma = ENS2009conexc_confe$fuma,
                        imc = ENS2009conexc_confe$imc,
                        GPAQ = ENS2009conexc_confe$GPAQ
                        )



# 1️⃣ Preparar variable de tiempo
ENS2009conexc_confe <- ENS2009conexc_confe %>%
  mutate(
    tiempo_anos = dias_transcurridos / 365.25
  )

# 2️⃣ Modelos de Cox con diseño de encuesta
cox_crudo <- svycoxph(Surv(tiempo_anos, muerte_cancer) ~ af_cancer_binaria,
                      design = survey_designkm)

cox_edad <- svycoxph(Surv(tiempo_anos, muerte_cancer) ~ af_cancer_binaria + edad,
                     design = survey_designkm)

cox_edad_sexo <- svycoxph(Surv(tiempo_anos, muerte_cancer) ~ af_cancer_binaria + edad + sexo,
                          design = survey_designkm)

cox_edad_sexo_nedu <- svycoxph(Surv(tiempo_anos, muerte_cancer) ~ af_cancer_binaria + edad + sexo + nedu,
                               design = survey_designkm)

cox_contodo<- svycoxph(Surv(tiempo_anos, muerte_cancer) ~ af_cancer_binaria + edad + sexo + nedu + zona + AUDIT + fuma + imc + GPAQ,
                       design = survey_designkm)

# 3️⃣ Tabla resumen de HR, IC y p-value
resumen <- bind_rows(
  tidy(cox_crudo) %>% mutate(modelo = "Crudo"),
  tidy(cox_edad) %>% mutate(modelo = "Edad"),
  tidy(cox_edad_sexo) %>% mutate(modelo = "Edad+Sexo"),
  tidy(cox_contodo) %>% mutate(modelo = "Con todo")
) %>%
  mutate(
    HR = exp(estimate),
    IC_inf = exp(estimate - 1.96*std.error),
    IC_sup = exp(estimate + 1.96*std.error)
  ) %>%
  select(modelo, term, HR, IC_inf, IC_sup, p.value)

# Mostrar tabla
kable(resumen, digits = 3, caption = "Modelos de Cox para muerte por cáncer")



# 4️⃣ Forest plot para HR deaf_cancer_binaria
resumen_hr <- resumen %>%
  filter(term == "af_cancer_binariaCon AF")

ggplot(resumen_hr, aes(x = modelo, y = HR, ymin = IC_inf, ymax = IC_sup)) +
  geom_pointrange(color = "blue", size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    x = "Modelo",
    y = "HR de AF cáncerl (IC 95%)",
    title = "Efecto de AF cáncer sobre muerte por cáncer",
    subtitle = "Forest plot de HR ajustados por distintos modelos"
  ) +
  theme_minimal(base_size = 14)

library(ggplot2)
library(dplyr)

# Filtrar solo HR de AF cáncer
resumen_hr <- resumen %>%
  filter(term == "af_cancer_binariaCon AF") %>%
  mutate(modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Con todo")))

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
    y = "HR de AF cáncer (IC 95%)",
    title = "Forest plot: Efecto de AF cáncerl sobre muerte por cáncer",
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


library(dplyr)
library(ggplot2)
library(stringr)

# --- Forest plot ENS 2009 (objeto: resumen) ---
df_forest_2009 <- resumen %>%
  filter(term %in% c("af_cancer_binariaCon AF", "sexoMujer", "as.factor(sexo)Mujer")) %>%
  mutate(
    variable = case_when(
      term == "af_cancer_binariaCon AF" ~ "AF familiar (Con AF vs Sin AF)",
      term %in% c("sexoMujer", "as.factor(sexo)Mujer") ~ "Sexo (Mujer vs Hombre)",
      TRUE ~ term
    ),
    modelo = factor(modelo, levels = c("Crudo", "Edad", "Edad+Sexo", "Con todo"))
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

# Guardar (opcional)
# ggsave("forest_af_y_sexo_ENS2009.png", p_forest_2009, width = 8, height = 5, dpi = 300)


