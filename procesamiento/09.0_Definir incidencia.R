source("procesamiento/04_etiquetado_variables.R", encoding = "UTF-8")

# ==========================================
# ENS 2003
# ==========================================
ens2003_final$egreso_cancer <- ifelse(!is.na(ens2003_final$DIAG1_EGR) & grepl("^(C|D0|D3|D4)", ens2003_final$DIAG1_EGR) & ens2003_final$DIAG1 != "D469", 1, 0)

#Se crea variable incidencia a partir de mortalidad y egresos
ens2003_final$incidencia <- ifelse(ens2003_final$muerte_cancer == "Cancer death" | ens2003_final$egreso_cancer == "1", 1, 0)

#Se deja fecha de egreso o defuncioón que aparezca primero
ens2003_final$fechadefinitiva <- if_else(
  ens2003_final$incidencia == 1,
  pmin(ens2003_final$FECHA_EGR, ens2003_final$FECHA_DEF, na.rm = TRUE),
  as.Date(NA) 
)

# ==========================================
# ENS 2009 - se repite lo de ens 2003
# ==========================================
ens2009_final$egreso_cancer <- ifelse(!is.na(ens2009_final$DIAG1_EGR) & grepl("^(C|D0|D3|D4)", ens2009_final$DIAG1_EGR) & ens2009_final$DIAG1 != "D469", 1, 0)

ens2009_final$incidencia <- ifelse(ens2009_final$muerte_cancer == "Cancer death" | ens2009_final$egreso_cancer == "1", 1, 0)

ens2009_final$fechadefinitiva <- if_else(
  ens2009_final$incidencia == 1,
  pmin(ens2009_final$FECHA_EGR, ens2009_final$FECHA_DEF, na.rm = TRUE),
  as.Date(NA) 
)

# ==========================================
# ENS 2016 - se repite
# ==========================================
ens2016_final$egreso_cancer <- ifelse(!is.na(ens2016_final$DIAG1_EGR) & grepl("^(C|D0|D3|D4)", ens2016_final$DIAG1_EGR) & ens2016_final$DIAG1 != "D469", 1, 0)

ens2016_final$incidencia <- ifelse(ens2016_final$muerte_cancer == "Cancer death" | ens2016_final$egreso_cancer == "1", 1, 0)

ens2016_final$fechadefinitiva <- if_else(
  ens2016_final$incidencia == 1,
  pmin(ens2016_final$FECHA_EGR, ens2016_final$FECHA_DEF, na.rm = TRUE),
  as.Date(NA) 
)

#=========================================================================
#Crear nueva variable de días transcurridos
ens2003_final <- ens2003_final %>%
  mutate(
    # Primero calculamos los días hasta el evento (dará NA para los censurados)
    dias_hasta_evento = as.numeric(fechadefinitiva - fecha_encuesta),
    dias_transcurridosfinal = if_else(
      incidencia == 1,
      dias_hasta_evento,
      dias_transcurridos # <-- Esto salva a los censurados de ser eliminados
    )
  )

ens2009_final <- ens2009_final %>%
  mutate(
    # Primero calculamos los días hasta el evento (dará NA para los censurados)
    dias_hasta_evento = as.numeric(fechadefinitiva - fecha_encuesta),
    dias_transcurridosfinal = if_else(
      incidencia == 1,
      dias_hasta_evento,
      dias_transcurridos # <-- Esto salva a los censurados de ser eliminados
    )
  )

ens2016_final <- ens2016_final %>%
  mutate(
    # Primero calculamos los días hasta el evento (dará NA para los censurados)
    dias_hasta_evento = as.numeric(fechadefinitiva - fecha_encuesta),
    dias_transcurridosfinal = if_else(
      incidencia == 1,
      dias_hasta_evento,
      dias_transcurridos # <-- Esto salva a los censurados de ser eliminados
    )
  )


#Chequear
table(ens2003_final$egreso_cancer, ens2003_final$muerte_cancer)
table(ens2003_final$incidencia)

table(ens2009_final$egreso_cancer, ens2009_final$muerte_cancer)
table(ens2009_final$incidencia)

table(ens2016_final$egreso_cancer, ens2016_final$muerte_cancer)
table(ens2016_final$incidencia)

