# Crear una nueva variable llamada egreso_cancer con restricciones ENS 2003. 
#sum(ens2003_final$DIAG1_EGR == "D469", na.rm = TRUE)#aqui hay 0
#ens2003_final$egreso_cancer <- ifelse(!is.na(ens2003_final$DIAG1_EGR) & grepl("^(C|D0|D3|D4)", ens2003_final$DIAG1_EGR) & ens2003_final$DIAG1 != "D469", 1, 0)
#table(ens2003_final$DIAG1_EGR, ens2003_final$egreso_cancer)
#sum(ens2003_final$egreso_cancer)
#hay 250 muertes por cáncer

source("procesamiento/04_etiquetado_variables.R", encoding = "UTF-8")

#LO NUEVO QUE HIZO EL CARLOS ES DESDE AQUÍ:

#Crear nueva variable que combine egresos y defunción
ens2003_final$egreso_cancer <- ifelse(!is.na(ens2003_final$DIAG1_EGR) & grepl("^(C|D0|D3|D4)", ens2003_final$DIAG1_EGR) & ens2003_final$DIAG1 != "D469", 1, 0)

ens2003_final$egresoydefuncion <- ifelse(ens2003_final$muerte_cancer == "Cancer death" | ens2003_final$egreso_cancer == "1", 1, 0)

ens2003_final$fechadefinitiva <- if_else(
  ens2003_final$egresoydefuncion == 1,
  if_else(!is.na(ens2003_final$FECHA_EGR), ens2003_final$FECHA_EGR, ens2003_final$FECHA_DEF),
  as.Date(NA) 
)

ens2009_final$egreso_cancer <- ifelse(!is.na(ens2009_final$DIAG1_EGR) & grepl("^(C|D0|D3|D4)", ens2009_final$DIAG1_EGR) & ens2009_final$DIAG1 != "D469", 1, 0)

ens2009_final$egresoydefuncion <- ifelse(ens2009_final$muerte_cancer == "Cancer death" | ens2009_final$egreso_cancer == "1", 1, 0)

ens2009_final$fechadefinitiva <- if_else(
  ens2009_final$egresoydefuncion == 1,
  if_else(!is.na(ens2009_final$FECHA_EGR), ens2009_final$FECHA_EGR, ens2009_final$FECHA_DEF),
  as.Date(NA) 
)


ens2016_final$egreso_cancer <- ifelse(!is.na(ens2016_final$DIAG1_EGR) & grepl("^(C|D0|D3|D4)", ens2016_final$DIAG1_EGR) & ens2016_final$DIAG1 != "D469", 1, 0)

ens2016_final$egresoydefuncion <- ifelse(ens2016_final$muerte_cancer == "Cancer death" | ens2016_final$egreso_cancer == "1", 1, 0)

ens2016_final$fechadefinitiva <- if_else(
  ens2016_final$egresoydefuncion == 1,
  if_else(!is.na(ens2016_final$FECHA_EGR), ens2016_final$FECHA_EGR, ens2016_final$FECHA_DEF),
  as.Date(NA) 
)


#Crear nueva variable de días transcurridos
ens2003_final <- ens2003_final %>%
  mutate(
    # Primero calculamos los días hasta el evento (dará NA para los censurados)
    dias_hasta_evento = as.numeric(fechadefinitiva - fecha_encuesta),
    dias_transcurridosfinal = if_else(
      egresoydefuncion == 1,
      dias_hasta_evento,
      dias_transcurridos # <-- Esto salva a los censurados de ser eliminados
    )
  )

ens2009_final <- ens2009_final %>%
  mutate(
    # Primero calculamos los días hasta el evento (dará NA para los censurados)
    dias_hasta_evento = as.numeric(fechadefinitiva - fecha_encuesta),
    dias_transcurridosfinal = if_else(
      egresoydefuncion == 1,
      dias_hasta_evento,
      dias_transcurridos # <-- Esto salva a los censurados de ser eliminados
    )
  )

ens2016_final <- ens2016_final %>%
  mutate(
    # Primero calculamos los días hasta el evento (dará NA para los censurados)
    dias_hasta_evento = as.numeric(fechadefinitiva - fecha_encuesta),
    dias_transcurridosfinal = if_else(
      egresoydefuncion == 1,
      dias_hasta_evento,
      dias_transcurridos # <-- Esto salva a los censurados de ser eliminados
    )
  )





