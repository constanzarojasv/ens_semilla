# 0.Cargar librerías
source("procesamiento/00_setup.R", encoding = "UTF-8")

ENS_2003_UNIDA <- read_rds("input/data-procesada/defunciones-bind-egresos/ens2003_bind.rds")
ENS_2009_UNIDA <- read_rds("input/data-procesada/defunciones-bind-egresos/ens2009_bind.rds")
ENS_2016_UNIDA <- read_rds("input/data-procesada/defunciones-bind-egresos/ens2016_bind.rds")

# 1. Excluir a menores de 25 años en base unida para el análisis (ENS 2003)
##Número de personas <25 años 
sum(ENS_2003_UNIDA$edad < 25, na.rm = TRUE)
sum(ENS_2009_UNIDA$edad < 25, na.rm = TRUE)
sum(ENS_2016_UNIDA$edad < 25, na.rm = TRUE)

#SE CREA SUB BASE
ENS2003conexc <- ENS_2003_UNIDA %>%
  filter(edad >= 25)
summary (ENS2003conexc$edad)

#CHEQUEO MUERTE POR CANCER EN NUEVA BASE
table(ENS2003conexc$muerte_cancer)

#GUARDO BASE ENS 2003 UNIDA Y CON EXCLUSIONES PARA ANALISIS
write_rds(ENS2003conexc, "input/data-procesada/data-analisis/ens2003_analisis.rds")

############################TERMINA ENS 2003##############################


###EXCLUSIONES
#Antecedente personal de cáncer
#ENS2003 no tiene la variable de cáncer activo
ENS_2009_UNIDA$ca_activo  #CAMBIAR NOMBRE DE BASES 2009 Y 2017 SEGUN BIND
ENS_2016_UNIDA$ca_activo

dim(ENS_2009_UNIDA)
#opcion para borrar la fila donde la persona fallecio antes de la encuesta
ENS_2009_UNIDA <- ENS_2009_UNIDA %>%
  filter(ID != "3228")
sum(ENS_2009_UNIDA$dias_transcurridos < 0, na.rm = TRUE)
dim(ENS_2009_UNIDA)

#EXCLUSIONES ENS 2009: MENORES DE 25 Y CANCER ACTIVO (ca_activo == 1)
# Número de personas <25 años 2009
sum(ENS_2009_UNIDA$edad < 25, na.rm = TRUE)
table(ENS_2009_UNIDA$edad<25, ENS_2009_UNIDA$ca_activo)

#SE CREA NUEVA BASE
ENS2009conexc <- ENS_2009_UNIDA %>%
  filter(edad >= 25, ca_activo != 1 | is.na(ca_activo))
summary (ENS2009conexc$edad)
dim(ENS_2009_UNIDA)
dim(ENS2009conexc)

#CHEQUEO NUEVA BASE
table(ENS2009conexc$ca_activo)
table(ENS2009conexc$muerte_cancer)

write_rds(ENS2009conexc, "input/data-procesada/data-analisis/ens2009_analisis.rds")

#############################TERMINA ENS 2009############################
##EXCLUSIONES EN ENS 2017: MISMOS CRITERIOS QUE ENS 2009
###EXCLUSIONES
#Antecedente personal de cáncer
#ENS2003 no tiene la variable de cáncer activo
ENS_2009_UNIDA$ca_activo  #CAMBIAR NOMBRE DE BASES 2009 Y 2017 SEGUN BIND
ENS_2016_UNIDA$ca_activo

dim(ENS_2016_UNIDA)
#no hay nadie que falleciera antes de la encuesta
sum(ENS_2016_UNIDA$dias_transcurridos < 0, na.rm = TRUE)

#EXCLUSIONES ENS 2016: MENORES DE 25 Y CANCER ACTIVO (ca_activo == 1)
# Número de personas <25 años 2009
sum(ENS_2016_UNIDA$edad < 25, na.rm = TRUE)
table(ENS_2016_UNIDA$edad<25, ENS_2016_UNIDA$ca_activo)

#SE CREA NUEVA BASE
ENS2016conexc <- ENS_2016_UNIDA %>%
  filter(edad >= 25, ca_activo != 1 | is.na(ca_activo))
summary (ENS2016conexc$edad)
dim(ENS_2016_UNIDA)
dim(ENS2016conexc)

#CHEQUEO NUEVA BASE
table(ENS2016conexc$ca_activo)
table(ENS2016conexc$muerte_cancer)

write_rds(ENS2016conexc, "input/data-procesada/data-analisis/ens2016_analisis.rds")
