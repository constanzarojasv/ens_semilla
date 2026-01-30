# 0.Cargar librerías
source("procesamiento/00_setup.R", encoding = "UTF-8")

ENS_2003_UNIDA <- read_rds("input/data-procesada/defunciones-bind-egresos/ens2003_bind.rds")
#ENS_2003_UNIDA <- read_rds("input/data-procesada/defunciones-bind-egresos/ens2003_bind.rds")
#ENS_2003_UNIDA <- read_rds("input/data-procesada/defunciones-bind-egresos/ens2003_bind.rds")
#MODIFIQUEN CON LA 2009 Y 2016

# 1. Excluir a menores de 25 años en base unida para el análisis (ENS 2003)
##Número de personas <25 años 
sum(ENS_2003_UNIDA$edad < 25, na.rm = TRUE)

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
ENS_DEF2009$ca_activo  #CAMBIAR NOMBRE DE BASES 2009 Y 2017 SEGUN BIND
ENS_DEF2017$ca_activo


#HAY UN CASO EN ENS 2009 QUE FALLECE ANTES DE LA FECHA DE ENCUESTA, SE ELIMINA
ENS_DEF2009 %>%
  filter(dias_transcurridos < 0) %>%
  select(ID, FECHA_DEF, fecha_encuesta, dias_transcurridos) 
class(ENS_DEF2009$dias_transcurridos)

#opcion para borrar la fila
ENS_DEF2009 <- ENS_DEF2009 %>%
  filter(ID != "3228")
sum(ENS_DEF2009$dias_transcurridos < 0, na.rm = TRUE)

#EXCLUSIONES ENS 2009: MENORES DE 25 Y CANCER ACTIVO (ca_activo == 1)
# Número de personas <25 años 2009
sum(ENS_DEF2009$edad < 25, na.rm = TRUE)
table(ENS_DEF2009$edad<25, ENS_DEF2009$ca_activo)

#SE CREA NUEVA BASE
ENS2009conexc <- ENS_DEF2009 %>%
  filter(edad >= 25, ca_activo != 1 | is.na(ca_activo))
summary (ENS2009conexc$edad)

#CHEQUEO NUEVA BASE
table(ENS2009conexc$ca_activo)
table(ENS2009conexc$muerte_cancer)

##EXCLUSIONES EN ENS 2017: MISMOS CRITERIOS QUE ENS 2009
ENS2016conexc <- ENS_DEF2017 %>%
  filter(edad >= 25, ca_activo != 1 | is.na(ca_activo))
summary (ENS2016conexc$edad)
table(ENS2016conexc$ca_activo)
table(ENS2016conexc$muerte_cancer)