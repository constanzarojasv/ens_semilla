# 0. Cargar setup (librerías)
source("procesamiento/00_setup.R", encoding = "UTF-8")

#LEER BASES DEFUNCIONES
ENS_DEF2003 <- read_sav("input/data-raw/ENS2003/ENS2003_DEF_2022.sav")
ENS_DEF2009 <- read_sav("input/data-raw/ENS2009/ENS2009_DEF_2022.sav")
ENS_DEF2017 <- read_sav("input/data-raw/ENS2016/ENS2016_DEF_2022.sav")

#AGREGAR VARIABLE ENS CON EL AÑO EN QUE SE HIZO LA ENS
ENS_DEF2003$ENS <- 2003
ENS_DEF2009$ENS <- 2010
ENS_DEF2017$ENS <- 2017
table(ENS_DEF2003$ENS)
table(ENS_DEF2009$ENS)
table(ENS_DEF2017$ENS)

#VARIABLES DE "DEPRESIÓN"
ENS_DEF2003$p52
ENS_DEF2009$sd1
ENS_DEF2017$sd1_F1
ENS_DEF2017$Sospecha_Depresion

#ARMONIZAR ID
ENS_DEF2003$folio
ENS_DEF2017$IdEncuesta
names(ENS_DEF2003)[names(ENS_DEF2003) == "folio"] <- "ID"
names(ENS_DEF2017)[names(ENS_DEF2017) == "IdEncuesta"] <- "ID"

#ARMONIZAR VARIABLE EDAD
class(ENS_DEF2003$edad)
class(ENS_DEF2009$EDAD)
names(ENS_DEF2009)[names(ENS_DEF2009) == "EDAD"] <- "edad"
class(ENS_DEF2009$edad)
class(ENS_DEF2017$Edad)
names(ENS_DEF2017)[names(ENS_DEF2017) == "Edad"] <- "edad"
class(ENS_DEF2017$edad)

#EVALUAR VARIABLE SEXO
class(ENS_DEF2003$sexo)
class(ENS_DEF2009$SEXO)
class(ENS_DEF2017$Sexo)

#ARMONIZAR VARIABLE SEXO
ENS_DEF2003$sexo<-as.factor(ENS_DEF2003$sexo)
ENS_DEF2009$SEXO<-as.factor(ENS_DEF2009$SEXO)
ENS_DEF2017$Sexo<-as.factor(ENS_DEF2017$Sexo)

names(ENS_DEF2009)[names(ENS_DEF2009) == "SEXO"] <- "sexo"
class(ENS_DEF2009$sexo)

names(ENS_DEF2017)[names(ENS_DEF2017) == "Sexo"] <- "sexo"
class(ENS_DEF2017$sexo)

#CHEQUEAR VARIABLE SEXO CONSTRUIDA
ENS_DEF2003$sexo
ENS_DEF2009$sexo
ENS_DEF2017$sexo

#ARMONIZAR VARIABLE NEDU
ENS_DEF2003$VAR00010
ENS_DEF2003$VAR00035
names(ENS_DEF2003)[names(ENS_DEF2003) == "VAR00035"] <- "NEDU"
ENS_DEF2003$NEDU
class(ENS_DEF2003$NEDU)

ENS_DEF2009$NEDU
class(ENS_DEF2009$NEDU)

ENS_DEF2017$NEDU1_MINSAL_1
names(ENS_DEF2017)[names(ENS_DEF2017) == "NEDU1_MINSAL_1"] <- "NEDU"
ENS_DEF2017$NEDU

ENS_DEF2003$NEDU<-as.factor(ENS_DEF2003$NEDU)
ENS_DEF2009$NEDU<-as.factor(ENS_DEF2009$NEDU)
ENS_DEF2017$NEDU<-as.factor(ENS_DEF2017$NEDU)

#CHEQUEAR VARIABLE NEDU
table(ENS_DEF2003$NEDU)
table(ENS_DEF2009$NEDU)
table(ENS_DEF2017$NEDU)

ENS_DEF2003$NEDU
ENS_DEF2009$NEDU
ENS_DEF2017$NEDU

#EVALUAR VARIABLE ZONA (1=URBANO, 2 = RURAL)
ENS_DEF2003$zona
ENS_DEF2009$ZONA
ENS_DEF2017$Zona

class(ENS_DEF2003$zona)
class(ENS_DEF2009$ZONA)
class(ENS_DEF2017$Zona)

ENS_DEF2003$zona<-as.factor(ENS_DEF2003$zona)
ENS_DEF2009$ZONA<-as.factor(ENS_DEF2009$ZONA)
ENS_DEF2017$Zona<-as.factor(ENS_DEF2017$Zona)

names(ENS_DEF2009)[names(ENS_DEF2009) == "ZONA"] <- "zona"
class(ENS_DEF2009$zona)

names(ENS_DEF2017)[names(ENS_DEF2017) == "Zona"] <- "zona"
class(ENS_DEF2017$zona)

# ARMONIZAR VARIABLE PESO
ENS_DEF2003$peso
ENS_DEF2009$peso<-ENS_DEF2009$m5p1
ENS_DEF2017$peso<-ENS_DEF2017$m4p1 ## Falta: -7777, -8888, -9999 dejarlos como missings

#LIMPIAR MISSING VALUES DE ENS 2003 
ENS_DEF2003$peso ##aqui 999.9 es NA y son 19 personas
summary(ENS_DEF2003$peso)
sum(ENS_DEF2003$peso == 999.9, na.rm = TRUE)
ENS_DEF2003$peso[ENS_DEF2003$peso == 999.9] <- NA

# LIMPIAR MISSING VALUES DE ENS 2009
ENS_DEF2009$peso ##aqui 777, 888 y 999 son NA y son 44 personas
summary(ENS_DEF2009$peso)
sum(ENS_DEF2009$peso %in% c(777, 888, 999), na.rm = TRUE)
ENS_DEF2009$peso[ENS_DEF2009$peso %in% c(777, 888, 999)] <- NA

# LIMPIAR MISSING VALUES DE ENS 2017
ENS_DEF2017$peso ##aqui -9999,-8888 y -7777 son NA y hay 0 personas
summary(ENS_DEF2017$peso)
sum(ENS_DEF2017$peso %in% c(-7777, -8888, -9999), na.rm = TRUE)
sum(ENS_DEF2017$peso %in% c(7777, 8888, 9999), na.rm = TRUE)

# ARMONIZAR VARIABLE TALLA
ENS_DEF2003$talla #aqui 999.9 es missing
ENS_DEF2009$talla<-ENS_DEF2009$m5p2
ENS_DEF2017$talla<-ENS_DEF2017$m4p2  

#LIMPIAR MISSING ENS 2003 
ENS_DEF2003$talla #aqui 999.9 es missing son 21 personas
sum(ENS_DEF2003$talla == 999.9, na.rm = TRUE)
ENS_DEF2003$talla[ENS_DEF2003$talla == 999.9] <- NA
summary(ENS_DEF2003$talla)

# LIMPIAR MISSING ENS 2009
ENS_DEF2009$talla #aqui 777, 888 y 999 es missing y son 64 personas
sum(ENS_DEF2009$talla %in% c(777, 888, 999), na.rm = TRUE)
ENS_DEF2009$talla[ENS_DEF2009$talla %in% c(777, 888, 999)] <- NA
summary(ENS_DEF2009$talla)

# LIMPIAR MISSING ENS 2017
ENS_DEF2017$talla #aqui -7777, -8888 y -9999 es missing y son 0 personas
summary(ENS_DEF2017$talla)
sum(ENS_DEF2017$talla %in% c(-7777, -8888, -9999), na.rm = TRUE)
sum(ENS_DEF2017$talla %in% c(7777, 8888, 9999), na.rm = TRUE)

# CREAR VARIABLE IMC 
ENS_DEF2003$imc<-ENS_DEF2003$peso/((ENS_DEF2003$talla/100)^2)
ENS_DEF2009$imc<-ENS_DEF2009$peso/((ENS_DEF2009$talla/100)^2)
ENS_DEF2017$imc<-ENS_DEF2017$peso/((ENS_DEF2017$talla/100)^2) 
summary(ENS_DEF2003$imc)
summary(ENS_DEF2009$imc)
summary(ENS_DEF2017$imc)

#CREAR VARIABLE ESTADO NUTRICIONAL 
ENS_DEF2003 <- ENS_DEF2003 %>%
  mutate(estado_nutricional = case_when(
    imc < 18.5 ~ "Enflaquecido",
    imc >= 18.5 & imc < 25 ~ "Normal",
    imc >= 25 & imc < 30 ~ "Sobrepeso",
    imc >= 30 ~ "Obesidad",
    TRUE ~ NA_character_   # para valores perdidos o fuera de rango
  ))
table(ENS_DEF2003$estado_nutricional)

ENS_DEF2009 <- ENS_DEF2009 %>%
  mutate(estado_nutricional = case_when(
    imc < 18.5 ~ "Enflaquecido",
    imc >= 18.5 & imc < 25 ~ "Normal",
    imc >= 25 & imc < 30 ~ "Sobrepeso",
    imc >= 30 ~ "Obesidad",
    TRUE ~ NA_character_   # para valores perdidos o fuera de rango
  ))
table(ENS_DEF2009$estado_nutricional)

ENS_DEF2017 <- ENS_DEF2017 %>%
  mutate(estado_nutricional = case_when(
    imc < 18.5 ~ "Enflaquecido",
    imc >= 18.5 & imc < 25 ~ "Normal",
    imc >= 25 & imc < 30 ~ "Sobrepeso",
    imc >= 30 ~ "Obesidad",
    TRUE ~ NA_character_   # para valores perdidos o fuera de rango
  ))
table(ENS_DEF2017$estado_nutricional)

# ARMONIZAR VARIABLE CIRCUNFERENCIA DE CINTURA
ENS_DEF2003$cintura<-ENS_DEF2003$cintura
ENS_DEF2009$cintura<-ENS_DEF2009$m5p3
ENS_DEF2017$cintura<-ENS_DEF2017$m4p3 ## Falta: -7777, -8888, -9999 dejarlos como missings

#LIMPIAR MISSING VALUES

ENS_DEF2003$cintura #aqui 999.9 es missing son 21 personas
sum(ENS_DEF2003$cintura == 999.9, na.rm = TRUE)
ENS_DEF2003$cintura[ENS_DEF2003$cintura == 999.9] <- NA
summary(ENS_DEF2003$cintura)

ENS_DEF2009 <- ENS_DEF2009 %>%
  mutate(cintura = zap_labels(cintura),                      # Elimina etiquetas (como 777 = NS/NR)
         cintura = ifelse(cintura %in% c(777, 888, 999), NA, cintura))  # Reemplaza esos códigos por NA
summary(ENS_DEF2009$cintura)

ENS_DEF2017 <- ENS_DEF2017 %>%
  mutate(cintura = zap_labels(cintura),  # Elimina etiquetas como -7777 = "No sabe"
         cintura = ifelse(cintura %in% c(-7777, -8888, -9999), NA, cintura))  # Reemplaza esos valores por NA

summary(ENS_DEF2017$cintura)

## Calcular variable índice cintura talla
ENS_DEF2003$ict<-ENS_DEF2003$cintura / ENS_DEF2003$talla
ENS_DEF2009$ict<-ENS_DEF2009$cintura / ENS_DEF2009$talla
ENS_DEF2017$ict<-ENS_DEF2017$cintura / ENS_DEF2017$talla

summary(ENS_DEF2009$ict)
summary(ENS_DEF2017$ict)


###creo "Edad_codificada" en ens 2003 y 2009
ENS_DEF2003$Edad_Codificada <- cut(ENS_DEF2003$edad,
                                   breaks = c(15, 24, 44, 64, Inf),
                                   labels = FALSE,
                                   right = TRUE,
                                   include.lowest = TRUE)

ENS_DEF2003$Edad_Codificada

ENS_DEF2009$Edad_Codificada <- cut(ENS_DEF2009$edad,
                               breaks = c(15, 24, 44, 64, Inf),
                               labels = FALSE,
                               right = TRUE,
                               include.lowest = TRUE)

ENS_DEF2009$Edad_Codificada

##armonizo fechas de encuestas al estilo date en la ENS 2009 
ENS_DEF2009 <- ENS_DEF2009 %>%
  mutate(
    fecha_encuesta = as.Date(
      sprintf("%04d-%02d-%02d", fenta, fentm, fentd)))


ENS_DEF2009$fecha_encuesta
class(ENS_DEF2009$fecha_encuesta)

#la ENS 2003 no tiene fecha de encuesta en la base de datos por lo que se colocará el primer dia de encuesta, 
#las entrevistas empezaron el mes de mayo de 2003
ENS_DEF2003 <- ENS_DEF2003 %>%
  mutate(fecha_encuesta = as.Date("2003-05-01"))

###Armonizo fecha encuesta en la ENS2017
ENS_DEF2017 <- ENS_DEF2017 %>%
  mutate(fecha_encuesta = as.Date(FechaInicioF1))
class(ENS_DEF2017$fecha_encuesta)

###TABACO###1:si//0:no//
class(ENS_DEF2003$p111)
ENS_DEF2003$p111<-as.numeric(ENS_DEF2003$p111)
class(ENS_DEF2003$p112)
ENS_DEF2003$p112<-as.numeric(ENS_DEF2003$p112)

ENS_DEF2003 <- ENS_DEF2003 %>%
  mutate(fuma = case_when(p112 %in% c(1, 2) ~ 1,             
      p112 == 3                              ~ 0,             
      is.na(p112) & p111 %in% c(2,3)         ~ 0,             
      TRUE                                   ~ NA_real_ 
      ))
tabyl(ENS_DEF2003$fuma)

ENS_DEF2009$fuma <- ifelse(ENS_DEF2009$ta2 %in% c(1, 2), 1,
                       ifelse(ENS_DEF2009$ta2 %in% c(3, 4), 0, NA))
tabyl(ENS_DEF2009$fuma)


ENS_DEF2017$fuma <- ifelse(ENS_DEF2017$ta3 %in% c(1, 2), 1,
                       ifelse(ENS_DEF2017$ta3 %in% c(3, 4), 0, NA))
tabyl(ENS_DEF2017$fuma)

#ACTIVIDAD (NO GPAQ)
ENS_DEF2003$p133
ENS_DEF2003$a17<-ENS_DEF2003$p133
ENS_DEF2003$a17[ENS_DEF2003$a17==9]<-NA
ENS_DEF2009$a17
ENS_DEF2009$a17[ENS_DEF2009$a17==9]<-NA
ENS_DEF2017$a17

# ACTIVIDAD FISICA (GPAQ)
ENS_DEF2017$GPAQ
ENS_DEF2009$GPAQ

#ARMONIZAR ANTECEDENTE FAMLIARES DE CANCER (2009 Y 2017)
ENS_DEF2017$af1d #Cáncer de mama
ENS_DEF2017$af1e #Cáncer de vesícula o vía biliar
ENS_DEF2017$af1f #Cáncer gástrico
ENS_DEF2017$af1g #Cáncer de tiroides
ENS_DEF2017$af2d #Cáncer de colon

ENS_DEF2009$af1d #Cáncer de mama
ENS_DEF2009$af1e #Cáncer de vesícula o vía biliar
ENS_DEF2009$af1f #Cáncer gástrico
ENS_DEF2009$af1g #Cáncer de colon
ENS_DEF2009$af1h #Cáncer de tiroides

#ARMONIZACION AF MAMA
ENS_DEF2017$afmama <- ENS_DEF2017$af1d
table(ENS_DEF2017$afmama, ENS_DEF2017$af1d)
ENS_DEF2009$afmama <- ENS_DEF2009$af1d
table(ENS_DEF2009$afmama, ENS_DEF2009$af1d)
ENS_DEF2017$afmama
ENS_DEF2009$afmama
ENS_DEF2017$afmama_conver <- recode(as.numeric(ENS_DEF2017$afmama), 
                                    "1=1; 2=2; else=NA")  # Sí = 1 NO = 2

table(ENS_DEF2017$afmama, ENS_DEF2017$afmama_conver)

ENS_DEF2009$afmama_conver <- recode(as.numeric(ENS_DEF2009$afmama), 
                                    "1=1; 2=2; else=NA")  # Sí = 1 NO = 2

table(ENS_DEF2009$afmama, ENS_DEF2009$afmama_conver)

#ARMONIZACION DE AF VESICULA
ENS_DEF2017$afvesicula <- ENS_DEF2017$af1e
table(ENS_DEF2017$afvesicula, ENS_DEF2017$af1e)
ENS_DEF2009$afvesicula <- ENS_DEF2009$af1e
table(ENS_DEF2009$afvesicula, ENS_DEF2009$af1e)

ENS_DEF2017$afvesicula
ENS_DEF2017$afvesicula_conver <- recode(as.numeric(ENS_DEF2017$afvesicula), 
                                    "1=1; 2=2; else=NA")  # Sí = 1 NO = 2

table(ENS_DEF2017$afvesicula, ENS_DEF2017$afvesicula_conver)


ENS_DEF2009$afvesicula
ENS_DEF2009$afvesicula_conver <- recode(as.numeric(ENS_DEF2009$afvesicula), 
                                    "1=1; 2=2; else=NA")  # Sí = 1 NO = 2

table(ENS_DEF2009$afvesicula, ENS_DEF2009$afvesicula_conver)

#ARMONIZACION AF GASTRICO
ENS_DEF2017$afgastrico <- ENS_DEF2017$af1f
table(ENS_DEF2017$afgastrico, ENS_DEF2017$af1f)
ENS_DEF2009$afgastrico <- ENS_DEF2009$af1f
table(ENS_DEF2009$afgastrico, ENS_DEF2009$af1f)

ENS_DEF2017$afgastrico
ENS_DEF2017$afgastrico_conver <- recode(as.numeric(ENS_DEF2017$afgastrico), 
                                        "1=1; 2=2; else=NA")  # Sí = 1 NO = 2

table(ENS_DEF2017$afgastrico, ENS_DEF2017$afgastrico_conver)


ENS_DEF2009$afgastrico
ENS_DEF2009$afgastrico_conver <- recode(as.numeric(ENS_DEF2009$afgastrico), 
                                        "1=1; 2=2; else=NA")  # Sí = 1 NO = 2

table(ENS_DEF2009$afgastrico, ENS_DEF2009$afgastrico_conver)

#ARMONIZACION AF TIROIDES
ENS_DEF2017$aftiroides <- ENS_DEF2017$af1g
table(ENS_DEF2017$aftiroides, ENS_DEF2017$af1g)
ENS_DEF2009$aftiroides <- ENS_DEF2009$af1h
table(ENS_DEF2009$aftiroides, ENS_DEF2009$af1h)

ENS_DEF2017$aftiroides
ENS_DEF2017$aftiroides_conver <- recode(as.numeric(ENS_DEF2017$aftiroides), 
                                        "1=1; 2=2; else=NA")  # Sí = 1 NO = 2

table(ENS_DEF2017$aftiroides, ENS_DEF2017$aftiroides_conver)


ENS_DEF2009$aftiroides
ENS_DEF2009$aftiroides_conver <- recode(as.numeric(ENS_DEF2009$aftiroides), 
                                        "1=1; 2=2; else=NA")  # Sí = 1 NO = 2

table(ENS_DEF2009$aftiroides, ENS_DEF2009$aftiroides_conver)

#ARMONIZACION AF COLON
ENS_DEF2017$afcolon <- ENS_DEF2017$af2d
table(ENS_DEF2017$afcolon, ENS_DEF2017$af2d) #Ojo que en 2017 se categoriza distinto (preguntan por edad)
ENS_DEF2009$afcolon <- ENS_DEF2009$af1g 
table(ENS_DEF2009$afcolon, ENS_DEF2009$af1g)
ENS_DEF2017$afcolon
ENS_DEF2009$afcolon


ENS_DEF2017$afcolon
ENS_DEF2017$afcolon_conver <- recode(as.numeric(ENS_DEF2017$afcolon), 
                                        "1=1; 2=1; 3=2; else=NA")  # Sí = 1 NO = 2

table(ENS_DEF2017$afcolon, ENS_DEF2017$afcolon_conver)


ENS_DEF2009$afcolon
ENS_DEF2009$afcolon_conver <- recode(as.numeric(ENS_DEF2009$afcolon), 
                                        "1=1; 2=2; else=NA")  # Sí = 1 NO = 2

table(ENS_DEF2009$afcolon, ENS_DEF2009$afcolon_conver)

#CREAR VARIABLE EXPOSICION AF FAMILIAR
#ENS 2009
# RECODIFICO TODOS LOS ANTECEDENTES FAMILIARES A BINARIO 0 Y 1
ENS_DEF2009$afmama_recod <- ifelse(ENS_DEF2009$afmama_conver == 1, 1,
                             ifelse(ENS_DEF2009$afmama_conver == 2, 0, NA))
ENS_DEF2009$afvesicula_recod <- ifelse(ENS_DEF2009$afvesicula_conver == 1, 1,
                                 ifelse(ENS_DEF2009$afvesicula_conver == 2, 0, NA))
ENS_DEF2009$afgastrico_recod <- ifelse(ENS_DEF2009$afgastrico_conver == 1, 1,
                                 ifelse(ENS_DEF2009$afgastrico_conver == 2, 0, NA))
ENS_DEF2009$aftiroides_recod <- ifelse(ENS_DEF2009$aftiroides_conver == 1, 1,
                                 ifelse(ENS_DEF2009$aftiroides_conver == 2, 0, NA))
ENS_DEF2009$afcolon_recod <- ifelse(ENS_DEF2009$afcolon_conver == 1, 1,
                              ifelse(ENS_DEF2009$afcolon_conver == 2, 0, NA))

table(ENS_DEF2009$afmama_recod, ENS_DEF2009$afmama_conver)

#CREO VARIABLE BINARIA (AL MENOS 1 ANTECEDENTE FAMLIIAR DE CANCER)
ENS_DEF2009$af_total_cancer <- apply(
  ENS_DEF2009[, c("afmama_recod", 
            "afvesicula_recod", 
            "afgastrico_recod", 
            "aftiroides_recod", 
            "afcolon_recod")],
  1,
  function(x) {
    if (all(is.na(x))) {
      return(NA)
    } else {
      return(sum(x, na.rm = TRUE))
    }
  }
)



ENS_DEF2009$af_cancer_binaria <- ifelse(ENS_DEF2009$af_total_cancer > 0, 1,
                                  ifelse(ENS_DEF2009$af_total_cancer == 0, 0, NA))

tabyl(ENS_DEF2009$af_cancer_binaria)

#RECODIFICO ENS 2016 
ENS_DEF2017$afmama_recod <- ifelse(ENS_DEF2017$afmama_conver == 1, 1,
                                   ifelse(ENS_DEF2017$afmama_conver == 2, 0, NA))
ENS_DEF2017$afvesicula_recod <- ifelse(ENS_DEF2017$afvesicula_conver == 1, 1,
                                       ifelse(ENS_DEF2017$afvesicula_conver == 2, 0, NA))
ENS_DEF2017$afgastrico_recod <- ifelse(ENS_DEF2017$afgastrico_conver == 1, 1,
                                       ifelse(ENS_DEF2017$afgastrico_conver == 2, 0, NA))
ENS_DEF2017$aftiroides_recod <- ifelse(ENS_DEF2017$aftiroides_conver == 1, 1,
                                       ifelse(ENS_DEF2017$aftiroides_conver == 2, 0, NA))
ENS_DEF2017$afcolon_recod <- ifelse(ENS_DEF2017$afcolon_conver == 1, 1,
                                    ifelse(ENS_DEF2017$afcolon_conver == 2, 0, NA))

table(ENS_DEF2017$afmama_recod, ENS_DEF2017$afmama_conver)

#CONSTRUYO VARIABLE BINARIA AF CANCER ENS 2016
ENS_DEF2017$af_total_cancer <- apply(
  ENS_DEF2017[, c("afmama_recod", 
                  "afvesicula_recod", 
                  "afgastrico_recod", 
                  "aftiroides_recod", 
                  "afcolon_recod")],
  1,
  function(x) {
    if (all(is.na(x))) {
      return(NA)
    } else {
      return(sum(x, na.rm = TRUE))
    }
  }
)

ENS_DEF2017$af_cancer_binaria <- ifelse(ENS_DEF2017$af_total_cancer > 0, 1,
                                        ifelse(ENS_DEF2017$af_total_cancer == 0, 0, NA))

tabyl(ENS_DEF2017$af_cancer_binaria)

#ARMONIZAR AUDIT 
#ENS 2017
vars_1 <- c("m7p9", "m7p10a", "m7p11a", "m7p11b", "m7p11c",
            "m7p12", "m7p13", "m7p14", "m7p15", "m7p16")

for (var in vars_1) {
  ENS_DEF2017[[paste0(var, "_conver")]] <- recode(as.numeric(ENS_DEF2017[[var]]), 
                                                  "1=0; 2=1; 3=2; 4=3; 5=4; else=NA")
}

ENS_DEF2017$m7p17_conver <- recode(as.numeric(ENS_DEF2017$m7p17), "1=0; 2=2; 3=4; else=NA")
ENS_DEF2017$m7p18_conver <- recode(as.numeric(ENS_DEF2017$m7p18), "1=0; 2=2; 3=4; else=NA")

# AUDIT_a: usa m7p11a_conver
ENS_DEF2017$AUDIT_pa <- ifelse(
  !is.na(ENS_DEF2017$m7p9_conver),
  rowSums(ENS_DEF2017[, c("m7p9_conver", "m7p10a_conver", "m7p11a_conver",
                          "m7p12_conver", "m7p13_conver", "m7p14_conver",
                          "m7p15_conver", "m7p16_conver", "m7p17_conver",
                          "m7p18_conver")], na.rm = TRUE),
  NA
)

# AUDIT_b: usa m7p11b_conver
ENS_DEF2017$AUDIT_pb <- ifelse(
  !is.na(ENS_DEF2017$m7p9_conver),
  rowSums(ENS_DEF2017[, c("m7p9_conver", "m7p10a_conver", "m7p11b_conver",
                          "m7p12_conver", "m7p13_conver", "m7p14_conver",
                          "m7p15_conver", "m7p16_conver", "m7p17_conver",
                          "m7p18_conver")], na.rm = TRUE),
  NA
)

# AUDIT_c: usa m7p11c_conver
ENS_DEF2017$AUDIT_pc <- ifelse(
  !is.na(ENS_DEF2017$m7p9_conver),
  rowSums(ENS_DEF2017[, c("m7p9_conver", "m7p10a_conver", "m7p11c_conver",
                          "m7p12_conver", "m7p13_conver", "m7p14_conver",
                          "m7p15_conver", "m7p16_conver", "m7p17_conver",
                          "m7p18_conver")], na.rm = TRUE),
  NA
)

#AUDIT 2009
for (var in vars_1) {
  ENS_DEF2009[[paste0(var, "_conver")]] <- recode(as.numeric(ENS_DEF2009[[var]]), 
                                                  "1=0; 2=1; 3=2; 4=3; 5=4; else=NA")
}

ENS_DEF2009$m7p17_conver <- recode(as.numeric(ENS_DEF2009$m7p17), "1=0; 2=2; 3=4; else=NA")
ENS_DEF2009$m7p18_conver <- recode(as.numeric(ENS_DEF2009$m7p18), "1=0; 2=2; 3=4; else=NA") 

# AUDIT_a: usa m7p11a_conver
ENS_DEF2009$AUDIT_pa <- ifelse(
  !is.na(ENS_DEF2009$m7p9_conver),
  rowSums(ENS_DEF2009[, c("m7p9_conver", "m7p10a_conver", "m7p11a_conver",
                          "m7p12_conver", "m7p13_conver", "m7p14_conver",
                          "m7p15_conver", "m7p16_conver", "m7p17_conver",
                          "m7p18_conver")], na.rm = TRUE),
  NA
)

# AUDIT_b: usa m7p11b_conver
ENS_DEF2009$AUDIT_pb <- ifelse(
  !is.na(ENS_DEF2009$m7p9_conver),
  rowSums(ENS_DEF2009[, c("m7p9_conver", "m7p10a_conver", "m7p11b_conver",
                          "m7p12_conver", "m7p13_conver", "m7p14_conver",
                          "m7p15_conver", "m7p16_conver", "m7p17_conver",
                          "m7p18_conver")], na.rm = TRUE),
  NA
)

# AUDIT_c: usa m7p11c_conver
ENS_DEF2009$AUDIT_pc <- ifelse(
  !is.na(ENS_DEF2009$m7p9_conver),
  rowSums(ENS_DEF2009[, c("m7p9_conver", "m7p10a_conver", "m7p11c_conver",
                          "m7p12_conver", "m7p13_conver", "m7p14_conver",
                          "m7p15_conver", "m7p16_conver", "m7p17_conver",
                          "m7p18_conver")], na.rm = TRUE),
  NA
) 

#ya están creados AUDIT_pa y AUDIT_pb en ambos dataframes
ENS_DEF2017$sexo
#Ahora crear categorías considerando Sexo
ENS_DEF2017$AUDIT_RIESGOSO <- NA

# Hombres (Sexo == 1)
ENS_DEF2017$AUDIT_RIESGOSO[ENS_DEF2017$sexo == 1 & ENS_DEF2017$AUDIT_pb >= 8] <- 1
ENS_DEF2017$AUDIT_RIESGOSO[ENS_DEF2017$sexo == 1 & ENS_DEF2017$AUDIT_pb < 8]  <- 0

# Mujeres (Sexo == 2)
ENS_DEF2017$AUDIT_RIESGOSO[ENS_DEF2017$sexo == 2 & ENS_DEF2017$AUDIT_pa >= 8] <- 1
ENS_DEF2017$AUDIT_RIESGOSO[ENS_DEF2017$sexo == 2 & ENS_DEF2017$AUDIT_pa < 8]  <- 0

table(ENS_DEF2017$AUDIT_RIESGOSO) # 1==Sí 2==No

#Ahora lo mismo pero en ENS 2009
ENS_DEF2009$sexo
ENS_DEF2009$AUDIT_RIESGOSO <- NA

# Hombres (Sexo == 1)
ENS_DEF2009$AUDIT_RIESGOSO[ENS_DEF2009$sexo == 1 & ENS_DEF2009$AUDIT_pb >= 8] <- 1
ENS_DEF2009$AUDIT_RIESGOSO[ENS_DEF2009$sexo == 1 & ENS_DEF2009$AUDIT_pb < 8]  <- 0

# Mujeres (Sexo == 2)
ENS_DEF2009$AUDIT_RIESGOSO[ENS_DEF2009$sexo == 2 & ENS_DEF2009$AUDIT_pa >= 8] <- 1
ENS_DEF2009$AUDIT_RIESGOSO[ENS_DEF2009$sexo == 2 & ENS_DEF2009$AUDIT_pa < 8]  <- 0

tabyl(ENS_DEF2009$AUDIT_RIESGOSO) # 1==Sí 2==No
tabyl(ENS_DEF2017$AUDIT_RIESGOSO)

#ARMONIZAR ANTECEDENTE PERSONAL DE CANCER
#ENS2009
ENS_DEF2009$ca_activo[!is.na(ENS_DEF2009$m6p14)|!is.na(ENS_DEF2009$m10p2A)]<-0
ENS_DEF2009$ca_activo[ENS_DEF2009$m6p14==1]<-1
ENS_DEF2009$ca_activo[ENS_DEF2009$m10p2A==1]<-1
tabyl(ENS_DEF2009$m6p14)
tabyl(ENS_DEF2009$m10p2A)
tabyl(ENS_DEF2009$ca_activo)

# m6p14. ¿Alguna vez un doctor o médico le ha dicho que tiene o que padece de Cáncer de mama?	
# m10p2A. ¿Alguna vez un doctor o médico le ha dicho que tiene o que padece de Cáncer de tiroides?


#ENS2017
ENS_DEF2017$m9p2A
ENS_DEF2017$m9p3A
ENS_DEF2017$m9p4A
ENS_DEF2017$m9p5A
ENS_DEF2017$m9p6A
ENS_DEF2017$m9p7A
ENS_DEF2017$m9p8A
ENS_DEF2017$ca_activo<-NA
ENS_DEF2017$ca_activo[!is.na(ENS_DEF2017$m9p2A)|!is.na(ENS_DEF2017$m9p3A)|!is.na(ENS_DEF2017$m9p4A)|!is.na(ENS_DEF2017$m9p5A)|
                     !is.na(ENS_DEF2017$m9p6A)|!is.na(ENS_DEF2017$m9p7A)|!is.na(ENS_DEF2017$m9p8A)]<-0
ENS_DEF2017$ca_activo[ENS_DEF2017$m9p2A==1]<-1
ENS_DEF2017$ca_activo[ENS_DEF2017$m9p3A==1]<-1
ENS_DEF2017$ca_activo[ENS_DEF2017$m9p4A==1]<-1
ENS_DEF2017$ca_activo[ENS_DEF2017$m9p5A==1]<-1
ENS_DEF2017$ca_activo[ENS_DEF2017$m9p6A==1]<-1
ENS_DEF2017$ca_activo[ENS_DEF2017$m9p7A==1]<-1
ENS_DEF2017$ca_activo[ENS_DEF2017$m9p8A==1]<-1
tabyl(ENS_DEF2017$ca_activo)

#ARMONIZAR CANCER
ENS_DEF2003$DIAG1
ENS_DEF2009$DIAG1
ENS_DEF2017$DIAG1

ENS_DEF2003$DIAG2
ENS_DEF2009$DIAG2
ENS_DEF2017$DIAG2

ENS_DEF2003$FECHA_DEF
ENS_DEF2009$FECHA_DEF
ENS_DEF2017$FECHA_DEF

ENS_DEF2003$REG_RES
ENS_DEF2009$REG_RES
ENS_DEF2017$REG_RES

ENS_DEF2003$DIA_DEF
ENS_DEF2009$DIA_DEF
ENS_DEF2017$DIA_DEF

ENS_DEF2003$MES_DEF
ENS_DEF2009$MES_DEF
ENS_DEF2017$MES_DEF

ENS_DEF2003$ANO_DEF
ENS_DEF2009$ANO_DEF
ENS_DEF2017$ANO_DEF

ENS_DEF2003$LUGAR_DEF
ENS_DEF2009$LUGAR_DEF
ENS_DEF2017$LUGAR_DEF

##en las tres encuestas la fecha de defuncion esta en formato "date"
class(ENS_DEF2003$FECHA_DEF)
class(ENS_DEF2009$FECHA_DEF)
class(ENS_DEF2017$FECHA_DEF)

#fecha de fin de seguimiento
FECHA_finSeg <- dmy("31-12-2022")

#dias transcurridos
ENS_DEF2003 <- ENS_DEF2003 %>%
  mutate(
    dias_transcurridos = as.numeric(
      if_else(
        is.na(FECHA_DEF),
        FECHA_finSeg,
        FECHA_DEF) - fecha_encuesta))

sum(ENS_DEF2003$dias_transcurridos < 0, na.rm = TRUE)


ENS_DEF2009 <- ENS_DEF2009 %>%
  mutate(
    dias_transcurridos = as.numeric(
      if_else(
        is.na(FECHA_DEF),
        FECHA_finSeg,
        FECHA_DEF) - fecha_encuesta))
sum(ENS_DEF2009$dias_transcurridos < 0, na.rm = TRUE)

ENS_DEF2017 <- ENS_DEF2017 %>%
  mutate(
    dias_transcurridos = as.numeric(
      if_else(
        is.na(FECHA_DEF),
        FECHA_finSeg,
        FECHA_DEF) - fecha_encuesta))
sum(ENS_DEF2017$dias_transcurridos < 0, na.rm = TRUE)


#Hay un caso que fallece antes de la encuesta, se va a eliminar pero después del bind

# Crear una nueva variable llamada muerte_cancer con restricciones ENS 2003. No hay muerte por diagnostico D0, D1 o D2 y excluir el D469 porque es sindrome mielodisplasico
sum(ENS_DEF2003$DIAG1 == "D469", na.rm = TRUE)#aqui hay 2
ENS_DEF2003$muerte_cancer <- ifelse(!is.na(ENS_DEF2003$DIAG1) & grepl("^(C|D3|D4)", ENS_DEF2003$DIAG1) & ENS_DEF2003$DIAG1 != "D469", 1, 0)
table(ENS_DEF2003$DIAG1, ENS_DEF2003$muerte_cancer)
sum(ENS_DEF2003$muerte_cancer)
#hay 250 muertes por cáncer

# Crear una nueva variable llamada muerte_cancer con restricciones ENS 2009. No hay muerte por diagnostico D0, D1 o D2 y excluir el D469 porque es sindrome mielodisplasico
sum(ENS_DEF2009$DIAG1 == "D469", na.rm = TRUE)#aqui hay 2
ENS_DEF2009$muerte_cancer <- ifelse(!is.na(ENS_DEF2009$DIAG1) & grepl("^(C|D3|D4)", ENS_DEF2009$DIAG1) & ENS_DEF2009$DIAG1 != "D469", 1, 0)
table(ENS_DEF2009$DIAG1, ENS_DEF2009$muerte_cancer)
sum(ENS_DEF2009$muerte_cancer)
#hay 178 muertes por cancer

# Crear una nueva variable llamada muerte_cancer con restricciones ENS 2016. No hay muerte por diagnostico D0, D1 o D2 y excluir el D469 porque es sindrome mielodisplasico
sum(ENS_DEF2017$DIAG1 == "D469", na.rm = TRUE)#aqui hay 1
ENS_DEF2017$muerte_cancer <- ifelse(!is.na(ENS_DEF2017$DIAG1) & grepl("^(C|D3|D4)", ENS_DEF2017$DIAG1) & ENS_DEF2017$DIAG1 != "D469", 1, 0)
table(ENS_DEF2017$DIAG1, ENS_DEF2017$muerte_cancer)
sum(ENS_DEF2017$muerte_cancer)
#hay 104 muertes por cáncer

##primero transformo los espacios vacios en NA
ENS_DEF2003$DIAG1[ENS_DEF2003$DIAG1 == ""] <- NA
ENS_DEF2009$DIAG1[ENS_DEF2009$DIAG1 == ""] <- NA
ENS_DEF2017$DIAG1[ENS_DEF2017$DIAG1 == ""] <- NA

####sumar los valores validos en DIAG1 de la ENS
sum(!is.na(ENS_DEF2003$DIAG1)) #aqui hay 965
sum(!is.na(ENS_DEF2009$DIAG1)) #aqui hay 742
sum(!is.na(ENS_DEF2017$DIAG1)) #aqui hay 442

#ahora creo una nueva variable de personas que fallecieron por otra causa 
ENS_DEF2003$muerte_otra_causa <- ifelse(!is.na(ENS_DEF2003$DIAG1) &  ( (!grepl("^(C|D3|D4)", ENS_DEF2003$DIAG1)) | ENS_DEF2003$DIAG1 =="D469" ),1, 0)
tabyl(ENS_DEF2003$muerte_otra_causa)
sum(ENS_DEF2003$muerte_otra_causa)
#aqui hay 715 personas

ENS_DEF2009$muerte_otra_causa <- ifelse(!is.na(ENS_DEF2009$DIAG1) &  ( (!grepl("^(C|D3|D4)", ENS_DEF2009$DIAG1)) | ENS_DEF2009$DIAG1 =="D469" ),1, 0)
tabyl(ENS_DEF2009$muerte_otra_causa)
sum(ENS_DEF2009$muerte_otra_causa)
#aqui hay 564 personas

ENS_DEF2017$muerte_otra_causa <- ifelse(!is.na(ENS_DEF2017$DIAG1) &  ( (!grepl("^(C|D3|D4)", ENS_DEF2017$DIAG1)) | ENS_DEF2017$DIAG1 =="D469" ),1, 0)
tabyl(ENS_DEF2017$muerte_otra_causa)
sum(ENS_DEF2017$muerte_otra_causa)
#aqui hay 338

#crear la variable muertes por cancer, muerte po otra causa y vivo
####no fallecidos
ENS_DEF2003 <- ENS_DEF2003 %>%
  mutate(no_fallecidos = case_when(
    muerte_cancer ==0 & muerte_otra_causa==0  ~ 1,
    muerte_cancer ==1 | muerte_otra_causa==1  ~ 0,
    TRUE ~ NA_real_       # para valores perdidos o fuera de rango
  ))
table(ENS_DEF2003$no_fallecidos, useNA = "always")
#aqui hay 965 fallecidos=250+715

ENS_DEF2009 <- ENS_DEF2009 %>%
  mutate(no_fallecidos = case_when(
    muerte_cancer ==0 & muerte_otra_causa==0  ~ 1,
    muerte_cancer ==1 | muerte_otra_causa==1  ~ 0,
    TRUE ~ NA_real_       # para valores perdidos o fuera de rango
  ))
table(ENS_DEF2009$no_fallecidos, useNA = "always")
#aqui hay 742 muertos=564+178

##FALLECIDOS
# ENS 2003
ENS_DEF2003 <- ENS_DEF2003 %>%
  mutate(fallecidos = case_when(
    muerte_cancer ==1 ~ 1,
    muerte_otra_causa ==1 ~ 2,
    no_fallecidos==1 ~ 3,
    TRUE ~ NA_real_       # para valores perdidos o fuera de rango
  ))
table(ENS_DEF2003$fallecidos, useNA = "always")

#ENS 2009
ENS_DEF2009 <- ENS_DEF2009 %>%
  mutate(fallecidos = case_when(
    muerte_cancer ==1 ~ 1,
    muerte_otra_causa ==1 ~ 2,
    no_fallecidos==1 ~ 3,
    TRUE ~ NA_real_       # para valores perdidos o fuera de rango
  ))
table(ENS_DEF2009$fallecidos, useNA = "always")

#CREAR VARIABLE EXPOSICION DEPRESION

#script armonizacion depresion esta pregunta no tiene NA
table(ENS_DEF2003$p52)
table(ENS_DEF2009$sd1)
table(ENS_DEF2017$sd1_F1)
ENS_DEF2003$Sd1_1<-ENS_DEF2003$p52
ENS_DEF2009$Sd1_1<-ENS_DEF2009$sd1
ENS_DEF2017$Sd1_1<-ENS_DEF2017$sd1_F1
table(ENS_DEF2003$Sd1_1, ENS_DEF2003$p52)
table(ENS_DEF2009$Sd1_1, ENS_DEF2009$sd1)
table(ENS_DEF2017$Sd1_1, ENS_DEF2017$sd1_F1)

ENS_DEF2003$Sd4_1<-ENS_DEF2003$p55
ENS_DEF2009$Sd4_1<-ENS_DEF2009$sd4
ENS_DEF2017$Sd4_1<-ENS_DEF2017$sd4_F1
table(ENS_DEF2003$Sd4_1, ENS_DEF2003$p55)
table(ENS_DEF2009$Sd4_1, ENS_DEF2009$sd4)
table(ENS_DEF2017$Sd4_1, ENS_DEF2017$sd4_F1)

ENS_DEF2003$Sd5_1<-ENS_DEF2003$p56
ENS_DEF2009$Sd5_1<-ENS_DEF2009$sd5
ENS_DEF2017$Sd5_1<-ENS_DEF2017$sd5_F1
table(ENS_DEF2003$Sd5_1, ENS_DEF2003$p56)
table(ENS_DEF2009$Sd5_1, ENS_DEF2009$sd5)
table(ENS_DEF2017$Sd5_1, ENS_DEF2017$sd5_F1)

ENS_DEF2003$Sd6_1<-ENS_DEF2003$p57
ENS_DEF2009$Sd6_1<-ENS_DEF2009$sd6
ENS_DEF2017$Sd6_1<-ENS_DEF2017$sd6_F1
table(ENS_DEF2003$Sd6_1, ENS_DEF2003$p57)
table(ENS_DEF2009$Sd6_1, ENS_DEF2009$sd6)
table(ENS_DEF2017$Sd6_1, ENS_DEF2017$sd6_F1)

ENS_DEF2003$p59[ENS_DEF2003$p59==9]<-NA
ENS_DEF2003$Sd8_1<-ENS_DEF2003$p59
ENS_DEF2009$Sd8_1<-ENS_DEF2009$sd8
ENS_DEF2017$Sd8_1<-ENS_DEF2017$sd8_F1
table(ENS_DEF2003$Sd8_1, ENS_DEF2003$p59)
table(ENS_DEF2009$Sd8_1, ENS_DEF2009$sd8)
table(ENS_DEF2017$Sd8_1, ENS_DEF2017$sd8_F1)

ENS_DEF2003$Sd9_1<-ENS_DEF2003$p60
ENS_DEF2009$Sd9_1<-ENS_DEF2009$sd9
ENS_DEF2017$Sd9_1<-ENS_DEF2017$sd9_F1
table(ENS_DEF2003$Sd9_1, ENS_DEF2003$p60)
table(ENS_DEF2009$Sd9_1, ENS_DEF2009$sd9)
table(ENS_DEF2017$Sd9_1, ENS_DEF2017$sd9_F1)

ENS_DEF2003$Sd10_1<-ENS_DEF2003$p61
ENS_DEF2009$Sd10_1<-ENS_DEF2009$sd10
ENS_DEF2017$Sd10_1<-ENS_DEF2017$sd10_F1
table(ENS_DEF2003$Sd10_1, ENS_DEF2003$p61)
table(ENS_DEF2009$Sd10_1, ENS_DEF2009$sd10)
table(ENS_DEF2017$Sd10_1, ENS_DEF2017$sd10_F1)

ENS_DEF2003$Sd11_1<-ENS_DEF2003$p62
ENS_DEF2009$Sd11_1<-ENS_DEF2009$sd11
ENS_DEF2017$Sd11_1<-ENS_DEF2017$sd11_F1
table(ENS_DEF2003$Sd11_1, ENS_DEF2003$p62)
table(ENS_DEF2009$Sd11_1, ENS_DEF2009$sd11)
table(ENS_DEF2017$Sd11_1, ENS_DEF2017$sd11_F1)

ENS_DEF2003$Sd12_1<-ENS_DEF2003$p63
ENS_DEF2009$Sd12_1<-ENS_DEF2009$sd12
ENS_DEF2017$Sd12_1<-ENS_DEF2017$sd12_F1
table(ENS_DEF2003$Sd12_1, ENS_DEF2003$p63)
table(ENS_DEF2009$Sd12_1, ENS_DEF2009$sd12)
table(ENS_DEF2017$Sd12_1, ENS_DEF2017$sd12_F1)

ENS_DEF2003$Sd13_1<-ENS_DEF2003$p64
ENS_DEF2009$Sd13_1<-ENS_DEF2009$sd13
ENS_DEF2017$Sd13_1<-ENS_DEF2017$sd13_F1
table(ENS_DEF2003$Sd13_1, ENS_DEF2003$p64)
table(ENS_DEF2009$Sd13_1, ENS_DEF2009$sd13)
table(ENS_DEF2017$Sd13_1, ENS_DEF2017$sd13_F1)

#esta pregunta no tiene NA
table(ENS_DEF2003$p71)
table(ENS_DEF2009$sd14)
table(ENS_DEF2017$sd14_F1)
ENS_DEF2003$Sd14_1<-ENS_DEF2003$p71
ENS_DEF2009$Sd14_1<-ENS_DEF2009$sd14
ENS_DEF2017$Sd14_1<-ENS_DEF2017$sd14_F1
table(ENS_DEF2003$Sd14_1, ENS_DEF2003$p71)
table(ENS_DEF2009$Sd14_1, ENS_DEF2009$sd14)
table(ENS_DEF2017$Sd14_1, ENS_DEF2017$sd14_F1)

ENS_DEF2003$Sd16_1<-ENS_DEF2003$p73
ENS_DEF2009$Sd16_1<-ENS_DEF2009$sd16
ENS_DEF2017$Sd16_1<-ENS_DEF2017$sd16_F1
table(ENS_DEF2003$Sd16_1, ENS_DEF2003$p73)
table(ENS_DEF2009$Sd16_1, ENS_DEF2009$sd16)
table(ENS_DEF2017$Sd16_1, ENS_DEF2017$sd16_F1)

ENS_DEF2003$Sd17_1<-ENS_DEF2003$p74
ENS_DEF2009$Sd17_1<-ENS_DEF2009$sd17
ENS_DEF2017$Sd17_1<-ENS_DEF2017$sd17_F1
table(ENS_DEF2003$Sd17_1, ENS_DEF2003$p74)
table(ENS_DEF2009$Sd17_1, ENS_DEF2009$sd17)
table(ENS_DEF2017$Sd17_1, ENS_DEF2017$sd17_F1)

ENS_DEF2003$p75[ENS_DEF2003$p75==9]<-NA
ENS_DEF2003$Sd18_1<-ENS_DEF2003$p75
ENS_DEF2009$Sd18_1<-ENS_DEF2009$sd18
ENS_DEF2017$Sd18_1<-ENS_DEF2017$sd18_F1
table(ENS_DEF2003$Sd18_1, ENS_DEF2003$p75)
table(ENS_DEF2009$Sd18_1, ENS_DEF2009$sd18)
table(ENS_DEF2017$Sd18_1, ENS_DEF2017$sd18_F1)

ENS_DEF2003$p77[ENS_DEF2003$p77==9]<-NA
ENS_DEF2003$Sd20_1<-ENS_DEF2003$p77
ENS_DEF2009$Sd20_1<-ENS_DEF2009$sd20
ENS_DEF2017$Sd20_1<-ENS_DEF2017$sd20_F1
table(ENS_DEF2003$Sd20_1, ENS_DEF2003$p77)
table(ENS_DEF2009$Sd20_1, ENS_DEF2009$sd20)
table(ENS_DEF2017$Sd20_1, ENS_DEF2017$sd20_F1)

ENS_DEF2003$p78[ENS_DEF2003$p78==9]<-NA
ENS_DEF2003$Sd21_1<-ENS_DEF2003$p78
ENS_DEF2009$Sd21_1<-ENS_DEF2009$sd21
ENS_DEF2017$Sd21_1<-ENS_DEF2017$sd21_F1
table(ENS_DEF2003$Sd21_1, ENS_DEF2003$p78)
table(ENS_DEF2009$Sd21_1, ENS_DEF2009$sd21)
table(ENS_DEF2017$Sd21_1, ENS_DEF2017$sd21_F1)

ENS_DEF2003$Sd22_1<-ENS_DEF2003$p79
ENS_DEF2009$Sd22_1<-ENS_DEF2009$sd22
ENS_DEF2017$Sd22_1<-ENS_DEF2017$sd22_F1
table(ENS_DEF2003$Sd22_1, ENS_DEF2003$p79)
table(ENS_DEF2009$Sd22_1, ENS_DEF2009$sd22)
table(ENS_DEF2017$Sd22_1, ENS_DEF2017$sd22_F1)

ENS_DEF2003$p80[ENS_DEF2003$p80==9]<-NA
ENS_DEF2003$Sd23_1<-ENS_DEF2003$p80
ENS_DEF2009$Sd23_1<-ENS_DEF2009$sd23
ENS_DEF2017$Sd23_1<-ENS_DEF2017$sd23_F1
table(ENS_DEF2003$Sd23_1, ENS_DEF2003$p80)
table(ENS_DEF2009$Sd23_1, ENS_DEF2009$sd23)
table(ENS_DEF2017$Sd23_1, ENS_DEF2017$sd23_F1)

ENS_DEF2003$p81[ENS_DEF2003$p81==9]<-NA
ENS_DEF2003$Sd24_1<-ENS_DEF2003$p81
ENS_DEF2009$Sd24_1<-ENS_DEF2009$sd24
ENS_DEF2017$Sd24_1<-ENS_DEF2017$sd24_F1
table(ENS_DEF2003$Sd24_1, ENS_DEF2003$p81)
table(ENS_DEF2009$Sd24_1, ENS_DEF2009$sd24)
table(ENS_DEF2017$Sd24_1, ENS_DEF2017$sd24_F1)

ENS_DEF2003$Sd25_1<-ENS_DEF2003$p82
ENS_DEF2009$Sd25_1<-ENS_DEF2009$sd25
ENS_DEF2017$Sd25_1<-ENS_DEF2017$sd25_F1
table(ENS_DEF2003$Sd25_1, ENS_DEF2003$p82)
table(ENS_DEF2009$Sd25_1, ENS_DEF2009$sd25)
table(ENS_DEF2017$Sd25_1, ENS_DEF2017$sd25_F1)

#armonizar la variabe sd18
ENS_DEF2003$Sd18_1<-ENS_DEF2003$p75
ENS_DEF2009$Sd18_1<-ENS_DEF2009$sd18
ENS_DEF2017$Sd18_1<-ENS_DEF2017$sd18_F1
table(ENS_DEF2003$Sd18_1, ENS_DEF2003$p75)
table(ENS_DEF2009$Sd18_1, ENS_DEF2009$sd18)
table(ENS_DEF2017$Sd18_1, ENS_DEF2017$sd18_F1)

#ya esta armnizado el ID en las tres ENS

#Selecciona solo las variables necesarias para unir las tres bases
base2003 <- ENS_DEF2003[, c("ID","Sd1_1", "Sd14_1", "Sd4_1", "Sd5_1", "Sd6_1", "Sd8_1", "Sd9_1", "Sd10_1", "Sd11_1", "Sd12_1", "Sd13_1", "Sd16_1", "Sd17_1", "Sd18_1", "Sd20_1", "Sd21_1", "Sd22_1", "Sd23_1", "Sd24_1", "Sd25_1", "ENS", "edad")]
base2009 <- ENS_DEF2009[, c("ID","Sd1_1", "Sd14_1", "Sd4_1", "Sd5_1", "Sd6_1", "Sd8_1", "Sd9_1", "Sd10_1", "Sd11_1", "Sd12_1", "Sd13_1", "Sd16_1", "Sd17_1", "Sd18_1", "Sd20_1", "Sd21_1", "Sd22_1", "Sd23_1", "Sd24_1", "Sd25_1", "ENS", "edad")]
base2017 <- ENS_DEF2017[, c("ID","Sd1_1", "Sd14_1", "Sd4_1", "Sd5_1", "Sd6_1", "Sd8_1", "Sd9_1", "Sd10_1", "Sd11_1", "Sd12_1", "Sd13_1", "Sd16_1", "Sd17_1", "Sd18_1", "Sd20_1", "Sd21_1", "Sd22_1", "Sd23_1", "Sd24_1", "Sd25_1", "ENS", "edad")]

#Leer base ENSPassi
ENSt2 <- read_rds("input/data-raw/ENSPassi/ENStt2.rds")

#unir las tres bases
ENSt2 <- rbind(base2003, base2009, base2017)
table(ENS_DEF2003$Sd25_1)
table(ENS_DEF2009$Sd25_1)
table(ENS_DEF2017$Sd25_1)
table(ENSt2$Sd25_1, ENSt2$ENS)

#recodifica los valores 2 por 0
ENSt2[c("Sd4_1", "Sd5_1", "Sd8_1", "Sd11_1", "Sd12_1", "Sd13_1", "Sd17_1", "Sd20_1", "Sd23_1", "Sd24_1", "Sd25_1")] <- lapply(ENSt2[c("Sd4_1", "Sd5_1", "Sd8_1", "Sd11_1", "Sd12_1", "Sd13_1", "Sd17_1", "Sd20_1", "Sd23_1", "Sd24_1", "Sd25_1")], function(x) ifelse(x == 2, 0, x))

#recodficia los 2 por 1 y los 3 por 0
ENSt2[c("Sd10_1", "Sd16_1", "Sd22_1")] <- lapply(ENSt2[c("Sd10_1", "Sd16_1", "Sd22_1")], function(x) ifelse(x == 2, 1, ifelse(x == 3, 0, x)))

table(ENS_DEF2003$Sd20_1)
table(ENS_DEF2009$Sd20_1)
table(ENS_DEF2017$Sd20_1)
table(ENSt2$Sd20_1, ENSt2$ENS)


#crear variable sd20_1aux que incluye las personas que respondieron, si respondieron 4 o 5 en la sd 18_1 entonces colocar el numero 0 en sd20_1
ENSt2$Sd20_1aux<-ENSt2$Sd20_1
ENSt2$Sd20_1aux[ENSt2$Sd18_1 == 4 |ENSt2$Sd18_1 == 5] <-0
table(ENSt2$Sd20_1)
table(ENSt2$Sd20_1aux)
table(ENSt2$ENS, ENSt2$Sd18_1)

ENSt2$Sd8_1aux<-ENSt2$Sd8_1
ENSt2$Sd8_1aux[ENSt2$Sd6_1 == 4 |ENSt2$Sd6_1 == 5] <-0
table(ENSt2$Sd8_1)
table(ENSt2$Sd8_1aux)
table(ENSt2$ENS, ENSt2$Sd6_1)

ENSt2$Sd10_1aux<-ENSt2$Sd10_1
ENSt2$Sd10_1aux[ENSt2$Sd9_1 == 2] <-0
table(ENSt2$Sd10_1)
table(ENSt2$Sd10_1aux)
table(ENSt2$ENS, ENSt2$Sd9_1)

ENSt2$Sd22_1aux<-ENSt2$Sd22_1
ENSt2$Sd22_1aux[ENSt2$Sd21_1 == 2] <-0
table(ENSt2$Sd22_1)
table(ENSt2$Sd22_1aux)
table(ENSt2$ENS, ENSt2$Sd21_1)

#veo la clase de las variables
class(ENSt2$Sd1_1)
class(ENSt2$Sd14_1)
class(ENSt2$Sd4_1)
class(ENSt2$Sd5_1)
class(ENSt2$Sd6_1)
class(ENSt2$Sd8_1)
class(ENSt2$Sd9_1)
class(ENSt2$Sd10_1)
class(ENSt2$Sd11_1)
class(ENSt2$Sd12_1)
class(ENSt2$Sd13_1)
class(ENSt2$Sd16_1)
class(ENSt2$Sd17_1)
class(ENSt2$Sd18_1)
class(ENSt2$Sd20_1)
class(ENSt2$Sd21_1)
class(ENSt2$Sd22_1)
class(ENSt2$Sd23_1)
class(ENSt2$Sd24_1)
class(ENSt2$Sd25_1)

#suma las respuestas que tienen que ver con estado de animo y anhedonia
ENSt2$Puntaje_animo <- rowSums(ENSt2[c("Sd4_1", "Sd5_1", "Sd8_1aux", "Sd10_1aux", "Sd11_1", "Sd12_1", "Sd13_1")], na.rm = TRUE)
ENSt2$Puntaje_denhedonia <- rowSums(ENSt2[c("Sd16_1", "Sd17_1", "Sd20_1aux", "Sd22_1aux", "Sd23_1", "Sd24_1", "Sd25_1")], na.rm = TRUE)

#si en la pregunta sd1_1 responde que no entonces el puntaje suma 0
ENSt2$Puntaje_animo[ENSt2$Sd1_1 == 2] <-0
ENSt2$Puntaje_denhedonia[ENSt2$Sd14_1 == 2] <-0

#suma puntaje de animo y anhedonia
ENSt2<-ENSt2 %>% 
  rowwise() %>% 
  mutate(puntaje_depresion = sum(Puntaje_animo, Puntaje_denhedonia, na.rm = TRUE))


table(ENSt2$puntaje_depresion) 
table(ENSt2$Sd1_1)
table(ENSt2$Sd14_1)

#define casos de depresion con un punto de corte
ENSt2$Depresion <- NA
ENSt2$Depresion[ENSt2$puntaje_depresion>=5] <- 1
ENSt2$Depresion[ENSt2$puntaje_depresion<5] <- 0

#cuantas preguntas hay faltantes por dimension
ENSt2$Num_missing_animo <- rowSums(is.na(ENSt2[,c("Sd4_1", "Sd5_1", "Sd8_1aux", "Sd10_1aux", "Sd11_1", "Sd12_1", "Sd13_1")]))
ENSt2$Num_missing_denhedonia <- rowSums(is.na(ENSt2[,c("Sd16_1", "Sd17_1", "Sd20_1aux", "Sd22_1aux", "Sd23_1", "Sd24_1", "Sd25_1")]))
ENSt2$Num_missing_animo
ENSt2$Num_missing_denhedonia

#crea variables alternativas de depresion
ENSt2$Depresion_1_AP <- ENSt2$Depresion
ENSt2$Depresion_1_AP2 <- ENSt2$Depresion

#crea una tabla
table(ENSt2$Depresion_1_AP,ENSt2$ENS, useNA = "always")

#crea un subset para inspeccionar de forma individual un individuo
checkdepre<-ENSt2 %>%
  subset(ENS=="2010") %>%
  select(ID,Sd1_1,Sd14_1,Depresion,Depresion_1_AP, contains("puntaje"), contains("Num_missing"), contains("Sd", ignore.case=T)) %>%
  subset(ID==0014) 

#transforma a NA los casos con puntajes bajos de animo, cierta cantidad de respuestas faltantes y diagnostico original de depresion ==0
ENSt2$Depresion_1_AP[ENSt2$Puntaje_animo==4 & ENSt2$Num_missing_animo>=1 & ENSt2$Num_missing_animo<=3 & ENSt2$Depresion_1_AP==0] <- NA
ENSt2$Depresion_1_AP[ENSt2$Puntaje_animo==3 & ENSt2$Num_missing_animo>=2 & ENSt2$Num_missing_animo<=4& ENSt2$Depresion_1_AP==0] <- NA
ENSt2$Depresion_1_AP[ENSt2$Puntaje_animo==2 & ENSt2$Num_missing_animo>=3 & ENSt2$Num_missing_animo<=5& ENSt2$Depresion_1_AP==0] <- NA
ENSt2$Depresion_1_AP[ENSt2$Puntaje_animo==1 & ENSt2$Num_missing_animo>=4 & ENSt2$Num_missing_animo<=6& ENSt2$Depresion_1_AP==0] <- NA
ENSt2$Depresion_1_AP[ENSt2$Puntaje_animo==0 & ENSt2$Num_missing_animo>=5 & ENSt2$Num_missing_animo<=7& ENSt2$Depresion_1_AP==0] <- NA

#tabla de revision 
table(ENSt2$Depresion_1_AP,ENSt2$ENS, useNA = "always")

#transforma a NA los casos con puntajes bajos de anhedonia, cierta cantidad de respuestas faltantes y diagnostico original de depresion ==0
ENSt2$Depresion_1_AP[ENSt2$Puntaje_denhedonia==4 & ENSt2$Num_missing_denhedonia>=1 & ENSt2$Num_missing_denhedonia<=3& ENSt2$Depresion_1_AP==0] <- NA
ENSt2$Depresion_1_AP[ENSt2$Puntaje_denhedonia==3 & ENSt2$Num_missing_denhedonia>=2 & ENSt2$Num_missing_denhedonia<=4& ENSt2$Depresion_1_AP==0] <- NA
ENSt2$Depresion_1_AP[ENSt2$Puntaje_denhedonia==2 & ENSt2$Num_missing_denhedonia>=3 & ENSt2$Num_missing_denhedonia<=5& ENSt2$Depresion_1_AP==0] <- NA
ENSt2$Depresion_1_AP[ENSt2$Puntaje_denhedonia==1 & ENSt2$Num_missing_denhedonia>=4 & ENSt2$Num_missing_denhedonia<=6& ENSt2$Depresion_1_AP==0] <- NA
ENSt2$Depresion_1_AP[ENSt2$Puntaje_denhedonia==0 & ENSt2$Num_missing_denhedonia>=5 & ENSt2$Num_missing_denhedonia<=7 & ENSt2$Depresion_1_AP==0] <- NA

#tabla de revision
table(ENSt2$Depresion_1_AP,ENSt2$ENS, useNA = "always")

#verificacion puntual
checkdepre<-ENSt2 %>%
  subset(ENS=="2010") %>%
  select(ID,Sd1_1,Sd14_1,Depresion,Depresion_1_AP, contains("puntaje"), contains("Num_missing"), contains("Sd", ignore.case=T)) %>%
  subset(ID==01067) 

#aplica 0 si las preguntas sd1_1 y sd14_1 son 2
ENSt2$Depresion_1_AP[is.na(ENSt2$Depresion_1_AP) & ENSt2$Sd1_1==2]<-0
ENSt2$Depresion_1_AP[is.na(ENSt2$Depresion_1_AP) & ENSt2$Sd14_1==2]<-0

#tabal de revision
table(ENSt2$Depresion_1_AP)
table(ENSt2$Depresion_1_AP,ENSt2$ENS, useNA = "always")

#Si ambas preguntas filtro están perdidas, se asume que no puede determinarse el estado de depresión
ENSt2$Depresion_1_AP[is.na(ENSt2$Sd1_1) & is.na(ENSt2$Sd14_1)]<-NA
ENSt2$Depresion_1_AP2[is.na(ENSt2$Sd1_1) & is.na(ENSt2$Sd14_1)]<-NA

#tabla de revision
table(ENSt2$Depresion_1_AP )
table(ENSt2$Depresion_1_AP,ENSt2$ENS, useNA = "always")

#verificacion 
checkdepre<-ENSt2 %>%
  subset(ENS=="2010") %>%
  select(ID,Sd1_1,Sd14_1,Depresion,Depresion_1_AP, contains("puntaje"), contains("Num_missing"), contains("Sd", ignore.case=F))

#tablas comparativas
table(checkdepre$Depresion,checkdepre$Depresion_1_AP, useNA = "always")
table(ENSt2$Depresion,ENSt2$Depresion_1_AP,ENSt2$ENS, useNA = "always")
table(ENSt2$Depresion,ENSt2$ENS)
table(ENSt2$Depresion_1_AP,ENSt2$ENS)

table(ENSt2$Depresion,ENSt2$ENS, useNA = "always")
table(ENSt2$Depresion_1_AP,ENSt2$ENS, useNA = "always")


#debo llevar la variable depresion_1_AP a cada una de las ENS
any(duplicated(ENSt2$ID))
sum(duplicated(ENSt2$ID))

#crear enst2_2003
ENSt2_2003 <- ENSt2 %>%
  filter(ENS == 2003) %>%
  select(ID, Depresion_1_AP)

#revisar si hay duplicados
dup_2003 <- ENSt2_2003 %>%
  count(ID) %>%
  filter(n > 1)
nrow(dup_2003)  # cuántos IDs duplicados hay
#no hay duplicados

faltantes_en_ENSt2 <- ENS_DEF2003 %>%
  anti_join(ENSt2_2003, by = "ID") %>%
  nrow()
faltantes_en_ENSt2  # cuántos IDs de ENS_DEF2003 no aparecen en ENSt2_2003
#no hay faltantes

ENS_DEF2003 <- ENS_DEF2003 %>%
  left_join(ENSt2_2003, by = "ID")

table(ENS_DEF2003$Depresion_1_AP, useNA = "always")

class(ENS_DEF2003$ID)
class(ENS_DEF2009$ID)
ENS_DEF2009$ID<-as.character(ENS_DEF2009$ID)
class(ENS_DEF2009$ID)
class(ENS_DEF2017$ID)
ENS_DEF2017$ID<-as.character(ENS_DEF2017$ID)
class(ENS_DEF2017$ID)

##ahora ens 2009
#crear enst2_2009
ENSt2_2009 <- ENSt2 %>%
  filter(ENS == 2010) %>%
  select(ID, Depresion_1_AP)

#revisar si hay duplicados
dup_2009 <- ENSt2_2009 %>%
  count(ID) %>%
  filter(n > 1)
nrow(dup_2009)  # cuántos IDs duplicados hay
#no hay duplicados

faltantes_en_ENSt2 <- ENS_DEF2009 %>%
  anti_join(ENSt2_2009, by = "ID") %>%
  nrow()
faltantes_en_ENSt2  # cuántos IDs de ENS_DEF2009 no aparecen en ENSt2_2009
#no hay faltantes

ENS_DEF2009 <- ENS_DEF2009 %>%
  left_join(ENSt2_2009, by = "ID")

table(ENS_DEF2009$Depresion_1_AP, useNA = "always")

##ahora ens 2017
#crear enst2_2017
ENSt2_2017 <- ENSt2 %>%
  filter(ENS == 2017) %>%
  select(ID, Depresion_1_AP)

#revisar si hay duplicados
dup_2017 <- ENSt2_2017 %>%
  count(ID) %>%
  filter(n > 1)
nrow(dup_2017)  # cuántos IDs duplicados hay
#no hay duplicados

faltantes_en_ENSt2 <- ENS_DEF2017 %>%
  anti_join(ENSt2_2017, by = "ID") %>%
  nrow()
faltantes_en_ENSt2  # cuántos IDs de ENS_DEF2017 no aparecen en ENSt2_2017
#no hay faltantes

ENS_DEF2017 <- ENS_DEF2017 %>%
  left_join(ENSt2_2017, by = "ID")

table(ENS_DEF2017$Depresion_1_AP, useNA = "always")

################################################################

############################################
#primero necesito agregar la variable fexp, strata y conglomerado de la base de datos de alvaro passi
##########################################
## 1. Leo el dataframe de álvaro (Cuidado nombre parecido)
df0_o<-readRDS("input/data-raw/ENSPassi/ENS_estrato.rds", refhook = NULL)
df0<-df0_o

#separo IDunicoENS
df0 <- df0 %>%
  separate(IDunicoENS, into = c("ENS", "ID"), sep = " ")

#me quedo solo con ens 2003
df2003 <- df0 %>%
  filter(ENS == 2003)

table(df2003$ID, ENS_DEF2003$ID)

setequal(df2003$ID, ENS_DEF2003$ID)
length(intersect(df2003$ID, ENS_DEF2003$ID))

ENS_DEF2003 <- ENS_DEF2003 %>%
  left_join(
    df0 %>% select(ID, Conglomerado_, fexp, strata_),  # variables a traer
    by = "ID"
  )


# 1. ENS 2003: Crear nueva variable y convertir a numérico
ENS_DEF2003 <- ENS_DEF2003 %>%
  mutate(FEXP_analisis = as.numeric(fact_af1))

# 2. ENS 2009: Crear nueva variable y convertir a numérico
ENS_DEF2009 <- ENS_DEF2009 %>%
  mutate(FEXP_analisis = as.numeric(FEXP2))

# 3. ENS 2017: Crear nueva variable y convertir a numérico
ENS_DEF2017 <- ENS_DEF2017 %>%
  mutate(FEXP_analisis = as.numeric(Fexp_F1F2p_Corr))



# 1. ENS 2003: Renombrar (crear nueva) y convertir
ENS_DEF2003 <- ENS_DEF2003 %>%
  mutate(estrato = as.numeric(strata_))

# 2. ENS 2009: Asegurar que sea numérico (ya tiene el nombre correcto)
ENS_DEF2009 <- ENS_DEF2009 %>%
  mutate(estrato = as.numeric(estrato))

# 3. ENS 2017: Renombrar (crear nueva) y convertir
ENS_DEF2017 <- ENS_DEF2017 %>%
  mutate(estrato = as.numeric(Estrato))


# 1. ENS 2003: Crear variable y convertir
ENS_DEF2003 <- ENS_DEF2003 %>%
  mutate(conglomerado = as.numeric(as.character(Conglomerado_)))

# 2. ENS 2009: Crear variable y convertir
ENS_DEF2009 <- ENS_DEF2009 %>%
  mutate(conglomerado = as.numeric(as.character(conglomerado1)))

# 3. ENS 2017: Crear variable y convertir
ENS_DEF2017 <- ENS_DEF2017 %>%
  mutate(conglomerado = as.numeric(as.character(Conglomerado)))

#Guardar bases
write_rds(ENS_DEF2003, "input/data-procesada/armonizadas/ens2003_def_armonizada.rds")
write_rds(ENS_DEF2009, "input/data-procesada/armonizadas/ens2009_def_armonizada.rds")
write_rds(ENS_DEF2017, "input/data-procesada/armonizadas/ens2016_def_armonizada.rds")
