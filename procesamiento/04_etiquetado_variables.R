#1. Cargar setup (librerías)
source("procesamiento/00_setup.R", encoding = "UTF-8")

#2. Cargar bases para análisis
ens2003_final <- read_rds("input/data-procesada/data-analisis/ens2003_final.rds")
ens2009_final <- read_rds("input/data-procesada/data-analisis/ens2009_final.rds")
ens2016_final <- read_rds("input/data-procesada/data-analisis/ens2016_final.rds")

#3. Etiquetar variables ENS 2003
#sexo
class(ens2003_final$sexo)
table(ens2003_final$sexo)
ens2003_final <- ens2003_final %>%
  mutate(sexo = factor(sexo, 
                       levels = c("1", "2"), 
                       labels = c("Male", "Female")))
label(ens2003_final$sexo) <- "Sex"
table(ens2003_final$sexo)

# variable edad
class(ens2003_final$edad)
label(ens2003_final$edad) <- "age"
table(ens2003_final$edad)

#NEDU
class(ens2003_final$NEDU)
table(ens2003_final$NEDU)
ens2003_final <- ens2003_final %>%
  mutate(NEDU = factor(NEDU, 
                       levels = c("1", "2", "3"), 
                       labels = c("<8 years", "8-12 years", ">12 years")))
label(ens2003_final$NEDU) <- "Education"
table(ens2003_final$NEDU)

#zona
class(ens2003_final$zona)
table(ens2003_final$zona)
ens2003_final <- ens2003_final %>%
  mutate(zona = factor(zona, 
                       levels = c("1", "2"), 
                       labels = c("Urban", "Rural")))
label(ens2003_final$zona) <- "Zone"
table(ens2003_final$zona)

#fuma 1= si, ocasionalmente o uno o mas cigarros al dia 0= no fuma o  ha dejado de fumar
class(ens2003_final$fuma)
ens2003_final$fuma<-as.factor(ens2003_final$fuma)
class(ens2003_final$fuma)
table(ens2003_final$fuma)
ens2003_final <- ens2003_final %>%
  mutate(fuma = factor(fuma, 
                       levels = c("0", "1"), 
                       labels = c("Non-smoker", "Smoker")))
label(ens2003_final$fuma) <- "Smoking status"
table(ens2003_final$fuma)

#estado nutricional
class(ens2003_final$estado_nutricional)
ens2003_final$estado_nutricional<-as.factor(ens2003_final$estado_nutricional)
class(ens2003_final$estado_nutricional)
table(ens2003_final$estado_nutricional)
ens2003_final <- ens2003_final %>%
  mutate(estado_nutricional = factor(estado_nutricional, 
                       levels = c("Enflaquecido", "Normal","Sobrepeso","Obesidad"), 
                       labels = c("Underweight", "Normal","Overweight","Obesity")))
label(ens2003_final$estado_nutricional) <- "nutritional status"
table(ens2003_final$estado_nutricional)

#actividad fisica (a17)
class(ens2003_final$a17)
ens2003_final$a17<-as.factor(ens2003_final$a17)
class(ens2003_final$a17)
table(ens2003_final$a17)
ens2003_final <- ens2003_final %>%
  mutate(a17 = factor(a17, 
                       levels = c("1", "2", "3", "4"), 
                       labels = c("Yes, 3 or more times per week", "Yes, 1 to 2 times per week", "Less than 4 times per month", "No exercise in the past month")))
label(ens2003_final$a17) <- "Physical activity"
table(ens2003_final$a17)

#muerte cancer
class(ens2003_final$muerte_cancer)
ens2003_final$muerte_cancer<-as.factor(ens2003_final$muerte_cancer)
class(ens2003_final$muerte_cancer)
table(ens2003_final$muerte_cancer)
ens2003_final <- ens2003_final %>%
  mutate(muerte_cancer = factor(muerte_cancer, 
                       levels = c("0", "1"), 
                       labels = c("No cancer death", "Cancer death")))
label(ens2003_final$muerte_cancer) <- "Cancer mortality"
table(ens2003_final$muerte_cancer)

#Fallecimiento
class(ens2003_final$fallecidos)
ens2003_final$fallecidos<-as.factor(ens2003_final$fallecidos)
class(ens2003_final$fallecidos)
table(ens2003_final$fallecidos)
ens2003_final <- ens2003_final %>%
  mutate(fallecidos = factor(fallecidos, 
                       levels = c("1", "2","3"), 
                       labels = c("Cancer death", "Other causes of death", "Alive")))
label(ens2003_final$fallecidos) <- "Vital status"
table(ens2003_final$fallecidos)

#Depresión
class(ens2003_final$Depresion_1_AP)
ens2003_final$Depresion_1_AP<-as.factor(ens2003_final$Depresion_1_AP)
class(ens2003_final$Depresion_1_AP)
table(ens2003_final$Depresion_1_AP)
ens2003_final <- ens2003_final %>%
  mutate(Depresion_1_AP = factor(Depresion_1_AP, 
                                 levels = c("0", "1"), 
                                 labels = c("No symptoms", "With symptoms")))
label(ens2003_final$Depresion_1_AP) <- "Depression"
table(ens2003_final$Depresion_1_AP)

######################################################################################################
#4. Etiquetar variables ENS 2009
#sexo
class(ens2009_final$sexo)
table(ens2009_final$sexo)
ens2009_final <- ens2009_final %>%
  mutate(sexo = factor(sexo, 
                       levels = c("1", "2"), 
                       labels = c("Male", "Female")))
label(ens2009_final$sexo) <- "Sex"
table(ens2009_final$sexo)

#EDAD
class(ens2009_final$edad)
label(ens2009_final$edad) <- "Age (years)"
table(ens2009_final$edad)

#edad categorica
class(ens2009_final$Edad_Codificada)
ens2009_final$Edad_Codificada<-as.factor(ens2009_final$Edad_Codificada)
table(ens2009_final$Edad_Codificada)
ens2009_final <- ens2009_final %>%
  mutate(Edad_Codificada = factor(Edad_Codificada, 
                       levels = c("2", "3", "4"), 
                       labels = c("25-44 years", "45-64 years", "≥65 years")))
label(ens2009_final$Edad_Codificada) <- "Age group"
table(ens2009_final$Edad_Codificada)

#NEDU
class(ens2009_final$NEDU)
table(ens2009_final$NEDU)
ens2009_final <- ens2009_final %>%
  mutate(NEDU = factor(NEDU, 
                       levels = c("1", "2", "3"), 
                       labels = c("<8 years", "8-12 years", ">12 years")))
label(ens2009_final$NEDU) <- "Education"
table(ens2009_final$NEDU)

#zona
class(ens2009_final$zona)
table(ens2009_final$zona)
ens2009_final <- ens2009_final %>%
  mutate(zona = factor(zona, 
                       levels = c("1", "2"), 
                       labels = c("Urban", "Rural")))
label(ens2009_final$zona) <- "Zone"
table(ens2009_final$zona)

#fuma 1= si, ocasionalmente o uno o mas cigarros al dia 0= no fuma o  ha dejado de fumar
class(ens2009_final$fuma)
ens2009_final$fuma<-as.factor(ens2009_final$fuma)
class(ens2009_final$fuma)
table(ens2009_final$fuma)
ens2009_final <- ens2009_final %>%
  mutate(fuma = factor(fuma, 
                       levels = c("0", "1"), 
                       labels = c("Non-smoker", "Smoker")))
label(ens2009_final$fuma) <- "Smoking status"
table(ens2009_final$fuma)

#estado nutricional
class(ens2009_final$estado_nutricional)
ens2009_final$estado_nutricional<-as.factor(ens2009_final$estado_nutricional)
class(ens2009_final$estado_nutricional)
table(ens2009_final$estado_nutricional)
ens2009_final <- ens2009_final %>%
  mutate(estado_nutricional = factor(estado_nutricional, 
                       levels = c("Enflaquecido", "Normal","Sobrepeso","Obesidad"), 
                       labels = c("Underweight", "Normal","Overweight","Obesity")))
label(ens2009_final$estado_nutricional) <- "Nutritional status"
table(ens2009_final$estado_nutricional)

#actividad fisica (a17)
class(ens2009_final$a17)
ens2009_final$a17<-as.factor(ens2009_final$a17)
class(ens2009_final$a17)
table(ens2009_final$a17)
ens2009_final <- ens2009_final %>%
  mutate(a17 = factor(a17, 
                       levels = c("1", "2", "3", "4"), 
                       labels = c("Yes, 3 or more times per week", "Yes, 1 to 2 times per week", "Less than 4 times per month", "No exercise in the past month")))
label(ens2009_final$a17) <- "Physical activity"
table(ens2009_final$a17)

#actividad fisica (GPAQ)
class(ens2009_final$GPAQ)
ens2009_final$GPAQ <- as.factor(ens2009_final$GPAQ)
table(ens2009_final$GPAQ)
class(ens2009_final$GPAQ)
ens2009_final <- ens2009_final %>%
  mutate(GPAQ = factor(GPAQ, 
                       levels = c("1", "2", "3"), 
                       labels = c("Low", "Moderate", "High")))
label(ens2009_final$GPAQ) <- "Physical activity level (GPAQ)"
table(ens2009_final$GPAQ)

#AUDIT
class(ens2009_final$AUDIT_RIESGOSO)
ens2009_final$AUDIT_RIESGOSO <- as.factor(ens2009_final$AUDIT_RIESGOSO)
table(ens2009_final$AUDIT_RIESGOSO)
class(ens2009_final$AUDIT_RIESGOSO)
ens2009_final <- ens2009_final %>%
  mutate(AUDIT_RIESGOSO = factor(AUDIT_RIESGOSO, 
                       levels = c("0", "1"), 
                       labels = c("No", "Yes")))
label(ens2009_final$AUDIT_RIESGOSO) <- "Risky alcohol consumption"
table(ens2009_final$AUDIT_RIESGOSO)

#muerte cancer
class(ens2009_final$muerte_cancer)
ens2009_final$muerte_cancer<-as.factor(ens2009_final$muerte_cancer)
class(ens2009_final$muerte_cancer)
table(ens2009_final$muerte_cancer)
ens2009_final <- ens2009_final %>%
  mutate(muerte_cancer = factor(muerte_cancer, 
                       levels = c("0", "1"), 
                       labels = c("No cancer death", "Cancer death")))
label(ens2009_final$muerte_cancer) <- "Cancer mortality"
table(ens2009_final$muerte_cancer)

#Fallecimiento
class(ens2009_final$fallecidos)
ens2009_final$fallecidos<-as.factor(ens2009_final$fallecidos)
class(ens2009_final$fallecidos)
table(ens2009_final$fallecidos)
ens2009_final <- ens2009_final %>%
  mutate(fallecidos = factor(fallecidos, 
                       levels = c("1", "2","3"), 
                       labels = c("Cancer death", "Other causes of death", "Alive")))
label(ens2009_final$fallecidos) <- "Vital status"
table(ens2009_final$fallecidos)

#Depresión
class(ens2009_final$Depresion_1_AP)
ens2009_final$Depresion_1_AP<-as.factor(ens2009_final$Depresion_1_AP)
class(ens2009_final$Depresion_1_AP)
table(ens2009_final$Depresion_1_AP)
ens2009_final <- ens2009_final %>%
  mutate(Depresion_1_AP = factor(Depresion_1_AP, 
                                 levels = c("0", "1"), 
                                 labels = c("No symptoms", "With symptoms")))
label(ens2009_final$Depresion_1_AP) <- "Depression"
table(ens2009_final$Depresion_1_AP)

#Antecedente familiar
class(ens2009_final$af_cancer_binaria)
ens2009_final$af_cancer_binaria <-as.factor(ens2009_final$af_cancer_binaria)
class(ens2009_final$af_cancer_binaria)
table(ens2009_final$af_cancer_binaria)
ens2009_final <- ens2009_final %>%
  mutate(af_cancer_binaria = factor(af_cancer_binaria, 
                                 levels = c("0", "1"), 
                                 labels = c("No", "Yes")))
label(ens2009_final$af_cancer_binaria) <- "FH of cancer"
table(ens2009_final$af_cancer_binaria)


##################################################################################################
#5. Etiquetar variabales ens 2016
#sexo
class(ens2016_final$sexo)
table(ens2016_final$sexo)
ens2016_final <- ens2016_final %>%
  mutate(sexo = factor(sexo, 
                       levels = c("1", "2"), 
                       labels = c("Male", "Female")))
label(ens2016_final$sexo) <- "Sex"
table(ens2016_final$sexo)

#variable edad
class(ens2016_final$edad)
label(ens2016_final$edad) <- "Age (years)"
table(ens2016_final$edad)

#variable edad categorica
class(ens2016_final$Edad_Codificada)
ens2016_final$Edad_Codificada<-as.factor(ens2016_final$Edad_Codificada)
table(ens2016_final$Edad_Codificada)
ens2016_final <- ens2016_final %>%
  mutate(Edad_Codificada = factor(Edad_Codificada, 
                       levels = c("2", "3", "4"), 
                       labels = c("25-44 years", "45-64 years", "≥65 years")))
label(ens2016_final$Edad_Codificada) <- "Age group"
table(ens2016_final$Edad_Codificada)


#NEDU
class(ens2016_final$NEDU)
table(ens2016_final$NEDU)
ens2016_final <- ens2016_final %>%
  mutate(NEDU = factor(NEDU, 
                       levels = c("1", "2", "3"), 
                       labels = c("<8 years", "8-12 years", ">12 years")))
label(ens2016_final$NEDU) <- "Education"
table(ens2016_final$NEDU)

#zona
class(ens2016_final$zona)
table(ens2016_final$zona)
ens2016_final <- ens2016_final %>%
  mutate(zona = factor(zona, 
                       levels = c("1", "2"), 
                       labels = c("Urban", "Rural")))
label(ens2016_final$zona) <- "Zone"
table(ens2016_final$zona)

#fuma 1= si, ocasionalmente o uno o mas cigarros al dia 0= no fuma o  ha dejado de fumar
class(ens2016_final$fuma)
ens2016_final$fuma<-as.factor(ens2016_final$fuma)
class(ens2016_final$fuma)
table(ens2016_final$fuma)
ens2016_final <- ens2016_final %>%
  mutate(fuma = factor(fuma, 
                       levels = c("0", "1"), 
                       labels = c("Non-smoker", "Smoker")))
label(ens2016_final$fuma) <- "Smoking status"
table(ens2016_final$fuma)

#estado nutricional
class(ens2016_final$estado_nutricional)
ens2016_final$estado_nutricional<-as.factor(ens2016_final$estado_nutricional)
class(ens2016_final$estado_nutricional)
table(ens2016_final$estado_nutricional)
ens2016_final <- ens2016_final %>%
  mutate(estado_nutricional = factor(estado_nutricional, 
                       levels = c("Enflaquecido", "Normal","Sobrepeso","Obesidad"), 
                       labels = c("Underweight", "Normal","Overweight","Obesity")))
label(ens2016_final$estado_nutricional) <- "Nutritional status"
table(ens2016_final$estado_nutricional)

#actividad fisica (a17)
class(ens2016_final$a17)
ens2016_final$a17<-as.factor(ens2016_final$a17)
class(ens2016_final$a17)
table(ens2016_final$a17)
ens2016_final <- ens2016_final %>%
  mutate(a17 = factor(a17, 
                       levels = c("1", "2", "3", "4"), 
                       labels = c("Yes, 3 or more times per week", "Yes, 1 to 2 times per week", "Less than 4 times per month", "No exercise in the past month")))
label(ens2016_final$a17) <- "Physical activity"
table(ens2016_final$a17)

#actividad fisica (GPAQ)
class(ens2016_final$GPAQ)
ens2016_final$GPAQ <- as.factor(ens2016_final$GPAQ)
table(ens2016_final$GPAQ)
class(ens2016_final$GPAQ)
ens2016_final <- ens2016_final %>%
  mutate(GPAQ = factor(GPAQ, 
                       levels = c("1", "2", "3"), 
                       labels = c("Low", "Moderate", "High")))
label(ens2016_final$GPAQ) <- "Physical activity level (GPAQ)"
table(ens2016_final$GPAQ)

#AUDIT
class(ens2016_final$AUDIT_RIESGOSO)
ens2016_final$AUDIT_RIESGOSO <- as.factor(ens2016_final$AUDIT_RIESGOSO)
table(ens2016_final$AUDIT_RIESGOSO)
class(ens2016_final$AUDIT_RIESGOSO)
ens2016_final <- ens2016_final %>%
  mutate(AUDIT_RIESGOSO = factor(AUDIT_RIESGOSO, 
                       levels = c("0", "1"), 
                       labels = c("No", "Yes")))
label(ens2016_final$AUDIT_RIESGOSO) <- "Risky alcohol consumption"
table(ens2016_final$AUDIT_RIESGOSO)

#muerte cancer
class(ens2016_final$muerte_cancer)
ens2016_final$muerte_cancer<-as.factor(ens2016_final$muerte_cancer)
class(ens2016_final$muerte_cancer)
table(ens2016_final$muerte_cancer)
ens2016_final <- ens2016_final %>%
  mutate(muerte_cancer = factor(muerte_cancer, 
                       levels = c("0", "1"), 
                       labels = c("No cancer death", "Cancer death")))
label(ens2016_final$muerte_cancer) <- "Cancer mortality"
table(ens2016_final$muerte_cancer)

#Fallecimiento 
# @JosefaPC me sale error en esta variable! La dejé fuera por mientras (coni)
#class(ens2016_final$fallecidos)
#ens2016_final$fallecidos<-as.factor(ens2016_final$fallecidos)
#class(ens2016_final$fallecidos)
#table(ens2016_final$fallecidos)
#ens2016_final <- ens2016_final %>%
#  mutate(fallecidos = factor(fallecidos, 
#                       levels = c("1", "2","3"), 
#                       labels = c("Cancer death", "Other causes of death", "Alive")))
#label(ens2016_final$fallecidos) <- "Vital status"
#table(ens2016_final$fallecidos)

#Depresión
class(ens2016_final$Depresion_1_AP)
ens2016_final$Depresion_1_AP<-as.factor(ens2016_final$Depresion_1_AP)
class(ens2016_final$Depresion_1_AP)
table(ens2016_final$Depresion_1_AP)
ens2016_final <- ens2016_final %>%
  mutate(Depresion_1_AP = factor(Depresion_1_AP, 
                                 levels = c("0", "1"), 
                                 labels = c("No symptoms", "With symptoms")))
label(ens2016_final$Depresion_1_AP) <- "Depression"
table(ens2016_final$Depresion_1_AP)

#Antecedente familiar
class(ens2016_final$af_cancer_binaria)
ens2016_final$af_cancer_binaria <-as.factor(ens2016_final$af_cancer_binaria)
class(ens2016_final$af_cancer_binaria)
table(ens2016_final$af_cancer_binaria)
ens2016_final <- ens2016_final %>%
  mutate(af_cancer_binaria = factor(af_cancer_binaria, 
                                 levels = c("0", "1"), 
                                 labels = c("No", "Yes")))
label(ens2016_final$af_cancer_binaria) <- "FH of cancer"
table(ens2016_final$af_cancer_binaria)


#falta etiquetar ICT @Caolos