# 0. Cargar librerías
source("procesamiento/00_setup.R", encoding = "UTF-8")

# 1.Leer bases de egresos
ens2003_egr <- read_sav("input/data-raw/ENS2003/ens2003_egr.sav")
ens2009_egr <- read_sav("input/data-raw/ENS2009/ens2009_egr.sav")
ens2016_egr <- read_sav("input/data-raw/ENS2016/ens2016_egr.sav")

# 2. Leer bases de defunciones (versión armonizada)
ENS_DEF2003_ARMONIZADA <- read_rds("input/data-procesada/armonizadas/ens2003_def_armonizada.rds")
ENS_DEF2009_ARMONIZADA <- read_rds("input/data-procesada/armonizadas/ens2009_def_armonizada.rds")
ENS_DEF2016_ARMONIZADA <- read_rds("input/data-procesada/armonizadas/ens2016_def_armonizada.rds")

# 3. Unir bases de datos de defunciones y de egresos hospitalarios
ens2003_egr <- ens2003_egr %>%
  rename(DIAG1_EGR = DIAG1)

ens2003_egr <- ens2003_egr %>%
  rename(DIAG2_EGR = DIAG2)

ens2003_egr <- ens2003_egr %>%
  rename(ID = folio)

# ver si tengo los mismos ID en ambas bases de datos
setequal(ENS_DEF2003_ARMONIZADA$ID, ens2003_egr$ID)
setequal(ENS_DEF2003_ARMONIZADA$ID, egresos_final$ID)

class(ens2003_egr$FECHA_EGR)

#primero quiero ver si hay id repetidos con diag1 de cancer

# 1. Definimos la condición de interés para no repetirla
# Usamos DIAG1_EGR como indicas para el filtro de texto
condicion_cancer <- quote(grepl("^(C|D3|D4)", DIAG1_EGR) & DIAG1_EGR != "D469")

# --- OBJETIVO 1 ---
# IDs repetidos, con diagnósticos distintos, donde AL MENOS UNO es del grupo C/D
ids_con_al_menos_un_cancer <- ens2003_egr %>%
  group_by(ID) %>%
  # Filtro: Más de un diagnóstico único en DIAG1_EGR Y al menos uno cumple la condición
  filter(n_distinct(DIAG1_EGR) > 1 & any(!!condicion_cancer)) %>%
  ungroup()

# --- OBJETIVO 2 ---
# IDs repetidos, con diagnósticos distintos, donde NINGUNO es del grupo C/D
ids_repetidos_sin_nada_de_cancer <- ens2003_egr %>%
  group_by(ID) %>%
  # Filtro: Más de un diagnóstico único en DIAG1_EGR Y ninguno cumple la condición
  filter(n_distinct(DIAG1_EGR) > 1 & all(!eval(condicion_cancer))) %>%
  ungroup()

# Conteo de personas únicas
length(unique(ids_con_al_menos_un_cancer$ID))
length(unique(ids_repetidos_sin_nada_de_cancer$ID))
print(ids_con_al_menos_un_cancer, n = 50)
print(ids_repetidos_sin_nada_de_cancer, n = 50)



#############################################################################################
egresos_final <- ens2003_egr %>%
  # 1. Asegurar que los datos estén en orden cronológico
  arrange(ID, FECHA_EGR) %>% 
  
  # 2. Crear una marca temporal de cáncer
  mutate(es_cancer = if_else(grepl("^(C|D3|D4)", DIAG1_EGR) & DIAG1_EGR != "D469", 1, 0)) %>%
  
  # 3. Re-ordenar por ID, luego por prioridad (cáncer primero) y fecha
  arrange(ID, desc(es_cancer), FECHA_EGR) %>%
  
  # 4. Agrupar y seleccionar solo la primera fila de este nuevo orden
  group_by(ID) %>%
  slice(1) %>%
  
  # 5. Limpieza final
  select(-es_cancer) %>% # Quitamos la marca temporal
  ungroup()

# Verifica si hay algún ID duplicado
any(duplicated(egresos_final$ID))

# Filtramos por los IDs específicos y seleccionamos la columna DIAG1_EGR
egresos_final %>% 
  filter(ID %in% c("0087", "0182", "0138")) %>% 
  select(ID, DIAG1_EGR)


# Unimos ambas bases por la columna "ID"
ens_2003_unida <- left_join(ENS_DEF2003_ARMONIZADA, egresos_final, by = "ID")

# Verificamos que sigamos teniendo 3619 filas
nrow(ens_2003_unida)

#Guardamos base unida de egresos y defunciones, ENS 2003
write_rds(ens_2003_unida, "input/data-procesada/defunciones-bind-egresos/ens2003_bind.rds")
#######################################################################

### REPETIR CON ENS 2009 Y 2016, TODO PREVIO EXCLUSIONES 




