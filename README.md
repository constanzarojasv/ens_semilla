# ens_semilla
Repositorio compartido para proyecto CECAN 

## Instrucciones de uso
1. Hacer pull
2. En pestaña Procesamiento, correr archivos en el siguiente orden:
 a. (Correrlo) 00_setup.R instala y carga paquetes. 
 b. (No correrlo) 01_armonización.R, 02_bind_egresos_defunciones.R y 03_exclusiones.R son lo que se ejecuta para crear la "data-procesada". No correrlas
 c. (Correrlo) 04_etiquetado_variables no es necesario correrlo porque se llama desde 05. 
 d. 05_analisis_descriptivo.R (tabla 1) y 06_calculo_de_tasas.R (cálculo de tasas) correrlos en ese orden. 
5. Falta crear archivos: 07_Antecedentes_Familiares.R (todo lo de Coni), 08_Depresion (todo lo de Josefa) y 09_ICT (todo lo de Carlos), 10_AFC_Depre_ICT (regresión conjunta). 
6. Cada archivo personal debe tener: Kaplan Meier, regresión de Cox y meta análisis. 