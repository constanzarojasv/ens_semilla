# Crear una nueva variable llamada egreso_cancer con restricciones ENS 2003. 
sum(ens2003_final$DIAG1_EGR == "D469", na.rm = TRUE)#aqui hay 0
ens2003_final$egreso_cancer <- ifelse(!is.na(ens2003_final$DIAG1_EGR) & grepl("^(C|D0|D3|D4)", ens2003_final$DIAG1_EGR) & ens2003_final$DIAG1 != "D469", 1, 0)
table(ens2003_final$DIAG1_EGR, ens2003_final$egreso_cancer)
sum(ens2003_final$egreso_cancer)
#hay 250 muertes por cáncer
