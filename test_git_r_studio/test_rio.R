## ---- coder: Cosaph ---- ##
## ---- coding utf-8 ---- ##

## ---- library ---- ##

library("rio") #nécéssité d'installer des format ("install.format()")
install_formats()
data_sas<-import("C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/chemin_d_acces.sas7bdat")

daa<-export(data_sas,"C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/election_leg.xlsx")
import(daa)

