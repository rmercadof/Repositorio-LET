library(readxl)
library(tidyverse)

Desempeno2016 <- read_excel(file.choose())
Desempeno2017 <- read_excel(file.choose())
Desempeno2018 <- read_excel(file.choose())
head(Desempeno2016)
head(Desempeno2017)
head(Desempeno2018)

DesarrolloPersonal2016 <- read_excel(file.choose())
DesarrolloPersonal2017 <- read_excel(file.choose())
DesarrolloPersonal2018 <- read_excel(file.choose())
#todas distintas
head(DesarrolloPersonal2016)
head(DesarrolloPersonal2017)
head(DesarrolloPersonal2018)

#
Puntajes2016 <- read_excel(file.choose())
Puntajes2017 <- read_excel(file.choose())
Puntajes2018 <- read_excel(file.choose())
head(Puntajes2016)
head(Puntajes2017)
head(Puntajes2018)

#Seleccionar variable de interes en Desempeno

Desempeno2016 <- select(Desempeno2016, RBD, 'Categoría Desempeño 2016')
Desempeno2017 <- select(Desempeno2017, RBD, 'Categoría Desempeño 2017')
Desempeno2018 <- select(Desempeno2018, RBD, 'Categoría Desempeño 2018')

Desempeno_variables <- c("rbd", "Desempeño")
colnames(Desempeno2016) <- Desempeno_variables
colnames(Desempeno2017) <- Desempeno_variables
colnames(Desempeno2018) <- Desempeno_variables
head(Desempeno2018)

#juntar los 3 años

Desempeno <- full_join(Desempeno2016, Desempeno2017)
Desempeno <- full_join(Desempeno, Desempeno2018)

#Arreglar las columnas de las bases para que queden iguales

DesarrolloPersonal2017 <- select(DesarrolloPersonal2017, agno, grado, rbd, ind_am, ind_cc, ind_hv, ind_pf)
DesarrolloPersonal2018 <- select(DesarrolloPersonal2018, grado, RBD, ind_am_rbd, ind_cc_rbd, ind_hv_rbd, ind_pf_rbd)

head(DesarrolloPersonal2016)
head(DesarrolloPersonal2017)
head(DesarrolloPersonal2018)

#Crear columna faltante EN 2018
agno <- rep(2018, 7414) 
DesarrolloPersonal2018 <- cbind(agno, DesarrolloPersonal2018)

DesarrolloPersonal_variables <- colnames(DesarrolloPersonal2016) #obtener nombres de variables
colnames(DesarrolloPersonal2018) <- DesarrolloPersonal_variables #redefinir nombres de variables en 2018

#Unir data 
DesarrolloPersonal <- full_join(DesarrolloPersonal2016,DesarrolloPersonal2017)
head(DesarrolloPersonal)
tail(DesarrolloPersonal)

DesarrolloPersonal <- full_join(DesarrolloPersonal,DesarrolloPersonal2018) #Base completa

#Seleccionar variables de interés en Puntajes

Puntajes2016 <- select(Puntajes2016, agno, grado, rbd, cod_grupo, prom_lect4b_rbd, prom_mate4b_rbd)
Puntajes2017 <- select(Puntajes2017, agno, grado, rbd, cod_grupo, prom_lect4b_rbd, prom_mate4b_rbd)
Puntajes2018 <- select(Puntajes2018, agno, grado, rbd, cod_grupo, prom_lect4b_rbd, prom_mate4b_rbd)

head(Puntajes2016)
head(Puntajes2017)
head(Puntajes2018)


#arreglar en la primera data2017, grupo economico a string

Puntajes2017$cod_grupo[which(Puntajes2017$cod_grupo==1)] <- rep("Bajo", length(Puntajes2017$cod_grupo==1))
Puntajes2017$cod_grupo[which(Puntajes2017$cod_grupo==2)] <- rep("Medio bajo", length(Puntajes2017$cod_grupo==2))
Puntajes2017$cod_grupo[which(Puntajes2017$cod_grupo==3)] <- rep("Medio", length(Puntajes2017$cod_grupo==3))
Puntajes2017$cod_grupo[which(Puntajes2017$cod_grupo==4)] <- rep("Medio alto", length(Puntajes2017$cod_grupo==4))
Puntajes2017$cod_grupo[which(Puntajes2017$cod_grupo==5)] <- rep("Alto", length(Puntajes2017$cod_grupo==5))

##data2018 a string

Puntajes2018$cod_grupo[which(Puntajes2018$cod_grupo=="1")] <- rep("Medio bajo", length(Puntajes2018$cod_grupo=="1"))
Puntajes2018$cod_grupo[which(Puntajes2018$cod_grupo=="2")] <- rep("Medio bajo", length(Puntajes2018$cod_grupo=="2"))
Puntajes2018$cod_grupo[which(Puntajes2018$cod_grupo=="3")] <- rep("Medio bajo", length(Puntajes2018$cod_grupo=="3"))
Puntajes2018$cod_grupo[which(Puntajes2018$cod_grupo=="4")] <- rep("Medio bajo", length(Puntajes2018$cod_grupo=="4"))
Puntajes2018$cod_grupo[which(Puntajes2018$cod_grupo=="5")] <- rep("Medio bajo", length(Puntajes2018$cod_grupo=="5"))

Puntajes <- full_join(Puntajes2016, Puntajes2017)
Puntajes <- full_join(Puntajes, Puntajes2018)

tail(Puntajes)

#Obtención de la base de datos
datos <- inner_join(Puntajes,DesarrolloPersonal)
datos <- inner_join(datos, Desempeno)
colnames(datos) <- c("Agno", "Grado", "rbd", "Grupo_socioeconomico", "Ptje Lectura","Ptje Matematica","Autoestima","Convivencia", "Habitos saludables", "Formacion ciudadana", "Desempeno")

#Guardar la base de datos nueva
write.csv(datos,"DatosSimce.csv")
