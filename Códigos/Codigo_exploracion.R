datos <- read.csv("C:/Users/Romina/Repositorio-LET/datos_ordenados/DatosSimce.csv")


    #exploración de los datos

datos <- filter(datos, PtjeLectura!=NA | PtjeLectura !=0)
datos <- filter(datos, PtjeMatematica!=NA | PtjeMatematica !=0)
datos <- filter(datos, is.na(Convivencia)==F)
datos <- filter(datos, is.na(Autoestima)==F)
datos <- filter(datos, is.na(HabitosSaludables)==F)
datos <- filter(datos, is.na(FormacionCiudadana)==F)

min(datos$PtjeLectura) #149
min(datos[datos$PtjeMatematica>0,"PtjeMatematica"], na.rm=T) #129

#maximos puntajes
max(datos$PtjeLectura) #396
max(datos$PtjeMatematica) #376

#promedio lectura

mean(datos$PtjeLectura) #261.8559

#promedio matematica

mean(datos$PtjeMatematica) # 261.8559


#Promedio Desempeño MATESMATICAS

mean(datos$PtjeMatematica[datos$Desempeno=="INSUFICIENTE"]) #[1] 228.4617

mean(datos$PtjeMatematica[datos$Desempeno=="MEDIO-BAJO" | datos$Desempeno=="MEDIO-BAJO (NUEVO)"]) #[1] 240.029

mean(datos$PtjeMatematica[datos$Desempeno=="MEDIO"]) #[1] 255.6693

mean(datos$PtjeMatematica[datos$Desempeno=="ALTO"]) # [1] 280.4555


#Promedio Desempeño Lectura

mean(datos$PtjeLectura[datos$Desempeno=="INSUFICIENTE"]) #[1] 240.3822

mean(datos$PtjeLectura[datos$Desempeno=="MEDIO-BAJO" | datos$Desempeno=="MEDIO-BAJO (NUEVO)"]) #[1] 251.534

mean(datos$PtjeLectura[datos$Desempeno=="MEDIO"]) #[1] 265.808

mean(datos$PtjeLectura[datos$Desempeno=="ALTO"]) # [1] 286.296



##Promedio puntaje segun nivel socioeconomico MATESMATICAS

mean(datos$PtjeMatematica[datos$Grupo_socioeconomico =="Bajo"]) #[1] 235.5431

mean(datos$PtjeMatematica[datos$Grupo_socioeconomico=="Medio bajo" | datos$Desempeno=="MEDIO-BAJO (NUEVO)"]) #[1] 247.0911

mean(datos$PtjeMatematica[datos$Grupo_socioeconomico=="Medio"]) #[1] 256.6302

mean(datos$PtjeMatematica[datos$Grupo_socioeconomico=="Medio alto"]) # [1] 272.7193

mean(datos$PtjeMatematica[datos$Grupo_socioeconomico=="Alto"]) # [1] 290.5961


##Promedio puntaje segun nivel socioeconomico Lectura

mean(datos$PtjeLectura[datos$Grupo_socioeconomico =="Bajo"]) #[1] 253.0105

mean(datos$PtjeLectura[datos$Grupo_socioeconomico=="Medio bajo" | datos$Desempeno=="MEDIO-BAJO (NUEVO)"]) #[[1] 259.4248

mean(datos$PtjeLectura[datos$Grupo_socioeconomico=="Medio"]) #[1] 265.0224

mean(datos$PtjeLectura[datos$Grupo_socioeconomico=="Medio alto"]) # [1] 280.7158

mean(datos$PtjeLectura[datos$Grupo_socioeconomico=="Alto"]) # [1] 296.0503



datos_correlaciones <- select(datos, PtjeLectura, PtjeMatematica, Autoestima, Convivencia, HabitosSaludables, FormacionCiudadana)
cor(datos_correlaciones)
datos_correlaciones

#En el análisis de correlaciones tenemos que hay algún tipo de correlación entre el indice convivencia escolar y buenos resultados en la prueba de lectura. 


#luego un analisis de regresion para predecir el puntaje a partir de otras variables 

m_Lectura <- lm(PtjeLectura ~Grupo_socioeconomico + Autoestima + Convivencia + HabitosSaludables + FormacionCiudadana + Desempeno, data=datos)

step(m_Lectura)
Backward <- step(m_Lectura, direction= "backward", k=2)

plot(m_Lectura)
imcdiag(m_Lectura)

m_lectura2 <- update(m_Lectura, ~.-Grupo_socioeconomico - Desempeno)

m_Lectura <- lm(PtjeLectura ~Grupo_socioeconomico + Autoestima + Convivencia + HabitosSaludables + FormacionCiudadana + Desempeno, data=datos)
