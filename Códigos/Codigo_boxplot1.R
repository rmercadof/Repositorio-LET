datos <- read.csv("C:/Users/Romina/Repositorio-LET/datos_ordenados/DatosSimce.csv")

boxplot(datos$Ptje.Lectura[datos$Grupo_socioeconomico=="Alto"], datos$Ptje.Lectura[datos$Grupo_socioeconomico=="Bajo"],
        main="Boxplot puntajes prueba Lectura", xlab = "Nivel economico alto--- Nivel economico Bajo")
boxplot(Ptje.Lectura ~ Grupo_socioeconomico)
