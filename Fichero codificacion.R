## Fichero trabajo final Data Science
# Diego Mochales, David Barreales, Emilio Portela, Nayeli Paucart

# Librerias
library(tidyverse)
library(tidyr)
library(haven)
library(dbplyr)
library(ggplot2)
library(ggrepel)
library(caret)
library(nnet)
library(openxlsx)
library(writexl)


# Fichero 
data <- read.csv("Data/3352.csv", sep = ";", header = TRUE, fill = TRUE, check.names = TRUE)



# Limpiamos de datos perdidos

# P5_1 Fuente de información
data$P5_1 <- na_if(data$P5_1, 98)
data$P5_1 <- na_if(data$P5_1, 99)
data$P5_1 <- as.factor(data$P5_1)

# EDAD 
data$EDAD <- na_if(data$EDAD, 99)
data$EDAD <- as.numeric(as.character(data$EDAD))

# CNO11 ocupacion
data$CNO11 <- na_if(data$CNO11, 99)
data$CNO11 <- as.factor(data$CNO11)

# ESTUDIOS 
data$ESTUDIOS <- na_if(data$ESTUDIOS, 9)
data$ESTUDIOS <- as.factor(data$ESTUDIOS)

# ESTADOCIVIL 
data$ESTADOCIVIL <- na_if(data$ESTADOCIVIL, 9)
data$ESTADOCIVIL <- as.factor(data$ESTADOCIVIL)

# SEXO 
data$SEXO <- as.factor(data$SEXO)

# ESCIDEOL escala de ideología
data$ESCIDEOL <- na_if(data$ESCIDEOL, 98)
data$ESCIDEOL <- na_if(data$ESCIDEOL, 99)
data$ESCIDEOL <- as.numeric(as.character(data$ESCIDEOL))

# TAMUNI Tamaño de municipio
data$TAMUNI <- na_if(data$TAMUNI, 0)
data$TAMUNI <- as.factor(data$TAMUNI)

# FIDELID Fidelidad de voto 
data$FIDELID <- na_if(data$FIDELID, 8)
data$FIDELID <- na_if(data$FIDELID, 9)
data$FIDELID <- as.factor(data$FIDELID)

# CLASESUB Clase social subjetiva
data$CLASESUB <- na_if(data$CLASESUB, 6)
data$CLASESUB <- na_if(data$CLASESUB, 8)
data$CLASESUB <- na_if(data$CLASESUB, 9)
data$CLASESUB <- as.factor(data$CLASESUB)

# RECUVOTOA partido votado
data$RECUVOTOA <- na_if(data$RECUVOTOA, 98)
data$RECUVOTOA <- na_if(data$RECUVOTOA, 99)
data$RECUVOTOA <- as.factor(data$RECUVOTOA)

# Eliminar cualquier fila que todavía tenga NA en las variables de interés
data <- drop_na(data, c(P5_1, EDAD, CNO11, ESTUDIOS, ESTADOCIVIL, SEXO, ESCIDEOL, TAMUNI, FIDELID, CLASESUB, RECUVOTOA))



#Creamos un nuevo dataframe
datos <- data



# Recodificamos las variables para ponerles sus categorías

# RECUVOTOA
datos$RECUVOTOA <- factor(datos$RECUVOTOA, levels = c(1, 2, 4, 17, 18, 21, 44, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 91, 92, 26, 39, 95, 96, 77, 98, 99),
                          labels = c("PP", "PSOE", "Ciudadanos", "PACMA", "VOX", "Unidas Podemos", "UPL", "Por Ávila", "Soria Ya", "Vía Burgalesa", "Puede Palencia",
                                     "Centrados (Segovia)", "Zamora Decide", "España Vaciada", "PCAS-TC-RC", "PREPAL", "Coalición por El Bierzo", "Por Zamora",
                                     "URCL (Unidad Regionalista de Castilla y León)", "Falange Española de las JONS", "PCPE", "Otro partido", "En blanco", "Voto nulo",
                                     "No recuerda", "No contesta"))

# P5_1
datos$P5_1 <- factor(datos$P5_1, levels = c(1, 2, 3, 4, 5, 6, 7, 95, 97, 98, 99),
                     labels = c("Prensa, en formato impreso", "Prensa, en formato digital", "Televisión", "Radio", "Redes sociales", 
                                "Contactos personales, reuniones, mítines, etc.", "(NO LEER) Otros medios", "(NO LEER) Ningún medio más", 
                                "(NO LEER) No se informó, no le interesa la política", "N.S.", "N.C."))

# CNO11
datos$CNO11 <- factor(datos$CNO11, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 99),
                      labels = c("Directores/as y gerentes", "Profesionales, científicos/as e intelectuales", "Técnicos/as y profesionales de nivel medio",
                                 "Personal de apoyo administrativo", "Trabajadores/as de los servicios y vendedores/as de comercios y mercados",
                                 "Agricultores/as y trabajadores/as cualificados/as agropecuarios/as, forestales y pesqueros/as", "Oficiales/as, operarios/as y artesanos/as",
                                 "Operadores/as de instalaciones y máquinas y ensambladores/as", "Ocupaciones elementales", "Ocupaciones militares y cuerpos policiales",
                                 "Otra/o", "No contesta"))

# ESTUDIOS
datos$ESTUDIOS <- factor(datos$ESTUDIOS, levels = c(1, 2, 3, 4, 5, 6, 7, 9),
                         labels = c("Sin estudios", "Primaria", "Secundaria 1ª etapa", "Secundaria 2ª etapa", "F.P.", "Superiores", "Otros", "N.C."))

# ESTADOCIVIL
datos$ESTADOCIVIL <- factor(datos$ESTADOCIVIL, levels = c(1, 2, 3, 4, 5, 9),
                            labels = c("Casado/a", "Soltero/a", "Viudo/a", "Separado/a", "Divorciado/a", "N.C."))

# SEXO
datos$SEXO <- factor(datos$SEXO, levels = c(1, 2),
                     labels = c("Hombre", "Mujer"))

# TAMUNI
datos$TAMUNI <- factor(datos$TAMUNI, levels = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("Menos o igual a 2.000 habitantes", "2.001 a 10.000 habitantes", "10.001 a 50.000 habitantes",
                                  "50.001 a 100.000 habitantes", "100.001 a 400.000 habitantes", "400.001 a 1.000.000 habitantes",
                                  "Más de 1.000.000 habitantes"))

# FIDELID
datos$FIDELID <- factor(datos$FIDELID, levels = c(1, 2, 3, 4, 5, 6, 8, 9),
                        labels = c("Votan siempre por el mismo partido", "Por lo general suelen votar por el mismo partido",
                                   "Según lo que más les convenza en ese momento, votan por un partido u otro o no votan", "(NO LEER) Votan en blanco o nulo",
                                   "(NO LEER) No suelen votar", "(NO LEER) Es la primera vez que votan", "No recuerda/N.S.", "N.C."))

# CLASESUB
datos$CLASESUB <- factor(datos$CLASESUB, levels = c(1, 2, 3, 4, 5, 6, 8, 9),
                         labels = c("Clase alta y media alta", "Clase media-media", "Clase media-baja", "Clase trabajadora/obrera/proletariado",
                                    "Clase baja/pobre", "Otras", "N.S.", "N.C."))
datos$CLASESUB <- as.character(datos$CLASESUB)
datos$CLASESUB[datos$CLASESUB == "Clase trabajadora/obrera/proletariado" | datos$CLASESUB == "Clase baja/pobre"] <- "Clase baja"
datos$CLASESUB <- factor(datos$CLASESUB, levels = c("Clase alta y media alta", "Clase media-media", "Clase media-baja", "Clase baja", "Otras", "N.S.", "N.C."))



# Convertir variables categóricas a factores 
datos$P5_1 <- as.factor(datos$P5_1)
datos$CNO11 <- as.factor(datos$CNO11)
datos$ESTUDIOS <- as.factor(datos$ESTUDIOS)
datos$ESTADOCIVIL <- as.factor(datos$ESTADOCIVIL)
datos$SEXO <- as.factor(datos$SEXO)
datos$TAMUNI <- as.factor(datos$TAMUNI)
datos$FIDELID <- as.factor(datos$FIDELID)
datos$CLASESUB <- as.factor(datos$CLASESUB)

# EDAD y ESCIDEOL a numéricas
datos$EDAD <- as.numeric(as.character(datos$EDAD))
datos$ESCIDEOL <- as.numeric(as.character(datos$ESCIDEOL))



# Crear variables binarias para Unidas Podemos, UPL y PSOE
datos$Voto_UP <- ifelse(datos$RECUVOTOA == "Unidas Podemos", 1, 0)
datos$Voto_UPL <- ifelse(datos$RECUVOTOA == "UPL", 1, 0)
datos$Voto_PSOE <- ifelse(datos$RECUVOTOA == "PSOE", 1, 0)

datos$UPL_UP <- ifelse(datos$RECUVOTOA == "UPL", 1, ifelse(datos$RECUVOTOA == "Unidas Podemos", 0, NA))
datos$UPL_PSOE <- ifelse(datos$RECUVOTOA == "UPL", 1, ifelse(datos$RECUVOTOA == "PSOE", 0, NA))

# Regresiones

# Unidas Podemos
modelo_up <- glm(Voto_UP ~ P5_1 + EDAD + CNO11 + ESTUDIOS + ESTADOCIVIL + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                 data = datos, family = binomial)

# UPL
modelo_upl <- glm(Voto_UPL ~ P5_1 + EDAD + CNO11 + ESTUDIOS + ESTADOCIVIL + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                  data = datos, family = binomial)

# PSOE
modelo_psoe <- glm(Voto_PSOE ~ P5_1 + EDAD + CNO11 + ESTUDIOS + ESTADOCIVIL + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                   data = datos, family = binomial)

#UPL vs UP
modelo_upl_up <- glm(UPL_UP ~ P5_1 + EDAD + CNO11 + ESTUDIOS + ESTADOCIVIL + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                     data = datos, family = binomial)

#UPL vs PSOE
modelo_upl_psoe <- glm(UPL_PSOE ~ P5_1 + EDAD + CNO11 + ESTUDIOS + ESTADOCIVIL + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                     data = datos, family = binomial)


# Resultados
summary(modelo_up)
summary(modelo_upl)
summary(modelo_psoe)
summary(modelo_upl_up)
summary(modelo_upl_psoe)

#Regresiones limpias
modelo_upl2 <- glm(Voto_UPL ~ P5_1 + EDAD + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                     data = datos, family = binomial)

modelo_up2 <- glm(Voto_UP ~ P5_1 + EDAD + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                   data = datos, family = binomial)

modelo_psoe2 <- glm(Voto_PSOE ~ P5_1 + EDAD + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                    data = datos, family = binomial)

modelo_upl_up2 <- glm(UPL_UP ~ P5_1 + EDAD + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                      data = datos, family = binomial)

modelo_upl_psoe2 <- glm(UPL_PSOE ~ P5_1 + EDAD + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                      data = datos, family = binomial)

# Resultados
summary(modelo_up2)
summary(modelo_upl2)
summary(modelo_psoe2)
summary(modelo_upl_up2)
summary(modelo_upl_psoe2)

# Guardamos las regresiones

# Unidas Podemos
coeficientesUP <- as.data.frame(coef(summary(modelo_up)))
coeficientesUP$Variable <- rownames(coeficientesUP)
rownames(coeficientesUP) <- NULL
write_xlsx(coeficientesUP, "Data/regresion_UP.xlsx")

#UPL
coeficientesUPL <- as.data.frame(coef(summary(modelo_upl)))
coeficientesUPL$Variable <- rownames(coeficientesUPL)
rownames(coeficientesUPL) <- NULL
write_xlsx(coeficientesUPL, "Data/regresion_UPL.xlsx")

#PSOE
coeficientesPSOE <- as.data.frame(coef(summary(modelo_psoe)))
coeficientesPSOE$Variable <- rownames(coeficientesPSOE)
rownames(coeficientesPSOE) <- NULL
write_xlsx(coeficientesPSOE, "Data/regresion_PSOE.xlsx")




