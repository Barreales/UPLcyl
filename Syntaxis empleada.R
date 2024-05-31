## Fichero trabajo final Data Science
# Diego Mochales, David Barreales, Emilio Portela, Nayely Paucar

# Librerias
library(tidyverse)
library(tidyr)
library(haven)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(caret)
library(nnet)
library(openxlsx)
library(writexl)
library(tree)
library(GGally)
library(sjPlot)
library(webshot)
library(htmltools)
library(hrbrthemes)

# Fichero 
data <- read.csv("Data/3352.csv", sep = ";", header = TRUE, fill = TRUE, check.names = TRUE)

## Limpiamos de datos perdidos

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


##Creamos un nuevo dataframe
datos <- data

## Recodificamos las variables para ponerles sus categorías

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

# EDADR recodificada
cortes <- seq(from = floor(18 / 15) * 15, to = ceiling(90 / 15) * 15, by = 15) # Crear los intervalos quinquenales

datos$EDADR <- cut(datos$EDAD, breaks = cortes, right = FALSE) # Usar la función cut() para agrupar las edades en los intervalos definidos

# EDAD y ESCIDEOL a numéricas
datos$EDAD <- as.numeric(as.character(datos$EDAD))
datos$ESCIDEOL <- as.numeric(as.character(datos$ESCIDEOL))

# Crear variables binarias para Unidas Podemos, UPL y PSOE
datos$Voto_UP <- ifelse(datos$RECUVOTOA == "Unidas Podemos", 1, 0)
datos$Voto_UPL <- ifelse(datos$RECUVOTOA == "UPL", 1, 0)
datos$Voto_PSOE <- ifelse(datos$RECUVOTOA == "PSOE", 1, 0)

datos$UPL_UP <- ifelse(datos$RECUVOTOA == "UPL", 1, ifelse(datos$RECUVOTOA == "Unidas Podemos", 0, NA))
datos$UPL_PSOE <- ifelse(datos$RECUVOTOA == "UPL", 1, ifelse(datos$RECUVOTOA == "PSOE", 0, NA))

# Crear una variable con el voto a cada partido
datos <- datos %>%
  mutate(Voto_Total = case_when(
    RECUVOTOA == "Unidas Podemos" ~ "UP",
    RECUVOTOA == "UPL" ~ "UPL",
    RECUVOTOA == "PSOE" ~ "PSOE",
    TRUE ~ NA_character_))

datos$Voto_Total <- factor(datos$Voto_Total, levels = c("UP", "UPL", "PSOE")) #Convertirla en factor

## Regresiones
datos$P5_1 <- relevel(datos$P5_1, ref = "(NO LEER) No se informó, no le interesa la política") #Calcular el logit sobre "No se informó"
datos$CLASESUB <- relevel(datos$CLASESUB, ref = "Clase media-media") #Calcular el logit sobre clase media-media
datos$FIDELID <- relevel(datos$FIDELID, ref = "Por lo general suelen votar por el mismo partido") #Calcular el logit sobre "voto suele ser el mismo". Que se ha considerado el valor intermedio

#Regresiones con todas las VD
modelos_todo <- list(
  glm(Voto_UP ~ P5_1 + EDAD + CNO11 + ESTUDIOS + ESTADOCIVIL + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, data = datos, family = binomial),
  glm(Voto_UPL ~ P5_1 + EDAD + CNO11 + ESTUDIOS + ESTADOCIVIL + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, data = datos, family = binomial),
  glm(Voto_PSOE ~ P5_1 + EDAD + CNO11 + ESTUDIOS + ESTADOCIVIL + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, data = datos, family = binomial),
  glm(UPL_UP ~ P5_1 + EDAD + CNO11 + ESTUDIOS + ESTADOCIVIL + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, data = datos, family = binomial),
  glm(UPL_PSOE ~ P5_1 + EDAD + CNO11 + ESTUDIOS + ESTADOCIVIL + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, data = datos, family = binomial))

tab_model(modelos_todo, 
          title = "Regresión con todas las VD", 
          dv.labels = c("UP","UPL", "PSOE","UPL-UP", "UPL-PSOE"),
          show.ci = FALSE)

#Regresiones significativas teóricas
modelos_t <- list(
  glm(Voto_UP ~ ESCIDEOL + FIDELID + CLASESUB, data = datos, family = binomial),
  glm(Voto_UPL ~ ESCIDEOL + FIDELID + CLASESUB, data = datos, family = binomial),
  glm(Voto_PSOE ~ ESCIDEOL + FIDELID + CLASESUB, data = datos, family = binomial),
  glm(UPL_UP ~ ESCIDEOL + FIDELID + CLASESUB, data = datos, family = binomial),
  glm(UPL_PSOE ~ ESCIDEOL + FIDELID + CLASESUB, data = datos, family = binomial))

tab_model(modelos_t, 
          title = "Regresión de las VD teóricas significativas", 
          dv.labels = c("UP","UPL", "PSOE","UPL-UP", "UPL-PSOE"),
          show.ci = FALSE)


#Regresiones con varaibles significativas teóricas + sociodemográficas 
modelos_ts <- list(
  glm(Voto_UP ~ ESCIDEOL + FIDELID + CLASESUB + EDAD + SEXO + TAMUNI, data = datos, family = binomial),
  glm(Voto_UPL ~ ESCIDEOL + FIDELID + CLASESUB + EDAD + SEXO + TAMUNI, data = datos, family = binomial),
  glm(Voto_PSOE ~ ESCIDEOL + FIDELID + CLASESUB + EDAD + SEXO + TAMUNI, data = datos, family = binomial),
  glm(UPL_UP ~ ESCIDEOL + FIDELID + CLASESUB + EDAD + SEXO + TAMUNI, data = datos, family = binomial),
  glm(UPL_PSOE ~ ESCIDEOL + FIDELID + CLASESUB + EDAD + SEXO + TAMUNI, data = datos, family = binomial))

tab_model(modelos_ts, 
          title = "Regresión de las VD teóricas + sociodemográficas significativas", 
          dv.labels = c("UP","UPL", "PSOE","UPL-UP", "UPL-PSOE"),
          show.ci = FALSE)

##Tablas de contingencia

#Decisión de voto según la ideología
ideol_total <- table(datos$Voto_Total, datos$ESCIDEOL) %>% print()  #Crear una tabla de contingencia
ideol_total_col <- prop.table(ideol_total, margin = 2) * 100 #Asignar porcentajes por columna o VI
print(ideol_total_col) 

ideol_total_chi<-chisq.test(ideol_total)#Realizar a la tabla un Chi-cuadrado
print(ideol_total_chi)

ideol_total_v <- sqrt(ideol_total_chi$statistic / (sum(ideol_total) * (min(dim(ideol_total)) - 1))) #Realizar a la tabla una V de Cramer
print(ideol_total_v)

ideol_total_res <- ideol_total_chi$stdres #Análisis de los residuos
print(ideol_total_res) 

#Decisión de voto según sexo
sexo_total <- table(datos$Voto_Total, datos$SEXO) %>% print()  #Crear una tabla de contingencia
sexo_total_col <- prop.table(sexo_total, margin = 2) * 100 #Asignar porcentajes por columna o VI
print(sexo_total_col) 
sexo_total_chi<-chisq.test(sexo_total) #Realizar a la tabla un Chi-cuadrado
print(sexo_total_chi)
sexo_total_v <- sqrt(sexo_total_chi$statistic / (sum(sexo_total) * (min(dim(sexo_total)) - 1))) #Realizar a la tabla una V de Cramer
print(sexo_total_v)
sexo_total_res <- sexo_total_chi$stdres #Análisis de los residuos
print(sexo_total_res) 

#Decisión de voto según fidelidad de voto
datos$FIDELID <- as.character(datos$FIDELID)
datos$FIDELID <- factor(datos$FIDELID, levels = c("Votan siempre por el mismo partido", "Por lo general suelen votar por el mismo partido",
                                                  "Según lo que más les convenza en ese momento, votan por un partido u otro o no votan")) #Eliminar categorias sin valores significativos
fidelid_total <- table(datos$Voto_Total, datos$FIDELID) %>% print()  #Crear una tabla de contingencia
fidelid_total_col <- prop.table(fidelid_total, margin = 2) * 100 #Asignar porcentajes por columna o VI
print(fidelid_total_col) 
fidelid_total_chi<-chisq.test(fidelid_total) #Realizar a la tabla un Chi-cuadrado
print(fidelid_total_chi)
fidelid_total_v <- sqrt(fidelid_total_chi$statistic / (sum(fidelid_total) * (min(dim(fidelid_total)) - 1))) #Realizar a la tabla una V de Cramer
print(fidelid_total_v)
fidelid_total_res <- fidelid_total_chi$stdres #Análisis de los residuos
print(fidelid_total_res) 

#Decisión de voto según autopercepción de clase social
datos$CLASESUB <- as.character(datos$CLASESUB)
datos$CLASESUB[datos$CLASESUB == "Clase trabajadora/obrera/proletariado" | datos$CLASESUB == "Clase baja/pobre"] <- "Clase baja"
datos$CLASESUB <- factor(datos$CLASESUB, levels = c("Clase alta y media alta", "Clase media-media", "Clase media-baja", "Clase baja")) #Recodificiación de categorías

clasesub_total <- table(datos$Voto_Total, datos$CLASESUB) %>% print()  #Crear una tabla de contingencia
clasesub_total_col <- prop.table(clasesub_total, margin = 2) * 100 #Asignar porcentajes por columna o VI
print(clasesub_total_col) 
clasesub_total_chi<-chisq.test(clasesub_total) #Realizar a la tabla un Chi-cuadrado
print(clasesub_total_chi)
clasesub_total_v <- sqrt(clasesub_total_chi$statistic / (sum(clasesub_total) * (min(dim(clasesub_total)) - 1))) #Realizar a la tabla una V de Cramer
print(clasesub_total_v)
clasesub_total_res <- clasesub_total_chi$stdres #Análisis de los residuos
print(clasesub_total_res) 

#Decisión de voto según la edad recodificada
edad_total <- table(datos$Voto_Total, datos$EDADR) %>% print()  #Crear una tabla de contingencia
edad_total_col <- prop.table(edad_total, margin = 2) * 100 #Asignar porcentajes por columna o VI
print(edad_total_col)
edad_total_chi <- chisq.test(edad_total) #Realizar a la tabla un Chi-cuadrado
print(edad_total_chi)
edad_total_v <- sqrt(edad_total_chi$statistic / (sum(edad_total) * (min(dim(edad_total)) - 1))) #Realizar a la tabla una V de Cramer
print(edad_total_v)
edad_total_res <- edad_total_chi$stdres #Análisis de los residuos
print(edad_total_res)

#Decisión de voto según el tamaño del municipio
datos$TAMUNI <- as.character(datos$TAMUNI)
datos$TAMUNI <- factor(datos$TAMUNI, levels = c("Menos o igual a 2.000 habitantes", "2.001 a 10.000 habitantes", "10.001 a 50.000 habitantes",
                                                "50.001 a 100.000 habitantes", "100.001 a 400.000 habitantes")) #Eliminar categorias sin valores significativos

tamuni_total <- table(datos$Voto_Total, datos$TAMUNI) %>% print()  #Crear una tabla de contingencia
tamuni_total_col <- prop.table(tamuni_total, margin = 2) * 100 #Asignar porcentajes por columna o VI
print(tamuni_total_col)
tamuni_total_chi <- chisq.test(tamuni_total) #Realizar a la tabla un Chi-cuadrado
print(tamuni_total_chi)
tamuni_total_v <- sqrt(tamuni_total_chi$statistic / (sum(tamuni_total) * (min(dim(tamuni_total)) - 1))) #Realizar a la tabla una V de Cramer
print(tamuni_total_v)
tamuni_total_res <- tamuni_total_chi$stdres #Análisis de los residuos
print(tamuni_total_res)

##Gráficos con aquellas VD cuya V de Cramer > 0.1
#Preparación de los datos
datos2 <- datos %>%
  filter(!is.na(Voto_Total) & !is.na(CLASESUB) & !is.na(FIDELID))

# Definir los colores personalizados para cada partido
colores_partidos <- c("PSOE" = "red",
                      "UPL" = "yellow",
                      "UP" = "purple")

# CLASESUB
porcentaje <- datos2 %>%
  group_by(Voto_Total, CLASESUB) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(Voto_Total) %>%
  mutate(total_count = sum(count),
         percentage = (count / total_count) * 100) %>%
  ungroup()

ggplot(porcentaje, aes(x = Voto_Total, y = percentage, fill = CLASESUB)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribución porcentual del voto dentro de cada partido político",
       x = "Partido político",
       y = "Porcentaje",
       fill = "Clase Subjetiva") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()


# FIDELID
porcentaje <- datos2 %>%
  group_by(Voto_Total, FIDELID) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(Voto_Total) %>%
  mutate(total_count = sum(count),
         percentage = (count / total_count) * 100) %>%
  ungroup()

ggplot(porcentaje, aes(x = Voto_Total, y = percentage, fill = FIDELID)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribución porcentual de la fidelidad dentro de cada partido político",
       x = "Partido político",
       y = "Porcentaje",
       fill = "Fidelidad del voto") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# EDADR
ggplot(datos2, aes(x = EDAD, fill = Voto_Total)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de la edad por partido político",
       x = "Edad",
       y = "Densidad",
       fill = "Partido político") +
  scale_fill_manual(values = colores_partidos) +
  theme_minimal()

# ESCIDEOL
ggplot(datos2, aes(x = ESCIDEOL, fill = Voto_Total)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de la ideología por partido político",
       x = "Ideología",
       y = "Densidad",
       fill = "Partido político") +
  scale_fill_manual(values = colores_partidos) +
  theme_minimal()


##Realización de árboles de clasificación
arbol_PSOE <- tree(datos$Voto_PSOE ~ P5_1 + EDAD + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                   data = datos, 
                   mincut = 2,       # Reducir el mínimo de observaciones en una rama
                   minsize = 10,     # Reducir el mínimo de observaciones en un nodo terminal
                   mindev = 0.01)    # Reducir la desviación mínima para dividir un nodo

plot(arbol_PSOE)
text(arbol_PSOE, pretty = 0)
arbol_PSOE

#Como se puede observar en la figura, el voto al PSOE se puede explicar a través de 5 nodos terminales.
#El primero, aquellos que se ubican ideológicamente por debajo de 4.5 y son menores de 61.5 años, teniendo una proporción de voto de 0.55.
#El segundo es similar al primero, pero con aquellos mayores de 61.5 años, que tienen una proporción de voto de 0.81.
#Los tres siguientes nodos son aquellos que su escala ideológica es mayor a 4.5. Si, a su vez, es mayor a 5.5 la proporción de voto es de 0.05.
#Aunque, si es inferior, los nodos terminales dependen de la fidelidad de voto. Si se sitúa entre 3 y 6 el voto es de 0.17, en el caso contrario pasa a ser de 0.44.

arbol_UPL <- tree(datos$Voto_UPL ~ P5_1 + EDAD + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                  data = datos, 
                  mincut = 2,       # Reducir el mínimo de observaciones en una rama
                  minsize = 10,     # Reducir el mínimo de observaciones en un nodo terminal
                  mindev = 0.01)    # Reducir la desviación mínima para dividir un nodo

plot(arbol_UPL)
text(arbol_UPL, pretty = 0)
arbol_UPL

arbol_UP <- tree(datos$Voto_UP ~ P5_1 + EDAD + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                 data = datos, 
                 mincut = 2,       # Reducir el mínimo de observaciones en una rama
                 minsize = 10,     # Reducir el mínimo de observaciones en un nodo terminal
                 mindev = 0.01)    # Reducir la desviación mínima para dividir un nodo

plot(arbol_UP)
text(arbol_UP, pretty = 0)
arbol_UP

arbol_UPL_UP <- tree(datos$UPL_UP ~ P5_1 + EDAD + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                     data = datos, 
                     mincut = 2,       # Reducir el mínimo de observaciones en una rama
                     minsize = 10,     # Reducir el mínimo de observaciones en un nodo terminal
                     mindev = 0.01)    # Reducir la desviación mínima para dividir un nodo

plot(arbol_UPL_UP)
text(arbol_UPL_UP, pretty = 0)
arbol_UPL_UP

arbol_UPL_PSOE <- tree(datos$UPL_PSOE ~ P5_1 + EDAD + SEXO + ESCIDEOL + TAMUNI + FIDELID + CLASESUB, 
                       data = datos, 
                       mincut = 2,       # Reducir el mínimo de observaciones en una rama
                       minsize = 10,     # Reducir el mínimo de observaciones en un nodo terminal
                       mindev = 0.01)    # Reducir la desviación mínima para dividir un nodo

plot(arbol_UPL_PSOE)
text(arbol_UPL_PSOE, pretty = 0)
arbol_UPL_PSOE

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
