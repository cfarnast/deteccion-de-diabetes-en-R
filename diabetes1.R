# CURSO: ANALITICA PREDICTIVA EN R Y MODELOS DE CLASIFICACION EN R.
# DIRECCION WEB: https://www.crehana.com/cl/cursos-online-data/analitica-predictiva-y-modelos-de-clasificacion-en-r/?source_page=Homepage&source_detail=Search%20Dialog&source=search&model_used=SEARCH_ENGINE_V2.2&product_name=Anal%C3%ADtica%20predictiva%20y%20modelos%20de%20clasificaci%C3%B3n%20en%20R&product_id=11620&keyword=ANALITICA%20PREDICTICA&item_type=course&position_selected=0
# INSTRUCTOR: DAVID ZARRUK
# ALUMNO: CHRISTIAN FARNAST

# PROYECTO FINAL DE CURSO: DETECCION DE DIABETES EN UN GRUPO DETERMINADO
# PARTE 1: ANALISIS EXPLORATORIO DE DATOS

# 1. CREAR DIRECTORIO DE TRABAJO Y CARGAR DATASET
setwd()
diabetes <- read.csv('diabetes_train.csv')

###############################################################################

# 2. CARGAR LIBRERIAS

library(tidyverse)
library(ggplot2)
library(funModeling)
library(corrr)
library(corrplot)

###############################################################################

# 3. DESARROLLO

# 3.1-¿CUANTOS DATOS TENEMOS PARA NUESTRO ANALISIS?

dim(diabetes)
# disponemos de 614 filas y 9 columnas

names(diabetes)
# los nombres de las variables son:
# "num_embarazos"      "plasma"             "presion_diastolica"
# "grosor_piel"        "insulina"           "bmi"               
# "diabetes_pedigree"  "edad"               "diabetes"

View(diabetes)

# 3.2-ENCONTRAR ESTADISTICAS DESCRIPTIVAS DE TODAS LAS VARIABLES
# MEDIA, DESVIACION ESTANDAR, MINIMO, MAXIMO, PERCENTILES 25, 50 Y 75.

# CON LA FUNCION profiling_num DEL PAQUETE funModeling SE OBTIENEN LAS ESTADISTICAS
# SOLICITADAS.

profiling_num(diabetes)
#             variable        mean     std_dev   p_25 
# 1      num_embarazos   3.8013029   3.3461914  1.000
# 2             plasma 120.4478827  32.7038676 99.000
# 3 presion_diastolica  68.5228013  19.6080343 64.000
# 4        grosor_piel  20.6563518  15.9581677  0.000
# 5           insulina  80.2996743 116.6440526  0.000
# 6                bmi  31.8903909   7.9752690 27.100
# 7  diabetes_pedigree   0.4811792   0.3368657  0.248
# 8               edad  33.1970684  11.7728054 24.000
# 9           diabetes   0.3469055   0.4763735  0.000
# 
#           
#                                       p_50    p_75     
# 1      num_embarazos                3.0000   6.000  
# 2             plasma              116.0000 141.000 
# 3 presion_diastolica               70.0000  80.000  
# 4        grosor_piel               23.0000  32.000  
# 5           insulina               37.0000 126.000 
# 6                bmi               32.0000  36.500  
# 7  diabetes_pedigree                0.3865   0.647   
# 8               edad               29.0000  40.000  
# 9           diabetes                0.0000   1.000

# LOS VALORES MAXIMOS Y MINIMOS SE PUEDEN CONSULTAR CON LA FUNCION summary() de R base
summary(diabetes)

# num_embarazos        plasma      presion_diastolica  grosor_piel   
# Min.   : 0.000   Min.   :  0.0   Min.   :  0.00     Min.   : 0.00  
# Max.   :17.000   Max.   :198.0   Max.   :122.00     Max.   :99.00 

# insulina          bmi        diabetes_pedigree      edad         diabetes     
# Min.   :  0.0   Min.   : 0.00   Min.   :0.0780    Min.   :21.0   Min.   :0.0000  
# Max.   :846.0   Max.   :67.10   Max.   :2.4200    Max.   :81.0   Max.   :1.0000

# 3.3-Hacer un análisis exploratorio para ver qué variables pueden ser las
# mejores al momento de predecir si una persona tiene diabetes. 
# Para esto, debes generar:
#   
# 3.3.1-Gráficos de dispersión de las variables 2-8 contra la variable edad. 
# ¿Cuáles variables parecieran ser buenas para explicar la edad de un abalón?
  
ggplot(diabetes, aes(x=edad, y=plasma, color = factor(diabetes))) +
  geom_point()*

ggplot(diabetes, aes(x=edad, y=num_embarazos, color = factor(diabetes))) +
  geom_point()

ggplot(diabetes, aes(x=edad, y=presion_diastolica, color = factor(diabetes))) +
  geom_point()*

ggplot(diabetes, aes(x=edad, y=grosor_piel, color = factor(diabetes))) +
  geom_point()*

ggplot(diabetes, aes(x=edad, y=bmi, color = factor(diabetes))) +
  geom_point()*

ggplot(diabetes, aes(x=edad, y=insulina, color = factor(diabetes))) +
  geom_point()*

ggplot(diabetes, aes(x=edad, y=diabetes_pedigree, color = factor(diabetes))) +
  geom_point()*

ggplot(diabetes, aes(x=edad, fill = factor(diabetes))) +
  geom_bar(position = 'fill')

ggplot(diabetes, aes(x=edad, fill = factor(diabetes))) +
  geom_boxplot() +
  coord_flip()

# Para los graficos de dispersion y salvo en el caso de la cantidad de embarazos,
# las demas restantes (exceptuando la variable insulina, que tiene una relacion inversa)
# parecen tener una relacion directa con la edad, sin embargo
# la dispersion de los puntos es muy evidente por lo que se requieren de otras metricas
# para evaluar cuál o cuáles de todas es mas relevante en comparacion con la edad

  
# 3.3.2-Gráfico de calor para ver las correlaciones existentes entre variables. 
# ¿Lo que se observa en el gráfico de calor concuerda con lo encontrado en los 
# gráficos de dispersión?, ¿Cuáles variables tienen correlaciones positivas 
# o negativas más fuertes con respecto a la variable edad?


corrplot(cor(diabetes), method = 'square')



# Considerando este mapa, las variables que mas se correlacionan con la edad son:
# 1. num_embarazos
# 2. plasma
# 3. presion_diastólica
# 4. diabetes

# 3.3.3-Gráficos de cajas y bigotes para todas las variables categóricas
# o discretas. ¿Cuáles variables son categóricas? ¿Qué categorías
# tienen mayor edad en promedio?
str(diabetes)
# corriendo la funcion str(diabetes), no hay variables categoricas, todas son numericas
# (propiamente numericas o enteros (int))

# Se realizarán  boxplots para las metricas que mayormente se 
# correlacionan con la edad

G1 <- ggplot(diabetes, aes(x=edad, fill = factor(diabetes))) +
  geom_boxplot() +
  coord_flip()

# para las demas metricas se haran grupos mas pequeños, con la funcion equal_freq()
# de funModeling

# diabetes$num_embarazos1 <- equal_freq(diabetes$edad, n_bins= 3)
# diabetes$plasma1 <- equal_freq(diabetes$plasma, n_bins= 8)
# diabetes$presion_diastolica1 <- equal_freq(diabetes$presion_diastolica, n_bins= 3)
# diabetes$edad1 <- equal_freq(diabetes$edad, n_bins= 5)

diabetes$num_embarazos1 <- NULL
diabetes$plasma1 <- NULL
diabetes$presion_diastolica1 <- NULL
diabetes$edad1 <- NULL

G2 <- ggplot(diabetes, aes(x=edad, fill = plasma1)) +
  geom_boxplot() + 
  coord_flip()
# para el caso de la cantidad de azucar en plasma y en comparacion con el grupo
# de concentracion en plasma (grupos 2 al 5, con un estado previo a una diabetes) 
# entre 88 y 100, los demas grupos inician su problemade resistencia a la 
# insulina bajo el umbral de los 30 años en promedio.
# Los ultimos dos grupos inician su enfermedad diabetica, preocupantemente
# con estos datos a una edad de 35 años em promedio, lo que guarda relacion con
# el grafico G1

G3 <- ggplot(diabetes, aes(x=edad, fill = presion_diastolica1)) +
  geom_boxplot() + 
  coord_flip()

# El ultimo grupo (celeste), muestra una presion diastolica elevada, lo que puede
# inferir en estos pacientes, conllevar hipertension arterial, tambien con una edad
# promedio de 35 años, al igual que los pacientes con diabetes mostrados en el grafico G2
# Esto puede concluir una posible correlacion de diabetes con presion diastolica elevada

G4 <- ggplot(diabetes, aes(presion_diastolica1, fill = factor(diabetes))) +
  geom_bar(position = 'fill')

# En este caso y a medida que aumenta la presion diastolica, aumenta la cantidad
# de pacientes con diabetes (no considar el primer grupo, que con esos niveles de
# glucosa, son pacientes con shock hipoglucemico), sin embargo no considero este grafico
# suficiente para correlacionar diabetes con presion diastolica elevada

# PARTE 2: MEDICION DEL DESEMPEÑO DE LOS MODELOS 2 Y 3.

# DESEMPEÑO DEL MODELO 2

df_train <- read.csv('diabetes_train.csv')
df_test <- read.csv('diabetes_test.csv')
View(df_train)
View(df_test)

# Métrica #1: Matriz de confusión 
confusionMatrix(as.factor(df_test$diabetes), as.factor(df_test$modelo_2))

# Métrica #2: Recall 
recall(as.factor(df_test$diabetes), as.factor(df_test$modelo_2), relevant = '1')

# Métrica #3: Precisión 
precision(as.factor(df_test$diabetes), as.factor(df_test$modelo_2), relevant = '1')

# Métrica #4: F1-score 
F_meas(as.factor(df_test$diabetes), as.factor(df_test$modelo_2), relevant = '1')

#Resultados modelo 2:
#####################

#matriz de confusión

#           Reference
# Prediction  0  1
#          0 87 12
#          1  6 49
#Sensitivity : 0.9355  <- tasa falsos positivos: 1-0.9355 = 0.0645 (6.5%)       
#Specificity : 0.8033 <- tasa falsos negativos: 1-0.8033 = 0.1967 (20%)
#recall: 0.80
#precision: 0.89
#f1 score:0.85
#accuracy(exactitud): (87+49)/(87+12+6+49) = 88%


# DESEMPEÑO DEL MODELO 3

df_train <- read.csv('diabetes_train.csv')
df_test <- read.csv('diabetes_test.csv')
View(df_train)
View(df_test)

# Métrica #1: Matriz de confusión 
confusionMatrix(as.factor(df_test$diabetes), as.factor(df_test$modelo_3))

# Métrica #2: Recall 
recall(as.factor(df_test$diabetes), as.factor(df_test$modelo_3), relevant = '1')

# Métrica #3: Precisión 
precision(as.factor(df_test$diabetes), as.factor(df_test$modelo_3), relevant = '1')

# Métrica #4: F1-score 
F_meas(as.factor(df_test$diabetes), as.factor(df_test$modelo_3), relevant = '1')

#Resultados modelo 3:
#####################

#matriz de confusión

#           Reference
# Prediction  0  1
#          0 77 22
#          1 13 42
#Sensitivity : 0.8556 <- tasa falsos positivos: 1-0.8556 = 0.1444 (14%)          
#Specificity : 0.6562 <- tasa falsos negativos: 1-0.6562 = 0.3438 (34%)
#recall: 0.65
#precision: 0.76
#f1 score:0.70
#accuracy(exactitud): (77+42)/(77+22+13+42) = 77%


# PARTE FINAL: PREDICCION DE DIABETES USANDO MODELOS: REGRESION LOGISTICA, REGRESION MULTIVARIADA
# LINEAL Y RANDOM FOREST

library(caret)
library(e1071)
library(corrplot)
library(corrr)
library(funModeling)
library(tidyverse)
library(randomForest)

df_train <- read.csv('diabetes_train.csv')
df_test <- read.csv('diabetes_test.csv')
View(df_train)

# CORRELACION DE DIABETES CON LAS DEMAS VARIABLES

diabetes <- df_train %>% correlate() %>% stretch()
View(diabetes)
# MODELO 1: REGRESION LINEAL MULTIVARIADA

modelo1 <- lm(data=df_train, diabetes ~ .)
summary(modelo1)

df_test$predicted_values <- predict(object = modelo1, newdata = df_test)
df_test$predicted_values <- ifelse(df_test$predicted_values > 0.5, 1, 0)

# Métrica #1: Matriz de confusión 
confusionMatrix(as.factor(df_test$diabetes), as.factor(df_test$predicted_values))

# Métrica #2: Recall 
recall(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #3: Precisión 
precision(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #4: F1-score 
F_meas(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

#accuracy: 0.76
#recall: 0.74
#precision: 0.52
#f1 score:0.61

# MODELO 2: REGRESION LOGISTICA

modelo2 <- glm(data=df_train, diabetes ~ ., family = binomial)
summary(modelo2)

df_test$predicted_values <- predict(modelo2, newdata = df_test, type = 'response')
df_test$predicted_values <- ifelse(df_test$predicted_values > 0.5, 1, 0)

# Métrica #1: Matriz de confusión 
confusionMatrix(as.factor(df_test$diabetes), as.factor(df_test$predicted_values))
df_status(df_test)
# Métrica #2: Recall 
recall(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #3: Precisión 
precision(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #4: F1-score 
F_meas(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')


#accuracy: 0.77
#recall: 0.75
#precision: 0.55
#f1 score:0.63

# MODELO 3: RANDOM FOREST

modelo3 <- randomForest(data=df_train, as.factor(diabetes) ~ .)
summary(modelo3)

df_test$predicted_values <- predict(object = modelo3, newdata = df_test)


# Métrica #1: Matriz de confusión 
confusionMatrix(as.factor(df_test$diabetes), as.factor(df_test$predicted_values))

# Métrica #2: Recall 
recall(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #3: Precisión 
precision(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #4: F1-score 
F_meas(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

#accuracy: 0.77
#recall: 0.73
#precision: 0.55
#f1 score:0.63

###########################################################################################

# EN ESTOS MODELOS UTILIZAREMOS LOS PARAMETROS MAS IMPORTANTES Y SE COMPARARAN CON LOS MODELOS
# QUE TUVIERON EN CUENTA A TODAS LAS VARIABLES VERSUS DIABETES

# MODELO 1: REGRESION LINEAL MULTIVARIADA

modelo1 <- lm(data=df_train, diabetes ~ plasma + bmi + insulina + edad + num_embarazos +
                diabetes_pedigree)
summary(modelo1)

df_test$predicted_values <- predict(object = modelo1, newdata = df_test)
df_test$predicted_values <- ifelse(df_test$predicted_values > 0.5, 1, 0)

# Métrica #1: Matriz de confusión 
confusionMatrix(as.factor(df_test$diabetes), as.factor(df_test$predicted_values))

# Métrica #2: Recall 
recall(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #3: Precisión 
precision(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #4: F1-score 
F_meas(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

#accuracy: 0.77
#recall: 0.76
#precision: 0.53
#f1 score:0.62

# MODELO 2: REGRESION LOGISTICA

modelo2 <- glm(data=df_train, diabetes ~ plasma + bmi + insulina + edad + num_embarazos +
                 diabetes_pedigree, family = binomial)
summary(modelo2)

df_test$predicted_values <- predict(modelo2, newdata = df_test, type = 'response')
df_test$predicted_values <- ifelse(df_test$predicted_values > 0.5, 1, 0)

# Métrica #1: Matriz de confusión 
confusionMatrix(as.factor(df_test$diabetes), as.factor(df_test$predicted_values))
df_status(df_test)
# Métrica #2: Recall 
recall(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #3: Precisión 
precision(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #4: F1-score 
F_meas(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

#accuracy: 0.77
#recall: 0.73
#precision: 0.55
#f1 score:0.63

# MODELO 3: RANDOM FOREST

modelo3 <- randomForest(data=df_train, as.factor(diabetes) ~ plasma + bmi + insulina + edad + num_embarazos +
                          diabetes_pedigree)
summary(modelo3)

df_test$predicted_values <- predict(object = modelo3, newdata = df_test)


# Métrica #1: Matriz de confusión 
confusionMatrix(as.factor(df_test$diabetes), as.factor(df_test$predicted_values))

# Métrica #2: Recall 
recall(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #3: Precisión 
precision(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

# Métrica #4: F1-score 
F_meas(as.factor(df_test$diabetes), as.factor(df_test$predicted_values), relevant = '1')

#accuracy: 0.72
#recall: 0.61
#precision: 0.55
#f1 score:0.58