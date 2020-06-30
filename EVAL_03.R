#######################################################
# UNIVERSIDAD NACIONAL DE INGENIERÍA
# CENTRO DE TECNOLOGÍAS DE INFORMACIÓN Y COMUNICACIONES
# Elaborado por: Victor Lazo
# Evaluación Nro. 03
#########################################################

getwd()
setwd(dir = "c:/Users/Victor Lazo/Desktop/R4DS/06 Class/Evaluacion/EVAL_03/")
rm(list=ls())
dir()

# Evaluacion de la 6ta Clase
#### Pregunta 1 ####

data("iris")
datos <- as.data.frame(iris)

##  Realice un analisis descriptivo del data frame iris (Libreria datasets) 
##  por especie : histograma, boxplot , diagrama de dispersion [12 graficos]
##  y en conjunto para las 3 especies : histograma, boxplot , diagrama de dispersion [4 graficos]

head(datos,5)
tail(datos)

# Verificando que no exista algún NA en la data
sapply(datos, function(x) sum(is.na(x)))

# Tipo de datos
class(datos)
str(datos)

# Resumen de información en la data frame - Min., 1st Qu., Median, Mean, 3rd Qu., Max.
summary(datos)

# Matriz de correlación
corr <- cor(datos[,1:4])
round(corr,3)

# Resumen de información en la data frame por especie - Min., 1st Qu., Median, Mean, 3rd Qu., Max.
by(datos, datos$Species,summary)

irisSet <- subset(datos, Species == "setosa")
irisVer<- subset(datos, Species == "versicolor")
irisVir <- subset(datos, Species == "virginica")

# Histograma de Sepal.Length de Iris Setosa, Iris Versicolor, Iris Virginica
x11()
par(mfrow=c(1,3))
hist(irisSet$Sepal.Length,ylim=c(0,20),col="yellow")
hist(irisVer$Sepal.Length,ylim=c(0,20),col="springgreen")
hist(irisVir$Sepal.Length,ylim=c(0,20),col="red")

# Histograma de Sepal.Width de Iris Setosa, Iris Versicolor, Iris Virginica
x11()
par(mfrow=c(1,3))
hist(irisSet$Sepal.Width,ylim=c(0,30),col="yellow")
hist(irisVer$Sepal.Width,ylim=c(0,30),col="springgreen")
hist(irisVir$Sepal.Width,ylim=c(0,30),col="red")

# Histograma de Petal.Length de Iris Setosa, Iris Versicolor, Iris Virginica
x11()
par(mfrow=c(1,3))
hist(irisSet$Petal.Length,ylim=c(0,20),col="yellow")
hist(irisVer$Petal.Length,ylim=c(0,20),col="springgreen")
hist(irisVir$Petal.Length,ylim=c(0,20),col="red")

# Histograma de Petal.Width de Iris Setosa, Iris Versicolor, Iris Virginica
x11()
par(mfrow=c(1,3))
hist(irisSet$Petal.Width,ylim=c(0,35),col="yellow")
hist(irisVer$Petal.Width,ylim=c(0,35),col="springgreen")
hist(irisVir$Petal.Width,ylim=c(0,35),col="red")

# Boxplot para cada tipo de iris
x11()
par(mfrow=c(1,3),mar=c(6,4,2,1))
boxplot(irisSet[,1:4], main="Setosa",ylim = c(0,8),las=2,col=rainbow(4))
boxplot(irisVer[,1:4], main="Versicolor",ylim = c(0,8),las=2,col=rainbow(4))
boxplot(irisVir[,1:4], main="Virginica",ylim = c(0,8),las=2,col=rainbow(4))

# Diagrama de dispersion de Iris Setosa
x11()
par(mfrow=c(3,3))
plot(irisSet$Sepal.Length, irisSet$Sepal.Width, pch=21,bg="green")
plot(irisSet$Sepal.Length, irisSet$Petal.Length, pch=21,bg="red")
plot(irisSet$Sepal.Length, irisSet$Petal.Width, pch=21,bg="yellow")
plot(irisSet$Sepal.Width, irisSet$Petal.Length, pch=21,bg="pink")
plot(irisSet$Sepal.Width, irisSet$Petal.Width, pch=21,bg="skyblue")
plot(irisSet$Petal.Length, irisSet$Petal.Width, pch=21,bg="peru")
mtext("Iris Setosa", side=3, outer=TRUE, line=-2)


# Diagrama de dispersion de Iris Versicolor
x11()
par(mfrow=c(3,3))
plot(irisVer$Sepal.Length, irisVer$Sepal.Width, pch=21,bg="green")
plot(irisVer$Sepal.Length, irisVer$Petal.Length, pch=21,bg="red")
plot(irisVer$Sepal.Length, irisVer$Petal.Width, pch=21,bg="yellow")
plot(irisVer$Sepal.Width, irisVer$Petal.Length, pch=21,bg="pink")
plot(irisVer$Sepal.Width, irisVer$Petal.Width, pch=21,bg="skyblue")
plot(irisVer$Petal.Length, irisVer$Petal.Width, pch=21,bg="peru")
mtext("Iris Versicolor", side=3, outer=TRUE, line=-2)

# Diagrama de dispersion de Iris Virginica
x11()
par(mfrow=c(3,3))
plot(irisVir$Sepal.Length, irisVir$Sepal.Width, pch=21,bg="green")
plot(irisVir$Sepal.Length, irisVir$Petal.Length, pch=21,bg="red")
plot(irisVir$Sepal.Length, irisVir$Petal.Width, pch=21,bg="yellow")
plot(irisVir$Sepal.Width, irisVir$Petal.Length, pch=21,bg="pink")
plot(irisVir$Sepal.Width, irisVir$Petal.Width, pch=21,bg="skyblue")
plot(irisVir$Petal.Length, irisVir$Petal.Width, pch=21,bg="peru")
mtext("Iris Virginica", side=3, outer=TRUE, line=-2)

# Histograma para las 3 especies
library(dplyr)
library(gridExtra)
H_SL <- datos %>% 
  ggplot(aes(Sepal.Length))+
  geom_histogram(aes(fill = Species), binwidth =0.2, col = "black")+
  labs(x = "Sepal Length (cm)", y = "Frecuencia")+
  theme(legend.position = "none")

H_SW <- datos %>% 
  ggplot(aes(Sepal.Width))+
  geom_histogram(aes(fill = Species), binwidth =0.2, col = "black")+
  labs(x = "Sepal Width (cm)", y = "Frecuencia")+
  theme(legend.position = "none")

H_PL <- datos %>% 
  ggplot(aes(Petal.Length))+
  geom_histogram(aes(fill = Species), binwidth =0.2, col = "black")+
  labs(x = "Petal Length (cm)", y = "Frecuencia")+
  theme(legend.position = "none")

H_PW <- datos %>% 
  ggplot(aes(Petal.Width))+
  geom_histogram(aes(fill = Species), binwidth =0.2, col = "black")+
  labs(x = "Petal Width (cm)", y = "Frecuencia")+
  theme(legend.position = "right")

grid.arrange(H_SL,H_SW,H_PL,H_PW, nrow=2, top = textGrob("Histograma Iris"))

# BoxPlot para las 3 especies

x11()
par(mfrow=c(2,2))
boxplot(datos$Sepal.Length~datos$Species,main="Sepal.Length",col=rainbow(3))
boxplot(datos$Sepal.Width~datos$Species,main="Sepal.Width",col=rainbow(3))
boxplot(datos$Petal.Length~datos$Species,main="Petal.Length",col=rainbow(3))
boxplot(datos$Petal.Width~datos$Species,main="Petal.Width",col=rainbow(3))

# Diagramas de dispersión de las 3 especies
x11()
pairs(datos[,1:4],col=datos[,5],oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.85,0.6, as.vector(unique(datos$Species)),fill=c(1,2,3))


#### Pregunta 2 ####
##  Contruya un arbol de regresion para el data frame iris (Libreria datasets)  y genere las predicciones 
##  para el siguiente data frame :
NuevaEspecie <- data.frame(
  Sepal.Length = 6.5, Sepal.Width = 3.0,
  Petal.Length = 5.2, Petal.Width = 2.0
)

str(datos)
cor(datos[,1:4])
library(corrplot)
corrplot(cor(datos[,1:4]))

mod_iris <- rpart(Species ~ ., data=datos)
class(mod_iris)
typeof(mod_iris)
mod_iris$splits
mod_iris
library(rpart)

predicciones_iris <- predict(mod_iris,newdata=NuevaEspecie, type="prob")
View(predicciones_iris)
# Tiene la probabilidad de 0.9782609 = 98% de ser Virginica




#### Pregunta 3 ####
## Cree un arbol de clasificacion para predecir la variable "diabetes" en los siguientes dos escenarios
## Escenario 1 : no considere la variable "pedigree" 
## Escenario 2 : no considere la variable "glucose" 
## Para cada uno de los escenario, genere nuevos data frames para predecir sus nuevos resultados 
## (predecir la variable "diabetes" cuyos posibles resultados son pos-neg)
library(mlbench)
data("PimaIndiansDiabetes2")

data_diabetes <- PimaIndiansDiabetes2

# Escenario 1 : no considere la variable "pedigree"

datos_diabetes_1 <- data_diabetes[,-7]
datos_diabetes_1 <- na.omit(datos_diabetes_1)
View(datos_diabetes_1)

mod_dia <- rpart(diabetes ~ ., data=datos_diabetes_1)
class(mod_dia)
typeof(mod_dia)
mod_dia

rpart.plot(mod_dia)
prp(mod_dia)
dev.off()

diabetes_level1 <- datos_diabetes_1[datos_diabetes_1$glucose>= 128,]
diabetes_level2 <- diabetes_level1[diabetes_level1$glucose < 166,]
diabetes_level3 <- diabetes_level2[diabetes_level2$age>=24,]
diabetes_level4 <- diabetes_level3[diabetes_level3$mass>=26,]
diabetes_level5 <- diabetes_level4[diabetes_level4$pregnant<8,]
diabetes_level6 <- diabetes_level5[diabetes_level5$pregnant>=4,]
diabetes_level7 <- diabetes_level6[diabetes_level6$pressure<76,]

View(diabetes_level7)

# Nuevo dataframe
NuevoDiabetes <- data.frame(
  pregnant = 15, glucose = 170, pressure=27, triceps= 34,
  insulin= 128, mass = 38, age=25) 


# Utilizamos la funcion predict y nuestro modelo mod_diabetes_sp
prediccion_diabetes <- predict(mod_dia,newdata=NuevoDiabetes,type = "prob")
# neg       pos
# 1 0.1086957 0.8913043
# Observamos que tenemos un 89 % de que el diagnostico sea POSITIVO


# Escenario 2 : no considere la variable "glucose"

datos_diabetes_2 <- data_diabetes[,-2]
datos_diabetes_2 <- na.omit(datos_diabetes_2)
View(datos_diabetes_2)

mod_dia <- rpart(diabetes ~ ., data=datos_diabetes_2)
class(mod_dia)
typeof(mod_dia)
mod_dia

rpart.plot(mod_dia)
prp(mod_dia)
dev.off()

diabetes_level1 <- datos_diabetes_2[datos_diabetes_2$insulin>=121,]
diabetes_level2 <- diabetes_level1[diabetes_level1$age > 29,]
diabetes_level3 <- diabetes_level2[diabetes_level2$pressure<77,]
diabetes_level4 <- diabetes_level3[diabetes_level3$pedigree>=0.33,]
diabetes_level5 <- diabetes_level4[diabetes_level4$pregnant<8,]
diabetes_level6 <- diabetes_level5[diabetes_level5$pregnant<8,]
diabetes_level7 <- diabetes_level6[diabetes_level6$triceps>=24,]

View(diabetes_level7)

# Nuevo dataframe
NuevoDiabetes2 <- data.frame(
  pregnant = 15, pedigree = 10, pressure=27, triceps= 34,
  insulin= 128, mass = 38, age=25) 


# Utilizamos la funcion predict y nuestro modelo mod_diabetes_sp
prediccion_diabetes <- predict(mod_dia,newdata=NuevoDiabetes2,type = "prob")
# neg       pos
# 1 0.25 0.75
# Observamos que tenemos un 75 % de que el diagnostico sea POSITIVO

#### Pregunta 4 ####
## Construta un arbol de regresion para el data frame Boston (libreria MASS) y genere las predicciones
## para los siguientes dos escenadios 
Prueba1 <-c(1 ,3, 9,12, 14,15,  32,  36,  45 , 59 , 66  ,94 , 95 ,130 ,146, 149 ,171, 188 ,193, 194, 209,
            210 ,218 ,227, 237 ,241, 255, 277,304 ,308 ,316 ,320, 334 ,349,366 ,367 ,371 ,378, 393 ,401 ,
            406 ,422, 423 ,453 ,455 ,485, 496, 505)
Prueba2 <- sample(primes::generate_primes(min=200,max = 506),size = 25)
Escenario1 <- Boston[Prueba1,-14] # dataframe de prueba 1
Escenario2 <- Boston[Prueba2,-14] # dataframe de prueba 2
#   Deacuerdo al arbol de regresion y a los resultados obtenidos usando los data frames de prueba :  Escenario1 y 
#   Escenario2, diga usted con cual de estos dos dataframes de prueba le fue "mejor" a las predicciones objtenidas
#   Sustente su respuesta a esta pregunta con un indicador cuantitativo y con un grafico.


library(rpart)
library(MASS)
data(Boston)

Prueba1 <-c(1 ,3, 9,12, 14,15,  32,  36,  45 , 59 , 66  ,94 , 95 ,130 ,146, 149 ,171, 188 ,193, 194, 209,
            210 ,218 ,227, 237 ,241, 255, 277,304 ,308 ,316 ,320, 334 ,349,366 ,367 ,371 ,378, 393 ,401 ,
            406 ,422, 423 ,453 ,455 ,485, 496, 505)
Prueba2 <- sample(primes::generate_primes(min=200,max = 506),size = 25)
Escenario1 <- Boston[Prueba1,-14] # dataframe de prueba 1
Escenario2 <- Boston[Prueba2,-14] # dataframe de prueba 2


# Escenario 01

mod1 <- rpart(medv ~ ., data=Boston)
class(mod1)
typeof(mod1)
mod1$splits
mod1
# n= 506 
# 
# node), split, n, deviance, yval
# * denotes terminal node
# 
# 1) root 506 42716.3000 22.53281  
# 2) rm< 6.941 430 17317.3200 19.93372  
# 4) lstat>=14.4 175  3373.2510 14.95600  
# 8) crim>=6.99237 74  1085.9050 11.97838 *
#   9) crim< 6.99237 101  1150.5370 17.13762 *
#   5) lstat< 14.4 255  6632.2170 23.34980  
# 10) dis>=1.5511 248  3658.3930 22.93629  
# 20) rm< 6.543 193  1589.8140 21.65648 *
#   21) rm>=6.543 55   643.1691 27.42727 *
#   11) dis< 1.5511 7  1429.0200 38.00000 *
#   3) rm>=6.941 76  6059.4190 37.23816  
# 6) rm< 7.437 46  1899.6120 32.11304  
# 12) lstat>=9.65 7   432.9971 23.05714 *
#   13) lstat< 9.65 39   789.5123 33.73846 *
#   7) rm>=7.437 30  1098.8500 45.09667 *

# Visualizacion del arbol mod1: prp [librerua rpart.plot]
library(rpart.plot)
rpart.plot(mod1)
prp(mod1)
dev.off()

datos_level1 <- Boston[Boston$rm<6.9,]
datos_level2 <- datos_level1[datos_level1$lstat<14,]
datos_level3 <- datos_level2[datos_level2$dis>=1.6,]
datos_level4 <- datos_level3[datos_level3$rm<6.5,]

mean(datos_level4$medv)
# [1] 21.73908

# Predicciones
predicciones_datos_1 <- predict(mod1,newdata=Escenario1)
View(predicciones_datos_1) # 48 filas


# Coeficiente de Variacion
coef_1 <- (sd(predicciones_datos_1)/mean(predicciones_datos_1))*100
# [1] 31.16449

plot(x = predicciones_datos_1, y = Escenario1$medv,
     main = "Modelo_Predicción vs Valor_Real", 
     xlab = "Predicción", 
     ylab = "Valor real", col = "green", pch = 19
)


#PREDICCIÓN ESCENARIO 2

predicciones_datos_2 <- predict(mod1,newdata=Escenario2)
View(predicciones_datos_2) # 48 filas

# Coeficiente de Variacion
coef_2 <- (sd(predicciones_datos_2)/mean(predicciones_datos_2))*100
# [1] 46.51619

plot(x = predicciones_datos_2, y = Escenario2$medv,
     main = "Modelo_Predicción vs Valor_Real", 
     xlab = "Predicción", 
     ylab = "Valor real", col = "red", pch = 19
)

coef_1

# coef_1 present menor dispersión por lo tanto, es la de mejor predicción




