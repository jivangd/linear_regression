rm(list = ls(all.names = TRUE))
getwd()

Data <- read.csv("Ex4A.csv")

Data$Sexo <- as.factor(Data$Sexo)
Data$Trat <- as.factor(Data$Trat)
summary(Data)

############## I)
levels(Data$Sexo)
#nivel de referencia "Hombre"

levels(Data$Trat)
#nivel de referencia "Control"

boxplot(Puntaje~Trat + Sexo , data= Data, col= "white", outline=FALSE)
#Con el tratamiento 1 para hombres vemos que hay mucha variabilidad en su 
#desempeño, el tratamiento 2 para hombres baja visibilimente la ansiedad,
#En general si se ven mejoras en promedio con el uso de cualquier tratamiento 
#como era de esperarse.

############## II)
# E(puntaje;sexo, trat)= b0 + b1 Trat1 + b2Trat2 + b3Mujer + b4(Trat1*Mujer) 
#+ b5 (Trat2*Mujer)

#Ajuste del modelo con interacción
fit=lm(Puntaje~Trat+Sexo+Trat:Sexo, data=Data)
summary(fit)
#Nivel de referencia Hombre;Control

#Hombre
#E(Puntaje;Trat=Control, Sexo=Hombre)= b0  = 10.1530
#E(Puntaje;Trat=Trat1, Sexo=Hombre)= b0 + b1 =  10.1530 - 1.8453 = 8.3077
#E(Puntaje;Trat=Trat2, Sexo=Hombre)= b0 + b2 = 10.1530 - 3.9427 = 6.2103

#Mujer
#E(Puntaje;Trat=Control,Sexo=Mujer)= b0 + b3 = 10.1530 + 0.3112 = 10.4642
#E(Puntaje;Trat=Trat1, Sexo=Mujer)= b0 + b1 + b3 + b4 = 
#10.4642 - 1.8453 - 0.3023 = 8.3166
#E(Puntaje;Trat=Trat2, Sexo=Mujer)= b0 + b2 + b3 + b5 = 10.4642 - 3.9427+ 3.5800
#= 10.1015

######################III)
#La hipótesis que se contrastan en la tabla ANOVA:

# H0: B1=B2=B3=B4=B5=0 vs Ha: B0!=0 o B1!=0 o B2!=0 o B3!=0 o B4!=0 o B5!=0
summary(fit)
## Se rechaza H0 pues p_value = 1.291e-08 < 0.05

#################IV)
#La hipótesis es 
#H0:
#E(Puntaje;Trat=Control, Sexo=Hombre)= E(Puntaje;Trat=Control,Sexo=Mujer)
#E(Puntaje;Trat=Trat1, Sexo=Hombre)= E(Puntaje;Trat=Trat1, Sexo=Mujer)
#E(Puntaje;Trat=Trat2, Sexo=Hombre)= E(Puntaje;Trat=Trat2, Sexo=Hombre)
#O bien
#b0 = b0 + b3
#b0 + b1 =  b0 + b1 + b3 + b4
#b0 + b2 = b0 + b2 + b3 + b5

##Simplificando y eliminando redundancia
#HO: b3=0, b4=0 y b5=0 vs Ha: b3!=0 o b4!=0 o b5!=0

library(multcomp)
K=matrix(c(0,0,0,1,0,0,
           0,0,0,0,1,0,
           0,0,0,0,0,1), ncol=6, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())

#Se rechaza H0 con una significancia de 0.025 pues p-value = 4.29e-06 < 0.025

#Ahora consideremos una prueba simultánea que ayude a identificar para qué 
#tratamiento se puede considerar que el sexo tiene un efecto

#Se incluyen las tres hipótesis individuales asociadas al sexo de las personas
#las cuáles ahora no elimanaremos redundancias

# H0_1: b3=0
# H0_2: b3+b4=0
# H0_3: b3+b5=0

K=matrix(c(0,0,0,1,0,0,
           0,0,0,1,1,0,
           0,0,0,1,0,1), ncol=6, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fit, linfct=K, rhs=m))

#Tenemos los siguientes p.values
#1 0.959 > 0.025
#2 1.000 > 0.025
#3 <1e-05 < 00.25 
#asociados respectivamente a las hipótesis nulas mencionadas anteriormente

#Por lo que se tiene evidencia a favor de que b3=0 y b3+b4=0 o bien b3=b4=0
#y se tiene evidencia en contra de que b3+b5=0 i.e. b5!=0 por lo que seguiremos 
#considerando esta estimación

#Podemos reducir el modelo de la siguiente maner
## E(puntaje;sexo, trat)= b0 + b1 Trat1 + b2Trat2 + b5 (Trat2*Mujer)


##############V)
fitred <- lm(Puntaje ~ Trat + I((Trat=="Trat2")*(Sexo=="Mujer")), data = Data)
summary(fitred)
#Hombre
#E(Puntaje;Trat=Control, Sexo=Hombre)= b0  = 10.3086 
#E(Puntaje;Trat=Trat1, Sexo=Hombre)= b0 + b1 =  10.3086 - 1.9965  = 8.3121
#E(Puntaje;Trat=Trat2, Sexo=Hombre)= b0 + b2 = 10.3086 - 4.0983 = 6.2103

#Mujer
#E(Puntaje;Trat=Control,Sexo=Mujer)=b0= 10.3086 
#E(Puntaje;Trat=Trat1, Sexo=Mujer)=b0+b1  = 8.3121
#E(Puntaje;Trat=Trat2, Sexo=Mujer)=b0+b2+b5 = 10.3086 - 4.0983 + 3.8913 = 10.1016

##############VI)
#Para que ver si el nuevo tratamiento (Trat2) es mejor que los demás debemos 
#considerar la siguiente hipótesis
#1) E(puntaje; Trat=Control,Sexo=Hombre)= b0  > E(puntaje; Trat=Trat2,Sexo=Hombre)=b0+b2
#2) E(puntaje; Trat=Control,Sexo=Mujer)= b0 > E(puntaje; Trat=Trat2, Sexo=Mujer)= b0+b2+b5
#3) E(puntaje; Trat=Trat1,Sexo=Hombre)= b0+b1  > E(puntaje; Trat=Trat2,Sexo=Hombre)=b0+b2
#Y 4) E(puntaje; Trat=Trat,Sexo=Mujer)= b0+b1 > E(puntaje; Trat=Trat2, Sexo=Mujer)= b0+b2+b5
#ya que a menores valores mejor es el efecto del tratamiento

#Se puede reducir a 
#b2<0
#b2+b5<0
#b2-b1<0
#b2+b5-b1<0

#Entonces las hipótesis nulas quedan:
#Ho1: 0 <= b2 
#Ho2: 0 <= b2 + b5
#Ho3: 0 <= b2 + b1
#Ho4: 0 <= b2 + b5 - b1 
K=matrix(c(0,0,1,0,0,0,
           0,0,1,0,0,1,
           0,-1,1,0,0,0,
           0,-1,1,0,0,1), ncol=6, nrow=4, byrow=TRUE)
m=c(0,0,0,0)
summary(glht(fit, linfct=K, rhs=m, alternative="less"))
#Tenemos los siguientes p-values
#1 < 0.001 < 0.05 Se Rechaza H0 entonces se tiene evidencia a favor del nuevo tratamiento es mejor
#2 0.66111 > 0.05 No se rechaza H0 entonces se tiene evidencia en contra de que el nuevo tratamiento es mejor
#3 0.00586 < 0.05 ...
#4 0.99947 > 0.05 ...
#Notemos que la evidencia a favor se encuentra en la población de hombres
#mientras la evidencia en contra se encuentra en la población de mujeres


#################VII)
#Para que ver si el nuevo tratamiento (Trat2) es mejor en hombres
#aunque el tratamiento actual es mejor en mujeres
#consideramos la siguiente hipótesis
#1) E(puntaje; Trat=Control,Sexo=Hombre)= b0  > E(puntaje; Trat=Trat2,Sexo=Hombre)=b0+b2
#2) E(puntaje; Trat=Trat1,Sexo=Hombre)= b0+b1 > E(puntaje; Trat=Trat2, Sexo= Hombre)= b0+b2

#3) E(puntaje; Trat=Control,Sexo=Mujer)= b0  > E(puntaje; Trat=Trat1,Sexo=Mujer)=b0+b1
#Y 4) E(puntaje; Trat=Trat2,Sexo=Mujer)= b0+b2+b5 > E(puntaje; Trat=Trat1, Sexo=Mujer)= b0+b1

#O bien se tiene que:
#b2<0
#b2-b1<0
#b1<0
#b1-b2-b5<0

#Entonces las hipótesis nulas quedan:
#Ho1: 0 <= b2 
#Ho2: 0 <= b2 - b1
#Ho3: 0 <= b1
#Ho4: 0 <= b1 - b2 - b5 
K=matrix(c(0,0,1,0,0,0,
           0,-1,1,0,0,0,
           0,1,0,0,0,0,
           0,1,-1,0,0,-1), ncol=6, nrow=4, byrow=TRUE)
m=c(0,0,0,0)
summary(glht(fit, linfct=K, rhs=m, alternative="less"))
#Tenemos los siguientes p-values
#1 < 0.001 < 0.05 
#2 0.00603 < 0.05 
#3 0.01675 < 0.05 
#4 0.20080 > 0.05 

#Como se rechazan los primeras dos hipótesis si podemos decir que hay suficiente
#evidencia a favor de que el tratamiento 2 o nuevo tratamiento tiene el
#mejor desempeño en hombres

#Como no se rechaza la última hipótesis hay evidencia en contra de que el tratamiento
#1 o actual no es el que tiene el mejor desempeño en mujeres. Sin embargo, si 
#hay evidencia a favor de que es mejor comparado con el control o es decir es mejor
#para la ansiedad el tratamiento actual que no hacer nada.
