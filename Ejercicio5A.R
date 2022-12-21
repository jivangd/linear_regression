rm(list = ls(all.names = TRUE))
getwd()

Datos <- read.csv("Ex5.csv")
Datos$Trat <- as.factor(Datos$Trat)
str(Datos)
levels(Datos$Trat)
#Control será el nivel de referencia

#################I)
summary(Datos)
#Vemos que se respetaron las edades de 16 a 60
#200 pertenecen al grupo control y 100 al grupo que se les administro el medicamento
#Tenemos mínimo de 2 anticuerpos en alguna persona y 23.7 anticuerpos como máximo

#Realizamos un plot para visualizar y analizar los datos 
library(ggplot2)
ggplot(Datos , aes ( x = Edad , y = Ant , color = Trat ) ) + geom_point () 

#Podemos ver con este gráfico que a mayor edad, menor cantidad de anticuerpos
#Por otro lado se observa que el grupo control tiende a tener menos anticuerpos


############################ II)

#Tenemos al modelo
# E(Ant;Trat,Edad)= b0 + b1Med + b2Edad + b3*(Med*Edad)

#Ajustamos el modelo

fit = lm( Ant ~ Trat + Edad + Trat:Edad, data=Datos)
summary(fit)



################  III)

# a) el grupo control
# E(Ant;Trat=Control,Edad)= b0 + b2Edad

# b) el grupo que recibe el medicamento.
# E(Ant;Trat=Med,Edad)= b0 + b1 + b2Edad + b3Edad = b0+b1+(b2+b3)Edad

############### IV)
# Para ver si la Edad afecta de la misma forma los promedios de generación de 
#anticuerpos anteriores, buscamos que b2+b3=b2 o bien que b3=0

#Es decir que las pendientes sean iguales en ambas rectas entonces la prueba de 
#hipotesis se reduce a:

#H0: b3=0 vs Ha: b3!=0

K= matrix(c(0,0,0,1), ncol=4, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fit, linfct=K, rhs=m),test=Ftest())

#p-value=1.836e-07 <alpha=0.05
#Por lo tanto, podemos concluir que se rechaza H0 con una significancia de 0.05,es
#decir, no afecta de la misma forma al grupo control y al grupo que recibe el 
#medicamento, por lo tanto no se puede modelar con rectas paralelas



############################## V)
#Como si es necesario considerar las interacciones del modelo, las rectas no son
#paralelas y como ya vimos se ve que el medicamente ayuda a la generación de anticuerpos

# Interpretacion dde los coeficiones:

#b0: Representa el grupo control y en este paramatro por sí solo nos da una idea
#de la cantidad de anticuerpos en una persona que no recibe nada para el Covid

#b1: Representa al grupo que recibe medicamento y se espera que sea positivo
#y de hecho entre más grande mejor será la efectividad del medicamento para el Covid
#por otro lado valores negativos nos arrojaría una alarma de que hay algo mal
#en su formulación

#b2; Representa la pendiente de la edad con un valor negativo ya que a mayor edad
#menor cantidad de anticuerpos

#b3: Representa la interacción que hay entre el medicamento y la edad se espera
#que este valor sea positivo para que contrarreste los efectos de b2 y ayude en
#el aumento de anticuerpos, un valor cercano a 0 nos diría que el medicamento 
#no tiene efecto mayor efecto por la edad



###################### IV)


# Realizaremos esa comparacion, usaremos intervalos de confianza simultaneos

ggplot( Datos , aes ( x = Edad , y = Ant , color = Trat ) ) + geom_point () +
  stat_smooth( method =lm , se= TRUE , fullrange = TRUE , level = 0.95,
               aes( fill = Trat ) ) + theme_bw () +
  scale_colour_manual( values = c(" blue", " purple") )

#Con una confianza del 95%, como la recta que modela el grupo que recibió 
#el medicamento queda por arriba, no se traslapan las rectas ni los intervalos
#de confianza en el intervalo [30,60] se cumple que las personas de 30 a 
# 60 años que recibieron el medicamento, tienen mas anticuerpos.
#Por lo tanto, el medicamento funciona aumentando el número de anticuerpos
