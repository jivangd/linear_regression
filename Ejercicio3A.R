rm(list = ls(all.names = TRUE))



y <- c(12,10,15,19,11,11,17,16,14,15,27,33,22,26,28,23,20,18,17)
x <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4)

Datos=data.frame(cbind(x,y))
Datos$x <- as.factor(Datos$x)
summary(Datos)

#########################I)
#Gráfico para describir los datos (boxplot)
boxplot(y ~ x, data = Datos, col = "white", outline=FALSE)
stripchart(x ~ y, data = Datos,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)
#########################II)

fit=lm(y ~ x, data = Datos)
summary(fit)
### El promedio de ventas del empaque 1 es E(y;empaque=1)=b0= 13.400
### El promedio de ventas del empaque 2 es E(y;empaque=2)=b0+b1= 13.4 +1.2 = 14.6
### El promedio de ventas del empaque 3 es E(y;empaque=3)=b0+b2= 13.4 +13.8 = 27.2
### El promedio de ventas del empaque 4 es E(y;empaque=4)=b0+b3= 13.4 + 6.1 = 19.5


################III
### Hiótesis que se contrastan con la tabla ANOVA
#H0: b1=0, b2=0, b3=0 vs Ha: b1 !=0  o b2!=0 o b3!=0
drop1(fit, test="F")
#Como se rechaza H0 pues p-value < 0.05 entonces nos dice que al menos una categoría
#o en este caso al menos un tipo empaque tiene una influencia en la cantidad de ventas 

################IV
#Recordemos que las betas es resultado de las diferencias de las esperanzas con el
#nivel de referencia:

#                      E(y;2)- E(y;referencia)= b0 + b1 - b0= b1
#                      E(y;3)- E(y;referencia)= b0 + b2 - b0= b2
#                      E(y;4)- E(y;referencia)= b0 + b3 - b0= b3

#De esta forma, consideraremos "las pruebas t" por renglon para las betas y las
#tomaremos del summary del modelo , ya establecido anteriormente
#Ademas, las pruebas a considerar son:
#      H0:b1=0 vs Ha: b1=!0
#      H0:b2=0 vs Ha: b2=!0
#      H0:b3=0 vs Ha: b3=!0
summary(fit)
  
# p-value=0.5677>0.05 para b1 
# p-value= 6.88e-06<0.05 para b2
# p-value= 0.0135<0.05 para b3

#Globalmente dado que al menos uno se rechazo (2) y con una significancia de 0.05, encontramos que existe
# se puede considerar que el tipo del paquete afecte las ventas promedio.

#V
#Realizar comparaciones por pares con la "Tukey"

summary(glht(fit, linfct= mcp(x ="Tukey")))

#Con los resultados que nos arroja summary, notamos que con una significancia de 
# aplha=0.05, encontramos evidencia de que existe diferencia en las ventas promedio
#entre los tipo de empaque  3 y 1, 3 y 2, 4 y 3


###VI
#Para esto basta con hacer una prueba simultánea
#Empaque 3 mejor que empaque 4
#b2-b3>0
#
#Empaque 3 mejor que empaque 2
#b2-b1>0
#y
#Empaque 3 mejor que empaque 1
#b2>0
#Revisar que la alternativa tenga la misma dirección
#Entonces Ho: b2-b3<=0, b2-b1<=0 y b2<=0
K=matrix(c(0,0,1,-1,
           0,-1,1,0,
           0,0,1,0), ncol=4, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fit, linfct=K, rhs=m, alternative="greater"))
### Como se rechaza H0 en las 3 pruebas con una significancia de alfa=0.05
#Se concluye que en efecto el empaque tipo 3 es el que aumenta las ventas en 
#comparación con el resto




