library(tidyverse)
library(moments)

#Calculo de estadısticos descriptivos
col=colesterol[,1]
media=mean(col)
varianza=var(col)
desviacion_tipica=sd(col)
recorrido_intercuartilico=IQR(col)
rango=max(col)-min(col)
mediana=median(col)

#Estudio del tipo de distribucion de los datos
asimetria=skewness(col) #Asimetria entre -0.5 y 0-5, por tanto simetrica
curtosis=kurtosis(col) #Aproximadamente 3, aproximadamente normal
intervalo_intercuartilico=quantile(col)[c(2,4)] #50% de los valores están entre 198 y 246
cociente_intercuartilico=recorrido_intercuartilico/rango
valores_atipicos=col[col<intervalo_intercuartilico[1]-1.5*recorrido_intercuartilico|col>intervalo_intercuartilico[2]+1.5*recorrido_intercuartilico] #345
#Podemos asumir que sigue una distribucion normal
#La estimacion de la media poblacional es la media muestral: 221.6
#Como n=135, podemos asumir que la varianza poblacional es la varianza muestral: 1361

#Analisis grafico de la distribucion de los datos
hist(col, breaks=21, main="Histograma de colesterol", xlab="Niveles de colesterol (mg/dL)", ylab="Frecuencia")
#'Ventajas: Muestra la distribucion de los datos
#'Inconvenientes: No se pueden ver frecuencias conjuntas simultaneamente
#'Caracteristicas: Muestra una distribucion normal
#'Conclusion: Util
stem(col)
#'Ventajas: Muestra la distribucion de los datos y datos individuales
#'Inconvenientes: Sin eje Y
#'Caracteristicas: Muestra una distribucion normal
#'Conclusion: Util
boxplot(col)
#'Ventajas: Muestra los cuartiles
#'Inconvenientes: Contiene poca informacion
#'Caracteristicas: Muestra un valor atipico
#'Conclusion: Util
marca_de_clase=14:34
slice=cut(col, breaks=seq(139, 349, 10))
slice=as.integer(slice)
slice=tabulate(slice)
names(slice)=marca_de_clase
pie(slice[slice>0])
#'Ventajas: Se pueden ver porcentajes
#'Inconvenientes: Hay demasiados categorias
#'Caracteristicas: Alrededor de 25% de los valores están entre 200 y 219 y 50% si incluimos hasta 249
#'Conclusion: Poco util

#Riesgo
riesgo=pnorm(200, media, desviacion_tipica, lower.tail = FALSE)
riesgo_alto=pnorm(240, media, desviacion_tipica, lower.tail = FALSE)
#Dado que un 31% de individuos tienen un riesgo alto, las observaciones son preocupantes desde un punto de vista medico