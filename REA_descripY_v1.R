#'Analisis Descriptivo para una variable
#'
#'More detailed description.
#'author, CJG-
#'
#'
#' @param dataset as dataframe
#' @param vd as numeric vector, dependent Variable
#'
#' @returns Descriptive Analysis, Univariate 1 VD
#'
#'
#' @examples
#'
#'
#'
#' @export
#' 
#' 
library(tidyverse)
library(PerformanceAnalytics)
library(psych)


descripY<-function(dataset,vd)
{
  sum3=0
  sum4=0
  n.dy<-nrow(dataset)
  min.dy<-min(vd)
  max.dy<-max(vd)
  
  # LIMPIAR ATIPICOS-
  
  #boxplot(datos$vd)
  
  #limpiando valores atipicos en funcion de BOXPLOT
  #1. identificar V.A. y dejarlos en un archivo "atipicos"
  #atipicos<-boxplot(datos$vd,plot=FALSE)$out
  #2. para un n suficientes eliminar desde la base crear un archivo con datos limpios
  #la coma puede depender de la cantidad de variables.
  
  #datos<-datos[-which(datos$vd %in% atipicos),] 
  
  


  promedio.dy<-mean(vd)
  mediana.dy<-median(vd)

  desvestm.dy<-sd(vd)
  desvestp.dy<-sqrt((n.dy-1)*sd(vd)^2/n.dy)

  for (i in 1: n.dy){
    sum3<-sum3+(vd[i]-promedio.dy)^3
    sum4<-sum4+(vd[i]-promedio.dy)^4
  }
  curtosis.dy<-sum4/(n.dy*desvestp.dy^4)-3
  asimetria.dy<-sum3/(n.dy*desvestp.dy^3)


  p25.dy<-quantile(vd,0.25)
  p75.dy<-quantile(vd,0.75)

  IQR.dy=p75.dy-p25.dy

  mint.dy <- p25.dy-1.5*IQR.dy
  maxt.dy <- p75.dy+1.5*IQR.dy


  
  info.dy<-data.frame(matrix(nrow = 1, ncol = 13))

  names(info.dy)<-c("n","promedio","mediana","desv.estd.m","curtosis","asimetria","min","max","p25","p75","iqr","bmin","bmax")

  info.dy[1,]<-c(n.dy, 
                 format(promedio.dy, digits=2, nsmall=3),
                 format(mediana.dy, digits = 2, nsmall=3), 
                 format(desvestm.dy, digits = 2, nsmall=3),
                 format(curtosis.dy, digits = 2, nsmall=3), 
                 format(asimetria.dy, digits = 2, nsmall=3),
                 format(min.dy,digits  = 2, nsmall=3), 
                 format(max.dy,digits  = 2, nsmall=3),
                 format(p25.dy,digits  = 2, nsmall=3), 
                 format(p75.dy,digits  = 2, nsmall=3),
                 format(IQR.dy,digits  = 2, nsmall=3), 
                 format(mint.dy,digits = 2, nsmall=3),
                 format(maxt.dy,digits = 2,nsmall=3))

  barras=trunc(3.322*log10(n.dy)+1)


  g1<-ggplot(data = dataset, aes(x = vd)) +
    geom_histogram(color = 'darkslategray', fill = 'steelblue',bins = barras) +
    xlab("") +
    ylab("Frecuencia") +
    ggtitle("")

  g2<-ggplot(data = dataset,aes(x=vd))+
    geom_boxplot()


  print(g1)
  print(g2)


  #infoFinal.dy<-data.frame(nombre,info.dy)
  return(info.dy)

}
descripY(datos,datos$vd)

