#'Analisis Descriptivo para una variable
#'
#'More detailed description.
#'author, CJG-
#'
#' @param dataset as dataframe
#' @param vd as numeric vector, dependent Variable
#' @param vi as numeric vector, independent Variable.
#'
#' @return Mean, median, sd, Kurtosis, Skewness, P25, P75, IQR, Min y Max boxplot.
#'
#'
#'
#'
#'
#'@export


# retorna un data frame contenedor de
# la descripcion estadistica por grupos


library (tidyverse)
library(ggridges)
library (PerformanceAnalytics)
library (psych)


descripYxG<- function(dataset, vd, vi)
{
  options(warn = -1)
  ni.e <- tapply(vd, vi, length)
  ng.e <- length(ni.e)

  y.e<-list()

  sum3.e<-1:ng.e
  sum4.e<-1:ng.e
  trat.e<-1:ng.e
  ds.p.e<-1:ng.e
  nbarr.e<-1:ng.e
  aux01<-0

  
  for (i in 1:ng.e){
    sum3.e[i]=0
    sum4.e[i]=0
    aux01=aux01+ni.e[i]
    trat.e[i]=vi[aux01]
    nbarr.e[i]
  }

  p25.e <- 1:ng.e
  p75.e <- 1:ng.e
  iqr.e <- 1:ng.e
  curt.e<- 1:ng.e
  asim.e<- 1:ng.e

  prom.e = tapply(vd, vi, mean)
  mediana.e = tapply(vd, vi, median)

  var.e  = tapply(vd, vi, var)
  ds.e   = tapply(vd, vi, sd)
##############################
#  for (i in 1 :  ng.e){
#    y.e<-subset(vd,vi==trat.e[i])
#    ds.p.e[i]<-sqrt(sd(y.e)^2*(ni.e[i]-1)/ni.e[i])

###########################    
  #  for (j in 1 : ni.e[i]){
  #    sum3.e[i]=sum3.e[i]+(y.e[j]-prom.e[i])^3
  #    sum4.e[i]=sum4.e[i]+(y.e[j]-prom.e[i])^4
  #  }
  #  curt.e[i]<-sum4.e[i]/(ni.e[i]*ds.p.e[i]^4)-3
  #  asim.e[i]<-sum3.e[i]/(ni.e[i]*ds.p.e[i]^3)

  #}
  
  curt.e =tapply(vd,vi, kurtosis)
  asim.e =tapply(vd,vi, skew)

  min.e = tapply(vd, vi, min)
  max.e = tapply(vd, vi, max)


  perct.e <- tapply(vd, vi, quantile)
  for (i in 1:ng.e) {
    p25.e[i] = perct.e[[i]][[2]]
    p75.e[i] = perct.e[[i]][[4]]
    iqr.e[i] = p75.e[i] - p25.e[i]
  }



  info.grupo <- data.frame(matrix(nrow = ng.e, ncol = 11))
  names(info.grupo) <- c("ni","Promedio","Mediana","Desv.Estd","Curtosis","Asimetria", "Min","Max","P25","P75","IQR")

  for (i in 1:ng.e) {
    n         = format(round(ni.e[i],0),nsmall = 0)
    promedio  = format(round(prom.e[i],4),nsmall = 4)
    mediana   = format(round(mediana.e[i],4),nsmall = 4)

    de        = format(round(ds.e[i],4),nsmall = 4)
    curtosis  = format(round(curt.e[i],4),nsmall = 4)
    asimetria = format(round(asim.e[i],4),nsmall = 4)
    minimo    = format(round(min.e[i],4),nsmall = 4)
    maximo    = format(round(max.e[i],4),nsmall = 4)
    P25       = format(round(p25.e[i],4),nsmall = 4)
    P75       = format(round(p75.e[i],4),nsmall = 4)
    IQR       = format(round(iqr.e[i],4),nsmall = 4)
    info.grupo[i,] <- c(n,promedio,mediana,de,curtosis,asimetria,minimo,maximo,P25,P75,IQR)
  }



  bp<-ggplot(data = dataset,aes(x=factor(vi),y=vd))+
  geom_boxplot(color = 'darkslategray', fill = 'steelblue') +
    xlab("") +
    ylab("") +
    ggtitle("")
print (bp)


#  bd <- ggplot(data=dataset,aes(x= vd,fill=as.factor(vi)))+
#    geom_density(alpha=0.5)+
#    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
#    theme_minimal(12)

#  print (bd)
  
  bd1 <- ggplot(data=dataset,aes(x= vd,y=as.factor(vi),fill=as.factor(vi)))+
    geom_density_ridges2()

  
  print (bd1)
  
  

  return(info.grupo)

}

