#'Analisis Descriptivo para una variable Dependiente con/sin una variable independiente
#'
#'
#'More detailed description.
#'author, CJG-
#'
#'
#' @param dataset as dataframe
#' @param vd as numeric vector, dependent Variable
#' @param vi as numeric vector, independent Variable (vi=NULL, if No exist)
#'
#' @returns Descriptive Analysis,
#'
#'
#' @examples
#'
#'
#' @export
#' 
#' Instrucciones 
#' si solo hace anaálisis de una variable dependiente (VD)
#' puede escribirlo 
#' 
#' 1- descripYG(dataset, vd=dataset$vd,vi=NULL)
#' 2- descripYG(dataset, dataset$vd,NULL)
#' 
#' 
#' si hara un analisis de la VD, de acuerdo a una independiente (VI)
#' 
#' 1- descripYG(dataset, vd=dataset$vd,vi=dataset$vi)
#' 2- descripYG(dataset, dataset$vd,dataset$vi)
#'
#'#' 
#' 
#' 
library(tidyverse)
library(ggridges)
library(PerformanceAnalytics)
library(psych)


descripYG<- function(dataset, vd, vi)
{
  
  # dataset con datos?
  if(nrow(dataset) == 0){
    stop ("el DATASET esta VACIO.")
  }else{
  
  options(warn = -1)
  
  # Verificar si hay NA en la variable de interés
  if (any(is.na(vd))) {
    stop("La variable de análisis tiene valores faltantes (NA).")
  }

  if (is.null(vi))
  {
    sum3<-0
    sum4<-0
    n.dy<-nrow(dataset)

    desvestp.dy<-sqrt((n.dy-1)*sd(vd)^2/n.dy)
    
    for (i in 1: n.dy){
      sum3<-sum3+(vd[i]-mean(vd))^3
      sum4<-sum4+(vd[i]-mean(vd))^4
    }
    curtosis.dy<-sum4/(n.dy*sd(vd)^4)-3
    asimetria.dy<-sum3/(n.dy*sd(vd)^3)
    
    IQR.dy<-quantile(vd,0.75)-quantile(vd,0.25)
    
    mint.dy <- quantile(vd,0.25)-1.5*IQR.dy
    maxt.dy <- quantile(vd,0.75)+1.5*IQR.dy
    
    
    
    info.dy<-data.frame(matrix(nrow = 1, ncol = 13))
    
    names(info.dy)<-c("n","promedio","mediana","desv.estd.m","curtosis","asimetria","min","max","p25","p75","iqr","bmin","bmax")
    
    info.dy[1,]<-c(n.dy, 
                   format(mean(vd), digits=2, nsmall=3),
                   format(median(vd), digits = 2, nsmall=3), 
                   format(sd(vd), digits = 2, nsmall=3),
                   format(curtosis.dy, digits = 2, nsmall=3), 
                   format(asimetria.dy, digits = 2, nsmall=3),
                   format(min(vd),digits  = 2, nsmall=3), 
                   format(max(vd),digits  = 2, nsmall=3),
                   format(quantile(vd,0.25),digits  = 2, nsmall=3), 
                   format(quantile(vd,0.75),digits  = 2, nsmall=3),
                   format(IQR.dy,digits  = 2, nsmall=3), 
                   format(mint.dy,digits = 2, nsmall=3),
                   format(maxt.dy,digits = 2,nsmall=3))
    
    barras<-trunc(3.322*log10(n.dy)+1)
    
    
    g1<-ggplot(data = dataset, aes(x = vd)) +
      geom_histogram(color = 'darkslategray', fill = 'steelblue',bins = barras) +
      xlab("") +
      ylab("Frecuencia") +
      ggtitle("")
    
    g2<-ggplot(data = dataset,aes(x=vd))+
      geom_boxplot()
    
    # Calcular la media y el error estándar para cada nivel de la variable dicotómica
    resumen <- datos %>%
      
      summarise(
        media = mean(y),
        sd = sd(y)
      )
    
    
    # Crear el gráfico de barras con barras de error
    #g3<- ggplot(resumen, aes(x=1, y = media)) +
    #  geom_bar(stat = "identity", position = "dodge", fill = "lightblue", color = "black") +
    #  geom_errorbar(aes(ymin = media - sd, ymax = media + sd), position = position_dodge(width = 0.8), width = 0.25) +
    #  labs(x = "g", y = "y") +
    #  theme_minimal()
    
    
    print(g1)
    print(g2)
    #print(g3)
    
    
    #infoFinal.dy<-data.frame(nombre,info.dy)
    return(info.dy)
######################################
######################################
######################################
    
      }else {
        
######################################
######################################
######################################
    
    ### Calculo por grupo definido por la VI
        
    options(warn = -1)
    ni.e <- tapply(vd, dataset[[vi]], length)
    ng.e <- length(ni.e)
    grp.e <-sort(unique(dataset[[vi]]))
    
    y.e<-list()
    
    sum3.e<-1:ng.e
    sum4.e<-1:ng.e
    trat.e<-1:ng.e
    ds.p.e<-1:ng.e
    nbarr.e<-1:ng.e
    aux01<-0
    
    
    for (i in 1:ng.e){
      sum3.e[i]<-0
      sum4.e[i]<-0
      aux01<-aux01+ni.e[i]
      trat.e[i]<-dataset[[vi]][aux01]
      nbarr.e[i]
    }
    
    p25.e <- 1:ng.e
    p75.e <- 1:ng.e
    iqr.e <- 1:ng.e
    curt.e<- 1:ng.e
    asim.e<- 1:ng.e
    
    prom.e <- tapply(vd, dataset[[vi]], mean)
    mediana.e <- tapply(vd, dataset[[vi]], median)
    
    var.e  <- tapply(vd, dataset[[vi]], var)
    ds.e   <- tapply(vd, dataset[[vi]], sd)

    curt.e <-tapply(vd,dataset[[vi]], kurtosis)
    asim.e <-tapply(vd,dataset[[vi]], skew)
    
    min.e <- tapply(vd, dataset[[vi]], min)
    max.e <- tapply(vd, dataset[[vi]], max)
    
    
    perct.e <- tapply(vd, dataset[[vi]], quantile)
    for (i in 1:ng.e) {
      p25.e[i] <- perct.e[[i]][[2]]
      p75.e[i] <- perct.e[[i]][[4]]
      iqr.e[i] <- p75.e[i] - p25.e[i]
    }
    
    
    
    info.grupo <- data.frame(matrix(nrow = ng.e, ncol = 12))
    names(info.grupo) <- c("dataset[[vi]]","ni","Promedio","Mediana","Desv.Estd","Curtosis","Asimetria", "Min","Max","P25","P75","IQR")
    
    for (i in 1:ng.e) {
      grp       = grp.e[i]
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
      info.grupo[i,] <- c(grp,n,promedio,mediana,de,curtosis,asimetria,minimo,maximo,P25,P75,IQR)
    }
    
    
    
    g1<-ggplot(data = dataset,aes(x=factor(dataset[[vi]]),y=vd))+
      geom_boxplot(color = 'darkslategray', fill = 'steelblue') +
      xlab("") +
      ylab("") +
      ggtitle("")
    print (g1)
    

    g2 <- ggplot(data=dataset,aes(x= vd,y=as.factor(dataset[[vi]]),fill=as.factor(dataset[[vi]])))+
      geom_density_ridges2()
    
    
    print (g2)
    
    # Calcular la media y el error estándar para cada nivel de la variable dicotómica
    resumen <- dataset %>%
      group_by(dataset$dataset[[vi]]) %>%
      summarise(
        media = mean(vd),
        sd = sd(vd)
      )
    
    
    # Crear el gráfico de barras con barras de error
   # g3<-ggplot(resumen, aes(x = factor(dataset[[vi]]), y = media)) +
  #    geom_bar(stat = "identity", position = "dodge", fill = "lightblue", color = "black") +
  #    geom_errorbar(aes(ymin = media - sd, ymax = media + sd), position = position_dodge(width = 0.8), width = 0.25) +
  #    labs(x = "dataset[[vi]]", y = "VD") +
   #   theme_minimal()
    
    #print(g3)
    
    return(info.grupo)
    
  }
  

  
  }  
}

