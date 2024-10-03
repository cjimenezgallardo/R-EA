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
library(moments)
library(patchwork)


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
    
    IQR.dy<-quantile(vd,0.75)-quantile(vd,0.25)
    
    mint.dy <- quantile(vd,0.25)-1.5*IQR.dy
    maxt.dy <- quantile(vd,0.75)+1.5*IQR.dy
    
    
    
    info.dy<-data.frame(matrix(nrow = 1, ncol = 13))
    
    names(info.dy)<-c("n","promedio","mediana","desv.estd.m","curtosis","asimetria","min","max","p25","p75","iqr","bmin","bmax")
    
    info.dy[1,]<-c(n.dy, 
                   format(mean(vd), digits=2, nsmall=3),
                   format(median(vd), digits = 2, nsmall=3), 
                   format(sd(vd), digits = 2, nsmall=3),
                   format(kurtosis(vd), digits = 2, nsmall=3), 
                   format(skewness(vd), digits = 2, nsmall=3),
                   format(min(vd),digits  = 2, nsmall=3), 
                   format(max(vd),digits  = 2, nsmall=3),
                   format(quantile(vd,0.25),digits  = 2, nsmall=3), 
                   format(quantile(vd,0.75),digits  = 2, nsmall=3),
                   format(IQR.dy,digits  = 2, nsmall=3), 
                   format(mint.dy,digits = 2, nsmall=3),
                   format(maxt.dy,digits = 2,nsmall=3))
    
    barras<-trunc(3.322*log10(n.dy)+1)
    
    
    g1<-ggplot(data = dataset, aes(x = vd)) +
      geom_histogram(color = 'white', fill = 'steelblue',bins = barras) +
      xlab("") +
      ylab("Frecuencia") +
      ggtitle("")+
      theme_classic()
    
    g2<-ggplot(dataset, aes(x = vd))+
      geom_boxplot(fill = "lightgreen",
                   width = 0.05)
      
    graficos <- g1 / g2 + plot_layout(heights = c(3, 1))
    
    print(graficos)
    
    

    
    #infoFinal.dy<-data.frame(nombre,info.dy)
    return(info.dy)

###################################################################   
###################################################################
####################################################################
      }else {
####################################################################
####################################################################
####################################################################
    
    ### Calculo por grupo definido por la VI
        
    options(warn = -1)
    ni.e <- tapply(vd, vi, length)
    ng.e <- length(ni.e)
    grp.e <-sort(unique(vi))
    
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
      trat.e[i]<-vi[aux01]
      nbarr.e[i]
    }
    
    p25.e <- 1:ng.e
    p75.e <- 1:ng.e
    iqr.e <- 1:ng.e
    curt.e<- 1:ng.e
    asim.e<- 1:ng.e
    
    prom.e <- tapply(vd, vi, mean)
    mediana.e <- tapply(vd, vi, median)
    
    var.e  <- tapply(vd, vi, var)
    ds.e   <- tapply(vd, vi, sd)

    curt.e <-tapply(vd,vi, kurtosis)
    asim.e <-tapply(vd,vi, skewness)
    
    min.e <- tapply(vd, vi, min)
    max.e <- tapply(vd, vi, max)
    
    
    perct.e <- tapply(vd, vi, quantile)
    for (i in 1:ng.e) {
      p25.e[i] <- perct.e[[i]][[2]]
      p75.e[i] <- perct.e[[i]][[4]]
      iqr.e[i] <- p75.e[i] - p25.e[i]
    }
    
    
    
    info.grupo <- data.frame(matrix(nrow = ng.e, ncol = 12))
    names(info.grupo) <- c("vi","ni","Promedio","Mediana","Desv.Estd","Curtosis","Asimetria", "Min","Max","P25","P75","IQR")
    
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
    
    
    bd1 <- ggplot(data=dataset,aes(x= vd,y=as.factor(vi),fill=as.factor(vi)))+
      geom_density_ridges2()
    print (bd1)    
    
    bp<-ggplot(data = dataset,aes(x=factor(vi),y=vd))+
      geom_boxplot(color = 'darkslategray', fill = 'steelblue') +
      xlab("") +
      ylab("") +
      ggtitle("")+
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      ) 
    print (bp)
    


    
    
    
    return(info.grupo)
    
  }
  

  
  } 
}

