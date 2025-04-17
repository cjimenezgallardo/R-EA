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
#' Instruccions 
#' if you only do analysis of one dependent variable (VD)
#' 
#' you can write the code as follows
#' 
#' 1- descripYG(dataset, vd=Dependent Variable,vi=NULL)
#' 2- descripYG(dataset, Dependent Variable)
#' 
#' 
#' si hara un analisis de la VD, de acuerdo a una independiente (VI)
#' 
#' 1- descripYG(dataset, vd=Dependent Variable,vi= Independent Variable)
#' 2- descripYG(dataset, Dependent Variable, Independent Variable)
#'
#'#' 
#' 
#' 
 
library(tidyverse)
library(ggridges)
library(moments)
library(patchwork)

descripYG <- function(dataset, vd, vi = NULL) {
  
  # Convertir nombres de variables en texto
  vd_name <- as.character(substitute(vd))
  vi_name <- if (!missing(vi)) as.character(substitute(vi)) else NULL
  
  # Verificar existencia en dataset
  if (!vd_name %in% names(dataset)) {
    stop(paste("La variable dependiente", vd_name, "no existe en el dataset."))
  }
  if (!is.null(vi_name) && !vi_name %in% names(dataset)) {
    stop(paste("La variable independiente", vi_name, "no existe en el dataset."))
  }
  
  # Extraer variables
  vd <- dataset[[vd_name]]
  vi <- if (!is.null(vi_name)) dataset[[vi_name]] else NULL
  
  # Eliminar NA antes del análisis
  dataset <- dataset %>% drop_na(all_of(c(vd_name, vi_name)))
  
  # Si no hay variable independiente
  if (is.null(vi)) {
    n.dy <- length(vd)
    IQR.dy <- IQR(vd)
    
    info.dy <- tibble(
      n = n.dy,
      promedio = mean(vd),
      mediana = median(vd),
      desv.estd = sd(vd),
      curtosis = kurtosis(vd),
      asimetria = skewness(vd),
      CV = sd(vd)/abs(mean(vd)),
      Min = min(vd),
      Max = max(vd),
      P25 = quantile(vd, 0.25),
      P75 = quantile(vd, 0.75),
      IQR = IQR.dy,
      bmin = quantile(vd, 0.25) - 1.5 * IQR.dy,
      bmax = quantile(vd, 0.75) + 1.5 * IQR.dy
    )
    
    barras <- trunc(3.322 * log10(n.dy) + 1)
    
    g1 <- ggplot(dataset, aes(x = !!sym(vd_name))) +
      geom_histogram(color = 'white', fill = 'steelblue', bins = barras) +
      xlab(vd_name) + ylab("Frecuencia") + theme_classic()
    
    g2 <- ggplot(dataset, aes(x = !!sym(vd_name))) +
      geom_boxplot(fill = "lightgreen", width = 0.05) +
      xlab(vd_name)
    
    graficos <- g1 / g2 + plot_layout(heights = c(3, 1))
    print(graficos)
    
    return(as.data.frame(info.dy))
    
  } else {
    # Cálculo por grupo
    info.grupo <- dataset %>%
      group_by(across(all_of(vi_name))) %>%
      summarise(
        ni = n(),
        Promedio = mean(!!sym(vd_name)),
        Mediana = median(!!sym(vd_name)),
        Desv.Estd = sd(!!sym(vd_name)),
        Curtosis = kurtosis(!!sym(vd_name)),
        Asimetria = skewness(!!sym(vd_name)),
        CV = sd(!!sym(vd_name))/abs(mean(!!sym(vd_name))),
        Min = min(!!sym(vd_name)),
        Max = max(!!sym(vd_name)),
        P25 = quantile(!!sym(vd_name), 0.25),
        P75 = quantile(!!sym(vd_name), 0.75),
        IQR = IQR(!!sym(vd_name)),
        .groups = "drop"
      ) %>%
      rename(Grupo = all_of(vi_name))
    
    bd1 <- ggplot(dataset, aes(x = !!sym(vd_name), y = as.factor(!!sym(vi_name)), fill = as.factor(!!sym(vi_name)))) +
      geom_density_ridges2() +
      labs(y = vi_name, x = vd_name) +
      theme_minimal()
    
    bp <- ggplot(dataset, aes(x = factor(!!sym(vi_name)), y = !!sym(vd_name))) +
      geom_boxplot(color = 'darkslategray', fill = 'steelblue') +
      xlab(vi_name) + ylab(vd_name) +
      theme_classic()
    
    print(bd1)
    print(bp)
    
    return(as.data.frame(info.grupo))
  }
}

