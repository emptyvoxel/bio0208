# ======================================================================
# Questão 2 - Genótipos e Fenótipos
# ======================================================================
library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")
source("functions.R")

dataset <- read.csv("dataset.csv")

# Calcula os valores fenotípicos e o efeito aditivo e de dominância
efeitos <- function(fenotipo, alelos) {
  AA <- mean(fenotipo[alelos == 2], na.rm=TRUE)
  Aa <- mean(fenotipo[alelos == 1], na.rm=TRUE)
  aa <- mean(fenotipo[alelos == 0], na.rm=TRUE)
  
  homozigose <- (AA + aa)/2
  
  efeito_aditivo <- AA - homozigose
  dominancia <- Aa - homozigose
  
  return(list(
    AA=AA, Aa=Aa, aa=aa,
    homozigose=homozigose,
    aditivo=efeito_aditivo, dominancia=dominancia
  ))
}

# Regressão e efeito médio de substituição
q2_plot <- function(df, fenotipo, locus, xlab, ylab) {
  plot <- regressao(df=df, x=locus, y=fenotipo, xlab=xlab, ylab=ylab)
  
  ggsave("./images/efeito médio de substituição.png", plot, width=6, height=4, dpi=200)
}

q2_plot(dataset, "Altura_Mae", "g1_c17_M", "Genótipo g1_c17", "Altura Materna")

