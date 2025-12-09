library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")
library("reshape2")
source("functions.R")

dataset <- read.csv("dataset.csv")

q4_matriz <- function() {
  matriz_correlacao(dataset, c(
    23, 24, 25, 26, # fenótipos
    33, # genótipo g1_c17_f1
    36 # genótipo g2_c3_f1
  ), "correlação genótipo-fenótipo")
}

q4_regressao <- function() {
  altura <- regressao(dataset, "g1_c17_f1", "Altura_f1_SEMS", xlab="Genótipo GH", ylab="Altura Filial")
  peso <- regressao(dataset, "g1_c17_f1", "Peso_F1_SEMS", xlab="Genótipo GH", ylab="Peso Filial")
  pe <- regressao(dataset, "g1_c17_f1", "Pé_f1_SEMS", xlab="Genótipo GH", ylab="Pé Filial")
  pesoNasc <- regressao(dataset, "g1_c17_f1", "Peso_Nas_F1_SEMS", xlab="Genótipo GH", ylab="PesoNasc Filial")
  
  plot <- grid.arrange(altura, peso, pe, pesoNasc, ncol = 2)
  ggsave("./images/correlação GH-fenótipos.png", plot, width=10, height=8, dpi=200)
}

q4_regressao()