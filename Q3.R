# ======================================================================
# Questão 3 - Herdabilidade
# ======================================================================
library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")
source("functions.R")

dataset <- read.csv("dataset.csv")

q3_plots <- function() {
  # Gera o plot para cada caractere
  altura <- regressao(dataset, "Altura_Parental", "Altura_f1_SEMS", xlab="Altura Parental", ylab="Altura Filial")
  peso <- regressao(dataset, "Peso_Parental", "Peso_F1_SEMS", xlab="Peso Parental", ylab="Peso Filial")
  pe <- regressao(dataset, "Pe_Parental", "Pé_f1_SEMS", xlab="Pé Parental", ylab="Pé Filial")
  pesoNasc <- regressao(dataset, "Peso_N_Parental", "Peso_Nas_F1_SEMS", xlab="Peso Nasc Parental", ylab="PesoNasc Filial")
  
  # Exibe todos os plots em um grid 2x2
  plot <- grid.arrange(altura, peso, pe, pesoNasc, ncol = 2)
  #ggsave("./images/herdabilidade.png", plot, width=10, height=8, dpi=200)
  plot
}

q3_plots()