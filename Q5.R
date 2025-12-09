library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")
library("reshape2")
source("functions.R")

dataset <- read.csv("dataset.csv")

# plota as correlações entre todos os caracteres (filiais e parentais)
q5_plot <- function() {
  matriz_correlacao(dataset, c(
    23, 24, 25, 26, # caracteres filiais (SEMS)
    16, 17, 18, 19  # caracteres parentais
  ), "correlação entre fenótipos")
}
