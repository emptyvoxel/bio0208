# ======================================================================
# Questão 2 - Genótipos e Fenótipos
# ======================================================================
library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")

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

efeitos(dataset$alturaMae, dataset$g1_c17_M)

# Regressão e efeito médio de substituição
q2_plot <- function() {
  model <- lm(temp$alturaMae ~ temp$g1_c17_M, data=temp)
  
  # Valores arredondados para 4 casas decimais
  slope <- round(coef(model)[2], 3)
  intercept <- round(coef(model)[1], 2)
  r2 <- round(summary(model)$r.squared, 4)
  
  label <- paste0("y = ", slope, "x", intercept, "  R² = ", r2)
  
  ggplot(temp, aes(y=temp$alturaMae, x=temp$g1_c17_M)) +
    geom_point(color="steelblue") + 
    geom_smooth(method="lm", se=TRUE, color="gray20") +
    annotate(
      "text",
      x=Inf, y=Inf, hjust=1.4, vjust=1.5, size=4,
      label=label
    ) +
    theme_minimal()
}