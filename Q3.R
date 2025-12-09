# ======================================================================
# Questão 3 - Herdabilidade
# ======================================================================
library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")

dataset <- read.csv("dataset.csv")

# Calcula e plota a herdabilidade de um caractere
herdabilidade <- function(df, parental, filial, xlab, ylab) {
  f <- as.formula(paste(filial, "~", parental)) # filial ~ parental
  model <- lm(f, data=df)
  
  # Valores arredondados para 4 casas decimais
  slope <- round(coef(model)[2], 4)
  intercept <- round(coef(model)[1], 4)
  h2 <- round(summary(model)$r.squared, 4) # h² = R²
  
  # Gambiarrinha honesta para exibir a equação da reta ajustada
  label = paste0("y = ", slope, "x - ", abs(intercept), "  h² = ", h2)
  
  # Retorna o plot do caractere filial x caractere parental
  ggplot(df, aes_string(parental, filial)) +
    geom_point(color="steelblue") + 
    geom_smooth(method="lm", se=TRUE, color="gray20") +
    annotate(
      "text",
      x=Inf, y=Inf, hjust=1.4, vjust=1.5, size=4, # tentativa e erro
      label=label
    ) +
    theme_minimal() +
    labs(x=xlab, y=ylab)
}

q3_plots <- function() {
  # Gera o plot para cada caractere
  altura <- herdabilidade(dataset, "alturaParental", "Altura_f1_SEMS", xlab="Altura Parental", ylab="Altura Filial")
  peso <- herdabilidade(dataset, "pesoParental", "Peso_F1_SEMS", xlab="Peso Parental", ylab="Peso Filial")
  pe <- herdabilidade(dataset, "peParental", "Pé_f1_SEMS", xlab="Pé Parental", ylab="Pé Filial")
  pesoNasc <- herdabilidade(dataset, "pesoNascParental", "Peso_Nas_F1_SEMS", xlab="Peso Nasc Parental", ylab="PesoNasc Filial")
  
  # Exibe todos os plots em um grid 2x2
  plot <- grid.arrange(altura, peso, pe, pesoNasc, ncol = 2)
  ggsave("./images/herdabilidade.png", plot, width=10, height=8, dpi=200)
}