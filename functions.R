# Helper functions para a P2 de Processos
library("ggplot2")
library("dplyr")
library("gridExtra")

dataset <- read.csv("dataset.csv")

# Questão 1 (orquestra porque são vários violinos)
orchestra <- function(df, caractere, ylab) {
  # Cria uma coluna temporária chamada "Total" agrupando os dois sexos
  temp <- bind_rows(df, df %>% mutate(sexo="Total"))
  
  ggplot(temp, aes_string(x="sexo", y=caractere, fill="sexo")) +
    geom_violin(trim=FALSE, alpha=.4) +
    geom_boxplot(width=.15, outlier.shape=NA, alpha=.8) +
    labs(x="Grupo", y=ylab) +
    theme_minimal() +
    scale_fill_manual(
      values = c(
        "F" = "steelblue", "M" = "tomato", "Total" = "gray60"
      )
    )
}

q1_violins <- function() {
  # Gera o plot para cada caractere
  altura <- orchestra(dataset, "alturaFilial", ylab="Altura")
  peso <- orchestra(dataset, "pesoFilial", ylab="Peso")
  pe <- orchestra(dataset, "peFilial", ylab="Comprimento do Pé")
  pesoNasc <- orchestra(dataset, "pesoNascFilial", ylab="Peso ao Nascimento")

  # Exibe todos os plots em um grid 2x2
  plot <- grid.arrange(altura, peso, pe, pesoNasc, ncol = 2)
  ggsave("./images/comparação-médias.png", plot, width=10, height=8, dpi=200)
}

q1_violins()

# Questão 3 - Herdabilidade
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
