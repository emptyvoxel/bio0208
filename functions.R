# Helper functions para a P2 de Processos
library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")
library("reshape2")

dataset <- read.csv("dataset.csv")

# Remove a redundância da matriz de correlação - Retirado de [1]
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

# Produz uma matriz de correlação entre os caracteres
# Código parcialmente adaptado de [1]
matriz_correlacao <- function(df, caracteres, title) {
  # Filtra os caracteres e gera uma matriz de correlação
  colunas <- df[, caracteres]
  
  # Gera uma matriz de correlação e retorna seu triângulo superior
  matriz_corr <- cor(colunas, use="pairwise.complete.obs")
  matriz_corr <- round(matriz_corr, 2)
  upper_tri <- get_upper_tri(matriz_corr)
  melted <- melt(upper_tri, na.rm=TRUE)
  
  plot <- ggplot(data=melted, aes(Var2, Var1, fill=value)) +
    geom_tile(color="white") +
    scale_fill_gradient2(
      low="blue", high="red", mid="white",
      midpoint=0, limit=c(-1, 1), space="Lab", 
      name="Correlação\nde Pearson"
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle=45, vjust=1, size=12, hjust=1),
      legend.justification = c(1, 0),
      legend.position = c(0.5, 0.7),
      legend.direction = "horizontal"
    ) +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    coord_fixed() +
    guides(
      fill = guide_colorbar(
        barwidth = 7, barheight = 1,
        title.position = "top", title.hjust = 0.5
      )
    )
  filename <- paste0("./images/", title, ".png")
  
  ggsave(filename, plot, width=6, height=6, dpi=200)
}


# Calcula e plota a herdabilidade de um caractere
regressao <- function(df, x, y, xlab, ylab) {
  f <- as.formula(paste(y, "~", x)) # y ~ x
  model <- lm(f, data=df)
  
  # Valores arredondados para 4 casas decimais
  slope <- round(coef(model)[2], 4)
  intercept <- round(coef(model)[1], 4)
  r2 <- round(summary(model)$r.squared, 4)
  
  # Gambiarrinha honesta para exibir a equação da reta ajustada
  label = paste0("y = ", slope, "x - ", abs(intercept), "  R² = ", r2)
  
  # Retorna o plot do caractere filial x caractere parental
  plot <- ggplot(df, aes_string(x, y)) +
    geom_point(color="steelblue") + 
    geom_smooth(method="lm", se=TRUE, color="gray20") +
    annotate(
      "text",
      x=Inf, y=Inf, hjust=1.4, vjust=1.5, size=4, # tentativa e erro
      label=label
    ) +
    theme_minimal() +
    labs(x=xlab, y=ylab)
  
  return(plot)
}

# FONTES
# [1] https://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization