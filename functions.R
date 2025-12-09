# Helper functions para a P2 de Processos
library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")
library("reshape2")

dataset <- read.csv("dataset.csv")

# ======================================================================
# Questão 1 - Caracteres, Sexo e Gerações
# ======================================================================
# Compara o peso entre sexos e gerações
peso_sexo_geracao <- function(df, ylab) {
  df_long <- df %>%
    mutate(familia = row_number()) %>%
    pivot_longer(
      cols = c(pesoFilial, pesoMae, pesoPai, pesoMaeJovem, pesoPaiJovem),
      names_to = "variavel",
      values_to = "valor"
    ) %>%
    mutate(
      group = case_when(
        variavel == "pesoFilial" & sexo == "F" ~ "F1 F",
        variavel == "pesoMaeJovem" ~ "F0 Jovem F",
        variavel == "pesoMae" ~ "F0 F",
        variavel == "pesoFilial" & sexo == "M" ~ "F1 M",
        variavel == "pesoPaiJovem" ~ "F0 Jovem M",
        variavel == "pesoPai" ~ "F0 M"
      )
    ) %>%
    mutate(
      sex_group = case_when(
        group %in% c("F1 F", "F0 F", "F0 Jovem F") ~ "F",
        group %in% c("F1 M", "F0 M", "F0 Jovem M") ~ "M"
      )
    )
  
  df_long$group <- factor(
    df_long$group,
    levels = c(
      "F1 F", "F0 Jovem F", "F0 F",
      "F1 M", "F0 Jovem M", "F0 M"
    )
  )
  
  plot <- ggplot(df_long, aes(x=group, y=valor, fill=sex_group)) +
    geom_violin(trim=FALSE, alpha=.4) +
    geom_boxplot(alpha=.8, outlier.alpha=.5) +
    theme_minimal(base_size=14) +
    coord_cartesian(ylim = c(20, 150)) + # Remove um outlier da *visualização* 
    scale_fill_manual(
      name = "Sexo",
      values = c("F" = "tomato", "M" = "steelblue")
    )  +
    labs(x="Grupos", y="Peso")
  
  ggsave(ggsave("./images/peso entre sexos e gerações.png", plot, width=10, height=8, dpi=200))
}

# Compara o valor de um caractere entre os sexos
caracteres_por_sexo <- function(df, caractere, ylab) {
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

# Plota a comparação de todos os caracteres entre os sexos
sexo_f1_plots <- function(df) {
  # Gera o plot para cada caractere
  altura <- caracteres_por_sexo(df, "alturaFilial", ylab="Altura")
  peso <- caracteres_por_sexo(df, "pesoFilial", ylab="Peso")
  pe <- caracteres_por_sexo(df, "peFilial", ylab="Comprimento do Pé")
  pesoNasc <- caracteres_por_sexo(df, "pesoNascFilial", ylab="Peso ao Nascimento")

  # Exibe todos os plots em um grid 2x2
  plot <- grid.arrange(altura, peso, pe, pesoNasc, ncol = 2)
  ggsave("./images/comparação entre sexos.png", plot, width=10, height=8, dpi=200)
}

# ======================================================================
# Questão 3 - Herdabilidade
# ======================================================================
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

# ======================================================================
# Questão 5 - Correlações fenotípicas
# ======================================================================
# Remove a redundância da matriz de correlação - Retirado de [1]
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

# Produz uma matriz de correlação entre os caracteres
# Código parcialmente adaptado de [1]
correlacao_caracteres <- function(df, caracteres) {
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
  
  ggsave("./images/correlação fenotípica.png", plot, width=6, height=6, dpi=200)
}

q5_plot <- function() {
  correlacao_caracteres(dataset, c(
    23, 24, 25, 26, # caracteres filiais (SEMS)
    16, 17, 18, 19  # caracteres parentais
  ))
}

# FONTES
# [1] https://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization