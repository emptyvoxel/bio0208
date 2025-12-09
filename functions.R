# Helper functions para a P2 de Processos
library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")

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
  ggsave("./images/comparação-médias.png", plot, width=10, height=8, dpi=200)
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
