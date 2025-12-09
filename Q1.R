# ======================================================================
# Questão 1 - Caracteres, Sexo e Gerações
# ======================================================================
library("ggplot2")
library("tidyr")
library("dplyr")
library("gridExtra")

dataset <- read.csv("dataset.csv")

# Compara o peso entre sexos e gerações
peso_sexo_geracao <- function(df) {
  df_long <- df %>%
    mutate(familia = row_number()) %>%
    pivot_longer(
      cols = c(pesoFilial, pesoMae, pesoPai, pesoMaeJovem, pesoPaiJovem),
      names_to = "variavel",
      values_to = "valor"
    ) %>%
    mutate(
      group = case_when(
        variavel == "Peso_Filial" & sexo == "F" ~ "F1 F",
        variavel == "Peso_Mae_Jovem" ~ "F0 Jovem F",
        variavel == "Peso_Mae" ~ "F0 F",
        variavel == "Peso_Filial" & sexo == "M" ~ "F1 M",
        variavel == "Peso_Pai_Jovem" ~ "F0 Jovem M",
        variavel == "Peso_Pai" ~ "F0 M"
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
  
  ggsave("./images/peso entre sexos e gerações.png", plot, width=10, height=8, dpi=200)
}

peso_sexo_geracao(dataset)

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
  altura <- caracteres_por_sexo(df, "Altura_Filial", ylab="Altura")
  peso <- caracteres_por_sexo(df, "Peso_Filial", ylab="Peso")
  pe <- caracteres_por_sexo(df, "Pe_Filial", ylab="Comprimento do Pé")
  pesoNasc <- caracteres_por_sexo(df, "Peso_Nasc_Filial", ylab="Peso ao Nascimento")
  
  # Exibe todos os plots em um grid 2x2
  plot <- grid.arrange(altura, peso, pe, pesoNasc, ncol = 2)
  ggsave("./images/comparação entre sexos.png", plot, width=10, height=8, dpi=200)
}