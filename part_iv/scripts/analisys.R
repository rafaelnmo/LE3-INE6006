
library(ggplot2)


ARQUIVO_DADOS <- "../data/servico_publico_dados_corrigido.csv"
ARQUIVO_AMOSTRA_OESTE <- "../data/amostra_renda_oeste.csv"
ARQUIVO_AMOSTRA_LESTE <- "../data/amostra_renda_leste.csv"

# Verificar se os arquivos de amostra já existem
if (file.exists(ARQUIVO_AMOSTRA_OESTE) && file.exists(ARQUIVO_AMOSTRA_LESTE)) {
  message("📂 Lendo amostras existentes...")
  amostra_oeste <- read.csv(ARQUIVO_AMOSTRA_OESTE, stringsAsFactors = FALSE)$Renda
  amostra_leste <- read.csv(ARQUIVO_AMOSTRA_LESTE, stringsAsFactors = FALSE)$Renda
} else {
  # Se o arquivo da amostra não existe, verifica e carrega os dados
  if (file.exists(ARQUIVO_DADOS)) {
    message("📂 Lendo dados de: ", ARQUIVO_DADOS)
    dados <- read.csv(ARQUIVO_DADOS,
                      sep = ",",
                      quote = "\"",
                      stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8")
  } else {
    stop("❌ Arquivo de dados não encontrado.")
  }
  # Remover linhas com dados faltantes (NA) nas colunas Região e Renda
  dados <- dados[!is.na(dados$Região) & !is.na(dados$Renda), ]

  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  # a) Criar variável RegiãoC com a recodificação desejada
  dados$RegiãoC <- NA_character_

  # Regiões OESTE: Aquitânia, Catamarca, Dalmácia
  dados$RegiãoC[dados$Região %in% c("Aquitânia", "Catamarca", "Dalmácia")] <- "OESTE"

  # Regiões LESTE: Nortúmbria, Querétaro
  dados$RegiãoC[dados$Região %in% c("Nortúmbria", "Querétaro")] <- "LESTE"

  # Remover linhas com RegiãoC NA (caso existam regiões fora das citadas)
  dados <- dados[!is.na(dados$RegiãoC), ]

  table(dados$RegiãoC)

  # Separar dados por RegiãoC
  renda_oeste <- dados$Renda[dados$RegiãoC == "OESTE"]
  renda_leste <- dados$Renda[dados$RegiãoC == "LESTE"]

  set.seed(as.numeric(Sys.time()))  
  # set.seed(123) # Para reprodutibilidade

  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  # c) Amostras aleatórias de de 15 rendas de eleitores da RegiãoC OESTE
  amostra_oeste <- sample(renda_oeste, 15)

  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  # d) Amostras aleatórias de de 15 rendas de eleitores da RegiãoC LESTE
  amostra_leste <- sample(renda_leste, 15)

  # Salvar amostra em CSV
  write.csv(data.frame(Renda = amostra_oeste), ARQUIVO_AMOSTRA_OESTE, row.names = FALSE)
  write.csv(data.frame(Renda = amostra_leste), ARQUIVO_AMOSTRA_LESTE, row.names = FALSE)

  cat("\n📈 [INFO] Resumo dados:\n")

  str(dados)

  # Gráfico de contagem das Regiões originais com labels
  print(
    ggplot(dados, aes(x = Região)) +
      geom_bar(fill = "steelblue") +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3, size = 4) +
      labs(title = "Distribuição dos Eleitores por Região Original",
          x = "Região", y = "Frequência") +
      theme_minimal()
  )

  # Gráfico de contagem após agrupamento em RegiãoC com labels
  print(
    ggplot(dados, aes(x = RegiãoC)) +
      geom_bar(fill = "darkorange") +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3, size = 4) +
      labs(title = "Distribuição dos Eleitores por Região Agrupada (RegiãoC)",
          x = "RegiãoC", y = "Frequência") +
      theme_minimal()
  )


  message("\n✅ Amostras salvas em arquivos CSV.\n")
}

# Mostrar as amostras
cat("Amostra Renda - OESTE:\n")
print(amostra_oeste)
cat("\nAmostra Renda - LESTE:\n")
print(amostra_leste)


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# e.1) Calcular intervalos de confiança 95% para a média (variância desconhecida, t-student)
conf_level <- 0.95
alpha <- 1 - conf_level

calc_ic <- function(amostra){
  n <- length(amostra)
  media <- mean(amostra)
  s <- sd(amostra)
  erro <- qt(1 - alpha/2, df = n - 1) * s / sqrt(n)
  c(media - erro, media + erro)
}

ic_oeste <- calc_ic(amostra_oeste)
ic_leste <- calc_ic(amostra_leste)

cat("\nIntervalo de Confiança 95% para média Renda OESTE: [", round(ic_oeste[1],3), ", ", round(ic_oeste[2],3), "]\n")
cat("Intervalo de Confiança 95% para média Renda LESTE: [", round(ic_leste[1],3), ", ", round(ic_leste[2],3), "]\n")

# Interpretação (pode ser usada em relatório)
cat("\nInterpretação:\n")
cat("Esses intervalos indicam onde, com 95% de confiança, esperamos que a média verdadeira da renda dos eleitores em cada região esteja.\n")


# Criar data frame com médias e ICs
df_ic <- data.frame(
  RegiãoC = c("OESTE", "LESTE"),
  Media = c(mean(amostra_oeste), mean(amostra_leste)),
  IC_inf = c(ic_oeste[1], ic_leste[1]),
  IC_sup = c(ic_oeste[2], ic_leste[2])
)

# Gráfico de barra com erro (IC) e labels
print(
  ggplot(df_ic, aes(x = RegiãoC, y = Media)) +
    geom_point(size = 4, color = "darkblue") +
    geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2, color = "steelblue", linewidth = 1) +
    
    # Labels das médias
    geom_text(aes(label = paste0("Média: ", round(Media, 2)), y = Media),
              vjust = -1.2, color = "black", size = 3.5, fontface = "bold") +
    
    # Labels dos ICs inferior e superior
    geom_text(aes(label = paste0("IC inf: ", round(IC_inf, 2)), y = IC_inf),
              vjust = 1.5, color = "black", size = 3) +
    geom_text(aes(label = paste0("IC sup: ", round(IC_sup, 2)), y = IC_sup),
              vjust = -0.8, color = "black", size = 3) +
    
    labs(
      title = "Intervalos de Confiança (95%) para a Média da Renda por RegiãoC",
      y = "Média da Renda", x = "RegiãoC"
    ) +
    theme_minimal()
)


# Função auxiliar para gerar dados da curva t
gerar_dados_t <- function(amostra, regiao_nome) {
  n <- length(amostra)
  media <- mean(amostra)
  s <- sd(amostra)
  erro <- s / sqrt(n)
  t_crit <- qt(0.975, df = n - 1)
  
  ic_inf <- media - t_crit * erro
  ic_sup <- media + t_crit * erro
  
  x <- seq(media - 4 * erro, media + 4 * erro, length.out = 500)
  y <- dt((x - media) / erro, df = n - 1) / erro
  
  data.frame(
    x = x,
    y = y,
    RegiãoC = regiao_nome,
    media = media,
    ic_inf = ic_inf,
    ic_sup = ic_sup
  )
}

# Gerar os dados para as duas regiões
df_oeste <- gerar_dados_t(amostra_oeste, "OESTE")
df_leste <- gerar_dados_t(amostra_leste, "LESTE")
df_total <- rbind(df_oeste, df_leste)

# Gráfico combinado
ggplot(df_total, aes(x = x, y = y, color = RegiãoC, fill = RegiãoC)) +
  geom_line(linewidth = 1) +
  geom_area(data = subset(df_total, x >= ic_inf & x <= ic_sup),
            aes(x = x, y = y), alpha = 0.3, color = NA) +
  geom_vline(data = df_total[!duplicated(df_total$RegiãoC), ],
             aes(xintercept = media, color = RegiãoC),
             linetype = "dashed", linewidth = 0.8) +
  # Labels de média
  geom_text(data = df_total[!duplicated(df_total$RegiãoC), ],
            aes(x = media, y = 0.9 * max(y), label = paste0("Média ", RegiãoC, ": ", round(media, 2))),
            angle = 90, vjust = -0.5, hjust = 0, size = 3.5, color = "black", fontface = "bold") +
  # Labels de IC inferior e superior
  geom_text(data = df_total[!duplicated(df_total$RegiãoC), ],
            aes(x = ic_inf, y = 0, label = round(ic_inf, 2)),
            vjust = 1.5, size = 3, color = "black") +
  geom_text(data = df_total[!duplicated(df_total$RegiãoC), ],
            aes(x = ic_sup, y = 0, label = round(ic_sup, 2)),
            vjust = 1.5, size = 3, color = "black") +
  annotate("text", x = mean(df_total$x), y = max(df_total$y) * 0.5,
           label = "Área sombreada = IC 95%", color = "black", size = 4) +
  labs(
    title = "Distribuições t da Renda com IC 95% (OESTE vs LESTE)",
    x = "Renda",
    y = "Densidade",
    fill = "RegiãoC",
    color = "RegiãoC"
  ) +
  theme_minimal() +
  theme(legend.position = "top")



#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# e.2) Verificar se os intervalos se sobrepõem (indício inicial se médias são diferentes)
if (ic_oeste[2] < ic_leste[1] | ic_leste[2] < ic_oeste[1]) {
  cat("Como os intervalos de confiança NÃO se sobrepõem, isso sugere que as médias podem ser diferentes.\n")
} else {
  cat("Como os intervalos de confiança SE sobrepõem, não há evidência clara a partir dos intervalos que as médias sejam diferentes.\n")
}

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# e.3) Teste para igualdade das variâncias (Teste F)

# var_oeste <- var(amostra_oeste)
# var_leste <- var(amostra_leste)

# cat("\nVariância amostral OESTE:", round(var_oeste, 3), "\n")
# cat("Variância amostral LESTE:", round(var_leste, 3), "\n")

# f_estat <- var_oeste / var_leste
# df1 <- length(amostra_oeste) - 1
# df2 <- length(amostra_leste) - 1

# # Valor crítico para teste bilateral (F-distribution)
# f_critico_inf <- qf(alpha/2, df1, df2)
# f_critico_sup <- qf(1 - alpha/2, df1, df2)

# cat("\nTeste F para igualdade das variâncias - estatística F:", round(f_estat,3), "\n")
# cat("Intervalo crítico para rejeição (5% bilateral): [", round(f_critico_inf,3), ", ", round(f_critico_sup,3), "]\n")

# if (f_estat < f_critico_inf | f_estat > f_critico_sup) {
#   cat("Resultado: Rejeita-se H0, indicando que as variâncias são diferentes.\n")
# } else {
#   cat("Resultado: Não rejeita-se H0, não havendo evidência para diferenças entre variâncias.\n")
# }

#--------------------------------------------------------------------------
# e.3) Teste para igualdade das variâncias (Teste F bilateral)
conf_level <- 0.95
alpha <- 1 - conf_level

# Variâncias amostrais
var_oeste <- var(amostra_oeste)
var_leste <- var(amostra_leste)

cat("\nVariância amostral OESTE:", round(var_oeste, 3), "\n")
cat("Variância amostral LESTE:", round(var_leste, 3), "\n")

# Definindo qual variância é maior para cálculo da estatística F
if (var_oeste > var_leste) {
  f_estat <- var_oeste / var_leste
  df1 <- length(amostra_oeste) - 1
  df2 <- length(amostra_leste) - 1
  maior_variancia <- "OESTE"
  menor_variancia <- "LESTE"
} else {
  f_estat <- var_leste / var_oeste
  df1 <- length(amostra_leste) - 1
  df2 <- length(amostra_oeste) - 1
  maior_variancia <- "LESTE"
  menor_variancia <- "OESTE"
}

# Valores críticos do teste bilateral (F-distribution)
f_critico_inf <- qf(alpha / 2, df1, df2)
f_critico_sup <- qf(1 - alpha / 2, df1, df2)

cat("\nTeste F para igualdade das variâncias - estatística F:", round(f_estat, 3), "\n")
cat("Graus de liberdade: df1 =", df1, ", df2 =", df2, "\n")
cat("Intervalo crítico para não rejeição de H0 (5% bilateral): [", round(f_critico_inf, 3), ", ", round(f_critico_sup, 3), "]\n")

# Decisão do teste
if (f_estat < f_critico_inf | f_estat > f_critico_sup) {
  cat("Resultado: Rejeita-se H0, indicando que as variâncias das regiões", maior_variancia, "e", menor_variancia, "são diferentes.\n")
} else {
  cat("Resultado: Não rejeita-se H0, não havendo evidência para diferença entre as variâncias das regiões", maior_variancia, "e", menor_variancia, ".\n")
}
