# Carregar bibliotecas necessárias
# library(dplyr)
library(ggplot2)
# library(pwr)
# library(readr)
# library(tidyr)
# library(stringdist)

ARQUIVO_DADOS <- "../data/servico_publico_dados_corrigido.csv"
ARQUIVO_AMOSTRA <- "../data/amostra_idade.csv"

# Se o arquivo de amostra já existe, carrega a amostra
if (file.exists(ARQUIVO_AMOSTRA)) {
  message("📂 Lendo amostra de: ", ARQUIVO_AMOSTRA)
  amostra_idade <- read.csv(ARQUIVO_AMOSTRA, stringsAsFactors = FALSE)$Idade
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

  # Verificar coluna 'Idade'
  if (!"Idade" %in% names(dados)) {
    stop("❌ Coluna 'Idade' não encontrada.")
  }

  dados$Idade <- as.numeric(dados$Idade)
  dados_idade <- na.omit(dados$Idade)

  # Gerar nova amostra se houver valores suficientes
  if (length(dados_idade) < 25){
    stop("❌ Menos de 25 valores válidos.")
  }

  set.seed(as.numeric(Sys.time()))  # Pode ajustar a semente aqui se quiser um valor fixo
  amostra_idade <- sample(dados_idade, size = 25)

  # Salvar a amostra em CSV
  write.csv(data.frame(Idade = amostra_idade), ARQUIVO_AMOSTRA, row.names = FALSE)
  message("✅ Amostra gerada e salva em: ", ARQUIVO_AMOSTRA)
}


# Exibir a amostra
cat("\n📊 [INFO] Amostra aleatória de 25 valores de Idade:\n")
print(amostra_idade)

# Resumo estatístico
cat("\n📈 [INFO] Resumo estatístico da amostra:\n")
print(summary(amostra_idade))

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# a) Construa um gráfico de probabilidade normal para os valores da amostra.
media_amostra <- mean(amostra_idade)
desvio_amostral <- sd(amostra_idade)
n <- length(amostra_idade)
alpha <- 0.05
t_critico <- qt(1 - alpha/2, df = n - 1)

cat("\n📍 a) Gráfico de probabilidade normal para os valores da amostra\n")

# Gráfico Q-Q plot da amostra de Idade
qqnorm(amostra_idade, main = "Gráfico de Probabilidade Normal - Idade (Amostra de 25)")
qqline(amostra_idade, col = "blue", lwd = 2)

# Gráfico Histograma com curva normal
df_amostra <- data.frame(Idade = amostra_idade)
ggplot(df_amostra, aes(x = Idade)) +
  geom_histogram(aes(y = after_stat(density)), bins = 8, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = media_amostra, sd = desvio_amostral),
                color = "red", linewidth = 1) +
  labs(title = "Histograma da Amostra com Curva Normal",
       x = "Idade", y = "Densidade") +
  theme_minimal()

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# b) Intervalo de confiança de 95% para a média
erro_padrao <- desvio_amostral / sqrt(n)
ic_inferior <- media_amostra - t_critico * erro_padrao
ic_superior <- media_amostra + t_critico * erro_padrao

cat("\n📍 b) Intervalo de 95% para a média populacional de Idade:\n")
cat(sprintf("Intervalo: [%.2f ; %.2f]\n", ic_inferior, ic_superior))
cat(sprintf("Interpretação: Estamos 95%% confiantes de que a média populacional de idade está entre %.2f e %.2f anos.\n", ic_inferior, ic_superior))


# Dados para a curva t
x_min <- ic_inferior - 3
x_max <- ic_superior + 3
x_vals <- seq(x_min, x_max, length.out = 300)
dens_vals <- dt((x_vals - media_amostra) / erro_padrao, df = n - 1) / erro_padrao

df_t <- data.frame(x = x_vals, y = dens_vals)

# Gráfico
ggplot(df_t, aes(x = x, y = y)) +
  geom_line(color = "blue", linewidth = 1.2) +  
  
  geom_area(data = subset(df_t, x >= ic_inferior & x <= ic_superior),
            aes(x = x, y = y),
            fill = "skyblue", alpha = 0.5) +
  
  geom_vline(xintercept = media_amostra, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = media_amostra, y = max(df_t$y) * 0.9,
           label = sprintf("Média: %.2f", media_amostra),
           angle = 90, vjust = -0.5, hjust = 0.5) +

  annotate("text", x = ic_inferior, y = 0,
           label = sprintf("%.2f", ic_inferior),
           vjust = 1.5, color = "darkgreen") +

  annotate("text", x = ic_superior, y = 0,
           label = sprintf("%.2f", ic_superior),
           vjust = 1.5, color = "darkgreen") +

  annotate("text", x = media_amostra, y = max(df_t$y)*0.5,
           label = "Área = 95%", size = 4, color = "black") +

  labs(
    title = "Distribuição t com Intervalo de Confiança de 95%",
    x = "Idade",
    y = "Densidade"
  ) +
  theme_minimal()



ggplot(df_amostra, aes(x = Idade)) +
  geom_histogram(aes(y = after_stat(density)), bins = 8,
                 fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = media_amostra, sd = desvio_amostral),
                color = "red", linewidth = 1) +
  annotate("text", x = media_amostra, y = 0.04, 
           label = sprintf("Média: %.2f", media_amostra), 
           color = "darkgreen", size = 4) +
  annotate("text", x = ic_inferior, y = 0.01, 
           label = sprintf("IC Inf: %.2f", ic_inferior), 
           color = "blue", size = 3.5, hjust = 1) +
  annotate("text", x = ic_superior, y = 0.01, 
           label = sprintf("IC Sup: %.2f", ic_superior), 
           color = "blue", size = 3.5, hjust = 0) +
  geom_vline(xintercept = media_amostra, color = "darkgreen", linetype = "dashed") +
  geom_vline(xintercept = ic_inferior, color = "blue", linetype = "dotted") +
  geom_vline(xintercept = ic_superior, color = "blue", linetype = "dotted") +
  labs(title = "Histograma com Curva Normal e Intervalo de Confiança",
       x = "Idade", y = "Densidade") +
  coord_cartesian(xlim = c(media_amostra - 2.5*desvio_amostral, media_amostra + 2.5*desvio_amostral)) +
  theme_minimal()


warnings()

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# c) Tamanho mínimo de amostra para precisão de 1 ano
precisao <- 1
n_min <- ceiling((t_critico * desvio_amostral / precisao)^2)
cat("\n📍 c) Tamanho mínimo de amostra para precisão de 1 ano:\n")
cat("Amostra necessária:", n_min, "\n")
cat("Amostra coletada:", n, "\n")
cat(ifelse(n >= n_min, "✅ A amostra é suficiente.\n", "❌ A amostra NÃO é suficiente.\n"))



#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# d) Teste t unilateral: H0: μ = 32 vs H1: μ > 32
media_hipotese <- 32
t_stat <- (media_amostra - media_hipotese) / erro_padrao
p_valor <- pt(t_stat, df = n - 1, lower.tail = FALSE)

cat("\n📍 d) Teste t para H0: média = 32 vs H1: média > 32\n")
cat(sprintf("t = %.3f, p-valor = %.4f\n", t_stat, p_valor))
cat(ifelse(p_valor < alpha,
           "✅ Rejeitamos H0. Evidências de que a média é maior que 32.\n",
           "❌ Não rejeitamos H0. Não há evidência suficiente de que a média é maior que 32.\n"))


# Distribuição t com região crítica (item d)
x_vals <- seq(media_hipotese - 4 * erro_padrao, media_hipotese + 4 * erro_padrao, length.out = 300)
t_dist <- dt((x_vals - media_hipotese) / erro_padrao, df = n - 1)
df_plot <- data.frame(x = x_vals, y = t_dist)

# Calculando os valores
limite_critico <- media_hipotese + t_critico * erro_padrao

# Gráfico
ggplot(df_plot, aes(x = x, y = y)) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = media_amostra, color = "blue", linetype = "dotted", lwd = 1.2) +
  geom_vline(xintercept = limite_critico, color = "red", linetype = "dashed") +
  annotate("text", x = media_amostra, y = max(t_dist) * 0.8,
           label = sprintf("Média amostra\n%.2f", media_amostra),
           hjust = -0.1, color = "blue", size = 4) +
  annotate("text", x = limite_critico, y = max(t_dist) * 0.7,
           label = sprintf("Limite crítico\n%.2f", limite_critico),
           hjust = 1.1, color = "red", size = 4) +
  labs(title = "Distribuição t - Teste unilateral (mu = 32)",
       x = "Idade", y = "Densidade") +
  theme_minimal()

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# e) Confronto entre intervalo e teste
cat("\n📍 e) Comparação entre intervalo de confiança (b) e teste (d):\n")
if (ic_inferior > media_hipotese) {
  cat("✅ O limite inferior do IC está acima de 32 → consistente com a rejeição de H0.\n")
} else {
  cat("⚠️ O intervalo inclui 32 → consistente com a não rejeição de H0.\n")
}


# Gráfico Intervalo de confiança (IC) com labels
ggplot(data = NULL, aes(x = 1, y = media_amostra)) +
  # Ponto da média
  geom_point(size = 3, color = "blue") +
  
  # Barra de erro (intervalo de confiança)
  geom_errorbar(aes(ymin = ic_inferior, ymax = ic_superior), width = 0.2, color = "blue", lwd = 1.2) +

  # Linha da hipótese nula
  geom_hline(yintercept = media_hipotese, linetype = "dashed", color = "red") +

  # Labels explicativos
  annotate("text", x = 1.05, y = ic_inferior, label = sprintf("Limite inferior\n%.2f", ic_inferior), hjust = 0, vjust = 1.5, color = "blue", size = 3.5) +
  annotate("text", x = 1.05, y = ic_superior, label = sprintf("Limite superior\n%.2f", ic_superior), hjust = 0, vjust = -0.5, color = "blue", size = 3.5) +
  annotate("text", x = 0.95, y = media_amostra, label = sprintf("Média amostra\n%.2f", media_amostra), hjust = 1, color = "blue", size = 3.5) +
  annotate("text", x = 1.05, y = media_hipotese, label = "Hipótese nula:\nu = 32", hjust = 0, vjust = 0.5, color = "red", size = 3.5) +

  # Limites do eixo y
  ylim(min(ic_inferior, media_hipotese) - 2, max(ic_superior, media_amostra) + 2) +

  # Título e tema
  labs(title = "Intervalo de Confiança da Média de Idade (95%)",
       x = "", y = "Idade") +
  theme_minimal()


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# f) Poder do teste se média real = 34
media_real <- 34
delta <- (media_real - media_hipotese) / erro_padrao
poder <- pt(delta - t_critico, df = n - 1, lower.tail = FALSE)
cat("\n📍 f) Poder do teste (média real = 34):\n")
cat(sprintf("Poder do teste: %.2f%%\n", poder * 100))
cat(ifelse(poder >= 0.80,
           "✅ Poder adequado (≥ 80%).\n",
           "❌ Poder baixo (< 80%). Considere aumentar a amostra.\n"))

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# g) Tamanho mínimo para detectar média = 34 com 95% de poder

# Dados da amostra
n <- length(amostra_idade)

# Hipótese
media_hipotese <- 34
alpha <- 0.05
poder <- 0.95

# Cálculo dos valores críticos da normal padrão
z_alpha <- qnorm(1 - alpha/2)  # bicaudal, 5% => 0.975
z_beta <- qnorm(poder)          # poder de 95%

# Diferença mínima a ser detectada
delta <- abs(media_hipotese - media_amostra)

# Cálculo do tamanho mínimo da amostra
n_g <- ceiling(((z_alpha + z_beta) * desvio_amostral / delta)^2)

# Saída dos resultados
cat("\n📍 Tamanho mínimo da amostra para detectar média = 34 com 95% de poder:\n")
cat("Amostra mínima necessária:", n_g, "\n")
cat("Amostra coletada:", n, "\n")
cat(ifelse(n >= n_g, "✅ A amostra é suficiente.\n", "❌ A amostra NÃO é suficiente.\n"))

# Criar data frame para o gráfico
df_n <- data.frame(
  Tipo = factor(c("Amostra coletada", "Amostra necessária"), 
                levels = c("Amostra coletada", "Amostra necessária")),
  Tamanho = c(n, n_g)
)

# Gráfico comparativo
ggplot(df_n, aes(x = Tipo, y = Tamanho, fill = Tipo)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = Tamanho), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  ylim(0, max(n_g, n) + 20) +
  labs(title = "Tamanho da Amostra: Coletada vs Necessária (Poder 95%, teste bilateral)",
       y = "Tamanho da Amostra", x = "") +
  theme_minimal() 

