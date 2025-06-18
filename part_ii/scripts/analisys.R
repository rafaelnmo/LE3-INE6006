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

# a) Construa um gráfico de probabilidade normal para os valores da amostra.
media_amostra <- mean(amostra_idade)
desvio_amostral <- sd(amostra_idade)
n <- length(amostra_idade)
alpha <- 0.05
t_critico <- qt(1 - alpha/2, df = n - 1)

cat("\n📍 a) Gráfico de probabilidade normal para os valores da amostra\n")

# Gerar Q-Q plot da amostra de Idade
qqnorm(amostra_idade, main = "Gráfico de Probabilidade Normal - Idade (Amostra de 25)")
qqline(amostra_idade, col = "blue", lwd = 2)

# Histograma com curva normal
df_amostra <- data.frame(Idade = amostra_idade)
ggplot(df_amostra, aes(x = Idade)) +
  geom_histogram(aes(y = after_stat(density)), bins = 8, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = media_amostra, sd = desvio_amostral),
                color = "red", linewidth = 1) +
  labs(title = "Histograma da Amostra com Curva Normal",
       x = "Idade", y = "Densidade") +
  theme_minimal()


# b) Intervalo de confiança de 95% para a média
erro_padrao <- desvio_amostral / sqrt(n)
ic_inferior <- media_amostra - t_critico * erro_padrao
ic_superior <- media_amostra + t_critico * erro_padrao

cat("\n📍 b) Intervalo de 95% para a média populacional de Idade:\n")
cat(sprintf("Intervalo: [%.2f ; %.2f]\n", ic_inferior, ic_superior))
cat(sprintf("Interpretação: Estamos 95%% confiantes de que a média populacional de idade está entre %.2f e %.2f anos.\n", ic_inferior, ic_superior))


# library(ggplot2)

# Dados da média e IC
df_ic <- data.frame(
  media = media_amostra,
  ic_inferior = ic_inferior,
  ic_superior = ic_superior
)

# Gráfico
ggplot(df_ic, aes(x = 1, y = media)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = ic_inferior, ymax = ic_superior), width = 0.1, color = "red", linewidth = 1.2) +
  scale_x_continuous(breaks = NULL) +
  labs(
    title = "Intervalo de Confiança de 95% para a Média da Idade",
    y = "Idade",
    x = NULL
  ) +
  theme_minimal()

# Curva da distribuição t com IC sombreado
x_vals <- seq(media_amostra - 4*desvio_amostral, media_amostra + 4*desvio_amostral, length.out = 300)
dens_vals <- dt((x_vals - media_amostra) / erro_padrao, df = n - 1) / erro_padrao

df_t <- data.frame(x = x_vals, y = dens_vals)

ggplot(df_t, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +
  geom_area(data = subset(df_t, x >= ic_inferior & x <= ic_superior), aes(x = x, y = y), fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = media_amostra, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "Curva t com Intervalo de Confiança de 95%",
    x = "Idade",
    y = "Densidade"
  ) +
  theme_minimal()


# Dados para plotagem
df_ic <- data.frame(
  media = media_amostra,
  ic_inferior = ic_inferior,
  ic_superior = ic_superior
)

ggplot(df_ic, aes(x = 1, y = media)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = ic_inferior, ymax = ic_superior), width = 0.2, color = "red", linewidth = 1.5) +
  geom_text(aes(y = ic_inferior, label = sprintf("Limite inferior: %.2f", ic_inferior)), vjust = 1.5, size = 4) +
  geom_text(aes(y = ic_superior, label = sprintf("Limite superior: %.2f", ic_superior)), vjust = -1, size = 4) +
  geom_text(aes(y = media, label = sprintf("Média: %.2f", media)), vjust = -2, color = "black", size = 4.5, fontface = "bold") +
  scale_x_continuous(breaks = NULL) +
  labs(
    title = "Intervalo de Confiança de 95% para a Média da Idade",
    y = "Idade",
    x = NULL
  ) +
  ylim(ic_inferior - 3, ic_superior + 3) +
  theme_minimal()


# Dados para a curva t
x_min <- ic_inferior - 3
x_max <- ic_superior + 3
x_vals <- seq(x_min, x_max, length.out = 300)
dens_vals <- dt((x_vals - media_amostra) / erro_padrao, df = n - 1) / erro_padrao

df_t <- data.frame(x = x_vals, y = dens_vals)

# Gráfico
ggplot(df_t, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.2) +
  geom_area(data = subset(df_t, x >= ic_inferior & x <= ic_superior),
            aes(x = x, y = y),
            fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = media_amostra, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(x = media_amostra, y = max(y)*0.9, label = sprintf("Média: %.2f", media_amostra)), angle = 90, vjust = -0.5, hjust = 0.5) +
  geom_text(aes(x = ic_inferior, y = 0, label = sprintf("%.2f", ic_inferior)), vjust = 1.5, color = "darkgreen") +
  geom_text(aes(x = ic_superior, y = 0, label = sprintf("%.2f", ic_superior)), vjust = 1.5, color = "darkgreen") +
  annotate("text", x = media_amostra, y = max(dens_vals)*0.5, label = "Área = 95%", size = 4, color = "black") +
  labs(
    title = "Distribuição t com Intervalo de Confiança de 95%",
    x = "Idade",
    y = "Densidade"
  ) +
  theme_minimal()


  library(ggplot2)

# Supondo que já tenha:
# media_amostra, desvio_amostral, ic_inferior, ic_superior, df_amostra

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



# c) Tamanho mínimo de amostra para precisão de 1 ano
precisao <- 1
n_min <- ceiling((t_critico * desvio_amostral / precisao)^2)
cat("\n📍 c) Tamanho mínimo de amostra para precisão de 1 ano:\n")
cat("Amostra necessária:", n_min, "\n")
cat("Amostra coletada:", n, "\n")
cat(ifelse(n >= n_min, "✅ A amostra é suficiente.\n", "❌ A amostra NÃO é suficiente.\n"))

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

ggplot(df_plot, aes(x = x, y = y)) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = media_amostra, color = "blue", linetype = "dotted", lwd = 1.2) +
  geom_vline(xintercept = media_hipotese + t_critico * erro_padrao, color = "red", linetype = "dashed") +
  annotate("text", x = media_amostra, y = max(t_dist) * 0.8, label = "Média amostra", hjust = -0.1, color = "blue") +
  annotate("text", x = media_hipotese + t_critico * erro_padrao, y = max(t_dist) * 0.7,
           label = "Limite crítico", hjust = 1.2, color = "red") +
  labs(title = "Distribuição t - Teste unilateral (mu = 32)",
       x = "Idade", y = "Densidade") +
  theme_minimal()


# Calculando os valores
limite_critico <- media_hipotese + t_critico * erro_padrao

# Plot atualizado com labels
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
  labs(title = "Distribuição t - Teste unilateral (μ = 32)",
       x = "Idade", y = "Densidade") +
  theme_minimal()


# e) Confronto entre intervalo e teste
cat("\n📍 e) Comparação entre intervalo de confiança (b) e teste (d):\n")
if (ic_inferior > media_hipotese) {
  cat("✅ O limite inferior do IC está acima de 32 → consistente com a rejeição de H0.\n")
} else {
  cat("⚠️ O intervalo inclui 32 → consistente com a não rejeição de H0.\n")
}

# Intervalo de confiança (IC)
ggplot(data = NULL, aes(x = 1, y = media_amostra)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = ic_inferior, ymax = ic_superior), width = 0.2, color = "blue", lwd = 1.2) +
  geom_hline(yintercept = media_hipotese, linetype = "dashed", color = "red") +
  annotate("text", x = 1.1, y = media_hipotese, label = "mu = 32", color = "red", hjust = 0) +
  ylim(min(ic_inferior, media_hipotese) - 2, max(ic_superior, media_amostra) + 2) +
  labs(title = "Intervalo de Confiança da Média de Idade (95%)",
       x = "", y = "Idade") +
  theme_minimal()


# f) Poder do teste se média real = 34
media_real <- 34
delta <- (media_real - media_hipotese) / erro_padrao
poder <- pt(delta - t_critico, df = n - 1, lower.tail = FALSE)
cat("\n📍 f) Poder do teste (média real = 34):\n")
cat(sprintf("Poder do teste: %.2f%%\n", poder * 100))
cat(ifelse(poder >= 0.80,
           "✅ Poder adequado (≥ 80%).\n",
           "❌ Poder baixo (< 80%). Considere aumentar a amostra.\n"))

# g) Tamanho mínimo para detectar média = 34 com 95% de poder
z_alpha <- qnorm(1 - alpha)
z_beta <- qnorm(0.80)
n_g <- ceiling(((z_alpha + z_beta) * desvio_amostral / (34 - media_hipotese))^2)

cat("\n📍 g) Tamanho mínimo da amostra para detectar média = 34 com 95% de poder:\n")
cat("Amostra mínima:", n_g, "\n")
cat("Amostra coletada:", n, "\n")
cat(ifelse(n >= n_g, "✅ A amostra é suficiente.\n", "❌ A amostra NÃO é suficiente.\n"))


