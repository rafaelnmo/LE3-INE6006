# Lista de pacotes necessários
packages <- c("dplyr", "ggplot2", "readr", "tidyr", "jsonlite", "stringdist")

# Instala somente os que ainda não estão instalados
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Carrega todos os pacotes
lapply(packages, library, character.only = TRUE)
