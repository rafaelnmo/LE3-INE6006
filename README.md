# LE3-INE6006 - Estimação de Parâmetros e Testes de Hipóteses – Serviço Público em Pindorama

Este projeto realiza análises estatísticas em bases de dados relacionadas ao serviço público em Pindorama, com foco em estimativas populacionais e testes de hipóteses. Esta documentação se aplica às partes **II** e **IV** do projeto.

## Como Executar

O projeto utiliza um `Makefile` para automatizar a instalação de pacotes e a execução dos scripts em R. Todos os comandos devem ser executados a partir da pasta `part_ii/` ou `part_iv/`, conforme o módulo desejado.

### 1. Instalar pacotes necessários

```bash
make install_packages
```

> *Observação:* pode ser necessário rodar com `sudo`, ou editar o script `install_packages.R` para usar um diretório local de bibliotecas.

### 2. Rodar a análise principal

```bash
make analisys
```

### 3. Executar tudo de uma vez (instalação + análise)

```bash
make all
```

---

## Targets Disponíveis do Makefile

| Target             | Descrição                                                     |
| ------------------ | ------------------------------------------------------------- |
| `help`             | Mostra todos os comandos disponíveis                          |
| `setup`            | Cria diretórios necessários para a execução                   |
| `install_packages` | Instala os pacotes R utilizados no projeto                    |
| `analisys`         | Executa o script principal de análise e gera relatório em PDF |
| `all`              | Executa `install_packages` seguido de `analisys`              |
| `clean`            | Remove arquivos temporários gerados pelos scripts             |
| `reset`            | Remove arquivos de dados auxiliares  |

---

## Estrutura de Diretórios

A estrutura do projeto é semelhante entre as partes II e IV. Abaixo, a organização típica:

```bash
├── data/             # Arquivos de dados utilizados na análise
│   └── *.csv
├── dbs/              # (Reservado para uso futuro com banco de dados, atualmente vazio)
├── logs/             # Logs de execução dos scripts
│   ├── analisys.log
│   └── install_packages.log
├── rpt/              # Resultados finais da análise, como relatórios em PDF
│   └── analisys.pdf
├── scripts/          # Scripts R e o Makefile principal
│   ├── analisys.R
│   ├── install_packages.R
│   └── Makefile
├── work/             # Diretório para controle da execução (toques de conclusão dos targets)
    ├── make/
    └── Makefile -> ../scripts/Makefile
```

---

## Requisitos

* R (versão 4.0 ou superior)
* GNU Make
* Permissão de escrita nos diretórios do projeto

---

## Observações

* Todos os logs de execução são salvos na pasta `logs/`.
* Os relatórios gerados em PDF serão salvos na pasta `rpt/`.
* Caso o script de instalação falhe por falta de permissão, você pode configurar um diretório de bibliotecas local no `install_packages.R`.

---

## Autor

**Rafael Oliveira**

