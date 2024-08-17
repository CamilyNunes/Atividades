# Script para Obtenção e Análise dos Dados do MapBiomas

# Este script faz download, extrai e lê dados de degradação e cobertura de biomas do site MapBiomas.
# Certifique-se de verificar os termos e condições de uso do site, bem como o arquivo robots.txt antes de prosseguir com o download automatizado.

# 1. Carregar as Bibliotecas Necessárias
install.packages(c("curl","readxl", "utils"))

library(curl)    # Para realizar downloads
library(readxl)  # Para ler arquivos Excel
library(utils)   # Para funções utilitárias como unzip

# 2. Definir o Diretório de Trabalho
## Defina o diretório onde os arquivos serão salvos e lidos.
## A função `setwd` define o diretório de trabalho atual, ou seja, o diretório onde o R irá procurar e salvar arquivos por padrão.
## Dica: Session > Set Working Directory > Choose Directory
setwd("C:/Users/camir/OneDrive - Universidade Federal de Rondonópolis/Camy_Atividade/Produto_1")

# 3. Criar Diretório para Armazenamento de Dados
## Criar um diretório chamado "Dados" se ele não existir.
dir.create("Dados", showWarnings = FALSE)

# 4. Download dos Dados de Cobertura e Transições de Municípios & Biomas
## URL dos dados de cobertura e transições
# Dados da coleção de estatística (COBERTURA E TRANSIÇÕES MUNICÍPIOS & BIOMAS (COLEÇÃO 8))
curl::multi_download(
  "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_8/downloads/statistics/tabela_geral_mapbiomas_col8_biomas_municipios.xlsx",
  resume = FALSE,
  "Dados/tabela_geral_mapbiomas_col8_biomas_municipios.xlsx"
)

# 5. Leitura dos Dados de Cobertura e Transições
## Ler a aba de cobertura de biomas
dados_degrad2 <- read_excel("Dados/tabela_geral_mapbiomas_col8_biomas_municipios.xlsx",
                            sheet = "COBERTURA_COL8.0",
                            skip = 0)
## Ler a aba de metadados
METADADOS <- read_excel("Dados/tabela_geral_mapbiomas_col8_biomas_municipios.xlsx",
                        sheet = "METADADOS",
                        skip = 2)

## Ler a aba de cobertura de biomas
transicoes <- read_excel("Dados/tabela_geral_mapbiomas_col8_biomas_municipios.xlsx",
                         sheet = "TRANSICOES_COL8.0",
                         skip = 0)
###Visualização dos Dados

### Cobertura e Uso

#1. Carregar as Bibliotecas Necessárias

# Instalação dos pacotes necessários (se ainda não estiverem instalados)
install.packages("gt")
install.packages("networkD3")

# Carregamento das bibliotecas
library(readxl)
library(dplyr)
library(tidyverse)
library(gt)
library(networkD3)

#2. Configuração de Opções
# Configura as opções para a visualização dos números em gráficos e tabelas
options(scipen = 999)

# 3. Análise de Cobertura e Uso da Terra no Município de Rondonópolis


##Nível 0: Análise de Cobertura no Município
# Preparação da tabela de cobertura no Nível 0 para o município de Rondonópolis
rondonopolis_level0_a <- dados_degrad2 %>% 
  filter(municipality == "Rondonópolis") %>% 
  group_by(level_0) %>% 
  summarise(
    `1985` = round(sum(`1985`, na.rm = TRUE) / 1000),
    `1990` = round(sum(`1990`, na.rm = TRUE) / 1000),
    `2000` = round(sum(`2000`, na.rm = TRUE) / 1000),
    `2010` = round(sum(`2010`, na.rm = TRUE) / 1000),
    `2020` = round(sum(`2020`, na.rm = TRUE) / 1000),
    `2022` = round(sum(`2022`, na.rm = TRUE) / 1000))

# Exibe a tabela com formatação utilizando o pacote gt
rondonopolis_level0_a %>% 
  gt() %>%
  fmt_number(
    columns = vars(`1985`, `1990`, `2000`, `2010`, `2020`, `2022`),
    sep_mark = ".",  # Ponto como separador de milhares
    dec_mark = ","   # Vírgula como separador decimal
  )

# Conferência dos totais de área por ano no Nível 0
colSums(rondonopolis_level0_a[, c(2:7)], na.rm = TRUE)

# Salva a área total para futuros cálculos
rondonopolis_area <- colSums(rondonopolis_level0_a[, c(2:7)], na.rm = TRUE)[1] * 1000

##espaço para interpretação
#Anthropic (Áreas Antrópicas): 
#Natural: 
#Not Applied: 


## Nível 0: Proporção da Cobertura no Município

# Preparação da tabela com proporção de cobertura no Nível 0
rondonopolis_level0_b <- dados_degrad2 %>% 
  filter(municipality == "Rondonópolis") %>% 
  group_by(level_0) %>% 
  summarise(
    `1985` = round(sum(`1985`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `1990` = round(sum(`1990`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2000` = round(sum(`2000`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2010` = round(sum(`2010`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2020` = round(sum(`2020`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2022` = round(sum(`2022`, na.rm = TRUE) / rondonopolis_area * 100, 2)
  )

# Exibe a tabela com formatação utilizando o pacote gt
rondonopolis_level0_b %>% gt()

##Nível 1: Análise de Cobertura no Município

# Preparação da tabela de cobertura no Nível 1 para o município de Rondonópolis
rondonopolis_level1_a <- dados_degrad2 %>% 
  filter(municipality == "Rondonópolis") %>% 
  group_by(level_1) %>% 
  summarise(
    `1985` = round(sum(`1985`, na.rm = TRUE) / 1000),
    `1990` = round(sum(`1990`, na.rm = TRUE) / 1000),
    `2000` = round(sum(`2000`, na.rm = TRUE) / 1000),
    `2010` = round(sum(`2010`, na.rm = TRUE) / 1000),
    `2020` = round(sum(`2020`, na.rm = TRUE) / 1000),
    `2022` = round(sum(`2022`, na.rm = TRUE) / 1000)
  )

# Exibe a tabela com formatação utilizando o pacote gt
rondonopolis_level1_a %>% gt()

# Conferência dos totais de área por ano no Nível 1
colSums(rondonopolis_level1_a[, c(2:7)], na.rm = TRUE)

# Atualiza a área total para os cálculos de proporção
rondonopolis_area <- colSums(rondonopolis_level1_a[, c(2:7)], na.rm = TRUE)[1] * 1000

## Nível 1: Proporção da Cobertura no Município

# Preparação da tabela com proporção de cobertura no Nível 1
rondonopolis_level1_b <- dados_degrad2 %>% 
  filter(municipality == "Rondonópolis") %>% 
  group_by(level_1) %>% 
  summarise(
    `1985` = round(sum(`1985`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `1990` = round(sum(`1990`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2000` = round(sum(`2000`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2010` = round(sum(`2010`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2020` = round(sum(`2020`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2022` = round(sum(`2022`, na.rm = TRUE) / rondonopolis_area * 100, 2))

# Exibe a tabela com formatação utilizando o pacote gt
rondonopolis_level1_b %>% gt()

##Nível 2: Análise de Cobertura no Município

# Preparação da tabela de cobertura no Nível 2 para o município de Rondonópolis
rondonopolis_level2_a <- dados_degrad2 %>% 
  filter(municipality == "Rondonópolis") %>% 
  group_by(level_2) %>% 
  summarise(
    `1985` = round(sum(`1985`, na.rm = TRUE) / 1000),
    `1990` = round(sum(`1990`, na.rm = TRUE) / 1000),
    `2000` = round(sum(`2000`, na.rm = TRUE) / 1000),
    `2010` = round(sum(`2010`, na.rm = TRUE) / 1000),
    `2020` = round(sum(`2020`, na.rm = TRUE) / 1000),
    `2022` = round(sum(`2022`, na.rm = TRUE) / 1000)
  )

# Exibe a tabela com formatação utilizando o pacote gt
rondonopolis_level2_a %>% gt()

# Conferência dos totais de área por ano no Nível 2
colSums(rondonopolis_level2_a[, c(2:7)], na.rm = TRUE)

# Atualiza a área total para os cálculos de proporção
rondonopolis_area <- colSums(rondonopolis_level2_a[, c(2:7)], na.rm = TRUE)[1] * 1000

##Nível 2: Proporção da Cobertura no Município

# Preparação da tabela com proporção de cobertura no Nível 2
rondonopolis_level2_b <- dados_degrad2 %>% 
  filter(municipality == "Rondonópolis") %>% 
  group_by(level_2) %>% 
  summarise(
    `1985` = round(sum(`1985`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `1990` = round(sum(`1990`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2000` = round(sum(`2000`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2010` = round(sum(`2010`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2020` = round(sum(`2020`, na.rm = TRUE) / rondonopolis_area * 100, 2),
    `2022` = round(sum(`2022`, na.rm = TRUE) / rondonopolis_area * 100, 2))

# Exibe a tabela com formatação utilizando o pacote gt
rondonopolis_level2_b %>% gt()