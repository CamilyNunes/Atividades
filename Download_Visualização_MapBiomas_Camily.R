# Script para Obtenção e Análise dos Dados do MapBiomas

# Este script faz download, extrai e lê dados de degradação e cobertura de biomas do site MapBiomas.
# Certifique-se de verificar os termos e condições de uso do site, bem como o arquivo robots.txt antes de prosseguir com o download automatizado.

# 1. Carregar as Bibliotecas Necessárias
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

# 4. Download dos Dados de Degradação
## URL dos dados de degradação: # [Dados de degradação] (https://brasil.mapbiomas.org/dados-do-modulo-mapbiomas-degradacao/)
## Realizar o download do arquivo zip
curl::multi_download(
  "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_8/degradation/statistics/brazil-degradation-statistics.zip",
  resume = FALSE,
  "dados/brazil-degradation-statistics.zip"
)

# 5. Extrair os Arquivos do Zip de Degradação
## Especificar o caminho do arquivo zip e o diretório de extração
zip_file <- "C:/Users/camir/OneDrive - Universidade Federal de Rondonópolis/Camy_Atividade/Produto_1/Dados/brazil-degradation-statistics.zip"

exdir <- "C:/Users/camir/OneDrive - Universidade Federal de Rondonópolis/Camy_Atividade/Produto_1/Dados"

# Extrair os arquivos do zip
unzip(zip_file, exdir = exdir)

# 6. Leitura dos Dados de Degradação
## Ler o arquivo Excel extraído com dados de degradação
dados_degrad <- read_excel ("dados/brazil-municipality_protected_area_sustainable_use-degradation-frequency-8.0.1a.xlsx",
                            skip = 0)

# 7. Download dos Dados de Cobertura e Transições de Municípios & Biomas
## URL dos dados de cobertura e transições
# Dados da coleção de estatística (COBERTURA E TRANSIÇÕES MUNICÍPIOS & BIOMAS (COLEÇÃO 8))
curl::multi_download(
  "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_8/downloads/statistics/tabela_geral_mapbiomas_col8_biomas_municipios.xlsx",
  resume = FALSE,
  "Dados/tabela_geral_mapbiomas_col8_biomas_municipios.xlsx"
)

# 8. Leitura dos Dados de Cobertura e Transições
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
#Objetivo: analisar a degradação do cerrado ao longo do tempo no municipio de Rondonópolis

# 1. Carregar as Bibliotecas Necessárias
library(ggplot2)    # Para criação de gráficos
library(ggthemes)   # Para temas adicionais de gráficos
library(RColorBrewer) # Para paletas de cores

# 2. Exploração Inicial dos Dados

## a) Listar Vignettes
# Vignettes são tutoriais e documentação de pacotes. 
# Útil para explorar funcionalidades adicionais.
vignette(package = "dplyr")
vignette(package = "ggplot2")

## b) Listar e Inspecionar Colunas
# Exibição geral do dataset
View(dados_degrad2)

# Inspecionar o dataset diretamente no console
dados_degrad2

# Verificar os tipos de dados das colunas
col_types <- sapply(dados_degrad2, class)
print(col_types)

# Verificar os nomes das colunas no metadados
names(METADADOS)

# Exibir a descrição das colunas no metadados
print(METADADOS[c("Campo/Field...1", "Descrição do campo...2")])


# 3. Tratamento dos Dados

## Verificar Presença de Valores Nulos
# Conta a quantidade de valores nulos por coluna
na_count <- sapply(dados_degrad2, function(x) sum(is.na(x)))
print(na_count)

# Análise das colunas com valores nulos
rows_with_na <- dados_degrad2[!complete.cases(dados_degrad2), ]
print(rows_with_na)

##Colunas sem valores nulos: As colunas como feature_id, biome, municipality, state_acronym, e outras, têm uma contagem de 0, o que significa que não há valores nulos nessas colunas.

##Colunas com valores nulos: A coluna "color" tem 520 valores nulos, o que indica que há 520 células vazias nesta coluna.

##Colunas de ano: As colunas correspondentes aos anos (1985, 1986, 1987, ..., 2022) apresentam números variados, indicando a quantidade de valores nulos por ano. 
 ###Por exemplo, a coluna "color" tem 520 valores nulos, enquanto as colunas de ano variam de 3421 para 2022 a 11565 para 1985. Isso sugere que as colunas relacionadas ao tempo (1985 até 2022) têm valores nulos, com uma quantidade que diminui ao longo dos anos, possivelmente indicando que a cobertura de dados pode ter mudado ao longo do tempo.


## Visualização Interativa dos Valores Nulos
# Gráfico interativo da contagem de valores nulos por coluna
if (!require(plotly)) install.packages("plotly")
library(plotly)

# Gráfico de barras interativo
rows_with_na <- data.frame(Column = names(na_count), NA_Count = na_count)
fig <- plot_ly(rows_with_na, x = ~Column, y = ~NA_Count, type = 'bar') %>%
  layout(title = "Contagem de Valores Nulos por Coluna",
         xaxis = list(title = 'Coluna'),
         yaxis = list(title = 'Contagem de Valores Nulos'))

fig

# 4. Análise Estatística Descritiva

#Frequência Absoluta

## Análise de Frequência por Bioma
# Tabela de frequências do campo 'biome'
frequencias <- table(dados_degrad2$biome)
df_frequencias <- as.data.frame(frequencias) # Converta a tabela para um data frame
names(df_frequencias) <- c("Bioma", "Frequência") # Renomeie as colunas para algo mais intuitivo, se desejar
df_frequencias_ordenado <- df_frequencias[order(-df_frequencias$Frequência), ] # Ordene o data frame pela frequência, do maior para o menor
print(df_frequencias_ordenado) # Exiba o data frame ordenado

#data frame com frequências
freq_abs_df <- data.frame(
  Bioma = c('Amazônia', 'Caatinga', 'Cerrado', 'Mata Atlântica', 'Pampa', 'Pantanal'),
  Frequência = c(6365, 11964, 18661, 35755, 2630, 234)
)

# Gráfico de barras da frequência por bioma
ggplot(freq_abs_df, aes(x = reorder(Bioma, Frequência), y = Frequência)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() + # Inverte os eixos para facilitar a leitura dos nomes das categorias
  labs(x = "Bioma", y = "Frequência", title = "Frequência Absoluta de Biomas") +
  theme_minimal()

## Análise de Frequência por Estado
frequencias_state <- table(dados_degrad2$state_acronym)
df_frequently_state <- as.data.frame(frequencias_state) # Converta a tabela para um data frame
names(df_frequently_state) <- c("Estado", "Frequência") # Renomeie as colunas para algo mais intuitivo, se desejar
df_frequently_state_orden <- df_frequently_state[order(-df_frequently_state$Frequência), ] # Ordene o data frame pela frequência, do maior para o menor
print(df_frequently_state_orden) # Exiba o data frame ordenado

# Top 10 estados com maior número de observações
top10_states <- df_frequently_state_orden %>%
  arrange(desc(Frequência)) %>%
  slice_head(n = 10)
print(top10_states) # Exibir o data frame ordenado com os top 10 estados

# Gráfico de barras dos top 10 estados
ggplot(top10_states, aes(x = Estado, y = Frequência)) +
geom_bar(stat = "identity", fill = "green1") +
labs(x = "Estado", y = "Frequência", title = "Frequência absoluta dos 10 estados com maior número de Observações",
         subtitle = "Dados de 1985 até 2023") +
theme_classic() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Análise de Frequência por Município-Estado

##De qual MUNICÍPIO-ESTADO o meu banco de dados tem mais informação?##

# Tabela de frequências por município-estado
freq_muni_state <- table(dados_degrad2$municipality_state)
df_freq_muni_state <- as.data.frame(freq_muni_state)
names(df_freq_muni_state) <- c("Município-Estado", "Frequência")
df_freq_muni_state_orden <- df_freq_muni_state[order(-df_freq_muni_state$Frequência), ]
print(df_freq_muni_state_orden)

# Top 10 municípios-estados com maior número de observações
top10_muni_state <- df_freq_muni_state_orden %>%
  arrange(desc(Frequência)) %>%
  slice_head(n = 10)

# Exibir o data frame ordenado com os top 10 de maior número de observações de Município - EStado do banco de dados
print(top10_muni_state)

# Gráfico de barras dos top 10 municípios-estados
ggplot(top10_muni_state, aes(x = `Município-Estado`, y = Frequência)) +
  geom_bar(stat = "identity", fill = "green2") +
  labs(x = "Município-Estado", y = "Frequência",
       title = "Top 10 Municípios-Estados",
       subtitle = "Dados de 1985 até 2023") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

#3. Análise da Cobertura e Uso da Terra no Estado de Mato Grosso (MT)
##Nível 0: Análise de Cobertura no Estado

#Os dados apresentados fornecem uma visão da cobertura da terra categorizadas como Antrópica (Anthropic), Natural e Não Aplicada (Not applied) ao longo dos anos de 1985 a 2022. 
#Esta análise será realizada em duas partes: primeiro, a análise dos valores absolutos (área em hectares) e, em seguida, a análise percentual.

# Preparação da tabela de cobertura no Nível 0 para o estado de Mato Grosso (MT)
level0_a<- dados_degrad2 %>% filter(state_acronym == "MT") %>% 
  group_by(level_0) %>% 
  summarise(`1985`=round(sum(`1985`,na.rm = TRUE)/1000),
            `1990`=round(sum(`1990`,na.rm = TRUE)/1000),
            `2000`=round(sum(`2000`,na.rm = TRUE)/1000),
            `2010`=round(sum(`2010`,na.rm = TRUE)/1000),
            `2020`=round(sum(`2020`,na.rm = TRUE)/1000),
            `2022`=round(sum(`2022`,na.rm = TRUE)/1000)) 

# Exibe a tabela com formatação utilizando o pacote gt
level0_a %>%
    gt() %>%
  fmt_number(
    columns = vars(`1985`, `1990`, `2000`, `2010`, `2020`, `2022`),
    sep_mark = ".",  # Ponto como separador de milhares
    dec_mark = ","   # Vírgula como separador decimal
  )

##Anthropic (Áreas Antrópicas): Houve um aumento constante na área antrópica de 11.280.122,0 ha em 1985 para 34.910.387,0 ha em 2022. Este crescimento indica uma expansão contínua de áreas utilizadas para atividades humanas, como urbanização, agricultura, e infraestrutura.

##Natural: A área classificada como natural diminuiu de 78.555.636,0 ha em 1985 para 54.934.746,0 ha em 2022. Este declínio reflete a perda de ecossistemas naturais, possivelmente devido à conversão em áreas antrópicas.

##Not Applied: Esta categoria, que pode incluir áreas não mapeadas ou indeterminadas, mostrou variações menores, começando em 480.647,0 ha em 1985 e aumentando para 471.272,0 ha em 2022, com um declínio inicial seguido de um aumento recente.

# Conferência dos totais de área por ano
colSums(level0_a[,c(2:7)],na.rm=TRUE)

# Salva a área total para futuros cálculos
area <- colSums(level0_a[,c(2:7)],na.rm=TRUE)[1]*1000

##Nível 0: Proporção da Cobertura no Estado
level0_b<- dados_degrad2 %>% filter(state_acronym == "MT") %>% 
  group_by(level_0) %>% 
  summarise(`1985`=round(sum(`1985`,na.rm = TRUE)/area*100,2),
            `1990`=round(sum(`1990`,na.rm = TRUE)/area*100,2),
            `2000`=round(sum(`2000`,na.rm = TRUE)/area*100,2),
            `2010`=round(sum(`2010`,na.rm = TRUE)/area*100,2),
            `2020`=round(sum(`2020`,na.rm = TRUE)/area*100,2),
            `2022`=round(sum(`2022`,na.rm = TRUE)/area*100,2)) 

# Exibe a tabela com formatação utilizando o pacote gt
level0_b %>%  gt()

#Espaço para interpretação
##Anthropic (Áreas Antrópicas): 
##Natural: 
##Not Applied: 


##Nível 1: Análise de Cobertura no Estado

# Preparação da tabela de cobertura no Nível 1 para o estado de Mato Grosso (MT)
nivel_01a<- dados_degrad2 %>% filter(state_acronym == "MT") %>% 
  group_by(level_1) %>% 
  summarise(`1985`=round(sum(`1985`,na.rm = TRUE)/1000),
            `1990`=round(sum(`1990`,na.rm = TRUE)/1000),
            `2000`=round(sum(`2000`,na.rm = TRUE)/1000),
            `2010`=round(sum(`2010`,na.rm = TRUE)/1000),
            `2020`=round(sum(`2020`,na.rm = TRUE)/1000),
            `2022`=round(sum(`2022`,na.rm = TRUE)/1000)) 

# Exibe a tabela com formatação utilizando o pacote gt
nivel_01a %>%  gt()

# Conferência dos totais de área por ano no Nível 1
colSums(nivel_01a[,c(2:7)],na.rm=TRUE)

# Atualiza a área total para os cálculos de proporção
area <- colSums(nivel_01a[,c(2:7)],na.rm=TRUE)[1]*1000

##Nível 1: Proporção da Cobertura no Estado

# Preparação da tabela com proporção de cobertura no Nível 1
nivel_01b<- dados_degrad2 %>% filter(state_acronym == "MT") %>% 
  group_by(level_1) %>% 
  summarise(`1985`=round(sum(`1985`,na.rm = TRUE)/area*100,2),
            `1990`=round(sum(`1990`,na.rm = TRUE)/area*100,2),
            `2000`=round(sum(`2000`,na.rm = TRUE)/area*100,2),
            `2010`=round(sum(`2010`,na.rm = TRUE)/area*100,2),
            `2020`=round(sum(`2020`,na.rm = TRUE)/area*100,2),
            `2022`=round(sum(`2022`,na.rm = TRUE)/area*100,2)) 

# Exibe a tabela com formatação utilizando o pacote gt
nivel_01b %>% gt()

#Espaço para interpretação

## Floresta (Forest):
## Formação Natural Não Florestal (Non Forest Natural Formation): 
## Agricultura (Farming): 
## Área Não Vegetada (Non Vegetated Area): 
## Água (Water): 
## Não Observado (Non Observed): 

## Nível 2: Análise de Cobertura no Estado

# Preparação da tabela de cobertura no Nível 2 para o estado de Mato Grosso (MT)
nivel_02a<- dados_degrad2 %>% filter(state_acronym == "MT") %>% 
  group_by(level_2) %>% 
  summarise(`1985`=round(sum(`1985`,na.rm = TRUE)/1000),
            `1990`=round(sum(`1990`,na.rm = TRUE)/1000),
            `2000`=round(sum(`2000`,na.rm = TRUE)/1000),
            `2010`=round(sum(`2010`,na.rm = TRUE)/1000),
            `2020`=round(sum(`2020`,na.rm = TRUE)/1000),
            `2022`=round(sum(`2022`,na.rm = TRUE)/1000)) 

# Exibe a tabela com formatação utilizando o pacote gt
nivel_02a %>% gt()

# Conferência dos totais de área por ano no Nível 2
colSums(nivel_02a [,c(2:7)],na.rm=TRUE)

# Atualiza a área total para os cálculos de proporção
area <- colSums(nivel_02a[,c(2:7)],na.rm=TRUE)[1]*1000

##Nível 2: Proporção da Cobertura no Estado

# Preparação da tabela com proporção de cobertura no Nível 2
nivel_02b<- dados_degrad2 %>% filter(state_acronym == "MT") %>% 
  group_by(level_2) %>% 
  summarise(`1985`=round(sum(`1985`,na.rm = TRUE)/area*100,2),
            `1990`=round(sum(`1990`,na.rm = TRUE)/area*100,2),
            `2000`=round(sum(`2000`,na.rm = TRUE)/area*100,2),
            `2010`=round(sum(`2010`,na.rm = TRUE)/area*100,2),
            `2020`=round(sum(`2020`,na.rm = TRUE)/area*100,2),
            `2022`=round(sum(`2022`,na.rm = TRUE)/area*100,2))
nivel_02b %>%  gt() # Exibindo a tabela com a função gt




# 4. Análise de Cobertura e Uso da Terra no Município de Rondonópolis


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


#1. Carregar o Arquivo CSV

#Primeiro, é necessário ler o arquivo CSV que contém a matriz de transição. 
#Você pode usar a função read.table() com as opções adequadas para ler o arquivo:  

# Carregar os dados do CSV
df_transicao <- read.table("transicoes_diagrama.csv", header = TRUE, sep = ";")

# Exibir a tabela carregada para conferência

dados_degrad2 %>% gt()

#2. Transformar a Matriz em um DataFrame Longo
#Para criar um gráfico de Sankey, você precisa transformar a matriz de transição em um formato "longo", onde cada linha representa uma transição entre categorias. Isso envolve derreter a matriz usando a função pivot_longer do tidyverse.


# Transformar a matriz de transição para o formato longo
df_long <- dados_degrad2 %>%
  pivot_longer(
    cols = -X,               # Colunas que não devem ser derretidas
    names_to = "target",     # Nome da nova coluna para os destinos
    values_to = "value"      # Nome da nova coluna para os valores
  ) %>%
  rename(source = X)         # Renomear a coluna das fontes


#3. Criar o DataFrame de Nós (Nodes)
Agora, crie um DataFrame para os nós, que são todas as categorias de origem e destino no gráfico.

r
Copiar código
# Criar DataFrame de nós (nodes)
nodes <- data.frame(name = unique(c(df_long$source, df_long$target)))
4. Criar IDs para Fontes e Destinos
Adicione IDs para as fontes e destinos, que são necessários para criar o gráfico de Sankey.

r
Copiar código
# Adicionar IDs para as fontes e destinos
df_long$IDsource <- match(df_long$source, nodes$name) - 1
df_long$IDtarget <- match(df_long$target, nodes$name) - 1
5. Criar e Visualizar o Gráfico de Sankey
Agora que os dados estão preparados, você pode criar o gráfico de Sankey.

r
Copiar código
# Preparar a escala de cores
ColourScal <- 'd3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Criar o gráfico de Sankey
sankeyNetwork(
  Links = df_long,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  sinksRight = FALSE,
  colourScale = ColourScal,
  nodeWidth = 40,
  fontSize = 13,
  nodePadding = 20
)