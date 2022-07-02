
# Configurando o diretório de trabalho
setwd("C:/Users/Erics")
getwd()

#Instalando os pacotes 
install.packages("tidyverse")
install.packages("rfm")
install.packages("factoextra")

# Imports
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
library(readxl)
library(rfm)
library(stats)
library(factoextra)

# Função para carregar os dados da planilha Excel
carrega_dados <- function()
{
  setwd('C:/Users/Eric/Desktop/DSA/Curso_R/6 - Banco de Dados')
  sheet1 <- read_excel('online_retail_II.xlsx', sheet = 'Year 2009-2010')
  sheet2 <- read_excel('online_retail_II.xlsx', sheet = 'Year 2010-2011')
  dados_combinados <- rbind(sheet1, sheet2)
  return(dados_combinados)
}
?rbind

# Executa a função
dados <- carrega_dados()
dim(dados)
View(dados)


# Função para checar valores ausentes
#Não da para fazer operação matemática com o vazio
verifica_missing <- function(x)
{
  return(colSums(is.na(x)))
  
}

# Executa a função
verifica_missing(dados)


# Vamos apenas excluir os registros com valores ausentes


# Função para limpar e pré-processar os dados

preprocessa_dados <- function(data1)
{
  # Criando uma coluna chamada TotalPrice
  data1$TotalPrice <- data1$Quantity * data1$Price
  
  # Remove registros com valores ausentes
  data1 <- na.omit(data1)
  
  # Removemos as linhas da coluna Invoice que contém a letra C (o que significa que este pedido foi cancelado)
  data1 <- data1[!grepl("C",data1$Invoice),]
  
  return(data1)
  
}


# Executa a função
dataset <- preprocessa_dados(dados)
View(dataset)
dim(dataset)

#Pular
# Verificando a distribuição da variável Total Price
ggplot(dataset,
       aes(x = TotalPrice)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 1.5) +
  labs(title = 'Distribuição da Variável TotalPrice')



# Número de clientes?
#length busca o cumprimento/tamanho
#unique busca valores unicos 

length(dataset$`Customer ID`) #transações
length(unique(dataset$`Customer ID`)) #cleinte únicos


# Total monetário gasto por cliente

total_gasto <- dataset %>%
  group_by(`Customer ID`) %>%
  summarise(Sum = sum(TotalPrice))

?group_by

View(total_gasto)


# Criando uma data customizada (Natal de 2011)

View(dataset)
max(dataset$InvoiceDate)
date1 = as.Date.character("25/12/2011","%d/%m/%Y")


# Função para converter as datas do formato POISxt para o formato Date
converte_data <- function(x)
{
  options(digits.secs = 3)
  return(as.Date(as.POSIXct(x$InvoiceDate, 'GMT')))
}


# Executa a função
dataset$InvoiceDate <- converte_data(dataset)
View(dataset)


# Função para calcular Recência, Frequência e Valor Monetário

calcula_rfm <- function(x){
  z <- x %>% group_by(`Customer ID`) %>% 
    summarise(Recency = as.numeric(date1 - max(InvoiceDate)), 
              Frequency = n(), 
              Monetary = sum(TotalPrice),
              primeira_compra = min(InvoiceDate))
  
  
  # Removendo transações com valores acima do 3º Quartil e abaixo do Quartil 1 (removendo outliers)
  Q1 <- quantile(z$Monetary, .25)
  Q3 <- quantile(z$Monetary, .75)
  IQR <- IQR(z$Monetary)                #amplitude interquartil - grau de espalhamento dos dados
  z <- subset(z, z$Monetary >= (Q1 - 1.5*IQR) & z$Monetary <= (Q3 + 1.5*IQR))
  return(z)
}


# Executa a função
valores_rfm <- calcula_rfm(dataset)
View(valores_rfm)



# Machine Learning - Clusterização Kmeans

# Set seed
set.seed(1029)

# Função para a segmentação de clientes com base nos valores RFM
segmenta_cliente <- function(rfm)
{
  # Cria uma lista
  resultados <- list()
  
  # Obtém os valores RFM
  dados_rfm <- select(rfm, c('Recency','Frequency','Monetary'))
  
  # Cria o modelo
  modelo_kmeans <- kmeans(dados_rfm, center = 5, iter.max = 50)
  
  # Plot do modelo
  resultados$plot <- fviz_cluster(modelo_kmeans, 
                                  data = dados_rfm, 
                                  geom = c('point'), 
                                  ellipse.type = 'euclid')
  
  # Organiza os dados
  dados_rfm$`Customer ID` <- rfm$`Customer ID`
  dados_rfm$clusters <- modelo_kmeans$cluster
  resultados$data <- dados_rfm
  
  return(resultados)
}

# Executa a função
grafico <- segmenta_cliente(valores_rfm)[1]
grafico

tabela_rfm <- segmenta_cliente(valores_rfm)[2]
View(as.data.frame(tabela_rfm))






