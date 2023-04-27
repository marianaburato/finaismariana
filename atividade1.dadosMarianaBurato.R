
##Atividade 01. Mariana Burato. 

library(tidyverse)
library(dplyr)
library(lubridate)

### Importação e organização tabela Vitor

tab_Vitor <- read.csv("C:/Users/Cliente/Documents/Dados abertos/atividade1_Vitor_Figueira_Arueira.csv", sep = ";")

## Renomeado as colunas
names(tab_Vitor)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

## Reordenando as colunas
attach(tab_Vitor)
tab_Vitor2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

view(tab_Vitor2)

#### Importação e organização tabela Lorrana

tab_Lo <- read.csv("C:/Users/Cliente/Documents/Dados abertos/Atividade1_Lorrana.csv", sep = ";")

## Renomeado as colunas
names(tab_Lo)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

## Reordenando as colunas
attach(tab_Lo2)
tab_Lo2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

view(tab_Lo2)

#### Importação e organização tabela Marina 

tab_Ma <- read.csv("C:/Users/Cliente/Documents/Dados abertos/atividade1_MARINA.csv", sep = ";")

## Renomeado as colunas
names(tab_Ma)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

## Reordenando as colunas
attach(tab_Ma)
tab_Ma2 = cbind(amostra, spp, data,site,latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

view(tab_Ma2)
### Importação e organização tabela Henrique

tab_Hen <- read.csv("C:/Users/Cliente/Documents/Dados abertos/atividade1_HenriqueSimfrone.csv", sep = ";")
## Renomeado as colunas
names(tab_Hen)[1:10] <- c("amostra", "spp", "tamanho_sepala","largura_sepala", "tamanho_petala", "largura_petala", "site", "longitude", "latitude","data")

## Reordenando as colunas
attach(tab_Hen)
tab_Hen2 = cbind(amostra, spp, data,site,latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

view(tab_Hen2)

### Importação e organização tabela Mariana

tab_Mari <- read.csv("C:/Users/Cliente/Documents/Dados abertos/atividade1_MARIANA-BURATO.csv", sep = ";")

## Renomeado as colunas
names(tab_Mari)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

## Reordenando as colunas
attach(tab_Mari)
tab_Mari2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

view(tab_Mari2)

### Importação e organização tabela Jonatha 

tab_Jon <- read.csv("C:/Users/Cliente/Documents/Dados abertos/atividade1_JonathaR.csv", sep = ";")

## Renomeado as colunas
names(tab_Jon)[1:10] <- c("amostra", "spp", "tamanho_sepala", "largura_sepala", "tamanho_petala", "largura_petala", "site","latitude", "longitude", "data")

## Reordenando as colunas
attach(tab_Jon)
tab_Jon2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)
view(tab_Jon2)

#### Unindo as tabelas em um unico arquivo

dados <- rbind(tab_Hen2, tab_Lo2, tab_Ma2, tab_Mari2, tab_Vitor2, tab_Jon2)

#### Transformando em data.frame
dados <- as.data.frame(dados)


#### Padronizando os nomes de cada espécie e o local

dados$spp[dados$spp == "IRIS_VERSICOLOR"] <- "Iris versicolor"
dados$spp[dados$spp == "IRIS_VIRGINICA"] <- "Iris virginica"
dados$spp[dados$spp == "IRIS_SETOSA"] <- "Iris setosa"
dados$spp[dados$spp == "iris_versicolor"] <- "Iris versicolor"
dados$spp[dados$spp == "iris_virginica"] <- "Iris virginica"
dados$spp[dados$spp == "iris_setosa"] <- "Iris setosa"
dados$spp[dados$spp == "Iris_versicolor"] <- "Iris versicolor"
dados$spp[dados$spp == "Iris_virginica"] <- "Iris virginica"
dados$spp[dados$spp == "Iris_setosa"] <- "Iris setosa"


dados$site[dados$site == "S3"] <- "Site3"
dados$site[dados$site == "S2"] <- "Site2"
dados$site[dados$site == "S1"] <- "Site1"
dados$site[dados$site == "site3"] <- "Site3"
dados$site[dados$site == "site2"] <- "Site2"
dados$site[dados$site == "site1"] <- "Site1"


#### Padronizando o formato das datas

dados$data[dados$data == "01/12/1929"] <- "01_12_1929"
dados$data[dados$data == "13/02/1930"] <- "13_02_1930"
dados$data[dados$data == "1929_12_01"] <- "01_12_1929"
dados$data[dados$data == "1930_02_13"] <- "13_02_1930"

#### Convertendo a coluna data com classe "factor" para "date"; padronizando o formato para YYY-MM-DD

dados$data <- dmy(dados$data)
is.Date(dados$data)
class(dados$data)

#### Ordenando a tabela pela coluna "spp"


dados<-dados[order(dados$spp),]

View(dados)

#### Exportando o arquivo em formato .csv

write.csv2(dados, "dados_unidosmari.csv")
