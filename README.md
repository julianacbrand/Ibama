# Ibama
Base de dados Ibama de licenças, fonte:Dados Abertos
############################################################
###### IBAMA CITES ###

#1. settar configuracoes
options(scipen = 999)

#2. instalar e carregar pacotes
#2.1 instalar
install.packages('tidyverse')
#para ler excel (.xlsx ou .xls) instalar
install.packages('readxl')
#pacote para escrever xlsx
install.packages("writexl")
#pacotes para graficos
install.packages("ggplot2")

#2.2 carregar
library(tidyverse)
library(readxl) #tem a funcao read_xlsx
library(writexl)
library(dplyr)
library(ggplot2)
library(stringr)
library(writexl)
#3. setar diretorio
#nao esquecer de trocar a '\' por '/'
setwd('<coloque_o_caminho_do_diretorio>')
#read_csv para arquivos separados com ',' e read_csv2 para arquivos
#separados com ';'. 
empresas_ibama <- read_csv2("relatorio.csv")
licencas_ibama <- read_csv2('licencas.csv')
empreendimentos_ibama <-read_csv2("empreendimento.csv")
licencas_sislic <- read_csv2("sislic-licencas.csv")
#4. explorar a base de empresas_ibama ----
#descricao das variaveis da base
glimpse(empresas_ibama)
#visualizar recortes da base
head(empresas_ibama, 50#equivale ao numero linhas visualizadas
     ) %>% view()

# Categoria de Atividades

library(scales)

categoria_atividades <- (empresas_ibama$`Categoria de Atividade`)

categoria_atividades

summary(categoria_atividades)

tabela1 <- empresas_ibama %>%
  #agrupar por categoria de atividade
  group_by(`Categoria de Atividade`) %>% 
  #contando o numero de cnpjs por categoria de atividades
  summarise(qte_empresas = n()) %>% 
  #para ver a tabela no editor
  view
