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
library(hrbrthemes)
library(kableExtra)
library(scales)
options(knitr.table.format = "html")
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
#  Categoria de Atividade


categoria_atividades <- (empresas_ibama$`Categoria de Atividade`)

categoria_atividades


summary(categoria_atividades)

tabela1 <- empresas_ibama %>%
  #agrupar por categoria de atividade
  group_by(`Categoria de Atividade`) %>% 
  #contando o numero de cnpjs por categoria
  summarise(qte_empresas = n()) %>% 
  #para ver a tabela no editor
  view



periodo_de_analise <- empresas_ibama$Ano

summary(periodo_de_analise)


# Barplot detalhamento de atividades
tabela1 %>%
  filter(!is.na(Detalhe)) %>%
  arrange(qte_empresas) %>%
  tail(20) %>%
  mutate(Detalhe=factor(Detalhe, Detalhe)) %>%
  ggplot( aes(x=Detalhe, y=qte_empresas) ) +
  geom_bar(stat="identity", fill="#69b3a2") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("Número de empresas por detalhamento de atividade")





  
