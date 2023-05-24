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
# pacotes para gráficos
install.packages("hrbrthemes")
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
glimpse(licencas_ibama)


#visualizar recortes da base
head(empresas_ibama, 50 #equivale ao numero linhas visualizadas
     ) %>% view()

#  Categoria de Atividade


categoria_atividades <- (empresas_ibama$`Categoria de Atividade`)

categoria_atividades


summary(categoria_atividades)

tabela1 <- empresas_ibama %>%
  #agrupar por categoria de atividade
  group_by(`Detalhe`) %>% 
  #contando o numero de cnpjs por categoria
  summarise(qte_empresas = n()) %>% 
  #para ver a tabela no editor
  view


 
 periodo_de_analise <- empresas_ibama$Ano

summary(periodo_de_analise)

# Renomeando as linhas

# create a dataframe


# display the dataframe
tabela1_df = data.frame(
  "Detalhe"= c("Importação ou exportação de fauna nativa brasileira",
               "comércio de peixes ornamentais",
               "Exploração de recursos aquáticos vivos","Importação ou exportação de fauna exótica",
               "importação ou exportação de fauna silvestre exótica",
               "Serraria e desdobramento de madeira","Exploração econômica da madeira ou lenha e subprodutos florestais (floresta nativa)",
               "Utilização do patrimônio genético natural",
               "Atividade de criação e exploração econômica de fauna exótica e de fauna silvestre ",
               "Fabricação de estruturas de madeira e de móveis",
               "Fabricação de chapas, placas de madeira aglomerada, prensada e compensada",
               "Produção de óleos, gorduras, ceras, vegetais e animais",
               "comércio de partes produtos e subprodutos","Exploração de recursos aquáticos vivos ",
               "Exploração econômica da madeira ou lenha e subprodutos florestais ","produtos alimentares"),
  "Empresas"= c(4448,3812,2335,985,665,523,405,373,294,182,138,110,109,87,74,57))

# display the dataframe
print(tabela1_df)

# rename dataframe rows
rownames(tabela1_df) <- c("Importação ou exportação de fauna nativa brasileira","comércio de peixes ornamentais",
                          "Exploração de recursos aquáticos vivos","Importação ou exportação de fauna exótica","importação ou exportação de fauna silvestre exótica",
                          "Serraria e desdobramento de madeira","Exploração econômica da madeira ou lenha e subprodutos florestais (floresta nativa)",
                          "Utilização do patrimônio genético natural","Atividade de criação e exploração econômica de fauna exótica e de fauna silvestre ","Fabricação de estruturas de madeira e de móveis",
                          "Fabricação de chapas, placas de madeira aglomerada, prensada e compensada","Produção de óleos, gorduras, ceras, vegetais e animais",
                          "comércio de partes produtos e subprodutos","Exploração de recursos aquáticos vivos ","Exploração econômica da madeira ou lenha e subprodutos florestais ","produtos alimentares")
# display the dataframe
print(tabela1_df)


tabela1_df


# Barplot detalhamento de atividades
tabela1_df %>%
  filter(!is.na(Detalhe)) %>%
  arrange(Empresas) %>%
  tail(20) %>%
  mutate(Detalhe=factor(Detalhe, Detalhe)) %>%
  ggplot( aes(x=Detalhe, y=Empresas) ) +
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

summary(tabela1_df$Empresas)

#criar tabela de categorias no R para salvar no excel
write_xlsx(#inserir nome do arquivo no r que vai ser salvo
  tabela1,
  #nomear o arquivo que vai ser salvo
  'tabela1.xlsx'
)

#criar tabela de categorias no R para salvar no excel
write_xlsx(#inserir nome do arquivo no r que vai ser salvo
  tabela2,
  #nomear o arquivo que vai ser salvo
  'tabela2.xlsx'
)


#4.1 ver quantas emp exportadoras e importadoras de fauna e flora  existem em cada estado
tabela3 <- empresas_ibama %>%
  #agrupar por estado
  group_by(Estado) %>% 
  #contando o numero de cnpjs por estado
  summarise(qte_empresas = n()) %>% 
  #para ver a tabela no editor
  view


#4.2. Ordenar de maior para o menor
tabela_ordenada <- arrange(tabela1,desc(qte_empresas))

tabela_ordenada
#4.3. Fazer gráfico de barras

# Criando banco para a tabela
df_tabela1 <- data.frame(Estado = c("SAO PAULO", "PARA", "RIO DE JANEIRO","ESPIRITO SANTO","PARANA"),
                 empresas = c(8415, 3947, 3620,2782,1596))

# Boxplot
ggplot(df_tabela1, aes(y = empresas, x = Estado, fill=Estado)) +
  geom_bar(stat = "identity")

#criar tabela no R para salvar no excel
write_xlsx(#inserir nome do arquivo no r que vai ser salvo
  tabela1,
  #nomear o arquivo que vai ser salvo
  'tabela1.xlsx'
  )

write_xlsx(#inserir nome do arquivo no r que vai ser salvo
  empresas_ibama,
  #nomear o arquivo que vai ser salvo
  'relatorio.xlsx'
)



#4.2 ver quantas emp exportadoras e importadoras de fauna e flora  existem em cada estado, por ano
tabela_ano <- empresas_ibama %>%
  #agrupar por estado
  group_by(Ano) %>% 
  #contando o numero de cnpjs por estado
  summarise(qte_empresas = n()) %>% 
  #para ver a tabela no editor
  view
#criar tabela no R para salvar no excel
write_xlsx(#inserir nome do arquivo no r que vai ser salvo
  tabela_ano,
  #nomear o arquivo que vai ser salvo
  'tabela_ano.xlsx'
)
tx_cres_empresas <- function(x)(x/lag(x)-1)*100
empresas_ano <- tx_cres_empresas(tabela_ano$qte_empresas)
empresas_ano



#4.3 produto mais exportado e importado
produto_import_export <- empresas_ibama %>% 
  #concertar erro de palavras escritas em caixa alta e caixa baixa
  mutate(`Nome do produto` = str_to_lower(`Nome do produto`)) %>% 
  #agrupar por produto
  group_by(Estado, `Nome do produto`,`Unidade de Medida` ) %>%
  #somar quantidade 
  summarise(qte_imp = sum(`Quantidade importada`,
                          na.rm = T)  ,
            qte_exp = sum(`Quantidade exportada`,
                          na.rm = T)) %>%
 
  #filtrar somente o para
  #filter(Estado == 'PARA') %>% 
  view()
write_xlsx(#inserir nome do arquivo no r que vai ser salvo
  produto_import_export,
  #nomear o arquivo que vai ser salvo
  'produtos.xlsx'
)
#4.4 agregar produtos por categoria de atividade por ano
#ver categoria 
unique(empresas_ibama$`Categoria de Atividade`)

empresas_ibama %>% 
  #concertar erro de palavras escritas em caixa alta e caixa baixa
  mutate(`Nome do produto` = str_to_lower(`Nome do produto`)) %>% 
  #agrupar por produto
  group_by(Estado, `Categoria de Atividade`, Ano, `Nome do produto`,`Unidade de Medida` ) %>%
  #somar quantidade 
  summarise(qte_imp = sum(`Quantidade importada`,
                          na.rm = T)  ,
            qte_exp = sum(`Quantidade exportada`,
                          na.rm = T)) %>%
 
  View()



#5. explorar a base de licensas_ibama ----

glimpse(licencas_ibama)
head(licencas_ibama, 10) %>% View

#5.1 ver exportacoes do brasil 
export_br <- licencas_ibama %>% 
  filter(PAIS_EXPORTADOR == 'Brasil') %>% 
  #criar ano
  mutate(ANO_SAIDA = as.numeric(str_sub(DAT_SAIDA_ENTRADA, 7,10))) %>% 
  group_by(ANO_SAIDA, DES_CITES_OBJETIVO_OPERACAO, NOM_ESPECIE, SIG_UNIDMED) %>% 
  summarise(qte_exportada  = sum(QTD_ESPECIE_PRODUTO, na.rm = T)) 


tabela_ordenada2 <- arrange (export_br, desc(qte_exportada))

tabela_3 <- head(tabela_ordenada2,50) 

 
#criar tabela no R para salvar no excel
write_xlsx(#inserir nome do arquivo no r que vai ser salvo
  tabela_3,
  #nomear o arquivo que vai ser salvo
  'tabela_3.xlsx'
) 


#Exportações do Brasil
licencas_ibama %>% 
  filter(PAIS_EXPORTADOR == 'Brasil') %>% 
  #criar ano
  mutate(ANO_SAIDA = as.numeric(str_sub(DAT_SAIDA_ENTRADA, 7,10))) %>% 
  group_by(ANO_SAIDA, DES_CITES_OBJETIVO_OPERACAO, NOM_ESPECIE, SIG_UNIDMED) %>% 
  summarise(qte_exportada  = sum(QTD_ESPECIE_PRODUTO, na.rm = T)) %>% 
  View

 
#5.2 quais fins de licenca por categoria
unique(licencas_ibama$DES_CITES_OBJETIVO_OPERACAO)

objetivos <- licencas_ibama %>% 
  filter(PAIS_EXPORTADOR == 'Brasil') %>% 
  #criar ano
  mutate(ANO_SAIDA = as.numeric(str_sub(DAT_SAIDA_ENTRADA, 7,10))) %>% 
  #quantas licencas foram emitidas por ano e por categoria de uso
  group_by(ANO_SAIDA, DES_CITES_OBJETIVO_OPERACAO) %>% 
  summarise(qte_licencas_ano  = n()) %>% View

objetivos
objetivos_ordenado <- arrange(objetivos, desc(qte_licencas_ano))

#Grafico de barras objetivos das licencas
# Criando banco para a tabela
df_objetivos <- data.frame(objetivos_licencas = c("Commercial/Trade/Fins comerciais", "Educational/Fins educativos", "Zoos/Jardim zoológico","Personal/Uso pessoal","Law enforcement / Ordem Judicial"),
                         qte_licencas = c(11138, 254, 111,89,43))


df_objetivos

# Boxplot
ggplot(df_objetivos, aes(y = qte_licencas, x = objetivos_licencas, fill=objetivos_licencas)) +
  geom_bar(stat = "identity")


#5.3 Ver principais destinos de exportação
pais_importador <- licencas_ibama %>%
  #agrupar por país
  group_by(PAIS_IMPORTADOR) %>% 
  #contando o numero de licencas por país importador
  summarise(qte_empresas = n())
  #para ver a tabela no editor
tabela_ordenada_pais <- arrange(pais_importador, desc(qte_empresas))
tabela_ordenada_pais
#criar tabela no R para salvar no excel
write_xlsx(#inserir nome do arquivo no r que vai ser salvo
  tabela_ordenada_pais,
  #nomear o arquivo que vai ser salvo
  'tabela_ordenada_pais.xlsx'
) 

#Grafico de barras objetivos das licencas por pais importador
# Criando banco para a tabela
df_import <- data.frame(pais_importador = c("Brasil","Estados Unidos da América","Alemanha","Japaõ","Federação Russa","França","Países Baixos","Reino Unido da Grã Bretanha", "Portugal","Canada"),
                           qte_licencas = c(102514,35446,14114,5975,4939,4706,2806,2612,1617,1482))


df_import

# Boxplot
ggplot(df_import, aes(y = qte_licencas, x =pais_importador, fill=pais_importador)) +
  geom_bar(stat = "identity")



#6 Explorar base de empreendimentos florestais

glimpse(empreendimentos_ibama)
#visualizar recortes da base
head(empreendimentos_ibama, 50#equivale ao numero linhas visualizadas
) %>% view()
#6.1 ver quantos empreendimentos  existem em cada estado
tabela5 <- empreendimentos_ibama %>%
  #agrupar por estado
  group_by(UF) %>% 
  #contando o numero de cnpjs por estado
  summarise(qte_empresas_emp = n()) %>% 
  #para ver a tabela no editor
  view
#6.2 ver quais atividades tiveram maior participaçao nos empreendimentos
tabela6 <- empreendimentos_ibama %>%
  #agrupar por estado
  group_by( Município ) %>% 
  #contando o numero de cnpjs por estado
  summarise(qte_atividade_emp = n()) %>% 
  #para ver a tabela no editor
  view
#6.2. Ordenar de maior para o menor
tabela_ordenada_emp <- arrange(tabela5,desc(qte_empresas_emp))
tabela_ordenada_emp_mun <- arrange(tabela6, desc(qte_atividade_emp))
#criar tabela no R para salvar no excel
write_xlsx(#inserir nome do arquivo no r que vai ser salvo
  tabela6,
  #nomear o arquivo que vai ser salvo
  'tabela6.xlsx'
)

#6.3. Fazer gráfico de barras

# Criando banco para a tabela
df_tabela5 <- data.frame(UF = c("SP", "ES", "MS","SC","RS","MT","PA","BA","GO","MG"),
                         empreendimentos = c(4972,4109,3437,2694,1777,1547,1169,356,278,4))

# Boxplot
ggplot(df_tabela5, aes(y = empreendimentos, x = UF, fill=UF)) +
  geom_bar(stat = "identity")

#criar tabela no R para salvar no excel
write_xlsx(#inserir nome do arquivo no r que vai ser salvo
  tabela5,
  #nomear o arquivo que vai ser salvo
  'tabela5.xlsx'
)
#7 Explorar base sislic-licencas

glimpse(licencas_sislic)
#visualizar recortes da base
head(licencas_sislic, 50#equivale ao numero linhas visualizadas
) %>% view()

