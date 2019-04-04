library(dplyr)
library(corrplot)
library(rgdal)
library(leaflet)
library(rgdal)
library(rstudioapi) 
library(readxl)
library(ggmap)
library(tidyverse)
library(ggplot2)
library(readr)

setwd("C:/Users/geisa.vasconcelos/workspace/trabalho/cluster_federais/")

IES<-read.csv2("C:/Users/geisa.vasconcelos/Downloads/microdados_censo/DADOS/DM_IES.CSV",sep="|")

IES = as.data.frame(IES, stringsAsFactors = FALSE)
IES = IES[c("NO_IES", "NO_MUNICIPIO_IES", "IN_CAPITAL_IES", "QT_TEC_FUND_INCOMP_FEM",
            "QT_TEC_FUND_INCOMP_MASC","QT_TEC_FUND_COMP_FEM", "QT_TEC_FUND_COMP_MASC","QT_TEC_MEDIO_FEM",
            "QT_TEC_MEDIO_MASC", "QT_TEC_SUPERIOR_FEM", "QT_TEC_SUPERIOR_MASC", "QT_TEC_ESPECIALIZACAO_FEM",
            "QT_TEC_ESPECIALIZACAO_MASC", "QT_TEC_MESTRADO_FEM", "QT_TEC_MESTRADO_MASC", "QT_TEC_DOUTORADO_FEM", 
            "QT_TEC_DOUTORADO_MASC", "IN_ACESSO_PORTAL_CAPES", "IN_ACESSO_OUTRAS_BASES", "IN_REPOSITORIO_INSTITUCIONAL", 
            "IN_BUSCA_INTEGRADA", "IN_SERVICO_INTERNET", "IN_PARTICIPA_REDE_SOCIAL", "IN_CATALOGO_ONLINE", 
            "QT_PERIODICO_ELETRONICO", "QT_LIVRO_ELETRONICO", 
            "VL_RECEITA_PROPRIA",
            "VL_TRANSFERENCIA",
            "VL_OUTRA_RECEITA",
            "VL_DES_PESSOAL_REM_DOCENTE",
            "VL_DES_PESSOAL_REM_TECNICO",
            "VL_DES_PESSOAL_ENCARGO",
            "VL_DES_CUSTEIO",
            "VL_DES_INVESTIMENTO",
            "VL_DES_PESQUISA",
            "VL_DES_OUTRAS")]

names(IES)


IES$NO_IES[duplicated(IES$NO_IES)]

nrow(IES[!duplicated(IES$NO_IES), ])

IES %>% dplyr::filter(., grepl('FEDERAL', NO_IES)) %>%  dplyr::filter(., !grepl('INSTITUTO|CENTRO', NO_IES)) %>%
  dplyr::filter(., !grepl('CAPITAL|DISTRITO', NO_IES)) -> teste

summary(teste)


universidades = as.character(teste[,1])
cidades = as.character(teste[,2])
dados = teste[,-c(1,2)]

convert <- function(x) as.numeric(as.character(x))

# Padronizando:
dados %>% apply(., 2, convert) %>% scale -> dados

#  Clustering: Kmeans
set.seed(2)
clusters = kmeans(dados, 4)
table(clusters$cluster)

universidades[clusters$cluster == 1]
universidades[clusters$cluster == 2]
universidades[clusters$cluster == 3]
universidades[clusters$cluster == 4]


# Analisando o grupo do qual a ufersa faz parte

grupo_3 = teste[clusters$cluster == 3, ]

summary(grupo_3 %>% apply(., 2, convert))


cluster_ufersa = teste[clusters$cluster == 3, ]
cluster_ufersa[,-1] %>% apply(., 2, convert) -> cluster_ufersa


# Calculando a correlação das variáveis:
matriz_para_correlacao = cluster_ufersa[,c("QT_TEC_FUND_INCOMP_FEM", "QT_TEC_FUND_INCOMP_MASC", "QT_TEC_FUND_COMP_FEM", 
                                          "QT_TEC_FUND_COMP_MASC", "QT_TEC_MEDIO_FEM", "QT_TEC_MEDIO_MASC", "QT_TEC_SUPERIOR_FEM", 
                                          "QT_TEC_SUPERIOR_MASC", "QT_TEC_ESPECIALIZACAO_FEM", "QT_TEC_ESPECIALIZACAO_MASC",  
                                          "QT_TEC_MESTRADO_FEM", "QT_TEC_MESTRADO_MASC", "QT_TEC_DOUTORADO_FEM", 
                                          "QT_TEC_DOUTORADO_MASC", "VL_RECEITA_PROPRIA", "VL_DES_PESSOAL_ENCARGO")]
mydata.cor = Hmisc::rcorr(matriz_para_correlacao)
mydata.cor$P %>% round(.,3)

# matriz de correlação:
corrplot(mydata.cor$r)

palette = colorRampPalette(c("green", "white", "red"))(20)
heatmap(x = mydata.cor$r, col = palette, symm = TRUE)


pheatmap::pheatmap(mydata.cor$r, treeheight_row = 0, treeheight_col = 0)


# Separando os grupos:

grupo_1 = teste[clusters$cluster == 1, ]
grupo_2 = teste[clusters$cluster == 2, ]
grupo_3 = teste[clusters$cluster == 3, ]
grupo_4 = teste[clusters$cluster == 4, ]
grupo_5 = teste[clusters$cluster == 5, ]


classificacao = cbind(universidades, cidades, clusters$cluster)

# latitude e longitude das universidades

# api <- "AIzaSyD_d2v7vjg4DI-0SUJzEEHFazw8q-OjhMw" # Text file with the API key
# register_google(key = api)
# getOption("ggmap")
# locations <- universidades %>%
#   geocode()

#classificacao = cbind(classificacao, locations)
#write.csv(classificacao, file = "classificacao.csv")

classificacao <- read_csv("classificacao.csv", 
                          locale = locale(encoding = "ISO-8859-1"))



# Importando o shapefile para o R

shp <- readOGR("Mapa", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")
#class(shp)

pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'orange'),
  domain = classificacao$V3
)


m <- leaflet(classificacao) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                              attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
m %>% addCircleMarkers(lng = ~lon, lat = ~lat, fillColor = ~pal(V3), popup = ~universidades, fillOpacity = 0.7,
                       popupOptions = popupOptions(minWidth = 20, closeOnClick = FALSE, closeButton = TRUE))
