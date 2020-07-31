library(sf)
library(rgdal)
library(sp)
library(raster)
library(tmap)
library(cleangeo)
library(spdep)
library(pgirmess)
library(spatialreg)
library(spgwr)
library(dplyr)
library(tidyverse)

##SHAPEFILE
df <- rgdal::readOGR("C:\\Users\\afonso.machado\\GIT\\br_municipios\\BRMUE250GC_SIR.shp")
class(df)

df_sf <- st_as_sf(df)
df_simplified <- st_simplify(df_sf)

##DADOS
dados <- read.csv2("agregadafinal.csv", stringsAsFactors = FALSE)
dados <- read.csv2("https://raw.githubusercontent.com/afonsohbm/ProjetoFinal/master/agregadafinal.csv")

names(dados)

dados <- dados %>% select(cod, percvotonovo, qtnovodepfed, rendapercapita2010,
                          GINI, T_ANALF18M)

as.tibble(dados)
dados$percvotonovo <- gsub(",", ".", dados$percvotonovo)
dados$percvotonovo <- as.double(dados$percvotonovo)

as.tibble(dados)

names(dados)[1] <- "CD_GEOCMU"
dados$CD_GEOCMU <- as.factor(dados$CD_GEOCMU)

##JOIN

df_sf_dados <- inner_join(df, dados, by = c("CD_GEOCMU", "CD_GEOCMU"))

class(df_sf_dados)


########  COMEÇANDO A SEGUIR A AULA
setores_nao_nulo <- df_sf_dados %>% subset(is.na(df_sf_dados$percvotonovo)==FALSE) %>% 
                                    subset(is.na(df_sf_dados$qtnovodepfed)==FALSE) %>% 
                                    subset(is.na(df_sf_dados$rendapercapita2010) == FALSE) %>% 
                                    subset(is.na(df_sf_dados$GINI) == FALSE) %>% 
                                    subset(is.na(df_sf_dados$T_ANALF18M) == FALSE)



setores_valido <- setores_nao_nulo %>% 
                    subset(setores_nao_nulo$rendapercapita2010 != 0) %>% 
                    subset(setores_nao_nulo$GINI != 0)

##ERROS TOPOLÒGICOS ==>>COLADO PRA FRENTE TUDO
library(cleangeo)
setores_sp <- as(setores_valido, "Spatial")

clgeo_IsValid(setores_sp)
analise_topologica <- clgeo_CollectionReport(setores_sp)
clgeo_SummaryReport(analise_topologica)
clgeo_SuspiciousFeatures(analise_topologica)
setores_clean <- clgeo_Clean(setores_sp)
clgeo_IsValid(setores_clean)

# Relacoes de vizinhanca
install.packages("spdep")
library(spdep)
## Contiguidade
vizinhanca <- poly2nb(setores_clean)
#View(vizinhanca)
#View(card(vizinhanca))
setores_clean$vizinhos <- card(vizinhanca)
#View(setores_clean@data)
setores_juntos <- subset(setores_clean, setores_clean$vizinhos != 0)
#View(setores_juntos@data)
vizinhanca2 <- poly2nb(setores_juntos)
#setores_xy <- coordinates(setores_juntos)
#plot(setores_sp, border="red")
#plot(x= vizinhanca2, coord = setores_xy, cex=0.6, add=TRUE)
#vizinhanca_sp <- nb2lines(vizinhanca2, coords = setores_xy, proj4string = crs(setores_juntos))
#plot(vizinhanca_sp)
## Lista de vizinhanca normalizada (pesos)
vizinhanca_pesos <- nb2listw(vizinhanca2)
#View(vizinhanca_pesos)
#plot(setores_sp, border="red")
#plot(vizinhanca_pesos, setores_xy, cex=0.6, add=TRUE)
vizinhanca_pesos_sp <- listw2lines(vizinhanca_pesos, coords = setores_xy, proj4string = crs(setores_sp))
#plot(vizinhanca_pesos_sp)

# Exercicio 1 - Fazer verificacao de correcao topologica, vizinhanca e vizinhanca normalizada para os municipios da RMSP. Visualizar grafo de vizinhanca como mapa.
## Dica: mais detalhes sobre a verficacao de correcao topologica estao na aula sobre análise espacial sobre dados vetoriais

## Analise de distancias
distancias <- nbdists(vizinhanca2, setores_xy)
#View(distancias)
#View(unlist(distancias))
#summary(unlist(distancias))

## Vizinhos em um raio de distancia
vizinhanca_400m <- dnearneigh(setores_xy, d1=0, d2=400) #N FOI
#View(vizinhanca_400m)
#plot(vizinhanca_400m, setores_xy, cex=0.3)

## Vizinhos mais proximos
vizinhos_4 <- knearneigh(setores_xy, k = 4)
#View(vizinhos_4$nn)
vizinhanca_4 <- knn2nb(vizinhos_4)
#View(vizinhanca_4)
#plot(setores_sp, border="red")
#plot(vizinhanca_4, setores_xy, cex=0.6, add=TRUE)

# Exercicio 2 - Fazer analise de distancias, vizinhanca com o raio de distancia media e com 3 vizinhos mais proximos dos municipios da RMSP, visualizando os mapas.

# Dependencia Espacial
## Indice Global de Moran
moran.test(x = setores_juntos$Renda, listw = vizinhanca_pesos)
## Correlogramas
correlograma_contiguidade <- sp.correlogram(neighbours = vizinhanca2, var = setores_juntos$Renda, order = 5, method = "I")
#plot(correlograma_contiguidade)
correlograma_contiguidade
#install.packages("pgirmess")
library(pgirmess)
correlograma_distancia <- correlog(setores_xy, setores_juntos$Renda) # NAO RODAR, DEMORA MUITO
#plot(correlograma_distancia)
#correlograma_distancia
### Testa o ìndice Global para vizinhanca acumulada de segunda ordem
vizinhanca_1e2 <- nblag(vizinhanca2, maxlag = 2)
#View(vizinhanca_1e2)
vizinhanca_ordem_2 <- nblag_cumul(vizinhanca_1e2)
#View(vizinhanca_ordem_2)
vizinhanca_peso_ordem_2 <- nb2listw(vizinhanca_ordem_2)
moran.test(x = setores_juntos$Renda, listw = vizinhanca_peso_ordem_2)

# Exercicio 3 - Calcule o indice global de moran para a expectativa de vida nos municipios da RMSP e faca dois correlogramas, um de contiguidade (ate ordem 3) e um de distância

## Indice Local de Moran
localmoran <- localmoran(x = setores_juntos$percvotonovo, listw = vizinhanca_pesos)
#View(localmoran)
#class(localmoran)
localmoran_df <- as.data.frame(localmoran)
setores_juntos$moran <- localmoran_df$Ii
setores_juntos$moran_p <- localmoran_df$`Pr(z > 0)`
#View(setores_juntos@data)
#summary(setores_juntos$moran)
tm_shape(setores_juntos) + 
  tm_fill("moran", style="fixed", breaks=c(-3,0,0.2,0.5,30), palette=c("red","lightblue", "blue", "blue4"))
tm_shape(setores_juntos) +
  tm_fill(col="moran_p", style="fixed", breaks=c(0,0.01, 0.05, 1), palette=c("darkblue", "blue", "gray"))

## Media dos valores vizinhos
setores_juntos$lag_percvotonovo <- lag.listw(vizinhanca_pesos, var=setores_juntos$percvotonovo)
#View(setores_juntos@data)
tm_shape(setores_juntos) + tm_fill("lag_percvotonovo", style="quantile")
## Lisa Maps
moran.plot(x = setores_juntos$percvotonovo, listw = vizinhanca_pesos, cex=0.6, labels=FALSE)
L1 <- factor(setores_juntos$percvotonovo < mean(setores_juntos$percvotonovo), labels=c("H", "L")) 
L2 <- factor(setores_juntos$lag_percvotonovo < mean(setores_juntos$lag_percvotonovo), labels=c("H", "L")) 
setores_juntos$lisa <- paste(L1, L2)
#View(setores_juntos@data)
tm_shape(setores_juntos) +
  tm_fill("lisa", palette=c("blue","green","yellow","red"))
lisa_map <- setores_juntos[setores_juntos$moran_p <= 0.05,]
table(lisa_map$lisa)
tm_shape(setores_juntos) + 
  tm_borders() +
  tm_shape(lisa_map) +
  tm_fill("lisa", palette = c("blue","red"))

# Exercicio 4 - A - Faca um mapa do indice local de moran para expectativa de vida nos municipios da RMSP, comparando com o mapa de significancia estatistica. 
#               B - Elabore um mapa de media dos vizinhos, um diagrama de espalhamento de moran, e um LISA map para os municipios da RMSP

## Suavizacao por estimador empirico bayesiano
## Estimador global
#bayes_global <- EBest(n=setores_juntos$deslizam,  x=setores_juntos$Pessoas)
#View(bayes_global)
#setores_juntos$desl_pes <- bayes_global$raw
#tm_shape(setores_juntos) +
  #tm_fill("desl_pes", style="fisher")
#setores_juntos$bayes_gl <- bayes_global$estmm
#tm_shape(setores_juntos) +
  #tm_fill("bayes_gl", style="fisher")

## Estimador local
#bayes_local <- EBlocal(ri=setores_juntos$deslizam, ni=setores_juntos$Pessoas, nb=vizinhanca2)
#View(bayes_local)
#setores_juntos$bayes_lc <- bayes_local$est
#tm_shape(setores_juntos) +
  #tm_fill("bayes_lc", style="fisher")

# Exercicio 5 - Mapeie o risco observado, a suavizacao bayesiana global e a local para os casos de inundacao nos setores censitarios do ABC

## Modelos espaciais
# Regressao simples
plot(data=setores_juntos, percvotonovo ~ numeroafiliados + rendapercapita2010 + GINI + percate40anos)
regressao_convencional <- lm(data=setores_juntos, percvotonovo ~ numeroafiliados + rendapercapita2010 + GINI) 
summary(regressao_convencional)
AIC(regressao_convencional)
plot(data=setores_juntos, Renda ~ rede_esg)
abline(regressao_convencional, col="red", lwd=2)
View(regressao_convencional$residuals)
setores_juntos$residuos <- regressao_convencional$residuals
tm_shape(setores_juntos) +
  tm_fill("residuos", style = "quantile", palette = heat.colors(5))
lm.morantest(regressao_convencional, listw = vizinhanca_pesos)

# Regressao espacial
library(spatialreg)
## Regressao com autocorrelacao espacial da variavel predita
regressao_espacial_lag <- lagsarlm(data=setores_juntos, 
                                   percvotonovo ~ numeroafiliados + rendapercapita2010 + GINI + percate40anos, 
                                   listw=vizinhanca_pesos) # demora um pouco
summary(regressao_espacial_lag, Nagelkerke = TRUE)
## Regressao com autocorrelacao espacial dos residuos
regressao_espacial_CAR <- spautolm(data=setores_juntos, 
                                   percvotonovo ~ numeroafiliados + rendapercapita2010 + GINI + percate40anos,
                                   listw=vizinhanca_pesos, family="CAR") # demora um pouco
summary(regressao_espacial_CAR, Nagelkerke = TRUE)

# Exercicio 6 - Modele a expectativa de vida em funcao de porcentagem de analfabetos na RMSP.
#               Compare os resultados de regressao convencional (e autocorrelacao espacial dos residuos), regressao espatial lag e error.

###########################################
## Regressao ponderada geografica
###########################################
library(spgwr)
### Raio global
raio <- gwr.sel(data=setores_juntos, percvotonovo ~ numeroafiliados + rendapercapita2010 + GINI + percate40anos) # NAO RODAR porque demora muito, e o resultado esta abaixo
raio
raio <- 131.0891 #782.3348
setores_gwr <- gwr(data=setores_juntos, percvotonovo ~ numeroafiliados + rendapercapita2010 + GINI + percate40anos , bandwidth=raio)
### Raio adaptativo
raio_adaptativo <- gwr.sel(data=setores_juntos, 
                           percvotonovo ~ numeroafiliados + rendapercapita2010 + GINI + percate40anos, 
                           adapt = TRUE) # NAO RODAR porque demora muito, e o resultado esta abaixo
raio_adaptativo
raio_adaptativo <- 0.01601395
setores_gwr_adaptativo <- gwr(data=setores_juntos, 
                              percvotonovo ~ numeroafiliados + rendapercapita2010 + GINI + percate40anos, 
                              adapt=raio_adaptativo)
### comparando o raio global e o adaptativo
View(setores_gwr$SDF@data)
sqrt(mean(setores_gwr$SDF$gwr.e^2))
sqrt(mean(setores_gwr_adaptativo$SDF$gwr.e^2))
tm_shape(setores_gwr_adaptativo$SDF) + tm_fill("rede_esg", style="fisher")
tm_shape(setores_gwr_adaptativo$SDF) + tm_fill("localR2", style="fisher")

# Agrupamento
## Kmeans
setores_padronizados <- as.data.frame(scale(setores_juntos@data[,c("percvotonovo", "numeroafiliados", "rendapercapita2010", "GINI", "percate40anos")]))
View(setores_padronizados)
agrupamento <- kmeans(setores_padronizados, centers = 4, iter.max = 10000, nstart=10000) #demora um pouco
agrupamento$centers
View(agrupamento$cluster)
setores_juntos$kmeans <- agrupamento$cluster
tm_shape(setores_juntos) +
  tm_fill("kmeans", style = "cat", palette = c("blue","green", "yellow", "red"))
aggregate(setores_juntos@data[,c("Renda","rede_esg","col_lixo")], by=list(setores_juntos$kmeans), FUN = mean)

# Regionalizacao
## Skater
distancia_vizinhanca <- nbcosts(nb = vizinhanca2, data = setores_padronizados)
View(distancia_vizinhanca)
pesos_distancia <- nb2listw(vizinhanca2, glist=distancia_vizinhanca, style="B")
View(pesos_distancia)
mst_setores <- mstree(pesos_distancia)
View(mst_setores)
plot(st_geometry(setores_sf), border="red")
plot(mst_setores, setores_xy, lab=NA, add=TRUE)
regionalizacao <- skater(mst_setores[,1:2], setores_padronizados, ncuts=3) # NAO RODAR, DEMORA MUITO
View(regionalizacao$groups)
table(regionalizacao$groups)
setores_juntos$skater <- regionalizacao$groups
tm_shape(setores_juntos) +
  tm_fill("skater", style = "cat", palette = c("blue","green", "yellow", "red"))
aggregate(setores_juntos@data[,c("Renda","rede_esg","col_lixo")], by=list(setores_juntos$skater), FUN = mean)

# Exercicio 7 - A - Realize um agrupamento (kmeans) e uma regionalizacao (skater) de 3 grupos para os municipios da RMSP, considerando os dados socioeconomicos disponiveis (menos populacao).
#               B - Agregue os atributos com a media de cada grupo ou regiao

#Grave os dados antes de sair
save.image("D:/R_CTA/aula8/aula8.RData")







################################## SCRIPT ENXUTO

df <- rgdal::readOGR("C:\\Users\\afonso.machado\\GIT\\br_municipios")
class(df)

df_sf <- st_as_sf(df)
df_simplified <- st_simplify(df_sf)

rm(df,df_sf)

##DADOS
dados <- readxl::read_xlsx("Base final.xlsx")
dados <- read.csv2("https://raw.githubusercontent.com/afonsohbm/ProjetoFinal/master/agregadafinal.csv")

names(dados)

dados <- dados %>% select(cod, percvotonovo, rendapercapita2010, qtnovodepfed,
                          GINI, T_ANALF18M)

as.tibble(dados)
dados$percvotonovo <- gsub(",", ".", dados$percvotonovo) %>% as.double()

as.tibble(dados)

names(dados)[1] <- "CD_GEOCMU"
dados$CD_GEOCMU <- as.factor(dados$CD_GEOCMU)

##JOIN
df_sf_dados <- inner_join(df_simplified, dados, by = c("CD_GEOCMU", "CD_GEOCMU"))

##
setores_nao_nulo <- df_sf_dados %>% subset(is.na(df_sf_dados$percvotonovo)==FALSE) %>% 
  subset(is.na(df_sf_dados$qtnovodepfed)==FALSE) %>% 
  subset(is.na(df_sf_dados$rendapercapita2010) == FALSE) %>% 
  subset(is.na(df_sf_dados$GINI) == FALSE) %>% 
  subset(is.na(df_sf_dados$T_ANALF18M) == FALSE)



setores_valido <- setores_nao_nulo %>% 
  subset(setores_nao_nulo$rendapercapita2010 != 0) %>% 
  subset(setores_nao_nulo$GINI != 0) %>% 
  subset(setores_nao_nulo$T_ANALF18M != 0)

rm(setores_nao_nulo)

##
#library(cleangeo)
setores_sp <- as(setores_valido, "Spatial")

#analise_topologica <- clgeo_CollectionReport(setores_sp)
#clgeo_SummaryReport(analise_topologica)
#clgeo_SuspiciousFeatures(analise_topologica)
setores_clean <- clgeo_Clean(setores_sp)
clgeo_IsValid(setores_clean)

vizinhanca <- poly2nb(setores_clean)
setores_clean$vizinhos <- card(vizinhanca)
setores_juntos <- subset(setores_clean, setores_clean$vizinhos != 0)

##
vizinhanca2 <- poly2nb(setores_juntos)
setores_xy <- coordinates(setores_juntos)
vizinhanca_sp <- nb2lines(vizinhanca2, coords = setores_xy, proj4string = crs(setores_juntos))

## Lista de vizinhanca normalizada (pesos)
vizinhanca_pesos <- nb2listw(vizinhanca2)
vizinhanca_pesos_sp <- listw2lines(vizinhanca_pesos, coords = setores_xy, proj4string = crs(setores_sp))

## Analise de distancias
distancias <- nbdists(vizinhanca2, setores_xy)

#### Modelos espaciais
# Regressao simples
plot(data=setores_juntos, percvotonovo ~ qtnovodepfed + rendapercapita2010 + GINI + T_ANALF18M)
regressao_convencional <- lm(data=setores_juntos, percvotonovo ~ qtnovodepfed + rendapercapita2010 + GINI + T_ANALF18M) 
summary(regressao_convencional)
AIC(regressao_convencional)
plot(data=setores_juntos, percvotonovo ~ qtnovodepfed + rendapercapita2010 + GINI + T_ANALF18M)
abline(regressao_convencional, col="red", lwd=2)
setores_juntos$residuos <- regressao_convencional$residuals
tm_shape(setores_juntos) +
  tm_fill("residuos", style = "quantile", palette = heat.colors(5))
lm.morantest(regressao_convencional, listw = vizinhanca_pesos)

# Regressao espacial
library(spatialreg)
## Regressao com autocorrelacao espacial da variavel predita
regressao_espacial_lag <- lagsarlm(data=setores_juntos, 
                                   percvotonovo ~ qtnovodepfed + rendapercapita2010 + GINI + T_ANALF18M, 
                                   listw=vizinhanca_pesos) # demora um pouco
summary(regressao_espacial_lag, Nagelkerke = TRUE)
## Regressao com autocorrelacao espacial dos residuos
regressao_espacial_CAR <- spautolm(data=setores_juntos, 
                                   percvotonovo ~ qtnovodepfed + rendapercapita2010 + GINI + T_ANALF18M,
                                   listw=vizinhanca_pesos, family="CAR") # demora um pouco
summary(regressao_espacial_CAR, Nagelkerke = TRUE)

#############################################
##GWR##
#############################################
## Regressao ponderada geografica
library(spgwr)
### Raio global
raio <- gwr.sel(data=setores_juntos, percvotonovo ~ qtnovodepfed, rendapercapita2010 + GINI + T_ANALF18M) # NAO RODAR porque demora muito, e o resultado esta abaixo
raio
setores_gwr <- gwr(data=setores_juntos, 
                   percvotonovo ~ qtnovodepfed + rendapercapita2010 + GINI + T_ANALF18M, 
                   bandwidth=raio)
### Raio adaptativo
raio_adaptativo <- gwr.sel(data=setores_juntos, 
                           percvotonovo ~ qtnovodepfed + rendapercapita2010 + GINI + T_ANALF18M, 
                           adapt = TRUE) # NAO RODAR porque demora muito, e o resultado esta abaixo
raio_adaptativo
setores_gwr_adaptativo <- gwr(data=setores_juntos, 
                              percvotonovo ~ qtnovodepfed + rendapercapita2010 + GINI + T_ANALF18M, 
                              adapt=raio_adaptativo)
### comparando o raio global e o adaptativo
sqrt(mean(setores_gwr$SDF$gwr.e^2))
tm_shape(setores_gwr$SDF) + tm_fill("localR2", style = "fisher")

sqrt(mean(setores_gwr_adaptativo$SDF$gwr.e^2))
tm_shape(setores_gwr_adaptativo$SDF) + tm_fill("rede_esg", style="fisher")
tm_shape(setores_gwr_adaptativo$SDF) + tm_fill("localR2", style="fisher")


###############################
##NATURAL BREAKS DAS ENTRADAS##

tm_shape(setores_juntos) + 
  tm_fill("rendapercapita2010", style = "fisher") + 
  tm_layout(title = "Natural Breaks Map", title.position = c("right","bottom"))

tm_shape(setores_juntos) + 
  tm_fill("qtnovodepfed", style = "fisher") + 
  tm_layout(title = "Natural Breaks Map", title.position = c("right","bottom"))

tm_shape(setores_juntos) + 
  tm_fill("GINI", style = "fisher") + 
  tm_layout(title = "Natural Breaks Map", title.position = c("right","bottom"))

tm_shape(setores_juntos) + 
  tm_fill("T_ANALF18M", style = "fisher") + 
  tm_layout(title = "Natural Breaks Map", title.position = c("right","bottom"))

tm_shape(setores_juntos) + 
  tm_fill("percvotonovo", style = "fisher") + 
  tm_layout(title = "Natural Breaks Map", title.position = c("right","bottom"))



