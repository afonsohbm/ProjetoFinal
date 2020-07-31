library(dplyr)
library(sf)
library(spdep)
library(tidyverse)


df <- rgdal::readOGR("C:\\Users\\afonso.machado\\GIT\\br_municipios")
df <- st_as_sf(df)
df_simplified <- st_simplify(df)

object.size(df_simplified)
object.size(geom_br)

geom_br <- st_geometry(df_simplified)
plot(geom_br)



dados_bkp <- readxl::read_xlsx("Base final.xlsx")
dados <- read.csv2("https://raw.githubusercontent.com/afonsohbm/ProjetoFinal/master/agregadafinal.csv")


dados$percvotonovo <-gsub(",", ".", dados$percvotonovo) %>% as.double()

names(dados)[1] <- "CD_GEOCMU"
dados$CD_GEOCMU <- as.factor(dados$CD_GEOCMU)

teste <- inner_join(df_simplified, dados, by = c("CD_GEOCMU", "CD_GEOCMU"))

class(teste)

# create Queens contiguity matrix
spatmatrix <- poly2nb(teste)

# create a neighbours list with spatial weights
listw <- nb2listw(spatmatrix)

#Tratamento para numero de afliados
afiliados <- arrange(dados, desc(numeroafiliados))[1:25, c("cod", "municipio", "percvotonovo", "numeroafiliados")]

ggplot(afiliados, aes(x = municipio, y = percvotonovo)) + 
  geom_col(fill = "orange") + 
  geom_abline(intercept = mean(afiliados$percvotonovo), slope = 0, color = "red") +
  coord_flip()

#Tratamento para renda
renda <- arrange(dados, desc(rendapercapita2010))[1:25, c("cod", "municipio", "percvotonovo", "rendapercapita2010")]
ggplot(renda, aes(x = municipio, y = percvotonovo)) + 
  geom_col(fill = "orange") + 
  geom_abline(intercept = mean(renda$percvotonovo), slope = 0, color = "red") +
 ggtitle("Votação percentual nas 25 cidades com maior renda percapita") +
  coord_flip()

#melhorando o tratamento da renda
dados %>%  
  top_n(25, rendapercapita2010) %>% 
  ggplot(aes(x = municipio, y = percvotonovo)) + 
    geom_col(fill = "orange") + 
    geom_abline(intercept = mean(top_n(dados, 25, rendapercapita2010)$percvotonovo), slope = 0, color = "red") +
    ggtitle("Votação percentual nas 25 cidades com maior renda percapita") +
    coord_flip()

#Tratamento para energia --> não rolou
dados %>% 
  top_n(25, energiaeletrica) %>% 
  ggplot(aes(x = municipio, y = percvotonovo)) +
    geom_col(fill = "orange") +
    geom_abline(intercept = mean(top_n(dados, 25, energiaeletrica)$percvotonovo), 
                slope = 0, color = "red") +
    ggtitle("Votação percentual nas 25 cidades com maior acesso à energia elétrica") +
    coord_flip()

#TxAlfabetização
dados %>% 
  top_n(25, txalfabetizacao) %>% 
  ggplot(aes(x = municipio, y = percvotonovo)) +
  geom_col(fill = "orange") +
  geom_abline(intercept = mean(top_n(dados, 25, txalfabetizacao)$percvotonovo), 
              slope = 0, color = "red") +
  ggtitle("Votação percentual nas 25 cidades com maior taxa de alfabetização") +
  coord_flip()

#E_ANOSESTUDO
dados %>% 
  top_n(25, E_ANOSESTUDO) %>% 
  ggplot(aes(x = municipio, y = percvotonovo)) +
  geom_col(fill = "orange") +
  geom_abline(intercept = mean(top_n(dados, 25, E_ANOSESTUDO)$percvotonovo), 
              slope = 0, color = "red") +
  ggtitle("Votação percentual nas 25 cidades com maior tempo de estudo") +
  coord_flip()

#IDHM
dados %>% 
  top_n(25, IDHM) %>% 
  ggplot(aes(x = municipio, y = percvotonovo)) +
  geom_col(fill = "orange") +
  geom_abline(intercept = mean(top_n(dados, 25, IDHM)$percvotonovo), 
              slope = 0, color = "red") +
  ggtitle("Votação percentual nas 25 cidades com maior IDH") +
  coord_flip()

#GINI
dados %>% 
  top_n(25, GINI) %>% 
  ggplot(aes(x=municipio, y = percvotonovo)) +
  geom_col(fill = "orange") + 
  geom_abline(intercept = mean(top_n(dados, 25, GINI)$percvotonovo),
              slope = 0, color = "red") +
  ggtitle("Votação percentual nas 25 cidades com maior índice de Gini") +
  coord_flip()

#T_ANALF18M
dados %>% 
  top_n(-25, T_ANALF18M) %>% 
  ggplot(aes(x=municipio, y = percvotonovo)) +
  geom_col(fill = "orange") + 
  geom_abline(intercept = mean(top_n(dados, -25, T_ANALF18M)$percvotonovo),
              slope = 0, color = "red") +
  ggtitle("Votação percentual nas 25 cidades com menor taxa de analfabetismo") +
  coord_flip()




#Pegando os municipios a baixo da média de votos.
teste <- filter(TRABCC, percvotonovo < mean(percvotonovo)) %>% mutate(tabela = "TRABCC") %>% select(1,5)

teste2 <- left_join(df_simplified, teste, by = c("CD_GEOCMU" = "cod"))



