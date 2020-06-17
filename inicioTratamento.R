library(dplyr)
library(sf)
library(spdep)


df <- rgdal::readOGR("C:\\Users\\afonso.machado\\GIT\\ProjetoFinal\\Artigos\\br_municipios")
df <- st_as_sf(df)
df_simplified <- st_simplify(df)

object.size(df2_simplified)

geom_br <- st_geometry(df2_simplified)
plot(geom_br)



dados <- readxl::read_xlsx("Base final.xlsx")
names(dados)[1] <- "CD_GEOCMU"
dados$CD_GEOCMU <- as.factor(dados$CD_GEOCMU)

teste <- inner_join(df_simplified, dados, by = c("CD_GEOCMU", "CD_GEOCMU"))

class(teste)

# create Queens contiguity matrix
spatmatrix <- poly2nb(teste)

# create a neighbours list with spatial weights
listw <- nb2listw(spatmatrix)

#Tratamento para numero de afliados
afiliados <- arrange(dados, desc(numeroafiliados))[1:25, c(1:2,4,6)]

ggplot(afiliados, aes(x = município, y = VOTOS)) + 
  geom_col(fill = "orange") + 
  geom_abline(intercept = mean(afiliados$VOTOS), slope = 0, color = "red") +
  coord_flip()

#Tratamento para renda
renda <- arrange(dados, desc(rendapercapita2010))[1:25, c(1:2,4,10)]
ggplot(renda, aes(x = município, y = VOTOS)) + 
  geom_col(fill = "orange") + 
  geom_abline(intercept = mean(renda$VOTOS), slope = 0, color = "red") +
 ggtitle("Votação percentual nas 25 cidades com maior renda percapita") +
  coord_flip()

#Tratamento para acesso à internet
internet <- arrange(dados, desc(acessointernet2015))[1:25,c(1:2,4,13)]
ggplot(internet, aes(x = município, y = acessointernet2015)) + 
  geom_col(fill = "orange") + 
  geom_abline(intercept = mean(internet$acessointernet2015), slope = 0, color = "red") +
  coord_flip()




