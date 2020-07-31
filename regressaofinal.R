setwd("X:/Documentos/MBA/Projeto Analítico/Base Proj Analítico")
getwd()

read.csv2("agregadafinal2.csv")->base
View(baseteste)

cor(base, use="complete.obs", method="kendall")

install.packages("corrplot")
library(corrplot)

corrplot(cor(agorasim), method="circle")

read.csv2("agregadafinal2.csv")->baseteste
baseteste$RDPC <- NULL
baseteste$cod <- NULL
baseteste$votos <- NULL
agorasim <- baseteste[,c(8,1,2,3,4,5,6,7,9:ncol(baseteste))]

fit <- lm(percvotonovo ~ numeroafiliados + rendapercapita2010 + percate40anos + qtnovodepfed
          + IDHM + E_ANOSESTUDO + T_ANALF18M + GINI + P_FORMAL + P_SUPER + TRABCC, data=base)
summary(fit) # mostra os resultados

fit2 <- lm(percvotonovo ~ numeroafiliados + rendapercapita2010 + percate40anos + qtnovodepfed
          + IDHM + E_ANOSESTUDO + T_ANALF18M + GINI + P_SUPER + TRABCC, data=base)
summary(fit2) # mostra os resultados

fit3 <- lm(percvotonovo ~ numeroafiliados + rendapercapita2010 + qtnovodepfed
          + E_ANOSESTUDO + T_ANALF18M + GINI + P_SUPER + TRABCC, data=base)
summary(fit3) # mostra os resultados

install.packages("faraway")
library(faraway)

vif(fit3)

fit3 <- lm(percvotonovo ~  rendapercapita2010 + qtnovodepfed
           + E_ANOSESTUDO + T_ANALF18M + GINI + P_SUPER + TRABCC, data=base)
summary(fit3) # mostra os resultados

vif(fit3)

install.packages("faraway")
library(faraway)

install.packages("lmtest")
library(lmtest)
dwtest(fit5)

res <- rstandard(fit5);res
shapiro.test(res)

install.packages("dgof")
library(dgof)
hist(base$percvotonovo)
ks.test(base$percvotonovo,"pnorm",mean(base$percvotonovo),sd(base$percvotonovo))

bptest(fit5)

whites.htest(fit5)

fit4 <- lm(percvotonovo ~  rendapercapita2010 + qtnovodepfed
           + pesotot + T_ANALF18M + GINI + P_SUPER + TRABCC, data=base)
summary(fit4) # mostra os resultados
vif(fit4)

fit5 <- lm(percvotonovo ~  rendapercapita2010 + qtnovodepfed + T_ANALF18M + GINI, data=base)
summary(fit5) # mostra os resultados
vif(fit5)

AIC(fit5)

install.packages("spgwr")
install.packages("spdep")
install.packages("tmap")
library(spgwr)
library(sp)
library(spdep)
library(sf)
library(tmap)

setwd("X:/Base Proj Analítico/shapefile/T_LM_MUNICIPIOS_2010")
getwd()
mapa2 <- st_read("T_LM_MUNICIPIOS_2010Polygon.shp")



fit.ols<-glm(percv_novo ~  renda_2010 + qtnov_pfed + T_ANALF18M + GINI, data = mapa2)
summary(fit.ols)

mapa2.sp <- as(mapa2, "Spatial")

gwr.b1<-gwr.sel(percv_novo ~  renda_2010 + qtnov_pfed + T_ANALF18M + GINI, mapa2.sp)
gwr.b1


gwr.fit1<-gwr(percv_novo ~ renda_2010 + qtnov_pfed + T_ANALF18M + GINI, data = mapa2.sp, bandwidth = gwr.b1, se.fit=T, hatmatrix=T)
gwr.fit1

gwr.b2<-gwr.sel(percv_novo ~  renda_2010 + qtnov_pfed + T_ANALF18M + GINI, mapa2.sp, gweight = gwr.bisquare)

gwr.fit2<-gwr(percv_novo ~  renda_2010 + qtnov_pfed + T_ANALF18M + GINI, mapa2.sp,
              bandwidth = gwr.b2, gweight = gwr.bisquare, se.fit=T, hatmatrix=T)
gwr.fit2

gwr.b3<-gwr.sel(percv_novo ~  renda_2010 + qtnov_pfed + T_ANALF18M + GINI, mapa2.sp, adapt = TRUE)

gwr.fit3<-gwr(percv_novo ~  renda_2010 + qtnov_pfed + T_ANALF18M + GINI, mapa2.sp, adapt=gwr.b3, se.fit=T, hatmatrix=T)
gwr.fit3

saveRDS(gwr.fit1, file = "reggwr1.RData")
