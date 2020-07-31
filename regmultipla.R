library(ggplot2)
library(dplyr)
library(GGally)
library(MASS)
library(tidyr)
library(stringr)
library(lubridate)
library(Rcpp)
library(rpart)
library(ROCR)

setwd("X:/Documentos/MBA/Projeto Analítico/Base Proj Analítico")
getwd()

read.csv2("agregada.csv")->novo
View(novo)

# Regressão Linear Múltipla
fit <- lm(percvotonovo ~ numeroafiliados + txalfabetizacao + energiaeletrica + X.catolicos + rendapercapita2010 + X.ate40anos + qtnovodepfed + acessointernet2015, data=novo)
summary(fit) # mostra os resultados

fit <- lm(percvotonovo ~ numeroafiliados + txalfabetizacao + energiaeletrica + X.catolicos + rendapercapita2010 + qtnovodepfed + acessointernet2015, data=novo)
summary(fit) # mostra os resultados

novo["percvotonovox100"]<-novo$percvotonovo * 100
View(novo)

fit <- lm(percvotonovox100 ~ numeroafiliados + txalfabetizacao + energiaeletrica + X.catolicos + rendapercapita2010 + qtnovodepfed + acessointernet2015, data=novo)
summary(fit) # mostra os resultados