######################################################################
##### Modelando a série temporal de roubo a veiculos e a ST de emprego
#######################################################################

options(warn=0)
requiredPackages = c('BETS','forecast','tidyverse','gdata','tidyr','caret','plyr','rlang','digest','dplyr','BETS')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) suppressMessages(suppressWarnings(install.packages(p)))
  suppressMessages(suppressWarnings(library(p,character.only = TRUE)))
}

library(forecast)

options(warn=0)
load(file="D:\\MBA Big Data\\TCC\\df_isp.RData")
head(df_isp)
summary(df_isp)

df <- df_isp %>%
  filter(DP %in% c("Delegacia de Roubos e Furtos de Automóveis"))  %>%
  group_by(ano, MES) %>%
  do(data.frame(nrow=nrow(.)))

df$MesNum <- NA

df$MesNum[df$MES=="janeiro"] <- 1
df$MesNum[df$MES=="fevereiro"] <- 2
df$MesNum[df$MES=="março"] <- 3
df$MesNum[df$MES=="abril"] <- 4
df$MesNum[df$MES=="maio"] <- 5
df$MesNum[df$MES=="junho"] <- 6
df$MesNum[df$MES=="julho"] <- 7
df$MesNum[df$MES=="agosto"] <- 8
df$MesNum[df$MES=="setembro"] <- 9
df$MesNum[df$MES=="outubro"] <- 10
df$MesNum[df$MES=="novembro"] <- 11
df$MesNum[df$MES=="dezembro"] <- 12

df$ano <- factor(df$ano)

head(df)

df2 <- df %>%
  arrange(ano, MesNum)

head(df2)

roubo <- ts(df$nrow, start = c(2010,1), frequency = 12)
plot(roubo)
str(roubo)
tail(roubo)

###################################
## Modelando somente e ST de roubo
###################################

# conjunto de teste e treinamento
training <- window(roubo, end = c(2017,12))
test <- window(roubo, start = c(2018,1))

## Naive
library(forecast)
fit.naive <- naive(training, h = 12) 
checkresiduals(fit.naive)
accuracy(fit.naive,test)
autoplot(fit.naive) + autolayer(test, series = "conjunto de teste") + 
  autolayer(fitted(fit.naive), series = "valores ajustados")

## SNaive
fit.snaive <- snaive(training, h = 12)
checkresiduals(fit.snaive)
accuracy(fit.snaive,test)
autoplot(fit.snaive) + autolayer(test, series = "conjunto de teste") + 
  autolayer(fitted(fit.snaive), series = "valores ajustados")

## Modelo de Holt Winters
fit.HW <- HoltWinters(training)
forc.HW <- forecast(object = fit.HW, h = 12, level = 0.95)
accuracy(forc.HW,test)
autoplot(forc.HW) + autolayer(test, series = "conjunto de teste") + 
  autolayer(fitted(forc.HW), series = "valores ajustados")

## Modelo SARIMA
fit_ARIMA <- auto.arima(training)
fit_ARIMA
t_test(fit_ARIMA)
checkresiduals(fit_ARIMA)

forc.SARIMA <- forecast(object = fit_ARIMA, h = 12, level = 0.95)
autoplot(forc.SARIMA) + autolayer(test, series = "conjunto de teste") + 
  autolayer(fitted(forc.SARIMA), series = "valores ajustados")
accuracy(forc.SARIMA,test)

fit_ARIMA <- auto.arima(training)
fit_ARIMA
t_test(fit_ARIMA)
checkresiduals(fit_ARIMA)

# Próximos passos - definir dummies para ocupaçao de militares no Rio
