######################################################################
##### Modelando a série temporal de roubo a veiculos e a ST de emprego
#######################################################################

options(warn=0)
requiredPackages = c('BETS','forecast','tidyverse','gdata','caret','plyr','rlang','digest','dplyr')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) suppressMessages(suppressWarnings(install.packages(p)))
  suppressMessages(suppressWarnings(library(p,character.only = TRUE)))
}

## Trazendo a série temporal de empregos formais gerados - Rio de Janeirocom o pacote BETS
# 13395
#install.packages("BETS") 12779
emprego2 <- BETSget(code = 12779) # serie com problema de leitura
library(BETS)
emprego <- BETSget(code = 13395)
plot(emprego)
str(emprego)

options(warn=0)
load(file="D:\\MBA Big Data\\TCC\\df_isp.RData")
head(df_isp)
summary(df_isp)

df <- df_isp %>%
  filter(DP %in% c("Delegacia de Roubos e Furtos de Automóveis"))  %>%
  group_by(ano, MES) %>%
  do(data.frame(nrow=nrow(.)))

df$MesNum <- ifelse(df$MES=="janeiro",1,df$MES)
df$MesNum <- ifelse(df$MES=="fevereiro",2,df$MES)
df$MesNum <- ifelse(df$MES=="março",3,df$MES)
df$MesNum <- ifelse(df$MES=="abril",4,df$MES)
df$MesNum <- ifelse(df$MES=="maio",5,df$MES)
df$MesNum <- ifelse(df$MES=="junho",6,df$MES)
df$MesNum <- ifelse(df$MES=="julho",7,df$MES)
df$MesNum <- ifelse(df$MES=="agosto",8,df$MES)
df$MesNum <- ifelse(df$MES=="setembro",9,df$MES)
df$MesNum <- ifelse(df$MES=="outubro",10,df$MES)
df$MesNum <- ifelse(df$MES=="novembro",11,df$MES)
df$MesNum <- ifelse(df$MES=="dezembro",12,df$MES)

df$ano <- factor(df$ano)

head(df)

roubo <- ts(df$nrow, start = c(2010,1), frequency = 12)
plot(roubo)
str(roubo)
tail(roubo)

## Modelando

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

# Próximos passos - usar a serie temporal de emprego acima como antecedente de determinado tipo de crime 



