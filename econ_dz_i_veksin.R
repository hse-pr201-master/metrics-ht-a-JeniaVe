library(tidyverse)
library(rio) 
library(lmtest)
library(skimr) 
library(mfx) 
library(texreg) 
library(mctest)
library(car)
library(perturb)
library(pastecs)
library(olsrr)
library(het.test)
library(sandwich)
library(nlme)

mydata <- read.table("C:/Users/Archaedas/Desktop/econometri_dz/forestfires.csv", header=TRUE,
                     sep=",")

# Удалим переменные координат парка, какая разница, какая часть выгорает сильнее
# Удалим переменную дня. Вряд ли есть разница между выгоранием в один день недели и в другой. Оставим месяцы.

mydata$X <- NULL
mydata$Y <- NULL
mydata$day <- NULL

# Удалим все наблюдения, где ничего не выгорело.
# Заметим, что переменная ветра в полученной выборке равно нулю практически во всех наблюдениях, ее можно удалить.

mydata <- mydata[!(mydata$area == 0),]

# Посмотрим на то, для каких месяцев в основном получены данные

ftable(mydata$month)

# Создадим дамми-переменные на каждый месяц и будем использовать в модели только те, которые чаще наблюдаются в отобранных данных.

mydata$jan <- 1L * (mydata$month == "jan")
mydata$feb <- 1L * (mydata$month == "feb")
mydata$mar <- 1L * (mydata$month == "mar")
mydata$apr <- 1L * (mydata$month == "apr")
mydata$may <- 1L * (mydata$month == "may")
mydata$jun <- 1L * (mydata$month == "jun")
mydata$jul <- 1L * (mydata$month == "jul")
mydata$aug <- 1L * (mydata$month == "aug")
mydata$sep <- 1L * (mydata$month == "sep")
mydata$oct <- 1L * (mydata$month == "opt")
mydata$nov <- 1L * (mydata$month == "nov")
mydata$dec <- 1L * (mydata$month == "dec")

# Оставшиеся данные делятся на индексы и погодные условия.

# Далее, посмотрим на описательные статистики к индексам: 

hist(mydata$FFMC, main="Histogram for Fine Fuel Moisture Code", xlab="value of index")
boxplot(mydata$FFMC, main="Fine Fuel Moisture Code", ylab="index")
stat.desc(mydata$FFMC)

hist(mydata$DMC, main="Histogram for Duff Moisture Code", xlab="value of index")
boxplot(mydata$DMC, main="Duff Moisture Code", ylab="index")
stat.desc(mydata$DMC)

hist(mydata$DC, main="Histogram for Drought Code", xlab="value of index")
boxplot(mydata$DC, main="Drought Code", ylab="index")
stat.desc(mydata$DC)

hist(mydata$ISI, main="Histogram for Initial Spread Index", xlab="value of index")
boxplot(mydata$ISI, main="Initial Spread Index", ylab="index")
stat.desc(mydata$ISI)

# К погодным условиям

hist(mydata$temp, main="Histogram for temperature", xlab="Temperature in Celsius", xlim=c(0,40))
boxplot(mydata$temp, main="Temperature", ylab="Temperature in Celsius")
stat.desc(mydata$temp)

hist(mydata$RH, main="Histogram for relative humidity", xlab="Relative humidity in %", xlim=c(0,100))
boxplot(mydata$RH, main="Relative humidity", ylab="Relative humidity in %")
stat.desc(mydata$RH)

hist(mydata$wind, main="Histogram for wind", xlab="Wind speed in km/h", xlim=c(0,10))
boxplot(mydata$wind, main="Wind speed", ylab="Wind speed in km/h")
stat.desc(mydata$wind)

# Для зависимой переменной area

hist(mydata$area, main="Histogram for Burned Area", xlab="the burned area in ha")
boxplot(mydata$area, main="Burned Area", ylab="ha")
stat.desc(mydata$area)

# Официальный сайт с данными индексов https://cwfis.cfs.nrcan.gc.ca/background/summary/fwi
# показывает, что эти индексы строятся на основе погодных данных, которые уже присутствуют в исходных данных
# Тогда вероятна мультиколлинеарность, проверим это:

# Посмотрим параметры VIF и CN в модели с включенными индексами и погодными условиями:

m1 = lm(data = mydata, area ~ 1 + FFMC + DMC + DC + ISI + temp + RH + wind)
summary(m1)
vif(lm(data = mydata, area ~ 1 + FFMC + DMC + DC + ISI + temp + RH + wind))
colldiag(lm(data = mydata, area ~ 1 + FFMC + DMC + DC + ISI + temp + RH + wind))

# Посмотрим на модель только с погодными условиями:

m2 = lm(data = mydata, area ~ 1 + temp + RH + wind)
summary(m2)
vif(lm(data = mydata, area ~ 1 + temp + RH + wind))
colldiag(lm(data = mydata, area ~ 1 + temp + RH + wind))

# Видно, что в 1-ом случае присутствует серьезная мультиколлиенарность.

# Оставим в модели только погодные условия и следующие месяцы: Август, Сентябрь, Июль и Март.
# тогда модель будет анализировать ситуацию в наиболее пожаропасное время.

# Также можно добавить зависимость от квадрата температуры.
# А также сделать переменную, где с комплементарностью: температуру * ветер. 


mydata$temp2 <- (mydata$temp) * (mydata$temp)
mydata$tempwind <- (mydata$temp) * (mydata$wind)

# Можно предположить, что количество сгоревших лесов положительно зависит от температуры и скорости ветра
# отрицательно от влажности, а также положительно от квадрата температуры и от произведения температура*ветер

# Построим линейную модель:

m3 = lm(data = mydata, area ~ 1 + temp + RH + wind + aug + sep + may + jul + temp2 + tempwind)
summary(m3)

# Все коэффициеты не значимы на интервалах 5% и даже 10%. При этом есть некоторые особенности:
# Коэффициенты при переменных погодных условий в целом более значимы чем коэффициенты дамми-переменных месяцев  
# Коэффициента при квадрате температуры и произведении температуры и скорости ветра еще более значимы (на уровне 28-40%)
# Целевая переменная, как выяснилось, отрицательно зависит от температуры и скорости ветра.
# Однако гипотезы о положительном влиянии искуственных переменных temp2 и tempwind
# и отрицательном влиянии влажности подтверждаются при повышенном уровне значимости.

ols_plot_resid_qq(m3)
ols_test_normality(m3)

# Остатки регрессии не являются нормальными по любому тесту 

stat.desc(mydata$temp2)
stat.desc(mydata$tempwind)

# Точечный прогноз

conf_interval_3 <- predict(m3, newdata=data.frame(temp=20.1, RH=41, wind=4, temp2=404.01, tempwind=69.5, aug=1, sep=0, may=0, jul=0),
                           interval="confidence",
                           level = 0.95)
conf_interval_3


# Индивидуальный прогноз

conf_interval_4 <- predict(m3, newdata=data.frame(temp=20.1, RH=41, wind=4, temp2=404.01, tempwind=69.5, aug=1, sep=0, may=0, jul=0),
                           interval="prediction",
                           level = 0.95)
conf_interval_4

# Судя по значениям интервалы скорее недоверительные из-за незначимости коэффициентов.

# Вероятно, гетероскедастичность может быть вызвана наиболее значимыми переменными:
# температурой в квадрате и произведением температуры и ветра. 

# Сделаем проверку гетероскедастичности

vcovHC(m3)
coeftest(m3, vcov = vcovHC(m3))

gqtest(m3)

# Построим регрессию со взвешенным МНК:

fdata <- mydata

fdata$month <- NULL
fdata$FFMC <- NULL
fdata$DMC <- NULL
fdata$DC <- NULL
fdata$ISI <- NULL
fdata$jan <- NULL
fdata$feb <- NULL
fdata$mar <- NULL
fdata$apr <- NULL
fdata$jun <- NULL
fdata$oct <- NULL
fdata$nov <- NULL
fdata$dec <- NULL
fdata$rain <- NULL

vf1 <- varFunc(~ 1 + temp + RH + wind + aug + sep + may + jul + temp2 + tempwind)

m4 <- gls(area ~ 1 + temp + RH + wind + aug + sep + may + jul + temp2 + tempwind, weights = vf1, data = fdata)
summary(m4)

# HC0 строим ошибки

mydata$predi <- predict(m3)
mydata$HC0errors <- mydata$area - mydata$predi

# Делим каждое наблюдение на его оцененную дисперсию:

hcdata <- mydata

hcdata$hcarea <- (mydata$area)/(abs(mydata$HC0errors))
hcdata$hctemp <- (mydata$temp)/(abs(mydata$HC0errors))
hcdata$hcRH <- (mydata$RH)/(abs(mydata$HC0errors))
hcdata$hcwind <- (mydata$wind)/(abs(mydata$HC0errors))
hcdata$hctemp2 <- (mydata$temp2)/(abs(mydata$HC0errors))
hcdata$hctempwind <- (mydata$tempwind)/(abs(mydata$HC0errors))
hcdata$hcmay <- (mydata$may)/(abs(mydata$HC0errors))
hcdata$hcaug <- (mydata$aug)/(abs(mydata$HC0errors))
hcdata$hcjul <- (mydata$jul)/(abs(mydata$HC0errors))
hcdata$hcsep <- (mydata$sep)/(abs(mydata$HC0errors))

m5 = lm(data = hcdata, hcarea ~ 1 + hctemp + hcRH + hcwind + hcaug + hcsep + hcmay + hcjul + hctemp2 + hctempwind)
summary(m5)

# PCA. Исключим месяцы. Выделим две главные компоненты и построим по ним регрессию

pcdata.pca <- prcomp(mydata[,c(6:9,23,24)], center = TRUE,scale. = TRUE)
summary(pcdata.pca)

pc1 <- pcdata.pca$x[,1]
pc2 <- pcdata.pca$x[,2]

mydata$pci <- pc1
mydata$pcii <- pc2

m6 = lm(data = mydata, area ~ 1 + pci + pcii)
summary(m6)

# Видно, что первая главная компонента становится значимой