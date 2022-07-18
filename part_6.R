library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # тесты
library("bstats") # больше тестов
library("zoo") # временные ряды
library("xts") # ещё ряды
library("dplyr") # манипуляции с данными
library("broom") # манипуляция
library("ggplot2") # графики
library("lubridate") # работа с датами
library("quantmod") # загрузка с finance.google.com
library("rusquant") # загрузка с finam.ru
library("sophisthse") # загрузка с sophist.hse.ru
library("Quandl") # загрузка с Quandl

# создадим вектор из строк
x <- c("2012-04-15","2011-08-17")
# конвертируем текстовый формат даты в стандартный формат даты
y <- ymd(x)
# теперь можно работать с датами как с обычными числами
y + days(20)
day(y)

# создадим временной ряд
x <- rnorm(5)
x
y <- ymd("2014-01-01")+days(0:4) # стартовая дата + дни от 0 до 4
y
ts <- zoo(x,order.by=y) # привязываем к каждому числа дату
ts
lag(ts,-1) # посмотрим прошлое значение переменной на шаг день назад
diff(ts) # на сколько менялись показатели

ts2 <- zooreg(x,start=as.yearqtr("2014-01"),freq=4) # hегулярный временной ряд с квартальными наблюдениями
ts2
ts3 <- zooreg(x,start=as.yearmon("2014-01"),freq=12) # vесячный
ts3
# базовые действия с временными рядами
data("Investment")
start(Investment) # когда начинаются данные
end(Investment) # когда заканчивается временной ряд
time(Investment) # посмотреть индексы
coredata(Investment) # значения без даты

# добавим пропущенные значения
dna <- Investment
dna[1,2] <- NA
dna[5,3] <- NA
na.approx(dna) # среднее значений по краям
na.locf(dna) # gеренос вперед предыдущего значения

# загрузка данных из сайта
a <- sophisthse("POPNUM_Y")
a
b <- Quandl("FRED/GNP")
b
# finance.google.com
sys.setlocal("LC_TIME","C")
getSymbols(Symbols = "AAPL",from="2010-01-01",to="2014-02-03",src="google") # fкции apple

getSymbols(Symbols = "GAZP",from="2011-01-02",to="2014-09-09",src="Finam") # акции Газпрома
head(GAZP)
tail(GAZP)

plot(GAZP)
autoplot(GAZP[,1:4]) # отдельные
autoplot(GAZP,[,1:4],facets = NULL) # на одном графике
chartSeries(GAZP) # все данные

# Анализ набора данных

d <- as.zoo(Investment) # yеупорядоченный ряд
autoplot(d[,1:2],facets = NULL)

# оценим модель с помощью наименьших квадратов и есть ли автокореляция

model <- lm(data=d, RealInv~RealInt+RealGNP)
summary(model) # результат
coeftest(model) # проверка гипотез о равенстве коэф. нулю
confint(model) # доверительные интервалы

# проверим есть ли автокорреляция
# дополним наш массив с помошбю оцененых остатков
d_aug <- augment(model, as.data.frame(d))
glimpse(d_aug)
qplot(data=d_aug, lag(.resid),.resid)

vcov(model) # расчеты несостоятельны, так как имеется автокорреляция
vcovHAC(model) # устойчиво к автокорреляции

coeftest(model,vcov. = vcovHAC(model))
conftable <- coeftest(model,vcov. = vcovHAC(model))
# построим доверительные интервалы
ci <- data.frame(estimate=conftable[,1],se_ac=conftable[,2])
ci

ci <- mutate(ci, left_95=estimate-1.96*se_ac, right_95=estimate+1.96*se_ac)
ci

# тесты на автокорреляцию
# Durbin-Watson H0: нет автокорреляции H1: автокорреляция 1-го порядка
dwt(model)
res <- dwt(model)
res$dw
res$p
res$r

# BG-test H0: нет автокорреляции H1: автокореляция k-го порядка
bgtest(model,order = 2)
# H0 не отвергается (не хватило дааных для отвержения)

