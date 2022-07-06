# Активируем пакеты
library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")

# генерируем случайные величины
# Z_1,......,Z_100 ~ N(5, 9)
z <- rnorm(100, mean=5, sd =3)
z[56]
qplot(z)

# построим функцию плотности
x <- seq(-10,15,by=0.5)
y <- dnorm(x, mean=5, sd=3)
qplot(x,y)
qplot(x,y, geom='line')

# P(Z<3)
pnorm(3, mean=5, sd=3)

# P(Z\in [4;9])
# P(Z<9)-P(Z<4)
pnorm(9, mean=5, sd=3) - pnorm(4, mean=5, sd=3)

# P(Z<a)=0.7 a?
qnorm(0.7, mean=5, sd=3)

# chisq, t, f
rchisq, dchisq, pchisq, qchisq
rt, dt, pt, qt

# мнржественная регрессия, проверка гипотез
h <- swiss
glimpse(h)

model <- lm(data=h, Fertility~Catholic+Agriculture+Examination)
summary(model)

coeftest(model)
confint(model) # доверительные интервалы

sjp.lm(model) # визуализация интервалов

# проверка гипотезы b_Cath=b_Agri

model_aux <- lm(data=h, Fertility~Catholic+I(Catholic+Agriculture)+Examination)
summary(model_aux)

linearHypothesis(model, "Catholic-Agriculture")

# стандартизированные коэффициенты

h_st <- mutate_each(h, "scale")
glimpse(h_st)
model_st <- lm(data=h_st, Fertility~Catholic+Agriculture+Examination)
summary(model_st)
sjp.lm(model, showStandardBeta = TRUE) # стандартизированные коэффициенты

# искусственный эксперимент

D <- matrix(nrow=100, rnorm(100*41, mean=0, sd=1))
df <- data.frame(D)

model_pusto <- lm(data=df, X1~. )
summary(model_pusto)

# сравнить несколько моделей
model2 <- lm(data=h, Fertility~Catholic+Agriculture)
compar_12 <- mtable(model, model2)
compar_12

# сохранение результатов работы 

stuff <- list(data=h, model=model2)
saveRDS(file = "mydata.RDS",stuff)

mylist <- readRDS("mydata.RDS")
mylist$model

# csv

t <- read.csv('flats_moscow.txt', sep="\t", dec=".", header=TRUE)
