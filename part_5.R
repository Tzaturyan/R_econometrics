# написание функций 
f <- function(x) { # aункция аозведения в квадрат
  res <- x^2
  return(res)
}
f(3)

fs <- function(x, stepen=2) { 
  res <- x^stepen
  return(res)
}
fs(2,stepen=5)

# подсчитаем процент пропущенне наблюдения
d <- cars
d[1,2] <- NA
d[3,1] <- NA
d

na_perc <- function(d) {
  if(!is.data.frame(d)) stop("d should ba a data.frame")  # проверка на то что входные данные это таблицы
  res <- sum(is.na(d))/nrow(d)/ncol(d)
  return(res)
}
na_perc(d)

# написание циклов
for (i in 5^10) {
  k <- i^2
  cat("i=",i," i^2=",k, "\n")
}

# загрузим файлы из нескольких переменных в одну таблицу

all_data <- NULL
for (fname in c("file01.csv","file02.csv")) {
  temp <- read.csv(fname)
  all_data <- rbind(all_data,temp)
}
head(all_data)

library("sandwich")
library("lmtest")
library("car")
library("dplyr")
library("broom")
library("ggplot2")

h <- read.table("flats_moscow.txt", head=TRUE)
head(h)
tail(h)

qplot(data=h, x=totsp, y=price)
# наблюдаем условную гетероскедастичность, так как видим большой разброс данных по x
model <- lm(price~totsp, data=h) # МНК
summary(model)

coeftest(model) # стандартные ошщибки рассчитаны исходя из гомоскедастичности
confint(model) # доверительный интервал неверный, так как неправильно считается оцена ков матрицы
vcov(model)

# проверим предварительно гетероскедастичность с помощью остатков графически
h <- augment(model,h)
glimse(h)
qplot(data=h,totsp,abs(.resid)) # dектор скедастичности

vcovHC(model) # получим оценку устойчивой гетероскедастичности
vcovHC(model,type="HC2") # для малой выборки
# проверим гипотезу о равенстве коэффициентов
сoeftest(model,vcov. =vcovHC(model))

# найдем доверительный интервал 95%

conftable <- сoeftest(model,vcov. =vcovHC(model))
ci <- data.frame(estimate=conftable[,1],se_hc=[,2])
ci < mutate(ci,left_ci=estimate-1.96*se_hc, right_ci=estimate+1.96*se_hc)

# Гетероскедастичность расширила доверительный интервал

# Nест на гетероскедастичность

bptest(model) # gjмимо оценки основной регрессии оценивается и вспомогательная
# p-value vаленький и гипотеза об условной гомоскедастичностью была отвергнута
bptesy(model, data=h, varformula = ~ totsp + I(totsp^2) )

# тест Голфилда Кванта
gqtest(model, order.by = ~totsp, data=h, fraction = 0.2)
# ubпотеза об условной гемоскедастичностью отвергается

# для борьбы с гетескедастичностью можно перевести переменные в логорифм, 
# заново оценить модель и когда мы снова проведем тест то получим значительное снижение гетероскедастичности

model2 <- lm(data=h, log(price)~log(totsp))
gqtest(model2, order.by = ~totsp, data=h, fraction = 0.2)

# на основании вывода о гетескедастичности выбираем какую стандартную ошибку используем
# избегаем двойного тестирования
# прибегаем к тесту когда в этом есть теоритическая необходимость









