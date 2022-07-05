x <- c(23,15,46,NA)
z <- c(5,6,NA,8)
mean(x) # среднее
mean(x,na.rm = TRUE) # среднее без пропусков

sum(x) # сумма
sum(x,na.rm = TRUE) # сумма без пропусков

d <- data.frame(rost=x,ves=z) # создадим таблицу

d[4,1] # выбор элементов таблицы
d[,2] # выбор всего столюца

d$rost # выбор переменой 

my_list <- list(a=7,b=10:20,table=d) # совмещение различных объектов

# загрузка пакетов

library("psych")
library("dplyr")
library("ggplot2")
library("GGally")

# откроем встроенный набор данных
d <- cars
glimpse(d) # посмотрим на данные
help(cars)
head(d)
tail(d)
describe(d)
ncol(d)
nrow(d)
str(d)
mean(d$speed)
# переведем фунты в км/ч и создание новых переменных
d2 <- mutate(d, speed=1.67*speed, 
             dist=0.3*dist, ratio=dist/speed)
# построим гистограмму
qplot(data=d2, dist)
# добавим описание к гистограмме
qplot(data=d2, dist, xlab="Длина тормозного пути (м)", 
      ylab="Кол-во машин", main="Данные 1920х годов")

# построим модель линейной регрессии
model <- lm(data=d2, dist~speed)
model
# получаем что при увеличении дистанции торможения 
# на 1 то скорость увеличивается на 0.7

beat_hat <- coef(model)
beat_hat
eps_hat <- residuals(model)
eps_hat
# y прогнозная
y_hat <- fitted(model)
y_hat
y <- d2$dist
# остаточная сумма квадратов
RSS <- deviance(model)
RSS
# сумма квадратов отклонения y от среднего
TSS <- sum((y-mean(y))^2)
ESS <- TSS-RSS
ESS
# отношение объясненной дисперсии к TSS 
R2 <- ESS/TSS
R2
# выборочная кореляция
cor(y,y_hat)^2
# выделим матрицу X
X <- model.matrix(model)
X
# применение модели
nd <- data.frame(speed=c(40,60))
nd

predict(model, nd)
 # визуализация результатов
qplot(data=d2, speed, dist) + stat_smooth(method="lm")

# Построим модель наименьших квадратовпо другим данным

t <- swiss
help(swiss)
glimpse(t)
describe(t)
# построим диагрыммы рассеяния для всех значений
ggpairs(t)

model2 <- lm(data=t,
            Fertility~Agriculture+Education+Catholic)
coef(model2)

fitted(model2) # спрогнозированное значение
residuals(model2) # остатки
deviance(model2) #RSS

report <- summary(model2)
report
report$r.squared

# создадим новую модель данных
nd2 <- data.frame(Agriculture=0.5,Catholic=0.5,Education=20)
# Применяем прогноз на новых данных
predict(model2,nd2)
