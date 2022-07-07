library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd") # визуализация множества качественных переменных
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")

h <- diamonds
glimpse(h)
help(diamonds)

qplot(data=h, carat, price) # построение графика
bg <- qplot(data=h, log(carat), log(price)) # перевод переменных в логарифм
bg + geom_hex() # функция для различения черных точек

f <- read.csv("flats_moscow.txt", sep="/", header=TRUE, dec=".")
glimpse(f)
qplot(data=f, totsp, price)
str(f)
qplot(data=f, log(totsp), log(price))

mosaic(data=f, ~walk+brick+floor, shade=TRUE) # визуализация качественных переменных

f <- mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)

# построение графиков колличественных и качественных переменных одновременно
qplot(data=f, log(price))
qplot(data=f, log(price), fill=brick)
qplot(data=f, log(price), fill=brick, position="dodge")
qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)
g2 <- qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)
g2 + facet_grid(walk~floor) # добавление переменных в график
# оценивание моделей с дамми-переменными
model_0 <- lm(data=f, log(price)~log(totsp))
model_1 <- lm(data=f, log(price)~log(totsp)+brick)
model_2 <- lm(data=f, log(price)~log(totsp)+brick+brick:log(totsp))

summary(model_0)
mtable(model_2)
sjp.lm(model_2) # интервалы значимости

# построение прогнозов

nw <- data.frame(totsp=c(60,60), brick=factor(c(1,0))) # новый набор данных
nw
# построим прогноз
predict(model_2, newdata=nw)
exp(predict(model_2, newdata=nw)) # выведение логарифма

exp(predict(model_2, newdata=nw, interval="confidence")) # с интервалом 95% по умолчанию для среднестратистической квартиры
exp(predict(model_2, newdata=nw, interval="prediction")) # с интервалом для конкреиной случайной квартиры

# Проверка гипотезы о линейных ограничениях, графическое представление результатов
waldtest(model_0, model_1)
# значение p-value jколо нуля, поэтому мы отвергаем H_0 (jграниченние выполнено ьщdel_0)
# Построим график
ggo <- qplot(data=f), log(totsp), log(price))
ggo + stat_smooth(method="lm")
ggo + stat_smooth(method="lm") + facet_grid(~walk)
ggo + aes(col=brick) + stat_smooth(method="lm") + facet_grid(~walk) # добавим раскраску в зависимости от переменной brick

# При оценивании моделей можно ориентироваться на показатель AIC, где чем меньше штраф от положительного числа тем лучше модель
mtable(model_9, model_1, model_2)

# Проверка гипотезы о пропущенных переменных, тест Рамсея
resettest(model_2)
# P-value 0.01, при 5% уровне мы можем не отвергать нулевую гипотезу что означает что переменных достаточно
