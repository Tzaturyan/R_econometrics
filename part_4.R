library("HSAUR")
library("dplyr")
library("psych")
library("lmtest")
library("glmnet")
library("ggplot2")
library("car")

h <- cars
qplot(data=h, speed, dist)

model <- lm(data=h, dist~speed) # построение модели
summary(model)

h <- mutate(h, speed2 = speed^2, speed3 = speed^3) # добавление переменных
model_mk <- lm(data=h, dist~speed + speed2 + speed3) # построение модели
summary(model_mk)

vif(model_mk) # коэффициент вздутия дисперсии
# значения коэффициентов вздутия дисперсии больше 10, поэтому имеет место быть мультиколлинеарности

X0 <- model.matrix(data=h, dist~0 + speed +speed2 +speed3) # матрица без свободных членов
head(X0)
cor(X0)
# Посмотрим на сколько разные прогнозы с мультиколлинеарностью и без нее
nd <- data.frame(speed=10, speed2=100, speed3=1000)

predict(model, newdata=nd, interval="prediction")
predict(model_mk, newdata=nd, interval="prediction")
# gолучилась несущественная разница интервалов
# а интервалы для коэффициентов отличаются существенно
confint(model)
confint(model_mk)
# влияние мультиколлинеарность на прогноз несущественное, но если мы хотим понять зависимости между отдельными 
# коэффициентами или хотим построить максимально короткий доверительный интервал то здесь разница существенная

# методы борьбы с мультиколлинеарностью
# Ридж и LASSO

y <- h$dist
x0 <- X0 <- model.matrix(data=h, dist~0 + speed +speed2 +speed3)

# LASSO
lambdas <- seq(50,0.1,length=30)
m_lasso <- glmnet(x0,y,alpha=1, lambda=lambdas)
plot(m_lasso,xvar="lambda",label=TRUE)
plot(m_lasso,xvar="dev",label=TRUE) # дисперсия
plot(m_lasso,xvar="norm",label=TRUE)

coef(m_lasso,s=c(0.1,1)) # оценки Лассо для двух разных лямбд

# Ридж

m_rr <- glmnet(x0,y,alpha=0, lambda=lambdas)
# кросс-валидация, выбор оптимальной лямбды
cv <- cv.glmnet(x0,y,alpha=1)
plot(cv)

cv$lambda.min
cv$lambda.1se

coef(cv,s="lambda.1se")

# метод главных компонент
h <- heptathlon
glimpse(h)
h <- select(h,-score) # удаляем искусственно рассчитанную переменную
# данные имеет разные единицы измерения, стандартизируем переменные
h.pca <- prcomp(h,scale=TRUE)
pca1 <- h.pca$x[,1]
v1 <- h.pca$rotation[,1]
v1
head(pca1)
summary(h.pca)
# видим что достаточно использование 4 переменных 
plot(h.pca)
biplot(h.pca,xlim=c(-1,1))
