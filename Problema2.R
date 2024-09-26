#Codigo para problema 2

mis_dades = iris
iris

y = mis_dades$Sepal.Length
y

x = mis_dades$Petal.Length
x

plot(x,y)

xbar = mean(x)
xbar
ybar = mean(y)
ybar
#pendiente (m)
m = sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)
m
b = ybar - m*xbar
b

#petal.length = 1.5
m*1.5+b

mod = lm(y~x)

data.frame(x=x)

ypredicted = predict(mod, data.frame(x=x))

plot(x, ypredicted, type="1")
points(x, y)

plot(x, y, pch=16, col("red"))
lines(x, ypredicted)

rsq = sum((ypredicted-ybar)^2)/sum((y-ybar)^2)

summary(mod) #multiple R-squared = rsq (minims cuadrats)

sqrt(rsq)
cor.test(x,y)
