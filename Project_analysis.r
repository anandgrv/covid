
setwd("C:/Users/matteo posenato/Documents/statistic learning 1/project/Project/MY_PROJECT/dataset")


data <- read.csv('global_covid19_diff.csv', stringsAsFactors = FALSE)

#model#
#1
#make a plot
plot(data$Italy_deaths,data$Italy_atm,  pch=20, col="blue")

#make simpler linear regression with atm and deaths
reg.out <- lm(data$Italy_atm~data$Italy_deaths)
summary(reg.out)

#ad the regression line in the plot
beta.hat <- coefficients(reg.out)
abline(beta.hat[1], beta.hat[2], col="blue", lwd=2)

abline(reg.out)

#confident interval and intercpet
confint(reg.out)
confint(reg.out, level=0.8)

#check the redisuals
e <- residuals(reg.out)
y.hat <- fitted.values(reg.out)
sum(e)
sum(data$Italy_atm*e)
sum(y.hat*e)

#2

#plot and linear regression
x <- data$Italy_incidence
y <- data$Spain_incidence
plot(x, y, pch= 20)
reg.out <- lm(x~y)
summary(reg.out)

#add the line in the plot

beta.hat <- coefficients(reg.out)
abline(beta.hat[1], beta.hat[2], col="blue", lwd=2)

abline(reg.out)

#confident interval and intercept
confint(reg.out)
confint(reg.out, level=0.8)

#check the residual and sum it
e <- residuals(reg.out)
y.hat <- fitted.values(reg.out)
sum(e)
sum(y*e)
sum(y.hat*e)

#prediction

#check the coefficient of Spain incidents and the intercept
coefficients(reg.out)

#sum the coefficient and multiply by the mean of Spain incidence
0.0005584873+0.2258139985*mean(y)

#add a red dot in the plot
points(mean(y),0.0006332813, pch=17, col="red")

#create a new dataframe with the mean of the spain incidents and predict
new.x <- data.frame(y = mean(y))
predict(reg.out, newdata=new.x)

#predict the confidence interval and the predoction interval
predict(reg.out, newdata=new.x, interval ="confidence")
predict(reg.out, newdata=new.x, interval ="prediction")

range(y)
xp <- seq(-0.02, 0.05, length=100)
new.x <- data.frame(y=xp)
new.conf <- predict(reg.out, newdata=new.x, interval = "confidence")

#add the intervals in the plot
lines(xp, new.conf[,2], lty=2, col="red", lwd=2)
lines(xp, new.conf[,3], lty=2, col="red", lwd=2)

new.pred <- predict(reg.out, newdata=new.x, interval = "prediction")
lines(xp, new.pred[,2], lty=2, col="blue", lwd=2)
lines(xp, new.pred[,3], lty=2, col="blue", lwd=2)


#make a two-side t-test for the incidents of the two county 
t.test(data$Italy_incidence,data$Spain_incidence)

## i'll try to replicate the t-test for all the possibility of country but don't work 
for (country1 in names(data)){
  for (country2 in names(data)){
  x <- country1
  y <- country2
  new_data <- data.frame(Country1 = x, Country2 = y)
  ##salvare il p value da qualche parte
  }}

