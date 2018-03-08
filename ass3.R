### Exercise 3

# Question 1
cream= read.table("cream.txt",header = TRUE)

cream$batch= as.factor(cream$batch)
cream$position= as.factor(cream$position)
cream$starter= as.factor(cream$starter)

creamaov= lm(acidity ~ starter + batch + position,data = cream)
summary(creamaov)

# Question 2
library(multcomp)
creammult= glht(creamaov,linfct = mcp(starter="Tukey"))
summary(creammult)

# Question 3
# In Word File

# Question 4
confint(creammult)

### Exercise 6

# Question 1
airpollution= read.table("airpollution.txt", header = TRUE)
pairs(oxidant ~ day + wind + temperature + humidity + insolation , data= airpollution, upper.panel= NULL)

# Question 2
oxidant_day= lm(oxidant ~ day, data = airpollution)
summary(oxidant_day)
#p_value: 0.582

oxidant_wind= lm(oxidant ~ wind, data = airpollution)
summary(oxidant_wind)
#p_value: 8.20e-07

oxidant_temperature=lm(oxidant ~ temperature, data = airpollution)
summary(oxidant_temperature)
#p_value: 1.17e-06

oxidant_humidity=lm(oxidant ~ humidity, data = airpollution)
summary(oxidant_humidity)
#p_value: 0.056317

oxidant_insolation=lm(oxidant ~ insolation, data = airpollution)
summary(oxidant_insolation)
#p_value: 0.00441

## step 1 - select the smallest p-value (wind)
oxidant1= lm(oxidant ~ wind, data = airpollution)
summary(oxidant1)

## step 2 - select second smallest p-value (temperature)
oxidant2=lm(oxidant ~ wind + temperature, data = airpollution)
summary(oxidant2)

## step 3 - select third smallest p-value (insolation)
oxidant3=lm(oxidant ~ wind + temperature + insolation, data = airpollution)
summary(oxidant3)

## Best Model
oxidant2

# Question 3
## step 1 - full model
oxidant5= lm(oxidant ~ wind + temperature + insolation + humidity + day, data = airpollution)
summary(oxidant5)

## step 2 - remove the highest p-value (day)
oxidant4= lm(oxidant ~ wind + temperature + insolation + humidity , data = airpollution)
summary(oxidant4)

## step 3 - remove second highest p-value (insolation)
oxidant3= lm(oxidant ~ wind + temperature + humidity , data = airpollution)
summary(oxidant3)

## step 4 - remove third highest p-value (humidity)
oxidant2= lm(oxidant ~ wind + temperature , data = airpollution)
summary(oxidant2)

# Question 4
## Best Model
oxidant2

# Question 5
par(mfrow=c(1,2))
qqnorm(residuals(oxidant2), main = "QQ-plot residuals")
plot(fitted(oxidant2),residuals(oxidant2))


