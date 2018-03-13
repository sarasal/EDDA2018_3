
# Loading necessary packages, so they are available in the current session.
library(multcomp)
library(lme4)

### Exercise 1
data_bread <- read.table(file = "bread.txt", header = TRUE)

# QUestion 1:
I = 3 #levels of temperature.
J = 2 #levels of humidity.
N = 3 #experimental units per combination of the two factors, given that the total of units is 18.
rbind(rep(1:I,each=N*J),rep(1:J,N*I),sample(1:(N*I*J)))

# Question 2
par(mfrow=c(1,2))
boxplot(hours~environment,data=data_bread, main ="Hours x Environment")
boxplot(hours~humidity,data=data_bread, main ="Hours x Humidity")

par(mfrow=c(1,1))
attach(data_bread)
interaction.plot(environment,humidity,hours)
interaction.plot(humidity,environment,hours)

# Question 3
data_bread$environment=as.factor(data_bread$environment)
data_bread$humidity=as.factor(data_bread$humidity)
breadaov=lm(hours~environment*humidity,data=data_bread)
anova(breadaov)
summary(breadaov)

# Question 4
contrasts(data_bread$environment)=contr.sum
contrasts(data_bread$humidity)=contr.sum
breadaov2=lm(hours~environment*humidity,data=data_bread)
anova(breadaov2)
summary(breadaov2)

# Question 5
#Checking the normality of the population.
par(mfrow=c(1,2))
qqnorm(residuals(breadaov2))

# An extra check is also the Shapiro test.
shapiro.test(residuals(breadaov2))

# Checking the assumption of equal population variances.
plot(fitted(breadaov2), residuals(breadaov2))

### Exercise 2
search = read.table("search.txt", header = TRUE)

# Question 1
B = 5
I = 3
N = 1

rbind(rep(1:I,each=N*B),rep(1:B,N*I),sample(1:(N*I*B)))


# Question 2
attach(search)

par(mfrow=c(1,2))
boxplot(time~skill, main="Boxplot Time x Skill Level", ylab="Time", xlab="Skill Level")
boxplot(time~interface, main="Boxplot Time x Interface", ylab="Time", xlab="Interface")

par(mfrow=c(1,2))
interaction.plot(skill, interface, time); interaction.plot(interface, skill, time)

# Question 3
search$skill <- factor(search$skill)
search$interface <-  factor(search$interface)
aovsearch = lm(time~interface+skill, data = search)
anova(aovsearch)
summary(aovsearch)

# Question 4
contrasts(search$skill)=contr.sum
contrasts(search$interface)=contr.sum
aovsearch = lm(time~interface+skill, data = search)
summary(aovsearch)

estimatedTime = 20.5467 +  2.1533  + (0 - (-2.3867) - 0.3133)  
estimatedTime

# Question 5
par(mfrow=c(1,2))
qqnorm(residuals(aovsearch))
qqline(residuals(aovsearch))
plot(fitted(aovsearch), residuals(aovsearch))

# Question 6
friedman.test(time, interface, skill, data = search)
#friedman.test(time~interface|skill, data = search)

# Question 7
oneaovsearch = lm(time~interface, data = search)
anova(oneaovsearch)

### Exercise 3

# Question 1
cream= read.table("cream.txt",header = TRUE)

cream$batch= as.factor(cream$batch)
cream$position= as.factor(cream$position)
cream$starter= as.factor(cream$starter)

creamaov= lm(acidity ~ starter + batch + position,data = cream)
anova(creamaov)
summary(creamaov)

# Question 2
creammult= glht(creamaov,linfct = mcp(starter="Tukey"))
summary(creammult)

# Question 3
# In Word File

# Question 4
confint(creammult)


### Exercise 4
library(lme4)
# Question 1
cow = read.table("cow.txt", header = TRUE)
cow$id = factor(cow$id)
cow$per = factor(cow$per)
cow$treatment = factor(cow$treatment)

cowlm = lm(milk~treatment+id+per, data = cow)
summary(cowlm)

# Question 2: Result from question 1

# Question 3
cowlmer = lmer(milk~treatment+order+per+(1|id), data = cow, REML=FALSE)
summary(cowlmer)
cowlmer1 = lmer(milk~order+per+(1|id), data = cow, REML=FALSE)
anova(cowlmer1, cowlmer)


# Question 4
attach(cow)
t.test(milk[treatment=="A"],milk[treatment=="B"],paired=TRUE)

### Exercise 5

# Question 1
nausea.frame=data.frame("nausea" = integer(),"medicin" = character(), stringsAsFactors = FALSE)
index = 1
for(i in 1:100){
  nausea.frame[i,] <- rbind(0, "Chlorpromazine")
  index = index + 1}

for(i in 1:52){
  nausea.frame[index,] <- rbind(1, "Chlorpromazine")
  index = index + 1}

for(i in 1:32){
  nausea.frame[index,] <- rbind(0, "Pentobarbital(100mg)")
  index = index + 1}

for(i in 1:35){
  nausea.frame[index,] <- rbind(1, "Pentobarbital(100mg)")
  index = index + 1}

for(i in 1:48){
  nausea.frame[index,] <- rbind(0, "Pentobarbital(150mg)")
  index = index + 1}

for(i in 1:37){
  nausea.frame[index,] <- rbind(1, "Pentobarbital(150mg)")
  index = index + 1}

#nausea.frame
attach(nausea.frame)
# Question 2
nausea.frame$medicin=as.factor(nausea.frame$medicin)

xtabs(~medicin+nausea)

# Question 3
attach(nausea.frame)

t = chisq.test(xtabs(~medicin+nausea))[[1]]
B=1000
tstar=numeric(B)
for (i in 1:B){
  medicinstar=sample(medicin)
  tstar[i]= chisq.test(xtabs(~medicinstar+nausea))[[1]]
}

hist(tstar)
t
pl=sum(tstar<t)/B
pr=sum(tstar>t)/B
p=2*min(pl,pr)
pl;pr;p

# Question 4
chisq.test(xtabs(~medicin+nausea))

### Exercise 6

# Question 1
airpollution= read.table("airpollution.txt", header = TRUE)
pairs(oxidant ~ day + wind + temperature + humidity + insolation , data= airpollution, upper.panel= NULL)

# Question 2
oxidant_day= lm(oxidant ~ day, data = airpollution)
summary(oxidant_day)
#Multiple R-squared: 0.01093

oxidant_wind= lm(oxidant ~ wind, data = airpollution)
summary(oxidant_wind)
#Multiple R-squared: 0.5863

oxidant_temperature=lm(oxidant ~ temperature, data = airpollution)
summary(oxidant_temperature)
#Multiple R-squared: 0.576

oxidant_humidity=lm(oxidant ~ humidity, data = airpollution)
summary(oxidant_humidity)
#Multiple R-squared: 0.124

oxidant_insolation=lm(oxidant ~ insolation, data = airpollution)
summary(oxidant_insolation)
#Multiple R-squared: 0.2552

## step 1 - select the highest R-squared (wind)
oxidant1= lm(oxidant ~ wind, data = airpollution)
summary(oxidant1)
#Multiple R-squared: 0.5863

## step 2 - select the highest R-squared (temperature as second variable) among below options
oxidant2=lm(oxidant ~ wind + temperature, data = airpollution)
summary(oxidant2)
#Multiple R-squared:0.7773 

oxidant2=lm(oxidant ~ wind + insolation, data = airpollution)
summary(oxidant2)
#Multiple R-squared:0.6613 

oxidant2=lm(oxidant ~ wind + humidity, data = airpollution)
summary(oxidant2)
#Multiple R-squared:0.5913 - one variable become insignificant

oxidant2=lm(oxidant ~ wind + day, data = airpollution)
summary(oxidant2)
#Multiple R-squared:0.5989 - one variable become insignificant

## step 3 - all added variable make previous variables insignificant - stop stepping up
oxidant3=lm(oxidant ~ wind + temperature + insolation, data = airpollution)
summary(oxidant3)
#Multiple R-squared:0.7816 - one variable become insignificant

oxidant3=lm(oxidant ~ wind + temperature + humidity, data = airpollution)
summary(oxidant3)
#Multiple R-squared:0.7964 - one variable become insignificant

oxidant3=lm(oxidant ~ wind + temperature + day, data = airpollution)
summary(oxidant3)
#Multiple R-squared:0.7958 - one variable become insignificant

## Best Model - step 2
oxidant2=lm(oxidant ~ wind + temperature, data = airpollution)
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

### Exercise 7

# Question 1
crime_expenses <- read.table(file = "expensescrime.txt", header = TRUE)

#Identifying potential correlations
pairs(crime_expenses, upper.panel=NULL)

# It's possible to see a potential correlation between expend and bad, lawyers, employ and pop.
# Verifying the histogram for these variables:
par(mfrow=c(1,5))
hist(crime_expenses$expend)
hist(crime_expenses$bad)
hist(crime_expenses$lawyers)
hist(crime_expenses$employ)
hist(crime_expenses$pop)

# Thus a first attempt to calculate the linear model will be made considering these factors.
attach(crime_expenses)
expenseslm = lm(expend~bad+lawyers+employ+pop, data=crime_expenses)
summary(expenseslm)
confint(expenseslm)
#Assessing the current model
par(mfrow=c(1,2))
qqnorm(residuals(expenseslm)) # There are
shapiro.test(residuals(expenseslm)) #Extract check using Shapiro's normality test.

plot(fitted(expenseslm),residuals(expenseslm))
# The variances for the different fitted values is concentrated with smaller value of fitted expenses.

#2nd Iteration:
# Considering that in the previous iteration lawyers and employ reject the null hypothesis the 2nd iteration will
# consider only these factors (bad will also be considered since it has the biggest estimated coeficient).
# Additionally, now we will calculate the regression considering interaction between the variables.
expenseslm2 = lm(expend~bad*lawyers*employ, data=crime_expenses)
summary(expenseslm2)
# Assessing the new regression parameters:
qqnorm(residuals(expenseslm2))
plot(fitted(expenseslm2),residuals(expenseslm2))
# The qqnorm presents a curved shape with some points far from the line, the qqnorm shows a concentration around certain fitted values (<1000).
# Given that bad is the variable with highest coeficient we will try to have better results by elevating bad to the
# power of 2.

crime_expenses$bad2 = crime_expenses$bad^2
expenseslm3 = lm(expend~bad*lawyers*employ*bad2, data=crime_expenses)
summary(expenseslm3)
qqnorm(residuals(expenseslm3))
plot(fitted(expenseslm3),residuals(expenseslm3))
# After this iteration the qqnorm graph presents a better slope and distance between the points.
# Also the residuals x ffited graph shows that the residuals are more spread.

#The model is: expenses = 136.3 -8.186*bad -0.1297*lawyers + 0.08236*employ - 0.1440*bad^2


