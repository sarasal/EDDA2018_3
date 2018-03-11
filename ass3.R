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

# QUestion 2:
par(mfrow=c(1,2))
boxplot(hours~environment,data=data_bread, main ="Hours x Environment")
boxplot(hours~humidity,data=data_bread, main ="Hours x Humidity")

par(mfrow=c(1,1))
attach(data_bread)
interaction.plot(environment,humidity,hours)
interaction.plot(humidity,environment,hours)

# QUestion 3:
data_bread$environment=as.factor(data_bread$environment)
data_bread$humidity=as.factor(data_bread$humidity)
breadaov=lm(hours~environment*humidity,data=data_bread)
anova(breadaov)

# QUestion 4:
contrasts(data_bread$environment)=contr.sum
contrasts(data_bread$humidity)=contr.sum
breadaov2=lm(hours~environment*humidity,data=data_bread)
anova(breadaov2)
summary(breadaov2)

# QUestion 5:
#Checking the normality of the population.
qqnorm(residuals(breadaov2))

# An extra check is also the Shapiro test.
shapiro.test(residuals(breadaov2))

# Checking the assumption of equal population variances.
plot(fitted(breadaov2), residuals(breadaov2))

### Exercise 2
search = read.table("search.txt", header = TRUE)

# Question 1:
?rbind
B = 5
I = 3
N = 1

rbind(rep(1:I,each=N*B),rep(1:B,N*I),sample(1:(N*I*B)))

#skill = as.factor(rep(1:B, N*I))
#interface = as.factor(rep(1:I, N*B))
#studentindex = sample(1:B*I*N)
#search_random_frame= data.frame(cbind(skill, interface, studentindex))
#search_random_frame

# Question 2:
attach(search)

par(mfrow=c(1,2))
boxplot(time~skill, main="Boxplot Time x Skill Level", ylab="Time", xlab="Skill Level")
boxplot(time~interface, main="Boxplot Time x Interface", ylab="Time", xlab="Interface")

par(mfrow=c(1,2))
interaction.plot(skill, interface, time); interaction.plot(interface, skill, time)

# Question 3:
search$skill <- factor(search$skill)
search$interface <-  factor(search$interface)
aovsearch = lm(time~interface+skill, data = search)
anova(aovsearch)

# Question 4: ?
summary(aovsearch)
# Result is generated from summary of anova
summary(aovsearch)
estimatedTime = 15.013 +  5.300  + 4.460  
estimatedTime

# Question 5:
par(mfrow=c(1,2))
qqnorm(residuals(aovsearch))
qqline(residuals(aovsearch))
plot(fitted(aovsearch), residuals(aovsearch))

# Question 6: ?
friedman.test(time, interface, skill, data = search)
#friedman.test(time~interface|skill, data = search)

# Question 7:
oneaovsearch = lm(time~interface, data = search)
anova(oneaovsearch)

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
  index = index + 1
}

for(i in 1:52){
  nausea.frame[index,] <- rbind(1, "Chlorpromazine")
  index = index + 1
}

for(i in 1:32){
  nausea.frame[index,] <- rbind(0, "Pentobarbital(100mg)")
  index = index + 1
}

for(i in 1:35){
  nausea.frame[index,] <- rbind(1, "Pentobarbital(100mg)")
  index = index + 1
}

for(i in 1:48){
  nausea.frame[index,] <- rbind(0, "Pentobarbital(150mg)")
  index = index + 1
}

for(i in 1:37){
  nausea.frame[index,] <- rbind(1, "Pentobarbital(150mg)")
  index = index + 1
}

nausea.frame

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

