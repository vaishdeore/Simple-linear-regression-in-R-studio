#Hello everyone. This is Vaishnavi and today we are here to take a look at a case study using simple linear regression.
#As it is a simple linear regression, we use one independent variable and one dependent variable.
#And the equation for this is Y=Bo + B1x + e.
#We use the independent variable to predict the dependent variable.
#Y is dependent and x is the independent variable here. 

#installing packages 
install.packages("DAAG")
library(DAAG)
data(ais)
View(ais)
head(ais,n=10)
?lattice #used for graphics
library(lattice)
install.packages("ggplot2")
library(ggplot2)

#Here the dataset that we have taken gives us the hemoglobin and hematocrit levels of male athletes.
#Our goal is to make a simple regression model  to predict hematocrit levels from hemoglobin levels. 
#Here hemoglobin is the independent variable and hematocrit is the dependent variable.
#Which means that we can try to predict hematocrit levels using hemoglobin levels. 

#we take for male athletes only 
ais2<-subset(ais,sex=="m")
ais3= ais2[,c(3,4)]
ais2
ais3 #subset column that corresponds to hg and hc 

#renaming variables 
colnames(ais3)<-c("Hematocrit","Hemoglobin")
ais3
#taking a look at the summary statistics 
summary(ais3)

#We need to satisfy the assumptions of linear regression :
#they are linearity, homoscedasticity, independence and normality. 
#To validate these assumptions, we use a few statistical tests 
#But first, we look for outliers using the box-plot test. 

#boxplot- to identify outliers 
par(mfrow=c(1,2))
par(mar = c(5, 5, 2, 2))
boxplot(ais3$Hemoglobin, ylab= "Hemoglobin value", col = "skyblue", border = "black",main= "Hemoglobin plot",horizontal = F)
boxplot(ais3$Hematocrit, ylab= "Hemoglobin value", col = "skyblue", border = "black",main="Hematocrit plot",horizontal = F)

#getting the values of individual outliers
boxplot.stats(ais3$Hemoglobin)$out 
boxplot.stats(ais3$Hematocrit)$out 

#removing outliers 
values_to_remove <- c(40.3,59.7)
newdata <- ais3[!(ais3$Hematocrit %in% values_to_remove), ]
newdata
values_to_remove <- c(18.0,19.2,18.5,17.7)
newdata1 <- newdata[!(newdata$Hemoglobin %in% values_to_remove), ]
newdata1

#After removing the outliers,we move forward to validate our assumptions 
#starting with satisfying assumptions 
#1- normality - by histogram 
hist(newdata1$Hemoglobin,main = "Hemoglobin",xlab = "Value",ylab = "Frequency",col = "skyblue",border = "black", breaks = 10)
hist(newdata1$Hematocrit,main = "Hematocrit",xlab = "Value",ylab = "Frequency",col = "skyblue",border = "black", breaks = 10)

#Linearity tells us if the relationship between X and Y is linear. 
#By looking at the q-q plot, we can see that there is a linear relationship between the two variables.
#2- linearity- qq plot 
qplot(Hemoglobin, Hematocrit, data = newdata1,
      main = "HEMAGLOBIN and HEMATOCRIT relationship") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(colour = "blue", size = 1.5) +
  scale_y_continuous(breaks = c(30:65), minor_breaks = NULL) +
  scale_x_continuous(breaks = c(10:25), minor_breaks = NULL)

#Homoscedasticity tells us if the variance is the same for any given value of X.
#For this we check the value of p in the test result.
#Here, our null hypothesis is that heteroscedasticity is not present in the model and our alternate hypothesis is that heteroscedasticity is present in the model.
#If it's below your level of significance, in this case, 0.05, it suggests heteroscedasticity, indicating non-constant variance in the residuals.
#Here our p- value is 0.6.
#Hence, we do not reject our null hypothesis and conclude that homoscedasticity is present. 
#Therefore we can say that the residuals are homoscedastic. 
#3 - Homoscedasticity - Breush Pagan test 
mod1 = lm(Hematocrit ~ Hemoglobin, data = newdata1)
install.packages("lmtest")
library(lmtest)
bptest(mod1)

#4 - Independence - And as we already know that the observations which are taken from different athletes are independent of each other. 

#Now, we move further towards building a linear model.
#The equation is Hematocrit = Bo + B1 Hemoglobin. 
#The null hypothesis is that there is no relationship between the two.
#And the alternative hypothesis is that there is a relationship between them.  

# fitting the linear model 
mod1 = lm(Hematocrit ~ Hemoglobin, data = newdata1)
summary(mod1)

#After plotting the linear model, we take a look at the summary statistic.
#Here we can see the value of the residuals, the estimate coefficient, the standard error.
#R-squared is the square of the correlation between two variables.
#Its value can vary between 0 and 1: a value close to 0 means that the regression model does not explain the variance in the response variable, while a number close to 1  suggests that the observed variance in the response variable is well explained. 
#In the current case, R-squared suggests the linear model fit explains  74% of the variance observed in the data. 
#Here, the p-value is less than 0.05. So we reject the null hypothesis and conclude that there is a relationship between hemoglobin and hematocrit. 
#Therefore we can conclude that it is a good fit. 
#Thank you 
