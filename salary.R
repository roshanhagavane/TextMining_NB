install.packages("naivebayes")
library(caret)
library(ggplot2)
library(psych)
library(naivebayes)
salary_train <- read.csv(file.choose())
salary_test <- read.csv(file.choose())
View(salary_train)

library(data.table)
setcolorder(salary_train,c("Salary"))
setcolorder(salary_test,c("Salary"))
View(salary_test)

salary_train$workclass <- as.factor(salary_train$workclass)
salary_train$education <- as.factor(salary_train$education)
salary_train$maritalstatus <- as.factor(salary_train$maritalstatus)
salary_train$occupation <- as.factor(salary_train$occupation)
salary_train$relationship <- as.factor(salary_train$relationship)
salary_train$race <- as.factor(salary_train$race)
salary_train$sex <- as.factor(salary_train$sex)
salary_train$native <- as.factor(salary_train$native)
salary_train$Salary <- as.factor(salary_train$Salary)
salary_train$educationno <- as.factor(salary_train$educationno)
str(salary_train)

salary_test$workclass <- as.factor(salary_test$workclass)
salary_test$education <- as.factor(salary_test$education)
salary_test$maritalstatus <- as.factor(salary_test$maritalstatus)
salary_test$occupation <- as.factor(salary_test$occupation)
salary_test$relationship <- as.factor(salary_test$relationship)
salary_test$race <- as.factor(salary_test$race)
salary_test$sex <- as.factor(salary_test$sex)
salary_test$native <- as.factor(salary_test$native)
salary_test$Salary <- as.factor(salary_test$Salary)
salary_test$educationno <- as.factor(salary_test$educationno)
str(salary_test)

plot(salary_train$workclass,salary_train$Salary)
plot(salary_train$education,salary_train$Salary)
plot(salary_train$maritalstatus,salary_train$Salary)
plot(salary_train$occupation,salary_train$Salary)
plot(salary_train$relationship,salary_train$Salary)
plot(salary_train$race,salary_train$Salary)
plot(salary_train$sex,salary_train$Salary)
plot(salary_train$native,salary_train$Salary)
plot(salary_train$educationno,salary_train$Salary)

sum(is.na(salary_train))
sum(is.na(salary_test))

attach(salary_train)

ggplot(data=salary_train,aes(x=Salary, y = age, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=salary_train,aes(x=Salary, y = capitalgain, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=salary_train,aes(x=Salary, y = hoursperweek, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=salary_train,aes(x=Salary, y = capitalloss, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

library(e1071)
model<-naiveBayes(salary_train$Salary~.,data=salary_train)
model
pred<-predict(model,newdata = salary_test[,-1])
mean(pred==salary_test[,1])

install.packages("gmodels")
library(gmodels)
CrossTable(pred,salary_test$Salary)

