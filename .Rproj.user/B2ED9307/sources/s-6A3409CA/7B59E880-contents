#install packages
#install.packages("caret")
#install.packages("e1071")

#Load Library
library("e1071")
library("caret")
library("gmodels")

#read csv file
CompanyNaiveBaye <- read.csv("Company.csv")
#read this file when running the code second time

#column names
colnames(CompanyNaiveBaye)

# clean data.
summary(CompanyNaiveBaye)
str(CompanyNaiveBaye)
any(is.na(CompanyNaiveBaye))

#remove unwanted columns.
head(CompanyN)
CompanyN <- CompanyNaiveBaye[ , -(c(3,6,9,10,18,20,24,26,28,31,32,34,35,36))]

#change 1st column name to Age
colnames(CompanyN)[1] <- "Age"

#write a new csv
write.csv(CompanyN, file="employeeD_new.csv", row.names = F, na = " ")

# check column names
colnames(CompanyN)

# read new csv file second time running code.
CompanyN<- read.csv("employeeD_new.csv")


#split data one for train and other for test
set.seed(2)

#takes all observation, randomly select 80% of employees and 20%, so two value.
#80% of time I want to select 1 and 20% time 2.
p <- sample(2, nrow(CompanyN), prob = c(0.8,0.2), replace =T)

train<- CompanyN[p==1,]
test<- CompanyN[p==2,]
nrow(train)
nrow(test)

#compare yes and no in two dataset
prop.table(table(train$Attrition))
prop.table(table(test$Attrition))

#naive bayes
emp <- naiveBayes(Attrition ~ Age + DailyRate + Department + Education + Education + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobSatisfaction + MonthlyIncome + NumCompaniesWorked + OverTime + PerformanceRating + YearsInCurrentRole, data = train)
emp

#prediction on test data set

prediction2<- predict(emp, test)
confusionMatrix(table(prediction2,test$Attrition))

#put tarin data in cross tables
CrossTable(prediction2,test$Attrition, prop.chisq = FALSE,prop.t = FALSE, dnn = c('predicted', 'actual'))

prediction3<- predict(emp, train)
confusionMatrix(table(prediction3,train$Attrition))

#put test data in crosstable 
CrossTable(prediction3,train$Attrition, prop.chisq = FALSE,prop.t = FALSE, dnn = c('predicted', 'actual'))
#probability
prediction2<- predict(emp, test, type="raw")
prediction2

