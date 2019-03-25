#Set Working Directory
setwd("C:/Users/King Carmo/Desktop/College/Data Mining/Project")
#Read file
Company <- read.csv(file="Company.CSV", sep = ",", header = TRUE)
Company

##KNN algorithm starts here

Company$EmployeeWorking <-NULL
Company$HoursPerWeek <-NULL
Company$BusinessTravel <-NULL
Company$NameOfDepartment <-NULL
Company$FieldOfEducation <-NULL
Company$Gender <-NULL
Company$JobPosition <-NULL
Company$MaritalStatus <-NULL
Company$Adult <-NULL
Company$OverTime <-NULL
str(Company)

#Make back up of file
write.csv(Company, file="CompanyBU.csv", row.names = F, na = " ")

#randomize
set.seed(132478)
Company_rand = Company[order(runif(1000)),]

str(Company_rand)
Company_rand$Attrition
table(Company_rand$Attrition)
factor(Company_rand$Attrition, levels = c("No", "Yes"),labels = c("N", "Y")) 
prop.table(table(Company_rand$Attrition))
summary(Company_rand)

normalize <- function(x) { return((x - min(x)) / (max(x)-min(x)))}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

Company_n <- as.data.frame(lapply(Company_rand[2:25], normalize))

summary(Company_n)


Company_train <- Company_n[1:1327, ]
Company_test <- Company_n[1328:1659, ]
Company_train_labels <- Company_rand[1:1327, 1]
Company_test_labels <- Company_rand[1328:1659, 1]

install.packages("class")
library(class)

sqrt(1327)
Company_test_pred <- knn(train = Company_train, test = Company_test, cl = Company_train_labels, k=19)

install.packages("gmodels")
library("gmodels")

CrossTable(x = Company_test_labels, y = Company_test_pred,prop.chisq=FALSE)
#End of KNN techniques

###############################################################################

# Decision Tree Code

install.packages("rpart")
install.packages("rpart.plot")
install.packages("party")
library(party)
require(rpart)
require(rpart.plot)


# reading the data set
Company = read.csv("smoted_Company.csv", header = TRUE)

# lets look at it structuer of the data
str(Company)


normalize <- function(x) { return((x - min(x)) / (max(x)-min(x)))}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

barplot(prop.table(table(Company$Attrition)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")



# randomize first
set.seed(1234) # sync the randomization / allows reproducibility

# create a training and a test sub-set
pd <- sample(2, nrow(Company),replace = TRUE, prob = c(0.8,0.2))
train <- Company[pd==1,] 
test <-Company[pd==2,] 

# data for Developing predictive Model 
table(train$Attrition)
prop.table(table(train$Attrition))

table(train$Attrition)
table(test$Attrition)

# check their distributions
prop.table(table(train$Attrition))
prop.table(table(test$Attrition))
#original
prop.table(table(Company$Attrition))

# install and load the C5.0 library
install.packages("C50")
library("C50")

# train the model
Company_model = C5.0(train[-1], train$Attrition)
summary(Company_model)

m2 = rpart(Attrition ~ ., data=Company[1:1659,], method="class")
m2 


# Decision with rpat
m2 <- rpart(Attrition~DailyRate+DistanceFromHome+JobInvolvement+MonthlyRate+RelationshipSatisfaction+WorkLifeBalance
            + TrainingTimesLastYear+RelationshipSatisfaction +EnvironmentSatisfaction+HourlyRate, Company)
library(rpart.plot)
rpart.plot(m2,extra = 4)


# test the model
Company_pred = predict(Company_model, test)  


# evaluation
install.packages("gmodels")
library("gmodels")

# CrossTable with all details
CrossTable(test$Attrition, Company_pred) 


# CrossTable that is a lot clearer
CrossTable(test$Attrition, Company_pred, prop.chisq = FALSE, prop.c 
           = FALSE, prop.r = FALSE, dnn =c('actual_Attrition', 'predicted_Attrition'))

##############################################
# boosting to 'hopefully' find a better result
##############################################

# train another model, but this time a series of models
Company_boost10 = C5.0(train[-1], train$Attrition, trials = 15)
Company_boost10

# test the models
Company_boost_predict10 = predict(Company_boost10, test) 

# evaluate this now
CrossTable(test$Attrition, Company_boost_predict10, prop.chisq = FALSE, prop.c = 
             FALSE, prop.r = FALSE, dnn =c('actual_Attrition', 'predicted_Attrition'))

#end of Decision tree
#############################################################################

#Association rules for IBM Company dataset
######################################

#rerun again 
Company <- read.csv("smoted_Company.csv", head = TRUE)
#sort the data with specific attributes
AR_Company <- Company[,names(Company) %in% c('Attrition','BusinessTravel','NameOfDepartment',
                              'FieldOfEducation','Gender','JobStatus','MaritalStatus')]

# write as a output file
write.csv(AR_Company, file="AR_Company.csv", row.names = F, na = " ")

#install.packages("arules")
library(arules)

set.seed(3653)
ar_Company <- read.transactions("AR_Company.csv", sep =",", rm.duplicates = TRUE)

#summary of Company data
summary(ar_Company)

#display first five row
inspect(ar_Company[1:5])

#build model bu using apriori function
apriori(ar_Company)
AR_rules <- apriori(ar_Company,parameter=list(support =0.01, confidence = 0.8, 
                                         minlen = 2), appearance=list(rhs=c("No", "Yes"), default="lhs"))
summary(AR_rules)

## keep tCompanyee decimal places
quality(AR_rules) <- round(quality(AR_rules), digits=4)

#Rules after sorting
sortRules <- (sort(AR_rules, by = "lift"))

## find redundant rules
subset.matrix <- is.subset(sortRules, sortRules, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1

## which rules are redundant
which(redundant)

## remove redundant rules
pruneRules <- sortRules[!redundant]
#pruneRules
inspect(pruneRules[1:5])

#Total of rules with Attrition 'Yes' and 'No'
subset(pruneRules, items %in% "Yes")
subset(pruneRules, items %in% "No")

#################################
#Visualize plot diagram

#install.packages("arulesViz")
#install.packages("viridisLite")
library(arulesViz)
library(viridisLite)

plot(sortRules)

plot(pruneRules)

plot((pruneRules[1:5]), method = "graph", control = list(type="items"))

plot(pruneRules[1:20], method = "paracoord", control = list(reorder = TRUE))

plot(pruneRules, method = "grouped")

#end of Association rules
##############################################################################

#Naive Bayes

#install packages
install.packages("caret")
install.packages("e1071")

#Load Library
library("e1071")
library("caret")
library("gmodels")

#read csv file
Company<- read.csv("Company.csv")
#read this file when running the code second time

#column names
colnames(Company)

# clean data.
summary(Company)
str(Company)
any(is.na(Company))

#remove unwanted columns.
CompanyN <- Company[ , -(c(3,6,9,10,18,20,24,26,28,31,32,34,35,36))]
#change 1st column name to Age
colnames(CompanyN)[1] <- "Age"
#write a new csv
write.csv(CompanyN, file="employeeD_new.csv", row.names = F, na = " ")

# check column names
colnames(CompanyN)

# read new csv file second time running code.
#CompanyN<- read.csv("employeeD_new.csv")
CompanyN<- read.csv("smoted_Company.csv")
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
emp <- naiveBayes(Attrition ~ Age + DailyRate + NameOfDepartment + Education + FieldOfEducation + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobStatus + JobSatisfaction + MonthlyIncome + NumCompaniesWorked + Adult + OverTime + PerformanceRating + HoursPerWeek + YearsInCurrentRole, data = train)
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

#end of Naive Bayes
############################################################################################

