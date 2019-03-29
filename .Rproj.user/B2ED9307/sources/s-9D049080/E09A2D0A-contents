#Read file
hr <- read.csv(file="Company.csv",head=TRUE,sep = ",", stringsAsFactors = FALSE)
hr

##KNN algorithm starts here

hr$EmployeeCount <-NULL
hr$StandardHours <-NULL
hr$BusinessTravel <-NULL
hr$Department <-NULL
hr$EducationField <-NULL
hr$Gender <-NULL
hr$JobRole <-NULL
hr$MaritalStatus <-NULL
hr$Over18 <-NULL
hr$OverTime <-NULL
str(hr)

#Make back up of file
write.csv(hr, file="hr.csv", row.names = F, na = " ")

#randomize
set.seed(214805)
hr_rand = hr[order(runif(1659)),]

str(hr_rand)
hr_rand$Attrition
table(hr_rand$Attrition)
factor(hr_rand$Attrition, levels = c("No", "Yes"),labels = c("N", "Y")) 
prop.table(table(hr_rand$Attrition))
summary(hr_rand)

normalize <- function(x) { return((x - min(x)) / (max(x)-min(x)))}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

hr_n <- as.data.frame(lapply(hr_rand[3:25], normalize))

summary(hr_n)


hr_train <- hr_n[1:1327, ]
hr_test <- hr_n[1328:1659, ]
hr_train_labels <- hr_rand[1:1327, 1]
hr_test_labels <- hr_rand[1328:1659, 1]

install.packages("class")
library(class)

sqrt(1327)
hr_test_pred <- knn(train = hr_train, test = hr_test, cl = hr_train_labels, k=19)

install.packages("gmodels")
library("gmodels")

CrossTable(x = hr_test_labels, y = hr_test_pred,prop.chisq=FALSE)
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
hr = read.csv("Company.csv", header = TRUE)

# lets look at it structuer of the data
str(hr)


normalize <- function(x) { return((x - min(x)) / (max(x)-min(x)))}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

barplot(prop.table(table(hr$Attrition)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")



# randomize first
set.seed(1234) # sync the randomization / allows reproducibility

# create a training and a test sub-set
pd <- sample(2, nrow(hr),replace = TRUE, prob = c(0.8,0.2))
train <- hr[pd==1,] 
test <-hr[pd==2,] 

# data for Developing predictive Model 
table(train$Attrition)
prop.table(table(train$Attrition))

table(train$Attrition)
table(test$Attrition)

# check their distributions
prop.table(table(train$Attrition))
prop.table(table(test$Attrition))
#original
prop.table(table(hr$Attrition))

# install and load the C5.0 library
install.packages("C50")
library("C50")

# train the model
hr_model = C5.0(train[,-3], train$Attrition)
summary(hr_model)

m2 = rpart(Attrition ~ ., data=hr[1:1470,], method="class")
m2 


# Decision with rpat
m2 <- rpart(Attrition~DailyRate+DistanceFromHome+JobInvolvement+MonthlyRate+RelationshipSatisfaction+WorkLifeBalance
            + TrainingTimesLastYear+RelationshipSatisfaction +EnvironmentSatisfaction+HourlyRate, hr)
library(rpart.plot)
rpart.plot(m2,extra = 4)


# test the model
hr_pred = predict(hr_model, train)  


# evaluation
install.packages("gmodels")
library("gmodels")

# CrossTable with all details
CrossTable(test$Attrition, hr_pred) 


# CrossTable that is a lot clearer
CrossTable(test$Attrition, hr_pred, prop.chisq = FALSE, prop.c 
           = FALSE, prop.r = FALSE, dnn =c('actual_Attrition', 'predicted_Attrition'))

##############################################
# boosting to 'hopefully' find a better result
##############################################

# train another model, but this time a series of models
hr_boost10 = C5.0(train[-1], train$Attrition, trials = 15)
hr_boost10

# test the models
hr_boost_predict10 = predict(hr_boost10, test) 

# evaluate this now
CrossTable(test$Attrition, hr_boost_predict10, prop.chisq = FALSE, prop.c = 
             FALSE, prop.r = FALSE, dnn =c('actual_Attrition', 'predicted_Attrition'))

#end of Decision tree
#############################################################################

#Association rules for IBM HR dataset
######################################

#rerun again 
hr <- read.csv("smoted_hr.csv", head = TRUE)
#sort the data with specific attributes
AR_hr <- hr[,names(hr) %in% c('Attrition','BusinessTravel','Department',
                              'EducationField','Gender','JobRole','MaritalStatus')]

# write as a output file
write.csv(AR_hr, file="AR_hr.csv", row.names = F, na = " ")

#install.packages("arules")
library(arules)

set.seed(3653)
ar_hr <- read.transactions("AR_hr.csv", sep =",", rm.duplicates = TRUE)

#summary of hr data
summary(ar_hr)

#display first five row
inspect(ar_hr[1:5])

#build model bu using apriori function
apriori(ar_hr)
AR_rules <- apriori(ar_hr,parameter=list(support =0.01, confidence = 0.8, 
                                         minlen = 2), appearance=list(rhs=c("No", "Yes"), default="lhs"))
summary(AR_rules)

## keep three decimal places
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
hr<- read.csv("hr.csv")
#read this file when running the code second time

#column names
colnames(hr)

# clean data.
summary(hr)
str(hr)
any(is.na(hr))

#remove unwanted columns.
hrN <- hr[ , -(c(3,6,9,10,18,20,24,26,28,31,32,34,35,36))]
#change 1st column name to Age
colnames(hrN)[1] <- "Age"
#write a new csv
write.csv(hrN, file="employeeD_new.csv", row.names = F, na = " ")

# check column names
colnames(hrN)

# read new csv file second time running code.
#hrN<- read.csv("employeeD_new.csv")
hrN<- read.csv("smoted_hr.csv")
#split data one for train and other for test
set.seed(2)

#takes all observation, randomly select 80% of employees and 20%, so two value.
#80% of time I want to select 1 and 20% time 2.
p <- sample(2, nrow(hrN), prob = c(0.8,0.2), replace =T)

train<- hrN[p==1,]
test<- hrN[p==2,]
nrow(train)
nrow(test)

#compare yes and no in two dataset
prop.table(table(train$Attrition))
prop.table(table(test$Attrition))

#naive bayes
emp <- naiveBayes(Attrition ~ Age + DailyRate + Department + Education + EducationField + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MonthlyIncome + NumCompaniesWorked + Over18 + OverTime + PerformanceRating + StandardHours + YearsInCurrentRole, data = train)
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





### Rest of DT Code ###


# evaluation
install.packages("gmodels")
library("gmodels")

# CrossTable with all details
CrossTable(test$Attrition, CompanyDT_pred) 


# CrossTable that is a lot clearer
CrossTable(test$Attrition, CompanyDT_pred, prop.chisq = FALSE, prop.c 
           = FALSE, prop.r = FALSE, dnn =c('actual_Attrition', 'predicted_Attrition'))


### boosting to '' find a better result ###

# train another model, but this time a series of models
CompanyDT_boost10 = C5.0(train[-1], train$Attrition, trials = 15)
CompanyDT_boost10

# test the models
CompanyDT_boost_predict10 = predict(CompanyDT_boost10, test) 

# evaluate this now
CrossTable(test$Attrition, CompanyDT_boost_predict10, prop.chisq = FALSE, prop.c = 
             FALSE, prop.r = FALSE, dnn =c('actual_Attrition', 'predicted_Attrition'))

