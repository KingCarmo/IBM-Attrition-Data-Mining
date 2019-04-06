#install.packages("rpart")
i#nstall.packages("rpart.plot")
#install.packages("party")
library(party)
library(rpart.plot)
library(rpart)


# reading the Company.csv data set
CompanyDT = read.csv("Company.csv", header = TRUE)

# lets look at it structure of the dataset from IBM
str(CompanyDT)


normalize <- function(x) { return((x - min(x)) / (max(x)-min(x)))}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

barplot(prop.table(table(CompanyDT$Attrition)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")



# randomize first
set.seed(6969) # sync the randomization / allows reproducibility

# create a training and a test sub-set
pd <- sample(2, nrow(CompanyDT),replace = TRUE, prob = c(0.8,0.2))
train <- CompanyDT[pd==1,] 
test <-CompanyDT[pd==2,] 

# data for Developing predictive Model 
table(train$Attrition)
prop.table(table(train$Attrition))

table(train$Attrition)
table(test$Attrition)

# check their distributions
prop.table(table(train$Attrition))
prop.table(table(test$Attrition))
#original
prop.table(table(CompanyDT$Attrition))

# install and load the C5.0 library
#install.packages("C50")
library(C50)

# train the model
CompanyDT_model = C5.0(train[,-c(2,22)], train$Attrition)
summary(CompanyDT_model)
plot(CompanyDT_model)

m2 = rpart(Attrition ~ ., data=CompanyDT[,-c(22)], method="class")
m2 


# Decision with rpart
m2 <- rpart(Attrition~DailyRate+DistanceFromHome+JobInvolvement+MonthlyRate+RelationshipSatisfaction+WorkLifeBalance
            + TrainingTimesLastYear+RelationshipSatisfaction +EnvironmentSatisfaction+HourlyRate, CompanyDT)
library(rpart.plot)
rpart.plot(m2,extra = 4)


# test the model (Error)
CompanyDT_pred = predict(CompanyDT_model, test)  
CompanyDT_pred

# evaluation

#install.packages("gmodels")
library("gmodels")

# CrossTable with all details
CrossTable(test$Attrition, CompanyDT_pred) 


# CrossTable that is a lot clearer
CrossTable(test$Attrition, CompanyDT_pred, prop.chisq = FALSE, prop.c 
           = FALSE, prop.r = FALSE, dnn =c('actual_Attrition', 'predicted_Attrition'))

