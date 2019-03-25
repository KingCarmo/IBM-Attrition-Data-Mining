install.packages("rpart")
install.packages("rpart.plot")
install.packages("party")
library(party)
require(rpart)
require(rpart.plot)


# reading the data set
Company = read.csv("Company.csv", header = TRUE)

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

# boosting to 'hopefully' find a better result
# train another model, but this time a series of models
Company_boost10 = C5.0(train[-1], train$Attrition, trials = 15)
Company_boost10

# test the models
Company_boost_predict10 = predict(Company_boost10, test) 

# evaluate this now
CrossTable(test$Attrition, Company_boost_predict10, prop.chisq = FALSE, prop.c = 
             FALSE, prop.r = FALSE, dnn =c('actual_Attrition', 'predicted_Attrition'))