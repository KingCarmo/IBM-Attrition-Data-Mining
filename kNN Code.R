#Read file
kNN <- read.csv(file="Company.csv",head=TRUE,sep = ",", stringsAsFactors = FALSE)
kNN

kNN$EmployeeCount <-NULL
kNN$StandardHours <-NULL
kNN$BusinessTravel <-NULL
kNN$Department <-NULL
kNN$EducationField <-NULL
kNN$Gender <-NULL
kNN$JobRole <-NULL
kNN$MaritalStatus <-NULL
kNN$Over18 <-NULL
kNN$OverTime <-NULL
str(kNN)

#randomize
set.seed(214805)
kNN_rand = kNN[order(runif(1470)),]

str(kNN_rand)
kNN_rand$Attrition
kNN$Attrition <- na.omit(kNN_rand$Attrition)
table(kNN_rand$Attrition)
factor(kNN_rand$Attrition, levels = c("No", "Yes"),labels = c("N", "Y")) 
prop.table(table(kNN_rand$Attrition))
summary(kNN_rand)

normalize <- function(x) { return((x - min(x)) / (max(x)-min(x)))}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

kNN_n <- as.data.frame(lapply(kNN_rand[3:25], normalize))

summary(kNN_n)


kNN_train <- kNN_n[1:1176, ]
kNN_test <- kNN_n[1177:1470, ]
kNN_train_labels <- kNN_rand[1:1176, 1]
kNN_test_labels <- kNN_rand[1177:1470, 1]

#install.packages("class")
library(class)

sqrt(1176)
kNN_test_pred <- knn(train = kNN_train, test = kNN_test, cl = kNN_train_labels, k=19)

install.packages("gmodels")
library("gmodels")

CrossTable(x = kNN_test_labels, y = kNN_test_pred,prop.chisq=FALSE)
#End of KNN techniques
