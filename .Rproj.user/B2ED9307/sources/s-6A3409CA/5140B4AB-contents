#Run CSV File
CompanyAssocRules <- read.csv("Company.csv", head = TRUE)
#sort the data with specific attributes
AR_CompanyAssocRules <- CompanyAssocRules[,names(CompanyAssocRules) %in% c('Attrition','BusinessTravel','Department',
                              'EducationField','Gender','JobRole','MaritalStatus')]

# write as a output file
write.csv(AR_CompanyAssocRules, file="AR_CompanyAssocRules.csv", row.names = F, na = " ")

#install.packages("arules")
library(arules)

set.seed(3653)
ar_CompanyAssocRules <- read.transactions("AR_CompanyAssocRules.csv", sep =",", rm.duplicates = TRUE)

#summary of CompanyAssocRules data
summary(ar_CompanyAssocRules)

#display first five row
inspect(ar_CompanyAssocRules[1:5])

#build model bu using apriori function
apriori(ar_CompanyAssocRules)
AR_rules <- apriori(ar_CompanyAssocRules,parameter=list(support =0.01, confidence = 0.8, 
                                         minlen = 2), appearance=list(rhs=c("No", "Yes"), default="lhs"))
summary(AR_rules)

## keep tCompanyAssocRulesee decimal places
quality(AR_rules) <- round(quality(AR_rules), digits=4)

#Rules after sorting
SortingRules <- (sort(AR_rules, by = "lift"))

## find redundant rules
subset.matrix <- is.subset(SortingRules, SortingRules, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1

## which rules are redundant
which(redundant)

## remove redundant rules
PruningRules <- SortingRules[!redundant]
#PruningRules
inspect(PruningRules[1:5])

#Total of rules with Attrition 'Yes' and 'No'
subset(PruningRules, items %in% "Yes")
subset(PruningRules, items %in% "No")

###Visualize plot diagram###

#install.packages("arulesViz")
#install.packages("viridisLite")
library(arulesViz)
library(viridisLite)

plot(SortingRules)
plot(PruningRules)
plot((PruningRules[1:5]), method = "graph", control = list(type="items"))
plot(PruningRules[1:20], method = "paracoord", control = list(reorder = TRUE))
plot(PruningRules, method = "grouped")

