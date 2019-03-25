#rerun again 
Company <- read.csv("Company.csv", head = TRUE)
#sort the data with specific attributes
AssocRule_Company <- Company[,names(Company) %in% c('Attrition','BusinessTravel','NameOfDepartment',
                                             'FieldOfEducation','Gender','JobStatus','MaritalStatus')]

# write as a output file
write.csv(AssocRule_Company, file="AssocRule_Company.csv", row.names = F, na = " ")

#install.packages("arules")
library(arules)

set.seed(3653)
AssocRule_Company <- read.transactions("AssocRule_Company.csv", sep =",", rm.duplicates = TRUE)

#summary of Company data
summary(AssocRule_Company)

#display first five row
inspect(AssocRule_Company[1:5])

#build model bu using apriori function
apriori(AssocRule_Company)
AssocRule_Rules <- apriori(AssocRule_Company,parameter=list(support =0.01, confidence = 0.8, 
                                              minlen = 2), appearance=list(rhs=c("No", "Yes"), default="lhs"))
summary(AssocRule_Rules)

## keep tCompanyee decimal places
quality(AssocRule_Rules) <- round(quality(AssocRule_Rules), digits=4)

#Rules after sorting
SortingRules <- (sort(AssocRule_Rules, by = "lift"))

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

#################################
#Visualize plot diagram

#install.packages("arulesViz")
#install.packages("viridisLite")
library(arulesViz)
library(viridisLite)

plot(SortingRules)

plot(PruningRules)

plot((PruningRules[1:5]), method = "graph", control = list(type="items"))

#plot(PruningRules[1:15], method = "paracoord", control = list(reorder = TRUE))

plot(PruningRules, method = "grouped")