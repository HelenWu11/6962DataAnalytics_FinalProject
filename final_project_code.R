library(dplyr)
library(ggplot2)
library(lubridate)
# date,county,state,fips,cases,deaths
# 2020-01-21,Snohomish,Washington,53061,1,0
CASE_DEATH <- read.csv("C:/Users/rwu/Desktop/Fall2020/6962-DataAnalytics/Final_Project/COVID-19/us-counties.csv")
TESTING <- read.csv("C:/Users/rwu/Desktop/Fall2020/6962-DataAnalytics/Final_Project/COVID-19/New_York_State_Statewide_COVID-19_Testing.csv")
POPULATION_BY_AGE_AND_SEX <- read.csv("C:/Users/rwu/Desktop/Fall2020/6962-DataAnalytics/Final_Project/COVID-19/Population_by_Age_and_Sex_-_Counties.csv")
INCOME_AND_BENEFITS_COUNTY <- read.csv("C:/Users/rwu/Desktop/Fall2020/6962-DataAnalytics/Final_Project/COVID-19/Income_and_Benefits_-_Counties.csv")
WORK_AT_HOME <- read.csv("C:/Users/rwu/Desktop/Fall2020/6962-DataAnalytics/Final_Project/COVID-19/Worked_at_Home_-_Counties.csv")

attach(CASE_DEATH)
attach(TESTING)
attach(POPULATION_BY_AGE_AND_SEX)
attach(INCOME_AND_BENEFITS_COUNTY)
attach(WORK_AT_HOME)

View(TESTING)

# filter the CASE_DEATH
CASE_DEATH <- filter(CASE_DEATH, CASE_DEATH$state=='New York')
View(CASE_DEATH)

# rename work_at_home dataset
colnames(WORK_AT_HOME) <- c("OBJECTID","COUNTY_NAME","COUNTY_PARENT_NAME", "Total_Population-Worked_at_home",	"Total_Population-Worked_at_home*margin_of_error")
WORK_AT_HOME <- filter(WORK_AT_HOME, WORK_AT_HOME$COUNTY_PARENT_NAME=='New York')
View(WORK_AT_HOME)

# drop not use columns from INCOME_AND_BENEFITS_COUNTY and rename columns
INCOME_AND_BENEFITS_COUNTY <- INCOME_AND_BENEFITS_COUNTY[, -c(8:17)]
INCOME_AND_BENEFITS_COUNTY <- INCOME_AND_BENEFITS_COUNTY[, -c(10:22)]
colnames(INCOME_AND_BENEFITS_COUNTY) <- c("OBJECTID", "GEO_ID", "COUNTY_NAME", "COUNTY_PARENT_NAME", "FIPS_CODE", "Total_Households", "Total_Households*margin_of_error", "TotalHouseholdswithIncome-$75,000_to_$99,999", "TotalHouseholdswithIncome-$75,000_to_$99,999*margin_of_error")
INCOME_AND_BENEFITS_COUNTY <- filter(INCOME_AND_BENEFITS_COUNTY, INCOME_AND_BENEFITS_COUNTY$COUNTY_PARENT_NAME=='New York')
View(INCOME_AND_BENEFITS_COUNTY)

# drop useless column from POPULATION_BY_AGE_AND_SEX and rename columns
POPULATION_BY_AGE_AND_SEX <- POPULATION_BY_AGE_AND_SEX[, c(1,2,3,4,5, 114,116,118)]
colnames(POPULATION_BY_AGE_AND_SEX) <- c("OBJECTID", "GEO_ID", "COUNTY_NAME", "COUNTY_PARENT_NAME", "Total_Population", "Total_Population-65_years_and_over",	"Total_Population-Male_65_and_over", "Total_Population-Female_65_and_over")
POPULATION_BY_AGE_AND_SEX <- filter(POPULATION_BY_AGE_AND_SEX, POPULATION_BY_AGE_AND_SEX$COUNTY_PARENT_NAME=='New York')
View(POPULATION_BY_AGE_AND_SEX)

summary(CASE_DEATH)
summary(TESTING)
summary(POPULATION_BY_AGE_AND_SEX)
summary(INCOME_AND_BENEFITS_COUNTY)
summary(WORK_AT_HOME)


str(TESTING)
TESTING$County <- as.factor(TESTING$County)
summary(TESTING$County)

# Making a TESTING month column
TESTING$Test.Date <- as.POSIXct(TESTING$Test.Date,format="%m/%d/%Y")
TESTING$Test_Date_Month <- month(TESTING$Test.Date)
summary(TESTING)


# filter out June
JUNE_TESTING <- filter(TESTING, month(TESTING$Test.Date)==6)

JUNE_TESTING_GROUP <- group_by(JUNE_TESTING, County)
JUNE_TESTING_GROUP_S <- summarise(JUNE_TESTING_GROUP,
                   count = n(), 
                  sum_new_positives = sum(New.Positives),
                  sum_cumulative_positive = sum(Cumulative.Number.of.Positives),
                  sum_tests = sum(Total.Number.of.Tests.Performed),
                  sum_cumulative_tests = sum(Cumulative.Number.of.Tests.Performed))
View(JUNE_TESTING_GROUP_S)


# Making a CASE_DEATH month column
CASE_DEATH$date <- as.Date(CASE_DEATH$date)
CASE_DEATH$Test_Date_Month <- month(CASE_DEATH$date)
summary(CASE_DEATH)

# filter out June
JUNE_CASE_DEATH <- filter(CASE_DEATH, month(CASE_DEATH$date)==6)

JUNE_CASE_DEATH_GROUP <- group_by(JUNE_CASE_DEATH, county)
JUNE_CASE_DEATH_GROUP_S <- summarise(JUNE_CASE_DEATH_GROUP,
                                  count = n(), 
                                  sum_cases = sum(cases),
                                  sum_deaths = sum(deaths))
colnames(JUNE_CASE_DEATH_GROUP_S) <- c("County", "count", "sum_cases", "sum_deaths")
JUNE_CASE_DEATH_GROUP_S[29, 1] <- "New York"
View(JUNE_CASE_DEATH_GROUP_S)



# Merging JUNE_CASE_DEATH_GROUP_S and JUNE_TESTING_GROUP_S
MERGE_JUNE_CASE_TESTING <- merge(JUNE_CASE_DEATH_GROUP_S, JUNE_TESTING_GROUP_S, by=c("County","count"))
View(MERGE_JUNE_CASE_TESTING)

# Replace county name
install.packages("stringr")
library(stringr)
WORK_AT_HOME$COUNTY_NAME <- str_replace(WORK_AT_HOME$COUNTY_NAME, " County", "")
colnames(WORK_AT_HOME)[2] <- "County"
View(WORK_AT_HOME)

POPULATION_BY_AGE_AND_SEX$COUNTY_NAME <- str_replace(POPULATION_BY_AGE_AND_SEX$COUNTY_NAME, " County", "")
colnames(POPULATION_BY_AGE_AND_SEX)[3] <- "County"
View(POPULATION_BY_AGE_AND_SEX)

INCOME_AND_BENEFITS_COUNTY$COUNTY_NAME <- str_replace(INCOME_AND_BENEFITS_COUNTY$COUNTY_NAME, " County", "")
colnames(INCOME_AND_BENEFITS_COUNTY)[3] <- "County"
View(INCOME_AND_BENEFITS_COUNTY)

# Combine all useful features
WAT <- WORK_AT_HOME[,c(2,4,5)]
IABC <- INCOME_AND_BENEFITS_COUNTY[,c(3,6,8)]
PBAAS <- POPULATION_BY_AGE_AND_SEX[,-c(1,2,4)]

MERGE_JUNE_CASE_TESTING <- merge(MERGE_JUNE_CASE_TESTING, WAT, by="County")
MERGE_JUNE_CASE_TESTING <- merge(MERGE_JUNE_CASE_TESTING, IABC, by="County")
MERGE_JUNE_CASE_TESTING <- merge(MERGE_JUNE_CASE_TESTING, PBAAS, by="County")
MERGE_JUNE_CASE_TESTING <- MERGE_JUNE_CASE_TESTING[,-c(10)]
View(MERGE_JUNE_CASE_TESTING)

MERGE_JUNE_CASE_TESTING[is.na(MERGE_JUNE_CASE_TESTING)] = 0
MERGE_JUNE_CASE_TESTING <- MERGE_JUNE_CASE_TESTING[,-c(2,3)]
#MERGE_JUNE_CASE_TESTING["deaths_rate"] = MERGE_JUNE_CASE_TESTING$sum_deaths / MERGE_JUNE_CASE_TESTING$Total_Population
MERGE_JUNE_CASE_TESTING["cumulative_positive_rate"] = MERGE_JUNE_CASE_TESTING$sum_cumulative_positive / MERGE_JUNE_CASE_TESTING$Total_Population
#MERGE_JUNE_CASE_TESTING["cumulative_test_rate"] = MERGE_JUNE_CASE_TESTING$sum_cumulative_tests / MERGE_JUNE_CASE_TESTING$Total_Population
#MERGE_JUNE_CASE_TESTING["wfh_rate"] = MERGE_JUNE_CASE_TESTING$`Total_Population-Worked_at_home` / MERGE_JUNE_CASE_TESTING$Total_Population


View(MERGE_JUNE_CASE_TESTING)

#==================================================================
summary(MERGE_JUNE_CASE_TESTING)
boxplot(log(MERGE_JUNE_CASE_TESTING$sum_new_positives))
boxplot(log(MERGE_JUNE_CASE_TESTING$sum_cumulative_positive))
boxplot(log(MERGE_JUNE_CASE_TESTING$sum_cumulative_tests))


hist(log(MERGE_JUNE_CASE_TESTING$sum_cumulative_positive))
hist(log(MERGE_JUNE_CASE_TESTING$sum_cumulative_tests))
hist(log(MERGE_JUNE_CASE_TESTING$sum_deaths))
hist(MERGE_JUNE_CASE_TESTING$`Total_Population-Worked_at_home`)


ggplot(MERGE_JUNE_CASE_TESTING, aes(x = log(sum_cumulative_positive), y = log(sum_deaths), colour = County)) + geom_point()
ggplot(MERGE_JUNE_CASE_TESTING, aes(x = log(sum_cumulative_tests), y = log(sum_cumulative_positive), colour = County)) + geom_point()


ggplot(data = MERGE_JUNE_CASE_TESTING, aes(x = County, y = log(sum_cumulative_positive))) +
  geom_point() +
  labs(x = "County",
       y = "sum_cumulative_positive")

ggplot(data = MERGE_JUNE_CASE_TESTING, aes(x = County, y = log(sum_cumulative_tests))) +
  geom_point() +
  labs(x = "County",
       y = "sum_cumulative_tests")

ggplot(data = MERGE_JUNE_CASE_TESTING, aes(x = County, y = log(sum_deaths))) +
  geom_point() +
  labs(x = "County",
       y = "sum_deaths")

plot(log(MERGE_JUNE_CASE_TESTING$sum_cumulative_tests), log(MERGE_JUNE_CASE_TESTING$sum_cumulative_positive))
plot(log(MERGE_JUNE_CASE_TESTING$sum_cumulative_tests), log(MERGE_JUNE_CASE_TESTING$sum_deaths))
plot(log(MERGE_JUNE_CASE_TESTING$`Total_Population-Worked_at_home`), log(MERGE_JUNE_CASE_TESTING$sum_cumulative_positive))
plot(log(MERGE_JUNE_CASE_TESTING$`Total_Population-65_years_and_over`), log(MERGE_JUNE_CASE_TESTING$sum_cumulative_positive))
#==================================================================


## ====================================================================
# First model Linear Regression
ALL_MONTH_TESTING_GROUP <- TESTING %>% group_by(Test_Date_Month, County)
ALL_MONTH_TESTING_GROUP_S <- ALL_MONTH_TESTING_GROUP %>% summarise(sum_new_positives = sum(New.Positives),
                                                                   sum_cumulative_positive = sum(Cumulative.Number.of.Positives),
                                                                   sum_tests = sum(Total.Number.of.Tests.Performed),
                                                                   sum_cumulative_tests = sum(Cumulative.Number.of.Tests.Performed))

View(ALL_MONTH_TESTING_GROUP_S)
ALL_MONTH_TESTING_GROUP_S$County <- as.factor(ALL_MONTH_TESTING_GROUP_S$County)

ggplot(ALL_MONTH_TESTING_GROUP_S, aes(Test_Date_Month, sum_cumulative_positive)) + geom_point(aes(colour = factor(County)))
ggplot(ALL_MONTH_TESTING_GROUP_S, aes(Test_Date_Month, sum_new_positives)) + geom_point(aes(colour = factor(County)))
ggplot(ALL_MONTH_TESTING_GROUP_S, aes(Test_Date_Month, sum_tests)) + geom_point(aes(colour = factor(County)))
ggplot(ALL_MONTH_TESTING_GROUP_S, aes(Test_Date_Month, sum_cumulative_tests)) + geom_point(aes(colour = factor(County)))

set.seed(1)
row.number <- sample(1:nrow(ALL_MONTH_TESTING_GROUP_S), 0.8*nrow(ALL_MONTH_TESTING_GROUP_S))
train_1 = ALL_MONTH_TESTING_GROUP_S[row.number,]
test_1 = ALL_MONTH_TESTING_GROUP_S[-row.number,]
dim(train_1)
dim(test_1)

model_month_county = lm(sum_cumulative_positive~., data=train_1)
summary(model_month_county)
par(mfrow=c(2,2))
plot(model_month_county)

pred_month_county <- predict(model_month_county, newdata = test_1)
rmse <- sqrt(sum((pred_month_county - test_1$sum_cumulative_positive)^2)/length(test_1$sum_cumulative_positive))
c(RMSE = rmse, R2=summary(model_month_county)$r.squared)

par(mfrow=c(1,1))
plot(test_1$sum_cumulative_positive, pred_month_county)

## =================================================================
# First model Linear Regression

# split the sample data and make the model
positive_model <- MERGE_JUNE_CASE_TESTING[,c(3,4,5,6,7,9,10,11)]
View(positive_model)

set.seed(1)
row.number <- sample(1:nrow(positive_model), 0.8*nrow(positive_model))
train = positive_model[row.number,]
test = positive_model[-row.number,]
dim(train)
dim(test)

# make default model
model1 = lm(sum_cumulative_positive~., data=train)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

# remove the less significant feature
model2 = update(model1, ~.-`Total_Population-Worked_at_home`-`Total_Population-65_years_and_over`)
summary(model2)
par(mfrow=c(2,2))
plot(model2)

# Plot the residual plot with all predictors
attach(train)
require(gridExtra)
plot1 = ggplot(train, aes(sum_new_positives, residuals(model2))) + geom_point() + geom_smooth()
plot2 = ggplot(train, aes(sum_tests, residuals(model2))) + geom_point() + geom_smooth()
plot3 = ggplot(train, aes(sum_cumulative_tests, residuals(model2))) + geom_point() + geom_smooth()
plot4 = ggplot(train, aes(`TotalHouseholdswithIncome-$75,000_to_$99,999`, residuals(model2))) + geom_point() + geom_smooth()
plot5 = ggplot(train, aes(Total_Population, residuals(model2))) + geom_point() + geom_smooth()
grid.arrange(plot1, plot2, plot3, plot4, plot5)

# Adding a square term to check for non-linearity
model3 = lm(formula = sum_cumulative_positive ~ sum_new_positives+sum_tests
            +sum_cumulative_tests+`TotalHouseholdswithIncome-$75,000_to_$99,999`+
              Total_Population+I(sum_new_positives^2)+I(sum_tests^2)+
              I(sum_cumulative_tests^2)+I(`TotalHouseholdswithIncome-$75,000_to_$99,999`^2)
            +I(Total_Population^2), data=train)
summary(model3)
par(mfrow=c(2,2))
plot(model3)

# Removing the insignificant variables
model4 = update(model3, ~.-sum_new_positives-Total_Population-I(sum_new_positives^2)-I(Total_Population^2))
summary(model4)
par(mfrow=c(2,2))
plot(model4)

pred1 <- predict(model4, newdata = test)
rmse <- sqrt(sum((pred1 - test$sum_cumulative_positive)^2)/length(test$sum_cumulative_positive))
c(RMSE = rmse, R2=summary(model4)$r.squared)

par(mfrow=c(1,1))
plot(test$sum_cumulative_positive, pred1)
#===================================================================

# Build second Model: Classification using Decision Tree Classifiers
hist(MERGE_JUNE_CASE_TESTING$cumulative_positive_rate)
summary(MERGE_JUNE_CASE_TESTING$cumulative_positive_rate)
boxplot(MERGE_JUNE_CASE_TESTING$cumulative_positive_rate)

MERGE_JUNE_CASE_TESTING$cumulative_positive_rate_category <-
   case_when(
    MERGE_JUNE_CASE_TESTING$cumulative_positive_rate < 0.0820 ~ "low",
    MERGE_JUNE_CASE_TESTING$cumulative_positive_rate > 0.2000 ~ "high",
    TRUE ~ "median"
  )

classfication_dataset <- MERGE_JUNE_CASE_TESTING[,c(3,4,5,6,7,8,9,10,11,15)]
# View(classfication_dataset)

library(C50)
table(classfication_dataset$cumulative_positive_rate_category)
classfication_dataset$cumulative_positive_rate_category <- as.factor(MERGE_JUNE_CASE_TESTING$cumulative_positive_rate_category)

# Load the Caret package which allows to partition on the data
library(caret)

index <- createDataPartition(classfication_dataset$cumulative_positive_rate_category, p=0.80, list=FALSE)

# Select 20% of the data for testing
testset <- classfication_dataset[-index,]

# Select 80% of the data to train the models
trainset <- classfication_dataset[index,]
View(trainset)

dim(trainset)
dim(testset)

# Structure of the data
str(trainset)

# Summary of the data
summary(trainset)

# Levels of the prediction column
levels(trainset$cumulative_positive_rate_category)

library(ggplot2)

# Scatter plot
g <- ggplot(data=trainset, aes(x = log(sum_tests), y = log(sum_new_positives)))
print(g)

g <-g + 
  geom_point(aes(color=cumulative_positive_rate_category, shape=cumulative_positive_rate_category)) +
  xlab("sum_tests") +
  ylab("sum_new_positives") +
  geom_smooth(method="lm")
print(g)

# Scatter plot
g <- ggplot(data=trainset, aes(x = log(Total_Population), y = log(`Total_Population-Worked_at_home`)))
print(g)

g <-g + 
  geom_point(aes(color=cumulative_positive_rate_category, shape=cumulative_positive_rate_category)) +
  xlab("Total_Population") +
  ylab("`Total_Population-Worked_at_home`") +
  geom_smooth(method="lm")
print(g)

library(ggthemes)
## Histogram
histogram <- ggplot(data=classfication_dataset, aes(x=log(sum_tests))) +
  geom_histogram(binwidth=0.2, color="black", aes(fill=cumulative_positive_rate_category)) + 
  xlab("sum_tests") +  
  ylab("Frequency") + 
  ggtitle("Histogram of sum_tests")+
  theme_economist()
print(histogram)

histogram <- ggplot(data=classfication_dataset, aes(x=log(`Total_Population-Worked_at_home`))) +
  geom_histogram(binwidth=0.2, color="black", aes(fill=cumulative_positive_rate_category)) + 
  xlab("Total_Population-Worked_at_home") +  
  ylab("Frequency") + 
  ggtitle("Histogram of Total_Population-Worked_at_home")+
  theme_economist()
print(histogram)

histogram <- ggplot(data=classfication_dataset, aes(x=log(`TotalHouseholdswithIncome-$75,000_to_$99,999`))) +
  geom_histogram(binwidth=0.2, color="black", aes(fill=cumulative_positive_rate_category)) + 
  xlab("TotalHouseholdswithIncome-$75,000_to_$99,999") +  
  ylab("Frequency") + 
  ggtitle("Histogram of TotalHouseholdswithIncome-$75,000_to_$99,999")+
  theme_economist()
print(histogram)

require(rpart)
rpart_model <- rpart(cumulative_positive_rate_category~., data=trainset)
# cp is the complexity parameter, the larger the cp is, the smaller the tree's nsplit is.
# rel error Indicates the average deviation ratio between the current classification model tree and the empty tree.
# Xerror is the cross validation error, and 
# XSTD is the standard deviation of the cross validation error
printcp(rpart_model)

plot(rpart_model)
text(rpart_model)

# Predictions on train dataset
pred<-table(predict(object = rpart_model,newdata = trainset[,1:9],type="class"))
pred


library(caret)
## Checking the accuracy using a confusion matrix by comparing predictions to actual 
## classifications
confusionMatrix(predict(object = rpart_model,newdata = trainset[,1:9],type="class"),trainset$cumulative_positive_rate_category)

confusionMatrix(predict(object = rpart_model,newdata = testset[,1:9],type="class"),testset$cumulative_positive_rate_category)

## =====================================================
library(caret)
# Classificationi model with KNN Classifers
KNN_classifer_dataset <- MERGE_JUNE_CASE_TESTING[,c(3,4,5,6,7,8,9,10,11,15)]
head(KNN_classifer_dataset)

# Because the features have different scale, if the data isn't normalized, it will lead to baised outcome
#Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

KNN_classifer_dataset_n <- as.data.frame(lapply(KNN_classifer_dataset[,1:9], normalize))

# Data splicing
index2 <- createDataPartition(KNN_classifer_dataset$cumulative_positive_rate_category, p=0.80, list=FALSE)
knn_train <- KNN_classifer_dataset_n[index2,]
knn_test <- KNN_classifer_dataset_n[-index2,]

dim(knn_train)
dim(knn_test)

# creating seperate dataframe for `cumulative_positive_rate_category` feature which is the target feature
train_labels <- KNN_classifer_dataset[index2,10]
test_labels <- KNN_classifer_dataset[-index2,10]

NROW(train_labels)
# Building a Machine Leaning model
#Install class package
#install.packages('class')
# Load class package
library(class)


# there are 48 observations in the training data set. The square root of 48 is around 6.928,
# therefore, will create two models. One with 'k' value as 6, and the other model with a 'k' value as 7.
knn_6 <- knn(train=knn_train, test=knn_test, cl=train_labels, k=6)
knn_7 <- knn(train=knn_train, test=knn_test, cl=train_labels, k=7)

# Model Evaluation
library(caret)
confusionMatrix(table(knn_6 ,test_labels))
confusionMatrix(table(knn_7 ,test_labels))

# optimization
# create a loop that calculates the accuracy of the KNN model for 'K' values ranging from
# 1 to 20. This way can check which 'k' value will result in the most accurate model:
i=1
k.optm=1
for (i in 1:20){
  knn.mod <- knn(train=knn_train, test=knn_test, cl=train_labels, k=i)
  k.optm[i] <- 100 * sum(test_labels == knn.mod)/NROW(test_labels)
  k=i
  cat(k,'=',k.optm[i],'
')
}
# From the output see that for k = 5, we achieve the maximum accuracy = 90%
#Accuracy plot
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")
knn_5 <- knn(train=knn_train, test=knn_test, cl=train_labels, k=5)

# Model Evaluation

confusionMatrix(table(knn_5 ,test_labels))

