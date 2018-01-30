library(readr)
library(reshape)
train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")

# grabing the save test data that includes the census data
train2 <- read_csv("data/train2.csv")
testCensus <- read_csv("data/testCensus.csv")

View(test)
names(train)


# looking at the distribution of price
hist(train$price)


# create a GLM model using price, zip code, and each variant 
glm_model = glm(made_purchase~price+zipcode+price*zipcode, data=train, family="binomial")
summary(glm_model)
test$made_purchase=predict(glm_model, test, type="response")
write.csv(test[,c("id", "made_purchase")], "glm/submission_price_zipcode.csv", row.names=F)
View(test)


# Create a cross validation to evaluate the model
train_length = nrow(train)
train1 = train[1:(train_length/2),]
valid1 = train[train_length: (train_length/2),]

# view cross validation of our glm model
glm_model = glm(made_purchase~price+zipcode+price*zipcode, data=train1, family="binomial")
summary(glm_model)

valid1$prediction=predict(glm_model, valid1, type="response")
sum(valid1$prediction==valid1$made_purchase)/nrow(valid1)


# Create a GLM model using the Census data
glm_model2 =  glm(made_purchase~price+income+price*income+population, data=train2, family="binomial")
summary(glm_model2)


testCensus$made_purchase=predict(glm_model2, testCensus, type="response")
testCensus$made_purchase[is.na(testCensus$made_purchase)]=0
write.csv(testCensus[,c("id", "made_purchase")], "glm/submission_census_price_zipcode.csv", row.names=F)


