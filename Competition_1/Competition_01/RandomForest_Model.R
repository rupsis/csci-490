library(readr)
library(party)
library(randomForest)
library(acs)
train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")
train2 <-read_csv("data/train2.csv")
test2 <- read_csv("data/test2.csv")


prediction = data.frame(test$id)
names(prediction)[1] <- 'id'

# creat a classification tree
ct = ctree(as.factor(made_purchase)~zipcode+price+v1+v2+v3+v4+v5+v6+v7+v8, data = train)
plot(ct)

# Create a random forest model
rm_model = randomForest(as.factor(made_purchase)~zipcode+price+v1+v2+v3+v4+v5+v6+v7+v8, ntree=2000, data=train)
summary(rm_model)

prediction$made_purchase=predict(rm_model, test, type="prob")
prediction$made_purchase = prediction$made_purchase[,1]

str(prediction)
View(prediction)
write.csv(prediction[,c("id", "made_purchase")], "rf/rf_submission.csv", row.names=F)

# Grab 

api.key.install("0405a34db2280646b25e92e02f95cd8003a04d7b")
#estimate(acs.fetch(endyear=2013,geo=geo.make(zip.code=46222),variable="C25074_001E"))

# Income and Population tables
incomeTableId     = "B19301_001"
populationTableId = "B01003_001"

# grab the income from each zipcode
for(i in 1:nrow(train2)){
  train2$income[i] = tryCatch({
    estimate(acs.fetch(endyear=2013,geo=geo.make(zip.code=train2$zipcode[i]),variable=incomeTableId))
  },
  error = function(cond){
    message(cond)
    return(NA)
  })
}


# get the population information for each zip code
for(i in 1:nrow(train2)){
  train2$population[i] = tryCatch({
    estimate(acs.fetch(endyear=2013,geo=geo.make(zip.code=train2$zipcode[i]),variable=populationTableId))
  },
  error = function(cond){
    message(cond)
    return(NA)
  })
}
View(testCensus)

# Fill in any NA with the average of the income/population
train2$income[is.na(train2$income)]=round(mean(train2$income,na.rm=T))
train2$population[is.na(train2$population)]=round(mean(train2$population,na.rm=T))


# write the census data to csv to avoid having to repeat the api calls
write.csv(train2, "data/train2.csv", row.names=F)


# Random Forest model using Income and Population
rm_model_2 = randomForest(as.factor(made_purchase)~zipcode+price+income+price*income+population, ntree=2000, data=train2)
summary(rm_model_2)

prediction$made_purchase=predict(rm_model_2, test2, type="prob")
prediction$made_purchase = prediction$made_purchase[,1]

str(prediction)
View(prediction)






