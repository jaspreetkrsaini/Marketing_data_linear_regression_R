########Project 1#########
## By Jaspreet Kaur Saini: StudentID- 8876747 
library(tidyverse)
library(stargazer)
library(ISLR2)
library(caret)
library(leaps)
library(ggpubr)
devtools::install_github(("kassambara/datarium"))
library(devtools)
library(datarium)

# Load the marketing  data from datarium

data(marketing)
head(marketing)

####### Exploring marketing data############
# Check the structure of the marketing data
str(marketing)
summary(marketing)

# plot all variables of marketing data
plot(marketing)

cor(marketing)

marketing %>% ggplot(aes(x = youtube, y = sales)) + geom_point() + geom_smooth(method="lm", se = FALSE)
marketing %>% ggplot(aes(x = facebook, y = sales)) + geom_point() + geom_smooth(method="lm", se = FALSE)
marketing %>% ggplot(aes(x = newspaper, y = sales)) + geom_point() + geom_smooth(method="lm", se = FALSE)

## intital modelling
# Fit initial linear regression model
reg1 <- lm(formula = sales ~ youtube + facebook + newspaper, data = marketing)

summary(reg1)
summary(reg1)$coefficient

##3.Diagnostics:
#  Let's perform diagnostics tests and assess the assumptions of the linear regression model.
# Diagnostic plots for initial model
par(mfrow = c(2, 2))
plot(reg1)

# 4.Model Selection:

# Improved model with non-linear terms

# Variable selection 
lm2 = train(sales ~ youtube + facebook + newspaper, data = marketing, method = "lm", trControl = trainControl(method = "cv", number = 10))
lm2$results


subset <- regsubsets(sales ~ youtube + facebook + newspaper , data = marketing, nvmax=3)
summary(subset)

# plot the adjr2
plot(subset, scale = "adjr2")

bestM = which.max(summary(subset)$adjr2)
bestM

# since best M is 2 i.e. using youtube and facebook
final_model_bestM = train(sales ~ youtube + facebook , data = marketing, method = "lm", trControl = trainControl(method = "cv", number = 10))
summary(final_model_bestM)


#5. Prediction and Summary:
# Let's use the final model for predictions and summarize the results.

# Predict using the final model
predicted_sales <- predict(final_model_besM, newdata = marketing)

# Summary of predictions
prediction_summary <- data.frame(Actual = marketing$sales, Predicted = predicted_sales)
head(prediction_summary)
par(mfrow = c(1, 1))
plot(prediction_summary )

#result for accuracy check for both the models
lm2$results
final_model_bestM$results
