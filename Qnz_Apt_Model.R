pacman::p_load(missForest, ggplot2, lubridate, stargazer, dplyr, stringr, skimr, magrittr)
library(readr)

#import csv
housing = "https://raw.githubusercontent.com/kapelner/QC_MATH_342W_Spring_2021/master/writing_assignments/housing_data_2016_2017.csv"
housingdata <- read_csv(url(housing))
head(housingdata)

#reorganize categorical and numerical values
housingdata %<>%
  mutate(garage_exists = ifelse(!is.na(garage_exists), 1, 0),
         cats_allowed = ifelse(!is.na(cats_allowed), 1, 0),
         condo_coop = ifelse(!is.na(condo_coop), 1, 0),
         dining_type = ifelse(!is.na(dining_type), "formal", "dining",
                              "combo", "other"),
         fuel_type = ifelse(!is.na(fuel_type), "gas", "oil", "electric",
                            "other"),
         total_taxes = as.numeric(total_taxes),
         parking_charges = as.numeric(parking_charges),
         sale_price = as.numeric(sale_price)
         )

imp = missForest(housingdata)

#delete and store null values
sapply(housingdata, function(x) sum(is.na(housingdata)))

housing_data_split = imp$ximp[!is.na(housingdata$sale_price ),]
sale_price = housingdata$sale_price[!is.na(housingdata$sale_price ),]

#update split data
housing_data_split = cbind(housing_data_split, sale_price)

#split train and test data
#set x as d
x_train = housingdata[1:2330]
x_test = housingdata[1:55]

#regression tree
pacman::p_load(rpart)

r_squared = function(x, y) lm(y~x)$r.squared

tree_reg = rpart(formula = sale_price ~ ., data= x_train, method = "anova")
printcp(tree_reg)
plotcp(tree_reg)
summary(tree_reg)

#linear model
pacman::p_load(xtable)

line_model = lm(sale_price~ ., data = x_train)
summary(line_model)

prediction_price = predict(line_model, x_train)
oos = predicted - x_train$sale_price
xtable(line_model)

#randomforest model
pacman::p_load(mlr)
rf_data = mutate(x_train, x_test)
random_forest_model = randomForest(formula = sale_price~ x_train + x_test .,
                                   data = x_train)
summary(random_forest_model)

YARF(rf_data, x_test, x_train, mod = 150)
oos_ = predict(random_forest_model, x_test)
oos_yhat = predict(random_forest_model, x_train)




