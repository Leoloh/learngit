#################################
#xgboost model
#cv
#train
#################################

library(tidyverse)
library(xgboost)

df = read.csv("EOW_MODEL_DATA_CLEANED.csv", stringsAsFactors = FALSE)
df = df[,-1]

summary(df)
sum(is.na(df))
str(df)


##########################################################################################
#one-hot encoding
#https://medium.com/@prashant.nair2050/creating-dummy-variables-for-categorical-data-in-r-programming-c2afe5d1a4ac
install.packages('fastDummies')

library('knitr')
library('fastDummies')

fastDummies_example <- data.frame(numbers = 1:3,
                                  gender  = c("male", "male", "female"),
                                  animals = c("dog", "dog", "cat"),
                                  dates   = as.Date(c("2012-01-01", "2011-12-31","2012-01-01")),
                                  stringsAsFactors = FALSE)
str(fastDummies_example)
knitr::kable(fastDummies_example)

results = fastDummies::dummy_cols(fastDummies_example, 
                     select_columns = 'animals')
results

results2 = fastDummies::dummy_cols(fastDummies_example,
                                   select_columns = c('gender', 'animals'))
results2

#https://amunategui.github.io/dummyVar-Walkthrough/
library(caret)

results3 = dummyVars("~.", data = fastDummies_example)
results3
trsf = data.frame(predict(results3, newdata = fastDummies_example))
trsf


#model.matrix()
results4 =model.matrix(~.+0, data = fastDummies_example)
results4 = data.frame(results4)
results4


########################################################################
str(df)

df_ohe = dummyVars("~.", data = df)
df_ohe = data.frame(predict(df_ohe, newdata = df))
df_ohe

names(df_ohe)
dim(df_ohe)

#random samples
n = nrow(df_ohe)
n

set.seed(2019)
train_idx = sample(n, floor(0.75*n))
length(train_idx)

train_df = df_ohe[train_idx, ]
test_df = df_ohe[-train_idx,]

dim(train_df) #29093, 90
dim(test_df) #9698, 90

X_train = train_df[,-1]
#y_train = train_df[,1]
y_train = data.frame(train_df[,1])
names(y_train) = 'SYM_TOT_COST'

X_test = test_df[,-1]
y_test = data.frame(test_df[,1])
names(y_test) = 'SYM_TOT_COST'

library('xgboost')
dtrain = xgb.DMatrix(data = as.matrix(X_train), label = as.matrix(y_train))
dtest = xgb.DMatrix(data = as.matrix(X_test), label = as.matrix(y_test))

params = list(booster = 'gbtree',
              objective = 'reg:linear',
              eta = 0.3,
              gamma = 0,
              max_depth = 6,
              min_child_weight = 1,
              subsample = 1,
              colsample_bytree = 1)

set.seed(2019)
mod_xgbcv = xgb.cv(params = params,
                   data = dtrain,
                   nrounds =100,
                   nfold = 5,
                   showsd = TRUE,
                   stratified =TRUE,
                   print_every_round = 20,
                   early_stop_round = 20,
                   maximize = FALSE)


params_2 = list(booster = 'gbtree',
              objective = 'reg:linear',
              eta = c(0.1, 0.3, 0.5),
              gamma = 0,
              max_depth = 6,
              min_child_weight = 1,
              subsample = 1,
              colsample_bytree = 1)

set.seed(2019)
mod_xgbcv_2 = xgb.cv(params = params_2,
                   data = dtrain,
                   nrounds =500,
                   nfold = 5,
                   showsd = TRUE,
                   stratified =TRUE,
                   print_every_round = 20,
                   early_stop_round = 20,
                   maximize = FALSE)


#min_rmse = min(mod_xgbcv_2[, test_rmse_mean])

summary(mod_xgbcv_2)
mod_xgbcv_2
#data.frame(mod_xgbcv_2)[,test_rmse_mean]
#mod_xgbcv
mod_xgbcv_2$evaluation_log[,test_rmse_mean]

min_rmse = min(mod_xgbcv_2$evaluation_log[,test_rmse_mean])
min_rmse
min_rmse_index = which.min(mod_xgbcv_2$evaluation_log[,test_rmse_mean])
min_rmse_index



################################################
#cv
#train
#
################################################

###### one-hot encoding ######
library(caret)

df_ohe = model.matrix(~.+0, data = df)
df_ohe = data.frame(df_ohe)

###### split dataset into training & testing ######
n = dim(df_ohe)[1]

set.seed(2019)
train_idx = sample(n, floor(0.75*n))
length(train_idx)

train_df = df_ohe[train_idx, ]
test_df = df_ohe[-train_idx,]

dim(train_df) #29093, 88
dim(test_df) #9698, 88

X_train = train_df[,-1]
y_train = data.frame(train_df[,1])
names(y_train) = 'SYM_TOT_COST'

X_test = test_df[,-1]
y_test = data.frame(test_df[,1])
names(y_test) = 'SYM_TOT_COST'

library('xgboost')
dtrain = xgb.DMatrix(data = as.matrix(X_train), label = as.matrix(y_train))
dtest = xgb.DMatrix(data = as.matrix(X_test), label = as.matrix(y_test))



library(caret)
ControlParameters = trainControl(method = 'cv',
                                number = 5,
                                savePredictions = TRUE
                                )


parameterGrid = expand.grid(eta = 0.1,
                            colsample_bytree =c(0.5,0.7),
                            max_depth = c(3,6),
                            nrounds = 100,
                            gamma = 1,
                            min_child_weight = 2,
                            subsample = 0.5)

set.seed(2019)
xgb_mod1 = train(SYM_TOT_COST~.,
                 data = train_df,
                 method = 'xgbTree',
                 trControl = ControlParameters,
                 tuneGrid = parameterGrid)


print(xgb_mod1)

###### optimal parameters ######
params = list(booster = 'gbtree',
              objective = 'reg:linear',
              eta = 0.1,
              gamma = 1,
              colsample_bytree =0.5,
              min_child_weight = 2,
              subsample = 0.5,
              eval_metric = 'mae')

xgb_mod2 = xgb.train(params = params,
                     data = dtrain,
                     nrounds = 100,
                     watchlist = list(train = dtrain, val = dtest),
                     print_every_n = 10,
                     early_stopping_rounds = 20,
                     maximize = FALSE
                     )

saveRDS(xgb_mod2, 'xgb_mod2.rds')

importance_matrix = xgb.importance(names(X_train),
                                        xgb_mod2)

xgb.plot.importance(importance_matrix[1:10,])
xgb.plot.importance(importance_matrix[1:20,])

test_pred = predict(xgb_mod2, dtest)
train_pred = predict(xgb_mod2, dtrain)


df[train_idx, 'Xgb_pred'] = train_pred
df[-train_idx, 'Xgb_pred'] = test_pred

#save prediction
write.csv(df, 'EOW_MODEL_DATA_XGB_PRED.csv')

#correlation 0.81 of train&test 
cor(df['SYM_TOT_COST'], df['Xgb_pred'])

#density plot
library(ggplot2)
#df_2: train+test

df_2 = df[,c('SYM_TOT_COST', 'Xgb_pred')]

library(reshape2)
df_2 = melt(df_2)

ggplot(df_2) + geom_density(aes(x = value, colour = variable)) +
  ggtitle('Density Distribution of SYM_TOT_COST vs. Predicted Cost of XGBoost')


ggplot(df_2) + geom_density(aes(x = value, colour = variable)) +
  ggtitle('Density Distribution of SYM_TOT_COST vs. Predicted Cost of XGBoost')+
  xlim(0, 4000)

###### scatter plot ######
ggplot(df, aes(x=Xgb_pred, y=SYM_TOT_COST)) + geom_point() +
  geom_smooth(method=lm) +
  ggtitle('Scatter plot of SYM_TOT_COST vs. Predicted Cost of Xgboost') +
  coord_fixed(ratio=1)


ggplot(df, aes(x=Xgb_pred, y=SYM_TOT_COST)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = 'blue') +
  ggtitle('Scatter plot of SYM_TOT_COST vs. Predicted Cost of Xgboost')+
  coord_fixed(ratio=1)


######  test dataset ######
df_test = df[-train_idx,]

# 9698, 14
dim(df_test)

df_test['Xgb_pred_test'] = test_pred

#correlation coefficient: 0.76
cor(df_test['SYM_TOT_COST'], df_test['Xgb_pred_test'])
write.csv(df_test, 'EOW_Xgb_pred_test.csv')

df_test_2 = df_test[,c('SYM_TOT_COST', 'Xgb_pred_test')]
df_test_2 = melt(df_test_2)


ggplot(df_test_2) + geom_density(aes(x = value, colour = variable)) +
  ggtitle('Density Distribution of SYM_TOT_COST vs. Predicted Cost of XGBoost (test)')


ggplot(df_test_2) + geom_density(aes(x = value, colour = variable)) +
  ggtitle('Density Distribution of SYM_TOT_COST vs. Predicted Cost of XGBoost (test)')+
  xlim(0, 4000)


ggplot(df_test, aes(x=Xgb_pred_test, y=SYM_TOT_COST)) + geom_point() +
  geom_smooth(method=lm) +
  ggtitle('Scatter plot of SYM_TOT_COST vs. Predicted Cost of Xgboost (test)') +
  coord_fixed(ratio=1)


ggplot(df_test, aes(x=Xgb_pred, y=SYM_TOT_COST)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = 'blue') +
  ggtitle('Scatter plot of SYM_TOT_COST vs. Predicted Cost of Xgboost (test)')+
  coord_fixed(ratio=1)
