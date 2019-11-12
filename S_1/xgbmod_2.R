###### xgboost model 2 ######

library(tidyverse)
library(xgboost)

df = read.csv("EOW_MODEL_DATA_CLEANED.csv", stringsAsFactors = FALSE)
df = df[,-1]

summary(df)
sum(is.na(df))
str(df)

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
                            colsample_bytree =c(0.5,0.7, 0.9),
                            max_depth = c(3,6,9),
                            nrounds = 1000,
                            gamma = 1,
                            min_child_weight = 2,
                            subsample = c(0.5,0.7,0.9))

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
                     nrounds = 1000,
                     watchlist = list(train = dtrain, val = dtest),
                     print_every_n = 10,
                     early_stopping_rounds = 20,
                     maximize = FALSE)
print(xgb_mod2)
saveRDS(xgb_mod2, 'xgb_mod3.rds')

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

#correlation 0.79 of train&test 
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







