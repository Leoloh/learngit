################################
# GLM
#SYM_TOT_COST
#ACTUAL_PERIL, ASBESTOS, ALT_ACCOMDNTN_IND, NLP_EOW_REASON
#MAX_FLOOR_SQM_DEC, MAX_CEILING_SQM_DEC, MAX_WALLS_SQM_DEC, MAX_FLOOR_SQM, MAX_CEILING_SQM,
#MAX_WALLS_SQM, MAX_ROOM_CNT, ANY_DRYING_IND
################################

library(tidyverse)
df = read.csv('EOW_MODEL_DATA_CLEANED.csv', stringsAsFactors = FALSE)

#38791, 14
dim(df)

summary(df)
sum(is.na(df))
str(df)

#exclude the index column
df = df[, -1]

#38791, 13
dim(df)

#46 levels
unique(df['ACTUAL_PERIL'])

#2 levels
unique(df['ASBESTOS_PRESENT'])

#32 levels
unique(df['NLP_EOW_REASON'])
dim(unique(df['NLP_EOW_REASON']))

df['ACTUAL_PERIL'] = lapply(df['ACTUAL_PERIL'], as.factor)
df['ASBESTOS_PRESENT'] = lapply(df['ASBESTOS_PRESENT'], as.factor)
df['NLP_EOW_REASON'] = lapply(df['NLP_EOW_REASON'], as.factor)

#2 levels
table(df['ALT_ACCOMDNTN_IND'])
df['ALT_ACCOMDNTN_IND'] = lapply(df['ALT_ACCOMDNTN_IND'], as.factor)

#11 levels
table(df['MAX_FLOOR_SQM_DEC'])
df['MAX_FLOOR_SQM_DEC'] = lapply(df['MAX_FLOOR_SQM_DEC'], as.factor)

#11 levels
table(df['MAX_CEILING_SQM_DEC'])
df['MAX_CEILING_SQM_DEC'] = lapply(df['MAX_CEILING_SQM_DEC'], as.factor)

#11 levels
table(df['MAX_WALLS_SQM_DEC'])
df['MAX_WALLS_SQM_DEC'] = lapply(df['MAX_WALLS_SQM_DEC'], as.factor)

#11 levels
table(df['MAX_ROOM_CNT'])
df['MAX_ROOM_CNT'] = lapply(df['MAX_ROOM_CNT'], as.factor)

#2 levels
table(df['ANY_DRYING_IND'])
df['ANY_DRYING_IND'] = lapply(df['ANY_DRYING_IND'], as.factor)

str(df)
#####################################################################
#GLM model 1:

mod_1 = glm(SYM_TOT_COST~., data = df, family = Gamma(link = 'log'))
summary(mod_1)
saveRDS(mod_1, 'mod_1.rds')

par(mfrow = c(2,2))
par(mar = rep(2,4))
plot(mod_1)

#GLM model 2:
mod_2 = glm(SYM_TOT_COST~.+0, dat = df, family = Gamma(link = 'log'))
summary(mod_2)
saveRDS(mod_2, 'mod_2.rds')

par(mfrow = c(2,2))
par(mar = rep(2,4))
plot(mod_2)

anova(mod_1, mod_2, test = 'Chisq')

y_pred_1 = predict(mod_1, type = 'response')
y_pred_2 = predict(mod_2, type = 'response')

df['GLM_Pred_1'] = y_pred_1
df['GLM_Pred_2'] = y_pred_2

write.csv(df, 'EOW_GLM_PRED.csv')
##########################################
df_2 = df[c('SYM_TOT_COST', 'GLM_Pred_1', 'GLM_Pred_2')]

library(reshape2)
df_3 = melt(df_2)

#variable, value
ggplot(df_3) + geom_density(aes(x = value, colour = variable)) +
  ggtitle('Density Distribution of SYM_TOT_COST vs. Predicted Cost of GLM1 and GLM2')


ggplot(df_3) + geom_density(aes(x=value, colour=variable)) +
  xlim(0, 4000) +
  ggtitle('Density Distribution of SYM_TOT_COST vs. Predicted Cost of GLM1 and GLM2')

##
cor(df_2['SYM_TOT_COST'], df_2['GLM_Pred_1']) #0.639
cor(df_2['GLM_Pred_1'], df_2['GLM_Pred_2'])  #1.0

library(ggplot2)
par(mfrow=c(1,1))
#ggplot(df_2, aes('GLM_Pred_1', 'SYM_TOT_COST')) + geom_point()
ggplot(df_2, aes(x=GLM_Pred_1, y=SYM_TOT_COST)) + geom_point() +
  geom_smooth(method=lm) +
  ggtitle('Scatter plot of SYM_TOT_COST vs. Predicted Cost of GLM1')

#plot(x=df['GLM_Pred_1'], y=df['SYM_TOT_COST'])

ggplot(df_2, aes(x=GLM_Pred_1, y=SYM_TOT_COST)) + geom_point() +
  geom_smooth(method=lm) +
  ggtitle('Scatter plot of SYM_TOT_COST vs. Predicted Cost of GLM1') +
  coord_fixed(ratio=1)

ggplot(df_2, aes(x=GLM_Pred_1, y=SYM_TOT_COST)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = 'blue') +
  ggtitle('Scatter plot of SYM_TOT_COST vs. Predicted Cost of GLM1')+
  coord_fixed(ratio=1)


