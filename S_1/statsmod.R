############################
# build severity models
# model1: SYM_TOT_COST ~ ACTUAL_PERIL+EOW_REASON+ASBESTOS_PRESENT+
#                        ASBESTOS_RESULT+ALT_ACCOMDNTN_IND+NLP_EOW_REASON+
#                        MAX_FLOOR_SQM_DEC+MAX_CEILING_SQM_DEC+MAX_WALLS_SQM_DEC+
#                        MAX_FLOOR_SQM+MAX_CEILING_SQM+MAX_WALLS_SQM+
#                        MAX_WALLS_ROOM_CNT+ANY_DRYING_IND
#
# response: SYM_TOT_COST
# independent varivales: 1. numerical: 
#                        2. factor  
#
#
#model2:
#
#
#
#
#model3:
#
#
#
##########################

library('tidyverse')

#df = read_csv('EOW_MODEL_DATA_20190719.csv')
#df_2 = read.csv('EOW_MODEL_DATA_20190719.csv', stringsAsFactors = TRUE,
#                na.strings = c("", "NA"))

df_2 = read.csv('EOW_MODEL_DATA_20190719.csv',
                na.strings = c("", "NA"))

str(df_2)

head(df)
names(df)
str(df)

df_2['SYM_TOT_COST']


str(df_2)
nrow(df_2)

# response: SYM_TIT_COST
summary(df_2['SYM_TOT_COST'])

ggplot(df_2, aes(x = SYM_TOT_COST)) + geom_histogram()
ggplot(df_2) + geom_histogram(aes(x = SYM_TOT_COST, y = ..density..)) +
  geom_density(aes(x = SYM_TOT_COST, y = ..density..))

ggplot(df_2, aes(x = '', y = SYM_TOT_COST)) + geom_violin()

sum(df_2['SYM_TOT_COST'] == 0)
sum(df_2['SYM_TOT_COST'] < 0)
sum(df_2['SYM_TOT_COST'] <= 0)

sum(df_2['SYM_TOT_COST'] >= 50000)
sum(df_2['SYM_TOT_COST'] >= 50000)/nrow(df_2)

df_2[df_2$SYM_TOT_COST <= 0, 'SYM_TOT_COST']
df_2[df_2$SYM_TOT_COST <= 0, ]

#exclude 9 obs with df_2['SYM_TOT_COST']<=0
df_3 = df_2[df_2$SYM_TOT_COST > 0,]

sum(is.na(df_3['SYM_TOT_COST']))

#missing values by columns
names(df_3)

#columns with unique ids
#CLAIMID, ASPREA_CLAIMNO, AVIVA_CLAIMNO_ADJ

#columns with numerical
#SYM_MTL_COST, SYM_LBR_COST, SYM_LBR_HRS, SYM_EQP_COST, SYM_MKT_COST, SYM_TOT_COST
#AA_COST, MAX_FLOOR_SQM, MAX_CEILING_SQM, MAX_WALLS_SQM

#levels for factors
#1 level: EWNF
unique(df_3['EDW_CLAIM_TYPE'])   

#3 levels: 2016, 2017, 2018
unique(df_3['LOSS_YEAR'])

#12 levels
unique(df_3['LOSS_MONTH'])

#48 levels
unique(df_3['ACTUAL_PERIL'])
nrow(unique(df_3['ACTUAL_PERIL']))

#27 levels
unique(df_3['EOW_REASON'])
nrow(unique(df_3['EOW_REASON']))


#3 levels
unique(df_3['ASBESTOS_PRESENT'])
nrow(unique(df_3['ASBESTOS_PRESENT']))


#4 levels
unique(df_3['ASBESTOS_RESULT'])
nrow(unique(df_3['ASBESTOS_RESULT']))


#2 levels
unique(df_3['ALT_ACCOMDNTN_IND'])


#32 levels
unique(df_3['NLP_EOW_REASON'])
nrow(unique(df_3['NLP_EOW_REASON']))

#11 level
unique(df_3['MAX_FLOOR_SQM_DEC'])
nrow(unique(df_3['MAX_FLOOR_SQM_DEC']))

#11 level
unique(df_3['MAX_CEILING_SQM_DEC'])
nrow(unique(df_3['MAX_CEILING_SQM_DEC']))


#11 levels
unique(df_3['MAX_WALLS_SQM_DEC'])
nrow(unique(df_3['MAX_WALLS_SQM_DEC']))

#11 level
unique(df_3['MAX_ROOM_CNT'])
nrow(unique(df_3['MAX_ROOM_CNT']))

#2 levels
unique(df_3['ANY_DRYING_IND'])

#13 levels
unique(df_3['NPD_PROP_TYPE'])
nrow(unique(df_3['NPD_PROP_TYPE']))

#11 level
unique(df_3['NPD_NO_BEDROOMS'])
nrow(unique(df_3['NPD_NO_BEDROOMS']))

#6 levels
unique(df_3['NPD_NO_BATHROOMS'])
nrow(unique(df_3['NPD_NO_BATHROOMS']))


#17 levels
unique(df_3['NPD_YOC'])
nrow(unique(df_3['NPD_YOC']))


#3 levels
unique(df_3['NPD_TENURE_FLAG'])
nrow(unique(df_3['NPD_TENURE_FLAG']))


#15 levels
unique(df_3['UK_REGION'])
nrow(unique(df_3['UK_REGION']))

#8 levels
unique(df_3['CUSTOMER_SEGMENT'])
nrow(unique(df_3['CUSTOMER_SEGMENT']))

#restructure datafarme with response: SYM_TOT_COST & 13 independent variables:
df_4 = df_3[c('SYM_TOT_COST', 'ACTUAL_PERIL', 'ASBESTOS_PRESENT',
             'ASBESTOS_RESULT', 'ALT_ACCOMDNTN_IND', 'NLP_EOW_REASON',
             'MAX_FLOOR_SQM_DEC', 'MAX_CEILING_SQM_DEC', 'MAX_WALLS_SQM_DEC',
             'MAX_FLOOR_SQM', 'MAX_CEILING_SQM', 'MAX_WALLS_SQM',
             'MAX_ROOM_CNT', 'ANY_DRYING_IND')]
#38791
nrow(df_4)

str(df_4)

#check missing values
sum(is.na(df_4['SYM_TOT_COST']))

#9917 25.5%
sum(is.na(df_4['ACTUAL_PERIL']))/nrow(df_4)

df_4['ACTUAL_PERIL'] = lapply(df_4['ACTUAL_PERIL'], as.character)

#df_4['ACTUAL_PERIL'] = as.character(df_4['ACTUAL_PERIL'])
str(df_4['ACTUAL_PERIL'])
#typeof(df_4['ACTUAL_PERIL'])
#class(df_4['ACTUAL_PERIL'])
#levels(df_4['ACTUAL_PERIL'])

df_4[is.na(df_4['ACTUAL_PERIL']), 'ACTUAL_PERIL'] = 'UKN'
df_4['ACTUAL_PERIL']
unique(df_4['ACTUAL_PERIL'])

df_4['ACTUAL_PERIL'] = lapply(df_4['ACTUAL_PERIL'], as.factor)
str(df_4['ACTUAL_PERIL'])

df_4['ACTUAL_PERIL']
str(df_4['ACTUAL_PERIL'])

#25833 66.5%
sum(is.na(df_4['ASBESTOS_PRESENT']))/nrow(df_4)

df_4['ASBESTOS_PRESENT'] = lapply(df_4['ASBESTOS_PRESENT'], as.character)
df_4[is.na(df_4['ASBESTOS_PRESENT']), 'ASBESTOS_PRESENT'] = 'UKN'
df_4['ASBESTOS_PRESENT'] = lapply(df_4['ASBESTOS_PRESENT'], as.factor)

#92.6%
sum(is.na(df_4['ASBESTOS_RESULT']))/nrow(df_4)

df_4['ASBESTOS_RESULT'] = lapply(df_4['ASBESTOS_RESULT'], as.character)
df_4[is.na(df_4['ASBESTOS_RESULT']), 'ASBESTOS_RESULT'] = 'UKN'
df_4['ASBESTOS_RESULT'] = lapply(df_4['ASBESTOS_RESULT'], as.factor)

#
sum(is.na(df_4['ALT_ACCOMDNTN_IND']))/nrow(df_4)


#54.1%
sum(is.na(df_4['NLP_EOW_REASON']))/nrow(df_4)

df_4['NLP_EOW_REASON'] = lapply(df_4['NLP_EOW_REASON'], as.character)
df_4[is.na(df_4['NLP_EOW_REASON']), 'NLP_EOW_REASON'] = 'UKN'
df_4['NLP_EOW_REASON'] = lapply(df_4['NLP_EOW_REASON'], as.factor)

#
sum(is.na(df_4['MAX_FLOOR_SQM_DEC']))/nrow(df_4)

#
sum(is.na(df_4['MAX_CEILING_SQM_DEC']))/nrow(df_4)

#
sum(is.na(df_4['MAX_WALLS_SQM_DEC']))/nrow(df_4)

#
sum(is.na(df_4['MAX_FLOOR_SQM']))/nrow(df_4)

#
sum(is.na(df_4['MAX_CEILING_SQM']))/nrow(df_4)

#
sum(is.na(df_4['MAX_WALLS_SQM']))/nrow(df_4)

#
sum(is.na(df_4['MAX_ROOM_CNT']))/nrow(df_4)

#
sum(is.na(df_4['ANY_DRYING_IND']))/nrow(df_4)

str(df_4)
#sum(is.na(df_4))
#sum(is.na(df_3))

#int -> factor
#ALT_ACCOMDNTN_IND, MAX_FLOOR_SQM_DEC, MAX_CEILING_SQM_DEC, MAX_WALLS_SQM_DEC
#MAX_ROOM_CNT, ANY_DRYING_IND

df_4['ALT_ACCOMDNTN_IND'] = lapply(df_4['ALT_ACCOMDNTN_IND'], as.factor)
df_4['MAX_FLOOR_SQM_DEC'] = lapply(df_4['MAX_FLOOR_SQM_DEC'], as.factor)
df_4['MAX_CEILING_SQM_DEC'] = lapply(df_4['MAX_CEILING_SQM_DEC'], as.factor)
df_4['MAX_WALLS_SQM_DEC'] = lapply(df_4['MAX_WALLS_SQM_DEC'], as.factor)
df_4['MAX_ROOM_CNT'] = lapply(df_4['MAX_ROOM_CNT'], as.factor)
df_4['ANY_DRYING_IND'] = lapply(df_4['ANY_DRYING_IND'], as.factor)

str(df_4)

write.csv(df_4, file = 'df_mod1.csv')

mod1 = glm(SYM_TOT_COST ~., data = df_4, family = Gamma(link = 'log'))
summary(mod1)

mod2 = glm(SYM_TOT_COST ~.-1, data = df_4, family = Gamma(link = 'log'))
summary(mod2)

anova(mod1, mod2, test = 'F')
anova(mod1, mod2, test = 'Chisq')
anova(mod1, mod2, test = 'LRT')

par(mfrow = c(2,2))
plot(mod2)

par(mfrow = c(2,2))
plot(mod1)

#df_4[,-1]
df_4['y_mod2'] = predict(mod2, df_4[,-1], type = 'response')

ggplot(df_4, aes(SYM_TOT_COST,y_mod2))+geom_point()+
  geom_smooth(method = 'lm')

ggplot(df_4, aes(SYM_TOT_COST, y_mod2))+geom_point()+
  geom_abline(intercept = 0, slope = 1, color = 'red')

install.packages('margins')
library('margins')

m = margins(mod2)

