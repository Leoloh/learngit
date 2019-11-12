#############################
# data cleaning
# data structure 
#
#############################

library(tidyverse)

df = read.csv('EOW_MODEL_DATA_20191010.csv', na.strings = c("", "N/A"))
write.csv(df, 'EOW_MODEL_DATA_20191010_2.csv')
#df[df['UK_REGION']=='Midlands',]
#df[df['ASBESTOS_PRESENT']=='YES',]

df[df['NLP_EOW_REASON']=='Category_Overflow-Pipe',]
print(df['NLP_EOW_REASON'])
print(table(df['NLP_EOW_REASON']))

str_trim(df['NLP_EOW_REASON'], side = 'both')

summary(df)
#names(df)

names(df)[1] = 'CLAIMID'
names(df)

#38800
str(df)
dim(df)

#clean dataset 
sum(is.na(df['LOSS_YEAR']))
unique(df['LOSS_YEAR'])

sum(is.na(df['LOSS_MONTH']))
unique(df['LOSS_MONTH'])

sum(is.na(df['EDW_CLAIM_TYPE']))
unique(df['EDW_CLAIM_TYPE'])

unique(df['ACTUAL_PERIL'])
sum(is.na(df['ACTUAL_PERIL']))

#9951
length(df[is.na(df['ACTUAL_PERIL']), 'ACTUAL_PERIL'])

df[df['ACTUAL_PERIL'] == 'OTHER',]

#1
df[df['ACTUAL_PERIL']=='WEIGHT OF SNOW']

length(df[df['ACTUAL_PERIL']=='SUBSIDENCE', 'ACTUAL_PERIL'])

sum(is.na(df['ACTUAL_PERIL']))
summary(df['ACTUAL_PERIL'], maxsum = 50)
table(df['ACTUAL_PERIL'])

dim(unique(df['ACTUAL_PERIL']))

#####################################################
df = read.csv('EOW_MODEL_DATA_20191010_2.csv', stringsAsFactors = FALSE)

df = df[,c('SYM_TOT_COST', 'ACTUAL_PERIL', 'ASBESTOS_PRESENT',
             'ALT_ACCOMDNTN_IND', 'NLP_EOW_REASON',
             'MAX_FLOOR_SQM_DEC', 'MAX_CEILING_SQM_DEC', 'MAX_WALLS_SQM_DEC',
             'MAX_FLOOR_SQM', 'MAX_CEILING_SQM', 'MAX_WALLS_SQM',
             'MAX_ROOM_CNT', 'ANY_DRYING_IND')]


str(df)
names(df)
sum(is.na(df))
summary(df)

########################################################################
table(df['EDW_CLAIM_TYPE'])

table(df['ACTUAL_PERIL'])
#df['ACTUAL_PERIL'] = str_trim(df['ACTUAL_PERIL'], side = 'left')

df[df['ACTUAL_PERIL'] == 'OTHER', 'ACTUAL_PERIL']
tail(df['ACTUAL_PERIL'])
head(df['ACTUAL_PERIL'])

df[df['ACTUAL_PERIL'] == 'EOW (NON WEATHER) - FROM WATER INSTALLATIONS', 'ACTUAL_PERIL']
df[is.na(df['ACTUAL_PERIL']), 'ACTUAL_PERIL'] = 'UNK'
df[df['ACTUAL_PERIL'] == 'OTHER', 'ACTUAL_PERIL'] = 'UNK'

table(df['ASBESTOS_PRESENT'])
sum(is.na(df['ASBESTOS_PRESENT']))

table(df['ALT_ACCOMDNTN_IND'])
sum(is.na(df['ALT_ACCOMDNTN_IND']))

table(df['NLP_EOW_REASON'])
#summary(df['NLP_EOW_REASON'])
sum(is.na(df['NLP_EOW_REASON']))
unique(df['NLP_EOW_REASON'])

df[is.na(df['NLP_EOW_REASON']),'NLP_EOW_REASON']
df[is.na(df['NLP_EOW_REASON']),'NLP_EOW_REASON'] = 'UNK'

######################################################################
######################################################################

#38791 obs, 13 cols
dim(df[df['SYM_TOT_COST'] > 0,])
df = df[df['SYM_TOT_COST'] > 0,]


## clean missing values
## ACTUAL_PERIL: categorical
sum(is.na(df['ACTUAL_PERIL']))
table(df['ACTUAL_PERIL'])

df[is.na(df['ACTUAL_PERIL']), 'ACTUAL_PERIL'] = 'UNK'
df[df['ACTUAL_PERIL'] == 'OTHER', 'ACTUAL_PERIL'] = 'UNK'

##ASBESTOS_PRESENT: categorical: NO, YES
sum(is.na(df['ASBESTOS_PRESENT']))
table(df['ASBESTOS_PRESENT'])

##ALT_ACCOMDNTN_IND: numerical: 0, 1
sum(is.na(df['ALT_ACCOMDNTN_IND']))
table(df['ALT_ACCOMDNTN_IND'])

##NLP_EOW_REASON: categorical
sum(is.na(df['NLP_EOW_REASON']))
table(df['NLP_EOW_REASON'])

df[is.na(df['NLP_EOW_REASON']),'NLP_EOW_REASON'] = 'UNK' 

##MAX_FLOOR_SQM_DEC: numerical: 0-10
sum(is.na(df['MAX_FLOOR_SQM_DEC']))
table(df['MAX_FLOOR_SQM_DEC'])

##MAX_CEILING_SQM_DEC: numerical: 0-10
sum(is.na(df['MAX_CEILING_SQM_DEC']))
table(df['MAX_CEILING_SQM_DEC'])

##MAX_WALLS_SQM_DEC: numerical: 0-10
sum(is.na(df['MAX_WALLS_SQM_DEC']))
table(df['MAX_WALLS_SQM_DEC'])

##MAX_FLOOR_SQM: numerical
sum(is.na(df['MAX_FLOOR_SQM']))

##MAX_CEILING_SQM: numerical
sum(is.na(df['MAX_CEILING_SQM']))

##MAX_WALLS_SQM: numerical
sum(is.na(df['MAX_WALLS_SQM']))

##MAX_ROOM_CNT: numerical 0-10
sum(is.na(df['MAX_ROOM_CNT']))
table(df['MAX_ROOM_CNT'])

##ANY_DRYING_IND: numerical: 0, 1
sum(is.na(df['ANY_DRYING_IND']))
table(df['ANY_DRYING_IND'])

sum(is.na(df))
write.csv(df, 'EOW_MODEL_DATA_CLEANED.csv')

######################################################
######################################################





