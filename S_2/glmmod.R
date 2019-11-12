###### fit glm models ######
library(tidyverse)

df = read.csv('EOW_MODEL_DATA_CLEANED_STAGE2.csv')

#38791, 20
dim(df)

str(df)
sum(is.na(df))

mod_1 = glm(SYM_TOT_COST~., data = df, family = Gamma(link='log'))
summary(mod_1)















