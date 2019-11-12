# -*- coding: utf-8 -*-
"""
Created on Fri Nov  1 11:12:14 2019

@author: LUY1
"""

#------------------------------------#
#EOW Stage 3
#clean data

import pandas as pd

df = pd.read_csv('EOW_MODEL_DATA_20191010_2.csv')

df = df.loc[:,['SYM_TOT_COST', 'ACTUAL_PERIL', 'ASBESTOS_PRESENT','ALT_ACCOMDNTN_IND', 'NLP_EOW_REASON', 'MAX_FLOOR_SQM_DEC',
        'MAX_CEILING_SQM_DEC', 'MAX_WALLS_SQM_DEC', 'MAX_FLOOR_SQM', 'MAX_CEILING_SQM', 'MAX_WALLS_SQM', 'MAX_ROOM_CNT', 
        'ANY_DRYING_IND', 'LOSS_YEAR', 'LOSS_MONTH', 'NPD_PROP_TYPE', 'NPD_NO_BEDROOMS', 'NPD_NO_BATHROOMS', 'NPD_YOC',
        'UK_REGION', 'CUSTOMER_SEGMENT']]

#exclude obs with SYM_TOT_COST <= 0
df = df.loc[df['SYM_TOT_COST']>0, :]

#Nan, OTHER -> UKN
df.loc[(df['ACTUAL_PERIL'] == 'OTHER') | (df['ACTUAL_PERIL'].isnull()), 'ACTUAL_PERIL'] = 'UKN'

#NaN -> UKN
df.loc[df['NLP_EOW_REASON'].isnull(), 'NLP_EOW_REASON'] = 'UKN'

#NaN -> UKN
df.loc[df['NPD_PROP_TYPE'].isnull(), 'NPD_PROP_TYPE'] = 'UKN'

#NaN -> 3
df.loc[df['NPD_NO_BEDROOMS'].isnull(), 'NPD_NO_BEDROOMS'] = 3

#NaN -> 1
df.loc[df['NPD_NO_BATHROOMS'].isnull(), 'NPD_NO_BATHROOMS'] = 1

#NaN -> 1935
df.loc[df['NPD_YOC'].isnull(), 'NPD_YOC'] = 1935

#NaN -> UKN
df.loc[df['UK_REGION'].isnull(), 'UK_REGION'] = 'UKN'

#NaN -> UKN
df.loc[df['CUSTOMER_SEGMENT'].isnull(), 'CUSTOMER_SEGMENT'] = 'UKN'


#-------------------------------------------------------
#xgboost model
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

X = df.iloc[:, 1:]
y = df['SYM_TOT_COST']

df_cats = df.select_dtypes(include = ['object'])
df_nums = X.select_dtypes(exclude = ['object'])

df_cats_ohe = pd.get_dummies(df_cats)

X_ohe = pd.concat([df_cats_ohe, df_nums], axis = 1)

from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(X_ohe, y, test_size = 0.2, random_state = 2019)

import xgboost as xgb
from xgboost import XGBRegressor

mod_xgb = xgb.XGBRegressor(objective = 'reg:squarederror',
                           booster = 'gbtree',
                           n_estimators = 200,
                           colsample_bytree = 0.8,
                           learning_rate = 0.1,
                           max_depth = 2,
                           subsample = 0.5,
                           random_state = 2019)

eval_set = [(X_train, y_train), (X_test, y_test)]

mod_xgb.fit(X_train, y_train,
            eval_metric = 'rmse',
            eval_set = eval_set,
            early_stopping_rounds = 20)

from datetime import datetime

now = datetime.now()

from joblib import dump

dump(mod_xgb, 'xgb_satge_3_'+str(now)[:10]+'.pkl')

