#!/usr/bin/env python
# coding: utf-8

# In[5]:


import pandas as pd
import numpy as np

forecast = pd.read_csv("./data/RawData/ScrapedData/PRCPCE.csv")

# 2007 Quarter 1 to Present (2019 Quarter 2)
# Current Year Forecasts
f1 = forecast[forecast["YEAR"] > 2006]
f1 = f1[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRCPCE1", "PRCPCE2", "PRCPCE3", "PRCPCE4", "PRCPCE5", "PRCPCE6", "PRCPCE7",
         "PRCPCE8", "PRCPCE9", "PRCPCE10"]]

f1.rename({"YEAR":"YEAR FORECAST MADE"}, axis=1, inplace=True)

f1["YEAR BEING FORECAST"] = f1["YEAR FORECAST MADE"]

# 2007 Quarter 1 to Present (2019 Quarter 2)
# Second Year Forecasts
f2 = forecast[forecast["YEAR"] > 2006]
f2 = f2[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRCPCE11", "PRCPCE12", "PRCPCE13", "PRCPCE14", "PRCPCE15", "PRCPCE16",
         "PRCPCE17", "PRCPCE18", "PRCPCE19", "PRCPCE20"]]

f2.rename({"PRCPCE11":"PRCPCE1", "PRCPCE12":"PRCPCE2", "PRCPCE13":"PRCPCE3", "PRCPCE14":"PRCPCE4", "PRCPCE15":"PRCPCE5",
           "PRCPCE16":"PRCPCE6", "PRCPCE17":"PRCPCE7", "PRCPCE18":"PRCPCE8", "PRCPCE19":"PRCPCE9",
           "PRCPCE20":"PRCPCE10", "YEAR":"YEAR FORECAST MADE"}, axis=1, inplace=True)

f2["YEAR BEING FORECAST"] = f2["YEAR FORECAST MADE"] + 1

# Stack current and second year forecasts

PRCPCE = pd.concat([f1, f2])

PRCPCE["Year.ID.ForecastYear.Quarter"] = PRCPCE["YEAR BEING FORECAST"].astype(str) + "-" + PRCPCE[
    "ID"].astype(str) + "-" + PRCPCE["YEAR FORECAST MADE"].astype(str) + "-" + PRCPCE["QUARTER"].astype(str)

PRCPCE.rename({"ID":"FORECASTER ID", "PRCPCE1":"BIN 1", "PRCPCE2":"BIN 2", "PRCPCE3":"BIN 3", "PRCPCE4":"BIN 4",
               "PRCPCE5":"BIN 5", "PRCPCE6":"BIN 6", "PRCPCE7":"BIN 7", "PRCPCE8":"BIN 8", "PRCPCE9":"BIN 9",
               "PRCPCE10":"BIN 10"}, axis=1, inplace=True)

PRCPCE["INDICATOR"] = np.repeat("Core PCE", len(PRCPCE))

PRCPCE["TDIST"] = PRCPCE["YEAR BEING FORECAST"] + 1 - (PRCPCE["YEAR FORECAST MADE"] + PRCPCE["QUARTER"]/4)

cols = ["Year.ID.ForecastYear.Quarter", "YEAR FORECAST MADE", "YEAR BEING FORECAST", "QUARTER", "FORECASTER ID",
        "INDUSTRY", "BIN 1", "BIN 2", "BIN 3", "BIN 4", "BIN 5", "BIN 6", "BIN 7", "BIN 8", "BIN 9", "BIN 10",
               "INDICATOR", "TDIST"]
PRCPCE = PRCPCE[cols]

PRCPCE.to_csv("./data/CleanData/Python/CPCE.csv", index=False)

print("Completed cleaning Core PCE Data~")