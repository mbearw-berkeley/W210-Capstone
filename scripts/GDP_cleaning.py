#!/usr/bin/env python
# coding: utf-8

# In[35]:


import numpy as np
import pandas as pd

forecast = pd.read_csv("./data/RawData/ScrapedData/PRGDP.csv")

# 1992 Quarter 1 to 2009 Quarter 1, Real GDP
# Current Year Forecasts
f6 = forecast[(forecast["YEAR"] > 1991) & (forecast["YEAR"]<2010) & 
              ((forecast["YEAR"] != 2009) | (forecast["QUARTER"] == 1))]
f6 = f6[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRGDP1", "PRGDP2", "PRGDP3", "PRGDP4", "PRGDP5",
         "PRGDP6", "PRGDP7", "PRGDP8", "PRGDP9", "PRGDP10"]]

f6.rename({"YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)
f6["YEAR BEING FORECAST"] = f6["YEAR FORECAST MADE"]

# 1992 Quarter 1 to 2009 Quarter 1, Real GDP
# Second Year Forecasts
f7 = forecast[(forecast["YEAR"] > 1991) & (forecast["YEAR"]<2010) & 
              ((forecast["YEAR"] != 2009) | (forecast["QUARTER"] == 1))]
f7 = f7[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRGDP11", "PRGDP12", "PRGDP13", "PRGDP14", "PRGDP15",
         "PRGDP16", "PRGDP17", "PRGDP18", "PRGDP19", "PRGDP20"]]

f7.rename({"PRGDP11":"PRGDP1", "PRGDP12":"PRGDP2", "PRGDP13":"PRGDP3", "PRGDP14":"PRGDP4", "PRGDP15":"PRGDP5", 
           "PRGDP16":"PRGDP6", "PRGDP17":"PRGDP7", "PRGDP18":"PRGDP8", "PRGDP19":"PRGDP9", "PRGDP20":"PRGDP10",
           "YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)
f7["YEAR BEING FORECAST"] = f7["YEAR FORECAST MADE"] + 1

#stack current and second year forecasts
tableRGDPa = pd.concat([f6, f7])

tableRGDPa["Year.ID.ForecastYear.Quarter"] = tableRGDPa["YEAR BEING FORECAST"].astype(str) + "-" + tableRGDPa[
    "ID"].astype(str) + "-" + tableRGDPa["YEAR FORECAST MADE"].astype(str) + "-" + tableRGDPa["QUARTER"].astype(str)

tableRGDPa.rename({"ID":"FORECASTER ID", "PRGDP1":"BIN 1", "PRGDP2":"BIN 2", "PRGDP3":"BIN 3","PRGDP4":"BIN 4",
           "PRGDP5":"BIN 5", "PRGDP6":"BIN 6", "PRGDP7":"BIN 7", "PRGDP8":"BIN 8","PRGDP9":"BIN 9",
           "PRGDP10":"BIN 10"}, axis=1, inplace=True)

tableRGDPa["INDICATOR"] = np.repeat("RealGDP", len(tableRGDPa))

tableRGDPa["TDIST"] = tableRGDPa["YEAR BEING FORECAST"] + 1 - (tableRGDPa["YEAR FORECAST MADE"] + tableRGDPa["QUARTER"]/4)

tableRGDPa["BIN 11"] = np.nan
tableRGDPa["BIN 12"] = np.nan
tableRGDPa["BIN 13"] = np.nan
tableRGDPa["BIN 14"] = np.nan
tableRGDPa["BIN 15"] = np.nan

cols = ['Year.ID.ForecastYear.Quarter', 'YEAR FORECAST MADE', 'YEAR BEING FORECAST', 'QUARTER', 'FORECASTER ID',
        'INDUSTRY', 'BIN 1',
       'BIN 2', 'BIN 3', 'BIN 4', 'BIN 5', 'BIN 6', 'BIN 7', 'BIN 8', 'BIN 9',
       'BIN 10', 'BIN 11', 'BIN 12', 'BIN 13', 'BIN 14', 'BIN 15', 'INDICATOR', 'TDIST',]

tableRGDPa = tableRGDPa[cols]

# 2009 Quarter 2 to Present (2019 Quarter 2)
# Current Year Forecasts
f8 = forecast[(forecast["YEAR"] > 2008) & 
              ((forecast["YEAR"] != 2009) | (forecast["QUARTER"] > 1))]
f8 = f8[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRGDP1", "PRGDP2", "PRGDP3", "PRGDP4", "PRGDP5",
         "PRGDP6", "PRGDP7", "PRGDP8", "PRGDP9", "PRGDP10", "PRGDP11"]]

f8.rename({"YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)
f8["YEAR BEING FORECAST"] = f8["YEAR FORECAST MADE"]

# 2009 Quarter 2 to Present (2019 Quarter 2)
# Second Year Forecasts
f9 = forecast[(forecast["YEAR"] > 2008) & 
              ((forecast["YEAR"] != 2009) | (forecast["QUARTER"] > 1))]
f9 = f9[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRGDP12", "PRGDP13", "PRGDP14", "PRGDP15",
         "PRGDP16", "PRGDP17", "PRGDP18", "PRGDP19", "PRGDP20", "PRGDP21", "PRGDP22"]]

f9.rename({"PRGDP12":"PRGDP1", "PRGDP13":"PRGDP2", "PRGDP14":"PRGDP3", "PRGDP15":"PRGDP4", "PRGDP16":"PRGDP5", 
           "PRGDP17":"PRGDP6", "PRGDP18":"PRGDP7", "PRGDP19":"PRGDP8", "PRGDP20":"PRGDP9", "PRGDP21":"PRGDP10",
           "PRGDP22":"PRGDP11", "YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)
f9["YEAR BEING FORECAST"] = f9["YEAR FORECAST MADE"] + 1

# 2009 Quarter 2 to Present (2019 Quarter 2)
# Third Year Forecasts
f10 = forecast[(forecast["YEAR"] > 2008) & 
              ((forecast["YEAR"] != 2009) | (forecast["QUARTER"] > 1))]
f10 = f10[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRGDP23", "PRGDP24", "PRGDP25", "PRGDP26", "PRGDP27", "PRGDP28", "PRGDP29",
     "PRGDP30", "PRGDP31", "PRGDP32", "PRGDP33"]]

f10.rename({"PRGDP23":"PRGDP1", "PRGDP24":"PRGDP2", "PRGDP25":"PRGDP3", "PRGDP26":"PRGDP4", "PRGDP27":"PRGDP5", 
           "PRGDP28":"PRGDP6", "PRGDP29":"PRGDP7", "PRGDP30":"PRGDP8", "PRGDP31":"PRGDP9", "PRGDP32":"PRGDP10",
           "PRGDP33":"PRGDP11", "YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)

f10["YEAR BEING FORECAST"] = f10["YEAR FORECAST MADE"] + 2

# 2009 Quarter 2 to Present (2019 Quarter 2)
# Fourth Year Forecasts
f11 = forecast[(forecast["YEAR"] > 2008) & 
              ((forecast["YEAR"] != 2009) | (forecast["QUARTER"] > 1))]
f11 = f11[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRGDP34", "PRGDP35", "PRGDP36", "PRGDP37", "PRGDP38", "PRGDP39", "PRGDP40",
     "PRGDP41", "PRGDP42", "PRGDP43", "PRGDP44"]]

f11.rename({"PRGDP34":"PRGDP1", "PRGDP35":"PRGDP2", "PRGDP36":"PRGDP3", "PRGDP37":"PRGDP4", "PRGDP38":"PRGDP5", 
           "PRGDP39":"PRGDP6", "PRGDP40":"PRGDP7", "PRGDP41":"PRGDP8", "PRGDP42":"PRGDP9", "PRGDP43":"PRGDP10",
           "PRGDP44":"PRGDP11", "YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)

f11["YEAR BEING FORECAST"] = f11["YEAR FORECAST MADE"] + 3

# Stack current, second, third, and fourth year forecasts
tableRGDPb = pd.concat([f8, f9, f10, f11])
tableRGDPb["Year.ID.ForecastYear.Quarter"] = tableRGDPb["YEAR BEING FORECAST"].astype(str) + "-" + tableRGDPb[
    "ID"].astype(str) + "-" + tableRGDPb["YEAR FORECAST MADE"].astype(str) + "-" + tableRGDPb["QUARTER"].astype(str)

tableRGDPb.rename({"ID":"FORECASTER ID", "PRGDP1":"BIN 1", "PRGDP2":"BIN 2", "PRGDP3":"BIN 3","PRGDP4":"BIN 4",
           "PRGDP5":"BIN 5", "PRGDP6":"BIN 6", "PRGDP7":"BIN 7", "PRGDP8":"BIN 8","PRGDP9":"BIN 9",
           "PRGDP10":"BIN 10", "PRGDP11":"BIN 11"}, axis=1, inplace=True)

tableRGDPb["INDICATOR"] = np.repeat("RealGDP", len(tableRGDPb))

tableRGDPb["TDIST"] = tableRGDPb["YEAR BEING FORECAST"] + 1 - (tableRGDPb["YEAR FORECAST MADE"] + tableRGDPb["QUARTER"]/4)

tableRGDPb["BIN 12"] = np.nan
tableRGDPb["BIN 13"] = np.nan
tableRGDPb["BIN 14"] = np.nan
tableRGDPb["BIN 15"] = np.nan

tableRGDPb = tableRGDPb[cols]

tableRGDP = pd.concat([tableRGDPa, tableRGDPb])

tableRGDP.to_csv("./data/CleanData/Python/GDP.csv", index=False)

print("Completed cleaning GDP Data~")