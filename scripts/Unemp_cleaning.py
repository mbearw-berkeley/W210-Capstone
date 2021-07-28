#!/usr/bin/env python
# coding: utf-8

# In[19]:


import pandas as pd
import numpy as np

forecast = pd.read_csv("./data/RawData/ScrapedData/PRUNEMP.csv")

# 2009 Quarter 2 to 2013 Quarter 4, PRUNEMP
# Current Year Forecasts
f1 = forecast[(forecast["YEAR"] > 2008) & (forecast["YEAR"]<2014) & 
              ((forecast["YEAR"] != 2009) | (forecast["QUARTER"] != 1)) & 
              ((forecast["YEAR"] != 2014) | (forecast["QUARTER"] != 1))]
f1 = f1[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRUNEMP1", "PRUNEMP2", "PRUNEMP3", "PRUNEMP4", "PRUNEMP5", "PRUNEMP6",
         "PRUNEMP7", "PRUNEMP8", "PRUNEMP9", "PRUNEMP10"]]

f1.rename({"YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)
f1["YEAR BEING FORECAST"] = f1["YEAR FORECAST MADE"]

# 2009 Quarter 2 to 2013 Quarter 4, PRUNEMP
# Second Year Forecasts
f2 = forecast[(forecast["YEAR"] > 2008) & (forecast["YEAR"] < 2014) & 
              ((forecast["YEAR"] != 2009) | (forecast["QUARTER"] != 1)) & 
              ((forecast["YEAR"] != 2014) | (forecast["QUARTER"] != 1))]
f2 = f2[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRUNEMP11", "PRUNEMP12", "PRUNEMP13", "PRUNEMP14", "PRUNEMP15", "PRUNEMP16",
         "PRUNEMP17", "PRUNEMP18", "PRUNEMP19", "PRUNEMP20"]]
f2.rename({"PRUNEMP11":"PRUNEMP1", "PRUNEMP12":"PRUNEMP2", "PRUNEMP13":"PRUNEMP3", "PRUNEMP14":"PRUNEMP4",
           "PRUNEMP15":"PRUNEMP5", "PRUNEMP16":"PRUNEMP6", "PRUNEMP17":"PRUNEMP7", "PRUNEMP18":"PRUNEMP8",
           "PRUNEMP19":"PRUNEMP9", "PRUNEMP20":"PRUNEMP10", "YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)

f2["YEAR BEING FORECAST"] = f2["YEAR FORECAST MADE"] + 1

# 2009 Quarter 2 to 2013 Quarter 4, PRUNEMP
# Third Year Forecasts
f3 = forecast[(forecast["YEAR"] > 2008) & (forecast["YEAR"] < 2014) & 
              ((forecast["YEAR"] != 2009) | (forecast["QUARTER"] != 1)) & 
              ((forecast["YEAR"] != 2014) | (forecast["QUARTER"] != 1))]
f3 = f3[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRUNEMP21", "PRUNEMP22", "PRUNEMP23", "PRUNEMP24", "PRUNEMP25", "PRUNEMP26",
         "PRUNEMP27", "PRUNEMP28", "PRUNEMP29", "PRUNEMP30"]]

f3.rename({"PRUNEMP21":"PRUNEMP1", "PRUNEMP22":"PRUNEMP2", "PRUNEMP23":"PRUNEMP3", "PRUNEMP24":"PRUNEMP4",
           "PRUNEMP25":"PRUNEMP5", "PRUNEMP26":"PRUNEMP6", "PRUNEMP27":"PRUNEMP7", "PRUNEMP28":"PRUNEMP8",
           "PRUNEMP29":"PRUNEMP9", "PRUNEMP30":"PRUNEMP10", "YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)

f3["YEAR BEING FORECAST"] = f3["YEAR FORECAST MADE"] + 2

# 2009 Quarter 2 to 2013 Quarter 4, PRUNEMP
# Fourth Year Forecasts
f4 = forecast[(forecast["YEAR"] > 2008) & (forecast["YEAR"] < 2014) & 
              ((forecast["YEAR"] != 2009) | (forecast["QUARTER"] != 1)) & 
              ((forecast["YEAR"] != 2014) | (forecast["QUARTER"] != 1))]
f4 = f4[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRUNEMP31", "PRUNEMP32", "PRUNEMP33", "PRUNEMP34", "PRUNEMP35", "PRUNEMP36",
         "PRUNEMP37", "PRUNEMP38", "PRUNEMP39", "PRUNEMP40"]]

f4.rename({"PRUNEMP31":"PRUNEMP1", "PRUNEMP32":"PRUNEMP2", "PRUNEMP33":"PRUNEMP3", "PRUNEMP34":"PRUNEMP4",
           "PRUNEMP35":"PRUNEMP5", "PRUNEMP36":"PRUNEMP6", "PRUNEMP37":"PRUNEMP7", "PRUNEMP38":"PRUNEMP8",
           "PRUNEMP39":"PRUNEMP9", "PRUNEMP40":"PRUNEMP10", "YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)

f4["YEAR BEING FORECAST"] = f4["YEAR FORECAST MADE"] + 3

# Stack current, second, third, and fourth year forecasts
tablePRUNEMPa = pd.concat([f1,f2,f3,f4])

# Creating a unique ID that combines Year Being Forecasted, Forecaster ID, Year Forecast Made, and Quarter

tablePRUNEMPa["Year.ID.ForecastYear.Quarter"] = tablePRUNEMPa["YEAR BEING FORECAST"].astype(str) + "-" + tablePRUNEMPa[
    "ID"].astype(str) + "-" + tablePRUNEMPa["YEAR FORECAST MADE"].astype(str) + "-" + tablePRUNEMPa["QUARTER"].astype(str)

#Renaming the columns
tablePRUNEMPa.rename({"ID":"FORECASTER ID", "PRUNEMP1":"BIN 1", "PRUNEMP2":"BIN 2", "PRUNEMP3":"BIN 3", "PRUNEMP4":"BIN 4",
                      "PRUNEMP5":"BIN 5", "PRUNEMP6":"BIN 6", "PRUNEMP7":"BIN 7", "PRUNEMP8":"BIN 8", "PRUNEMP9":"BIN 9",
                      "PRUNEMP10":"BIN 10"}, axis=1, inplace=1)

#Creating a new column with the type of indicator

tablePRUNEMPa["INDICATOR"] = np.repeat("Unemployment", len(tablePRUNEMPa))

#Creating a new column with the TDIST
tablePRUNEMPa["TDIST"] = tablePRUNEMPa["YEAR BEING FORECAST"] + 1 - (
    tablePRUNEMPa["YEAR FORECAST MADE"] + tablePRUNEMPa["QUARTER"]/4)

# Reorganize table columns
cols = ["Year.ID.ForecastYear.Quarter", "YEAR FORECAST MADE", "YEAR BEING FORECAST", "QUARTER", "FORECASTER ID",
        "INDUSTRY", "BIN 1", "BIN 2", "BIN 3", "BIN 4", "BIN 5", "BIN 6", "BIN 7", "BIN 8", "BIN 9", "BIN 10",
               "INDICATOR", "TDIST"]
tablePRUNEMPa = tablePRUNEMPa[cols]

# 2014 Quarter 1 to Present (2016 Q4)
# Current Year Forecasts
f5 = forecast[(forecast["YEAR"] > 2013) & 
              ((forecast["YEAR"] != 2013) | (forecast["QUARTER"] > 4))]
f5 = f5[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRUNEMP1", "PRUNEMP2", "PRUNEMP3", "PRUNEMP4", "PRUNEMP5", "PRUNEMP6",
         "PRUNEMP7", "PRUNEMP8", "PRUNEMP9", "PRUNEMP10"]]

f5.rename({"YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)
f5["YEAR BEING FORECAST"] = f5["YEAR FORECAST MADE"]

# Second Year Forecasts
f6 = forecast[(forecast["YEAR"] > 2013) & 
              ((forecast["YEAR"] != 2013) | (forecast["QUARTER"] > 4))]
f6 = f6[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRUNEMP11", "PRUNEMP12", "PRUNEMP13", "PRUNEMP14", "PRUNEMP15", "PRUNEMP16",
         "PRUNEMP17", "PRUNEMP18", "PRUNEMP19", "PRUNEMP20"]]
f6.rename({"PRUNEMP11":"PRUNEMP1", "PRUNEMP12":"PRUNEMP2", "PRUNEMP13":"PRUNEMP3", "PRUNEMP14":"PRUNEMP4",
           "PRUNEMP15":"PRUNEMP5", "PRUNEMP16":"PRUNEMP6", "PRUNEMP17":"PRUNEMP7", "PRUNEMP18":"PRUNEMP8",
           "PRUNEMP19":"PRUNEMP9", "PRUNEMP20":"PRUNEMP10", "YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)

f6["YEAR BEING FORECAST"] = f6["YEAR FORECAST MADE"] + 1

# Third Year Forecasts
f7 = forecast[(forecast["YEAR"] > 2013) &
              ((forecast["YEAR"] != 2013) | (forecast["QUARTER"] > 4))]
f7 = f7[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRUNEMP21", "PRUNEMP22", "PRUNEMP23", "PRUNEMP24", "PRUNEMP25", "PRUNEMP26",
         "PRUNEMP27", "PRUNEMP28", "PRUNEMP29", "PRUNEMP30"]]

f7.rename({"PRUNEMP21":"PRUNEMP1", "PRUNEMP22":"PRUNEMP2", "PRUNEMP23":"PRUNEMP3", "PRUNEMP24":"PRUNEMP4",
           "PRUNEMP25":"PRUNEMP5", "PRUNEMP26":"PRUNEMP6", "PRUNEMP27":"PRUNEMP7", "PRUNEMP28":"PRUNEMP8",
           "PRUNEMP29":"PRUNEMP9", "PRUNEMP30":"PRUNEMP10", "YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)

f7["YEAR BEING FORECAST"] = f7["YEAR FORECAST MADE"] + 2

# Fourth Year Forecasts
f8 = forecast[(forecast["YEAR"] > 2013) &
              ((forecast["YEAR"] != 2013) | (forecast["QUARTER"] > 4))]
f8 = f8[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRUNEMP31", "PRUNEMP32", "PRUNEMP33", "PRUNEMP34", "PRUNEMP35", "PRUNEMP36",
         "PRUNEMP37", "PRUNEMP38", "PRUNEMP39", "PRUNEMP40"]]

f8.rename({"PRUNEMP31":"PRUNEMP1", "PRUNEMP32":"PRUNEMP2", "PRUNEMP33":"PRUNEMP3", "PRUNEMP34":"PRUNEMP4",
           "PRUNEMP35":"PRUNEMP5", "PRUNEMP36":"PRUNEMP6", "PRUNEMP37":"PRUNEMP7", "PRUNEMP38":"PRUNEMP8",
           "PRUNEMP39":"PRUNEMP9", "PRUNEMP40":"PRUNEMP10", "YEAR": "YEAR FORECAST MADE"}, axis=1, inplace=True)

f8["YEAR BEING FORECAST"] = f8["YEAR FORECAST MADE"] + 3


# Stack current, second, third, and fourth year forecasts
tablePRUNEMPb = pd.concat([f5,f6,f7,f8])

# Creating a unique ID that combines Year Being Forecasted, Forecaster ID, Year Forecast Made, and Quarter
tablePRUNEMPb["Year.ID.ForecastYear.Quarter"] = tablePRUNEMPb["YEAR BEING FORECAST"].astype(str) + "-" + tablePRUNEMPb[
    "ID"].astype(str) + "-" + tablePRUNEMPb["YEAR FORECAST MADE"].astype(str) + "-" + tablePRUNEMPb["QUARTER"].astype(str)

#Renaming the columns
tablePRUNEMPb.rename({"ID":"FORECASTER ID", "PRUNEMP1":"BIN 1", "PRUNEMP2":"BIN 2", "PRUNEMP3":"BIN 3", "PRUNEMP4":"BIN 4",
                      "PRUNEMP5":"BIN 5", "PRUNEMP6":"BIN 6", "PRUNEMP7":"BIN 7", "PRUNEMP8":"BIN 8", "PRUNEMP9":"BIN 9",
                      "PRUNEMP10":"BIN 10"}, axis=1, inplace=1)

#Creating a new column with the type of indicator

tablePRUNEMPb["INDICATOR"] = np.repeat("Unemployment", len(tablePRUNEMPb))

#Creating a new column with the TDIST
tablePRUNEMPb["TDIST"] = tablePRUNEMPb["YEAR BEING FORECAST"] + 1 - (
    tablePRUNEMPb["YEAR FORECAST MADE"] + tablePRUNEMPb["QUARTER"]/4)

# Reorganize table columns
tablePRUNEMPb = tablePRUNEMPb[cols]

PRUNEMP = pd.concat([tablePRUNEMPa, tablePRUNEMPb])

PRUNEMP.to_csv("./data/CleanData/Python/UNEMP.csv", index=False)
print("Completed cleaning Unemployment Data~")
