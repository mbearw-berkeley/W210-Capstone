
import pandas as pd
import numpy as np


forecast = pd.read_csv("./data/RawData/ScrapedData/PRCCPI.csv") 



# 2007 Quarter 1 to Present (2019 Quarter 2)
# Current Year Forecasts
f1 = forecast[forecast["YEAR"] > 2006]
f1 = f1[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRCCPI1", "PRCCPI2", "PRCCPI3", "PRCCPI4", "PRCCPI5", "PRCCPI6",
         "PRCCPI7", "PRCCPI8", "PRCCPI9", "PRCCPI10"]]

f1.rename({"YEAR":"YEAR FORECAST MADE"}, axis=1, inplace=True)
f1["YEAR BEING FORECAST"] = f1["YEAR FORECAST MADE"]

# 2007 Quarter 1 to Present (2019 Quarter 2)
# Second Year Forecasts
f2 = forecast[forecast["YEAR"] > 2006]
f2 = f2[["YEAR", "QUARTER", "ID", "INDUSTRY", "PRCCPI11", "PRCCPI12", "PRCCPI13", "PRCCPI14", "PRCCPI15", "PRCCPI16",
         "PRCCPI17", "PRCCPI18", "PRCCPI19", "PRCCPI20"]]

f2.rename({"PRCCPI11":"PRCCPI1", "PRCCPI12":"PRCCPI2", "PRCCPI13":"PRCCPI3", "PRCCPI14":"PRCCPI4", "PRCCPI15":"PRCCPI5",
           "PRCCPI16":"PRCCPI6", "PRCCPI17":"PRCCPI7", "PRCCPI18":"PRCCPI8", "PRCCPI19":"PRCCPI9", "PRCCPI20":"PRCCPI110",
           "YEAR":"YEAR FORECAST MADE"}, axis=1, inplace=True)

f2["YEAR BEING FORECAST"] = f2["YEAR FORECAST MADE"] + 1

# Stack current and second year forecasts

PRCCPI = pd.concat([f1, f2])

PRCCPI["Year.ID.ForecastYear.Quarter"] = PRCCPI["YEAR BEING FORECAST"].astype(str) + "-" + PRCCPI[
    "ID"].astype(str) + "-" + PRCCPI["YEAR FORECAST MADE"].astype(str) + "-" + PRCCPI["QUARTER"].astype(str)

PRCCPI.rename({"ID":"FORECASTER ID", "PRCCPI1":"BIN 1", "PRCCPI2":"BIN 2", "PRCCPI3":"BIN 3", "PRCCPI4":"BIN 4",
               "PRCCPI5":"BIN 5", "PRCCPI6":"BIN 6", "PRCCPI7":"BIN 7", "PRCCPI8":"BIN 8", "PRCCPI9":"BIN 9",
               "PRCCPI10":"BIN 10"}, axis=1, inplace=True)

PRCCPI["INDICATOR"] = np.repeat("Core CPI", len(PRCCPI))

PRCCPI["TDIST"] = PRCCPI["YEAR BEING FORECAST"] + 1 - (PRCCPI["YEAR FORECAST MADE"] + PRCCPI["QUARTER"]/4)

cols = ["Year.ID.ForecastYear.Quarter", "YEAR FORECAST MADE", "YEAR BEING FORECAST", "QUARTER", "FORECASTER ID",
        "INDUSTRY", "BIN 1", "BIN 2", "BIN 3", "BIN 4", "BIN 5", "BIN 6", "BIN 7", "BIN 8", "BIN 9", "BIN 10",
               "INDICATOR", "TDIST"]
PRCCPI = PRCCPI[cols]




PRCCPI.to_csv("./data/CleanData/Python/CCPI.csv", index=False)

print("Completed cleaning Core CPI Data~")





