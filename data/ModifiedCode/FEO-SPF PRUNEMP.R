#######################################################
#
#   R analysis code for the FEO-SPF project
#   analyzing data from the Survey of Professional Forecasters
#   Probability of Civilian Unemployment Rate (PRUNEMP)
#   Sandy Campbell 
#   September 18, 2019
#
#######################################################

#library(DataComputing)
library(dplyr)
library(readxl)
library(foreign)

# Reading original data and actuals into the R code
# The file we are using was downloaded from the SPF website on July 4th 2019.
forecast <- read_excel("/Users/Isaac/Downloads/Individual_PRUNEMP.xlsx") 
# actuals <- read_excel("Actuals PRUNEMP_08182019.xlsx", sheet = "Actuals for R", na = c("", "NA"))

# View(forecast)

# We begin our analysis by reorganizing the probability distribution forecasts for PRUNEMP.
# We divide up the file downloaded from the SPF website whenever the ranges, forecast
# horizons, or variables change (See Table 10 of Documentation). 

# 2009 Quarter 2 to 2013 Quarter 4, PRUNEMP
# Current Year Forecasts
f1 <- forecast %>% 
  filter(YEAR > 2008 & YEAR < 2014) %>%
  filter(YEAR != 2009 | QUARTER != 1) %>%
  filter(YEAR != 2014 | QUARTER != 1) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRUNEMP1, PRUNEMP2, PRUNEMP3, PRUNEMP4, PRUNEMP5, PRUNEMP6, PRUNEMP7, PRUNEMP8, PRUNEMP9, PRUNEMP10)

f1 <- rename(f1, `YEAR FORECAST MADE` = `YEAR`)
f1 <- f1 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

# 2009 Quarter 2 to 2013 Quarter 4, PRUNEMP
# Second Year Forecasts
f2 <- forecast %>% 
  filter(YEAR > 2008 & YEAR < 2014) %>%
  filter(YEAR != 2009 | QUARTER != 1) %>%
  filter(YEAR != 2014 | QUARTER != 1) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRUNEMP11, PRUNEMP12, PRUNEMP13, PRUNEMP14, PRUNEMP15, PRUNEMP16, PRUNEMP17, PRUNEMP18, PRUNEMP19, PRUNEMP20)

f2 <- rename(f2, `PRUNEMP1` = `PRUNEMP11`, `PRUNEMP2` = `PRUNEMP12`, `PRUNEMP3` = `PRUNEMP13`, `PRUNEMP4` = `PRUNEMP14`, `PRUNEMP5` = `PRUNEMP15`, `PRUNEMP6` = `PRUNEMP16`, `PRUNEMP7` = `PRUNEMP17`, `PRUNEMP8` = `PRUNEMP18`, `PRUNEMP9` = `PRUNEMP19`, `PRUNEMP10` = `PRUNEMP20`)
f2 <- rename(f2, `YEAR FORECAST MADE` = `YEAR`)
f2 <- f2 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)

# 2009 Quarter 2 to 2013 Quarter 4, PRUNEMP
# Third Year Forecasts
f3 <- forecast %>% 
  filter(YEAR > 2008 & YEAR < 2014) %>%
  filter(YEAR != 2009 | QUARTER != 1) %>%
  filter(YEAR != 2014 | QUARTER != 1) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRUNEMP21, PRUNEMP22, PRUNEMP23, PRUNEMP24, PRUNEMP25, PRUNEMP26, PRUNEMP27, PRUNEMP28, PRUNEMP29, PRUNEMP30)

f3 <- rename(f3, `PRUNEMP1` = `PRUNEMP21`, `PRUNEMP2` = `PRUNEMP22`, `PRUNEMP3` = `PRUNEMP23`, `PRUNEMP4` = `PRUNEMP24`, `PRUNEMP5` = `PRUNEMP25`, `PRUNEMP6` = `PRUNEMP26`, `PRUNEMP7` = `PRUNEMP27`, `PRUNEMP8` = `PRUNEMP28`, `PRUNEMP9` = `PRUNEMP29`, `PRUNEMP10` = `PRUNEMP30`)
f3 <- rename(f3, `YEAR FORECAST MADE` = `YEAR`)
f3 <- f3 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 2)

# 2009 Quarter 2 to 2013 Quarter 4, PRUNEMP
# Fourth Year Forecasts
f4 <- forecast %>% 
  filter(YEAR > 2008 & YEAR < 2014) %>%
  filter(YEAR != 2009 | QUARTER != 1) %>%
  filter(YEAR != 2014 | QUARTER != 1) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRUNEMP31, PRUNEMP32, PRUNEMP33, PRUNEMP34, PRUNEMP35, PRUNEMP36, PRUNEMP37, PRUNEMP38, PRUNEMP39, PRUNEMP40)

f4 <- rename(f4, `PRUNEMP1` = `PRUNEMP31`, `PRUNEMP2` = `PRUNEMP32`, `PRUNEMP3` = `PRUNEMP33`, `PRUNEMP4` = `PRUNEMP34`, `PRUNEMP5` = `PRUNEMP35`, `PRUNEMP6` = `PRUNEMP36`, `PRUNEMP7` = `PRUNEMP37`, `PRUNEMP8` = `PRUNEMP38`, `PRUNEMP9` = `PRUNEMP39`, `PRUNEMP10` = `PRUNEMP40`)
f4 <- rename(f4, `YEAR FORECAST MADE` = `YEAR`)
f4 <- f4 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 3)

# Stack current, second, third, and fourth year forecasts
tablePRUNEMPa <- rbind(f1,f2,f3,f4)

# Creating a unique ID that combines Year Being Forecasted, Forecaster ID, Year Forecast Made, and Quarter

tablePRUNEMPa$Year.ID.ForecastYear.Quarter <- paste(tablePRUNEMPa$`YEAR BEING FORECAST`, tablePRUNEMPa$ID, tablePRUNEMPa$`YEAR FORECAST MADE`, tablePRUNEMPa$QUARTER, sep = "-")

#Renaming the columns

tablePRUNEMPa <- rename(tablePRUNEMPa, `FORECASTER ID` = `ID`, `BIN 1` = `PRUNEMP1`, `BIN 2` = `PRUNEMP2`, `BIN 3` = `PRUNEMP3`, `BIN 4` = `PRUNEMP4`, `BIN 5` = `PRUNEMP5`, `BIN 6` = `PRUNEMP6`, `BIN 7` = `PRUNEMP7`, `BIN 8` = `PRUNEMP8`, `BIN 9` = `PRUNEMP9`, `BIN 10` = `PRUNEMP10`)

#Creating a new column with the type of indicator

tablePRUNEMPa$INDICATOR <- "Unemployment"

#Creating a new column with the TDIST

tablePRUNEMPa <- tablePRUNEMPa %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Reorganize table columns

tablePRUNEMPa <- tablePRUNEMPa %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `BIN 1`, `BIN 2`, `BIN 3`, `BIN 4`, `BIN 5`, `BIN 6`, `BIN 7`, `BIN 8`, `BIN 9`, `BIN 10`, INDICATOR, TDIST)

#View(tablePRUNEMP)

# 2014 Quarter 1 to Present (2016 Q4)
# Current Year Forecasts
f5 <- forecast %>%
  filter(YEAR > 2013) %>%
  filter(YEAR != 2013 | QUARTER > 4) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRUNEMP1, PRUNEMP2, PRUNEMP3, PRUNEMP4, PRUNEMP5, PRUNEMP6, PRUNEMP7, PRUNEMP8, PRUNEMP9, PRUNEMP10)

f5 <- rename(f5, `YEAR FORECAST MADE` = `YEAR`)
f5 <- f5 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

# Second Year Forecasts
f6 <- forecast %>%
  filter(YEAR > 2013) %>%
  filter(YEAR != 2013 | QUARTER > 4) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRUNEMP11, PRUNEMP12, PRUNEMP13, PRUNEMP14, PRUNEMP15, PRUNEMP16, PRUNEMP17, PRUNEMP18, PRUNEMP19, PRUNEMP20)

f6 <- rename(f6, `PRUNEMP1` = `PRUNEMP11`, `PRUNEMP2` = `PRUNEMP12`, `PRUNEMP3` = `PRUNEMP13`, `PRUNEMP4` = `PRUNEMP14`, `PRUNEMP5` = `PRUNEMP15`, `PRUNEMP6` = `PRUNEMP16`, `PRUNEMP7` = `PRUNEMP17`, `PRUNEMP8` = `PRUNEMP18`, `PRUNEMP9` = `PRUNEMP19`, `PRUNEMP10` = `PRUNEMP20`)
f6 <- rename(f6, `YEAR FORECAST MADE` = `YEAR`)
f6 <- f6 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)

# Third Year Forecasts
f7 <- forecast %>%
  filter(YEAR > 2013) %>%
  filter(YEAR != 2013 | QUARTER > 4) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRUNEMP21, PRUNEMP22, PRUNEMP23, PRUNEMP24, PRUNEMP25, PRUNEMP26, PRUNEMP27, PRUNEMP28, PRUNEMP29, PRUNEMP30)

f7 <- rename(f7, `PRUNEMP1` = `PRUNEMP21`, `PRUNEMP2` = `PRUNEMP22`, `PRUNEMP3` = `PRUNEMP23`, `PRUNEMP4` = `PRUNEMP24`, `PRUNEMP5` = `PRUNEMP25`, `PRUNEMP6` = `PRUNEMP26`, `PRUNEMP7` = `PRUNEMP27`, `PRUNEMP8` = `PRUNEMP28`, `PRUNEMP9` = `PRUNEMP29`, `PRUNEMP10` = `PRUNEMP30`)
f7 <- rename(f7, `YEAR FORECAST MADE` = `YEAR`)
f7 <- f7 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 2)

# Fourth Year Forecasts
f8 <- forecast %>%
  filter(YEAR > 2013) %>%
  filter(YEAR != 2013 | QUARTER > 4) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRUNEMP31, PRUNEMP32, PRUNEMP33, PRUNEMP34, PRUNEMP35, PRUNEMP36, PRUNEMP37, PRUNEMP38, PRUNEMP39, PRUNEMP40)

f8 <- rename(f8, `PRUNEMP1` = `PRUNEMP31`, `PRUNEMP2` = `PRUNEMP32`, `PRUNEMP3` = `PRUNEMP33`, `PRUNEMP4` = `PRUNEMP34`, `PRUNEMP5` = `PRUNEMP35`, `PRUNEMP6` = `PRUNEMP36`, `PRUNEMP7` = `PRUNEMP37`, `PRUNEMP8` = `PRUNEMP38`, `PRUNEMP9` = `PRUNEMP39`, `PRUNEMP10` = `PRUNEMP40`)
f8 <- rename(f8, `YEAR FORECAST MADE` = `YEAR`)
f8 <- f8 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 3)

# Stack current, second, third, and fourth year forecasts
tablePRUNEMPb <- rbind(f5,f6,f7,f8)

# Creating a unique ID that combines Year Being Forecasted, Forecaster ID, Year Forecast Made, and Quarter

tablePRUNEMPb$Year.ID.ForecastYear.Quarter <- paste(tablePRUNEMPb$`YEAR BEING FORECAST`, tablePRUNEMPb$ID, tablePRUNEMPb$`YEAR FORECAST MADE`, tablePRUNEMPb$QUARTER, sep = "-")

#Renaming the columns

tablePRUNEMPb <- rename(tablePRUNEMPb, `FORECASTER ID` = `ID`, `BIN 1` = `PRUNEMP1`, `BIN 2` = `PRUNEMP2`, `BIN 3` = `PRUNEMP3`, `BIN 4` = `PRUNEMP4`, `BIN 5` = `PRUNEMP5`, `BIN 6` = `PRUNEMP6`, `BIN 7` = `PRUNEMP7`, `BIN 8` = `PRUNEMP8`, `BIN 9` = `PRUNEMP9`, `BIN 10` = `PRUNEMP10`)

#Creating a new column the type of indicator

tablePRUNEMPb$INDICATOR <- "Unemployment"

#Creating a new column with the TDIST

tablePRUNEMPb <- tablePRUNEMPb %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Reorganize table columns

tablePRUNEMPb <- tablePRUNEMPb %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `BIN 1`, `BIN 2`, `BIN 3`, `BIN 4`, `BIN 5`, `BIN 6`, `BIN 7`, `BIN 8`, `BIN 9`, `BIN 10`, INDICATOR, TDIST)

PRUNEMP <- as.data.frame(rbind(tablePRUNEMPa, tablePRUNEMPb))

# Converting the data to be numeric
PRUNEMP[PRUNEMP=="#N/A"] <- NA
PRUNEMP[,colnames(PRUNEMP)[!(colnames(PRUNEMP) %in% c("INDICATOR", "Year.ID.ForecastYear.Quarter"))]] <- sapply(PRUNEMP[,colnames(PRUNEMP)[!(colnames(PRUNEMP) %in% c("INDICATOR", "Year.ID.ForecastYear.Quarter"))]], as.numeric)

write.csv(PRUNEMP,"/Users/Isaac/Downloads/UNEMP.csv", row.names = FALSE, na ="")
