#######################################################
#
#   R analysis code for the FEO-SPF project
#   Analyzing data from the Survey of Professional Forecasters
#   Sandy Campbell 
#   September 18, 2019
#
#######################################################

#library(DataComputing)
library(dplyr)
library(readxl)
library(foreign)

# reading original data and actuals into the R code
# The file we are using was downloaded from the SPF website on July 4th 2019.
forecast <- read_excel("/Users/Isaac/Downloads/Individual_PRGDP.xlsx", sheet = 1) 
# actuals <- read_excel("Actuals PRGDP_08012019.xlsx", sheet = "Actuals for R", na = c("", "NA"))

# View(forecast)

# We begin our analysis by reorganizing the probability distribution forecasts for GDP.
# We divide up the file downloaded from the SPF website whenever the ranges, forecast
# horizons, or variables change, e.g. RGDP --> NGDP (See Table 8)

# 1968 Quarter 4 to 1973 Quarter 1, Nominal GNP
f1 <- forecast %>% 
  filter(YEAR < 1974) %>%
  filter(YEAR != 1973 | QUARTER == 1) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRGDP1, PRGDP2, PRGDP3, PRGDP4, PRGDP5, PRGDP6, PRGDP7, PRGDP8, PRGDP9, PRGDP10, PRGDP11, PRGDP12, PRGDP13, PRGDP14, PRGDP15)

f1 <- rename(f1, `YEAR FORECAST MADE` = `YEAR`)
f1 <- f1 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

# Creating a unique ID that combines Year Being Forecasted, Forecaster ID, Year Forecast Made, and Quarter
f1$Year.ID.ForecastYear.Quarter <- paste(f1$`YEAR BEING FORECAST`, f1$ID, f1$`YEAR FORECAST MADE`, f1$QUARTER, sep = "-")

# Renaming the columns
f1 <- rename(f1, `FORECASTER ID` = `ID`, `BIN 1` = `PRGDP1`, `BIN 2` = `PRGDP2`, `BIN 3` = `PRGDP3`, `BIN 4` = `PRGDP4`, `BIN 5` = `PRGDP5`, `BIN 6` = `PRGDP6`, `BIN 7` = `PRGDP7`, `BIN 8` = `PRGDP8`, `BIN 9` = `PRGDP9`, `BIN 10` = `PRGDP10`, `BIN 11` = `PRGDP11`, `BIN 12` = `PRGDP12`, `BIN 13` = `PRGDP13`, `BIN 14` = `PRGDP14`, `BIN 15` = `PRGDP15`)

# Creating a new column: the type of indicator
f1$INDICATOR <- "NominalGNP"

# Creating a new column with the TDIST
f1 <- f1 %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Reorganizing the columns
f1 <- f1 %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `BIN 1`, `BIN 2`, `BIN 3`, `BIN 4`, `BIN 5`, `BIN 6`, `BIN 7`, `BIN 8`, `BIN 9`, `BIN 10`, `BIN 11`, `BIN 12`, `BIN 13`, `BIN 14`, `BIN 15`, INDICATOR, TDIST)

# ------------------------------------------------------
# 1973 Quarter 2 to 1974 Quarter 3, Nominal GNP
f2 <- forecast %>%
  filter(YEAR > 1972 & YEAR < 1975) %>%
  filter(YEAR != 1973 | QUARTER != 1) %>%
  filter(YEAR != 1974 | QUARTER != 4) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRGDP1, PRGDP2, PRGDP3, PRGDP4, PRGDP5, PRGDP6, PRGDP7, PRGDP8, PRGDP9, PRGDP10, PRGDP11, PRGDP12, PRGDP13, PRGDP14, PRGDP15)

f2 <- rename(f2, `YEAR FORECAST MADE` = `YEAR`)
f2 <- f2 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

# Creating a unique ID that combines Year Being Forecasted, Forecaster ID, Year Forecast Made, and Quarter
f2$Year.ID.ForecastYear.Quarter <- paste(f2$`YEAR BEING FORECAST`, f2$ID, f2$`YEAR FORECAST MADE`, f2$QUARTER, sep = "-")

# Renaming the columns
f2 <- rename(f2, `FORECASTER ID` = `ID`, `BIN 1` = `PRGDP1`, `BIN 2` = `PRGDP2`, `BIN 3` = `PRGDP3`, `BIN 4` = `PRGDP4`, `BIN 5` = `PRGDP5`, `BIN 6` = `PRGDP6`, `BIN 7` = `PRGDP7`, `BIN 8` = `PRGDP8`, `BIN 9` = `PRGDP9`, `BIN 10` = `PRGDP10`, `BIN 11` = `PRGDP11`, `BIN 12` = `PRGDP12`, `BIN 13` = `PRGDP13`, `BIN 14` = `PRGDP14`, `BIN 15` = `PRGDP15`)

# Creating a new column: the type of indicator
f2$INDICATOR <- "NominalGNP"

# Creating a new column with the TDIST
f2 <- f2%>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Reorganizing the columns
f2 <- f2 %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `BIN 1`, `BIN 2`, `BIN 3`, `BIN 4`, `BIN 5`, `BIN 6`, `BIN 7`, `BIN 8`, `BIN 9`, `BIN 10`, `BIN 11`, `BIN 12`, `BIN 13`, `BIN 14`, `BIN 15`, INDICATOR, TDIST)

# ------------------------------------------------------
# 1974 Quarter 4 to 1981 Quarter 2, Nominal GNP
f3 <- forecast %>%
  filter(YEAR > 1973 & YEAR < 1982) %>%
  filter(YEAR != 1974 | QUARTER == 4) %>%
  filter(YEAR != 1981 | QUARTER < 3) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRGDP1, PRGDP2, PRGDP3, PRGDP4, PRGDP5, PRGDP6, PRGDP7, PRGDP8, PRGDP9, PRGDP10, PRGDP11, PRGDP12, PRGDP13, PRGDP14, PRGDP15)

f3 <- rename(f3, `YEAR FORECAST MADE` = `YEAR`)
f3 <- f3 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

# Creating a unique ID that combines Year Being Forecasted, Forecaster ID, Year Forecast Made, and Quarter
f3$Year.ID.ForecastYear.Quarter <- paste(f3$`YEAR BEING FORECAST`, f3$ID, f3$`YEAR FORECAST MADE`, f3$QUARTER, sep = "-")

# Renaming the columns
f3 <- rename(f3, `FORECASTER ID` = `ID`, `BIN 1` = `PRGDP1`, `BIN 2` = `PRGDP2`, `BIN 3` = `PRGDP3`, `BIN 4` = `PRGDP4`, `BIN 5` = `PRGDP5`, `BIN 6` = `PRGDP6`, `BIN 7` = `PRGDP7`, `BIN 8` = `PRGDP8`, `BIN 9` = `PRGDP9`, `BIN 10` = `PRGDP10`, `BIN 11` = `PRGDP11`, `BIN 12` = `PRGDP12`, `BIN 13` = `PRGDP13`, `BIN 14` = `PRGDP14`, `BIN 15` = `PRGDP15`)

# Creating a new column: the type of indicator
f3$INDICATOR <- "NominalGNP"

# Creating a new column with the TDIST
f3 <- f3 %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

#Reorganizing the columns
f3 <- f3 %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `BIN 1`, `BIN 2`, `BIN 3`, `BIN 4`, `BIN 5`, `BIN 6`, `BIN 7`, `BIN 8`, `BIN 9`, `BIN 10`, `BIN 11`, `BIN 12`, `BIN 13`, `BIN 14`, `BIN 15`, INDICATOR, TDIST)

# ------------------------------------------------------
# 1981 Quarter 3 to 1991 Quarter 4, Real GNP
# Current Year Forecasts
f4 <- forecast %>%
  filter(YEAR > 1980 & YEAR < 1992) %>%
  filter(YEAR != 1981 | QUARTER > 2) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRGDP1, PRGDP2, PRGDP3, PRGDP4, PRGDP5, PRGDP6)

f4 <- rename(f4, `YEAR FORECAST MADE` = `YEAR`)
f4 <- f4 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

# 1981 Quarter 3 to 1991 Quarter 4, Real GNP
# Second Year Forecasts
f5 <- forecast %>%
  filter(YEAR > 1980 & YEAR < 1992) %>%
  filter(YEAR != 1981 | QUARTER > 2) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRGDP7, PRGDP8, PRGDP9, PRGDP10, PRGDP11, PRGDP12)

f5 <- rename(f5, `PRGDP1` = `PRGDP7`, `PRGDP2` = `PRGDP8`, `PRGDP3` = `PRGDP9`, `PRGDP4` = `PRGDP10`, `PRGDP5` = `PRGDP11`, `PRGDP6` = `PRGDP12`)
f5 <- rename(f5, `YEAR FORECAST MADE` = `YEAR`)
f5 <- f5 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)

# Stack current and second year forecasts 
tableRGNP <- rbind(f4, f5)

# Creating a unique ID that combines Year Being Forecasted, Forecaster ID, Year Forecast Made, and Quarter
tableRGNP$Year.ID.ForecastYear.Quarter <- paste(tableRGNP$`YEAR BEING FORECAST`, tableRGNP$ID, tableRGNP$`YEAR FORECAST MADE`, tableRGNP$QUARTER, sep = "-")

# Renaming the columns, adding Indicator, creating TDist
tableRGNP <- rename(tableRGNP, `FORECASTER ID` = `ID`, `BIN 1` = `PRGDP1`, `BIN 2` = `PRGDP2`, `BIN 3` = `PRGDP3`, `BIN 4` = `PRGDP4`, `BIN 5` = `PRGDP5`, `BIN 6` = `PRGDP6`)

tableRGNP$INDICATOR <- "RealGNP"

tableRGNP <- tableRGNP %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Adding empty columns to RGNP
tableRGNP[, "BIN 7"] <- NA
tableRGNP[, "BIN 8"] <- NA
tableRGNP[, "BIN 9"] <- NA
tableRGNP[, "BIN 10"] <- NA
tableRGNP[, "BIN 11"] <- NA
tableRGNP[, "BIN 12"] <- NA
tableRGNP[, "BIN 13"] <- NA
tableRGNP[, "BIN 14"] <- NA
tableRGNP[, "BIN 15"] <- NA

# Reorganizing table columns
tableRGNP <- tableRGNP %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `BIN 1`, `BIN 2`, `BIN 3`, `BIN 4`, `BIN 5`, `BIN 6`, `BIN 7`, `BIN 8`, `BIN 9`, `BIN 10`, `BIN 11`, `BIN 12`, `BIN 13`, `BIN 14`, `BIN 15`, INDICATOR, TDIST)

# ------------------------------------------------------
# 1992 Quarter 1 to 2009 Quarter 1, Real GDP
# Current Year Forecasts
f6 <- forecast %>%
  filter(YEAR > 1991 & YEAR < 2010) %>%
  filter(YEAR != 2009 | QUARTER == 1) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRGDP1, PRGDP2, PRGDP3, PRGDP4, PRGDP5, PRGDP6, PRGDP7, PRGDP8, PRGDP9, PRGDP10)

f6 <- rename(f6, `YEAR FORECAST MADE` = `YEAR`)
f6 <- f6 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

# 1992 Quarter 1 to 2009 Quarter 1, Real GDP
# Second Year Forecasts
f7 <- forecast %>%
  filter(YEAR > 1991 & YEAR < 2010) %>%
  filter(YEAR != 2009 | QUARTER == 1) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRGDP11, PRGDP12, PRGDP13, PRGDP14, PRGDP15, PRGDP16, PRGDP17, PRGDP18, PRGDP19, PRGDP20)

f7 <- rename(f7, `PRGDP1` = `PRGDP11`, `PRGDP2` = `PRGDP12`, `PRGDP3` = `PRGDP13`, `PRGDP4` = `PRGDP14`, `PRGDP5` = `PRGDP15`, `PRGDP6` = `PRGDP16`, `PRGDP7` = `PRGDP17`, `PRGDP8` = `PRGDP18`, `PRGDP9` = `PRGDP19`, `PRGDP10` = `PRGDP20`)
f7 <- rename(f7, `YEAR FORECAST MADE` = `YEAR`)
f7 <- f7 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)

# Stack current and second year forecasts
tableRGDPa <- rbind(f6, f7)

tableRGDPa$Year.ID.ForecastYear.Quarter <- paste(tableRGDPa$`YEAR BEING FORECAST`, tableRGDPa$ID, tableRGDPa$`YEAR FORECAST MADE`, tableRGDPa$QUARTER, sep = "-")

tableRGDPa <- rename(tableRGDPa, `FORECASTER ID` = `ID`, `BIN 1` = `PRGDP1`, `BIN 2` = `PRGDP2`, `BIN 3` = `PRGDP3`, `BIN 4` = `PRGDP4`, `BIN 5` = `PRGDP5`, `BIN 6` = `PRGDP6`, `BIN 7` = `PRGDP7`, `BIN 8` = `PRGDP8`, `BIN 9` = `PRGDP9`, `BIN 10` = `PRGDP10`)

tableRGDPa$INDICATOR <- "RealGDP"

tableRGDPa <- tableRGDPa %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Adding empty columns to RGDP
tableRGDPa[, "BIN 11"] <- NA
tableRGDPa[, "BIN 12"] <- NA
tableRGDPa[, "BIN 13"] <- NA
tableRGDPa[, "BIN 14"] <- NA
tableRGDPa[, "BIN 15"] <- NA

tableRGDPa <- tableRGDPa %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `BIN 1`, `BIN 2`, `BIN 3`, `BIN 4`, `BIN 5`, `BIN 6`, `BIN 7`, `BIN 8`, `BIN 9`, `BIN 10`, `BIN 11`, `BIN 12`, `BIN 13`, `BIN 14`, `BIN 15`, INDICATOR, TDIST)

# ------------------------------------------------------
# 2009 Quarter 2 to Present (2019 Quarter 2)
# Current Year Forecasts
f8 <- forecast %>%
  filter(YEAR > 2008) %>%
  filter(YEAR != 2009 | QUARTER > 1) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRGDP1, PRGDP2, PRGDP3, PRGDP4, PRGDP5, PRGDP6, PRGDP7, PRGDP8, PRGDP9, PRGDP10, PRGDP11)

f8 <- rename(f8, `YEAR FORECAST MADE` = `YEAR`)
f8 <- f8 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

# 2009 Quarter 2 to Present (2019 Quarter 2)
# Second Year Forecasts
f9 <- forecast %>%
  filter(YEAR > 2008) %>%
  filter(YEAR != 2009 | QUARTER > 1) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRGDP12, PRGDP13, PRGDP14, PRGDP15, PRGDP16, PRGDP17, PRGDP18, PRGDP19, PRGDP20, PRGDP21, PRGDP22)

f9 <- rename(f9, `PRGDP1` = `PRGDP12`, `PRGDP2` = `PRGDP13`, `PRGDP3` = `PRGDP14`, `PRGDP4` = `PRGDP15`, `PRGDP5` = `PRGDP16`, `PRGDP6` = `PRGDP17`, `PRGDP7` = `PRGDP18`, `PRGDP8` = `PRGDP19`, `PRGDP9` = `PRGDP20`, `PRGDP10` = `PRGDP21`, `PRGDP11` = `PRGDP22`)
f9 <- rename(f9, `YEAR FORECAST MADE` = `YEAR`)
f9 <- f9 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)

# 2009 Quarter 2 to Present (2019 Quarter 2)
# Third Year Forecasts
f10 <- forecast %>%
  filter(YEAR > 2008) %>%
  filter(YEAR != 2009 | QUARTER > 1) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRGDP23, PRGDP24, PRGDP25, PRGDP26, PRGDP27, PRGDP28, PRGDP29, PRGDP30, PRGDP31, PRGDP32, PRGDP33)

f10 <- rename(f10, `PRGDP1` = `PRGDP23`, `PRGDP2` = `PRGDP24`, `PRGDP3` = `PRGDP25`, `PRGDP4` = `PRGDP26`, `PRGDP5` = `PRGDP27`, `PRGDP6` = `PRGDP28`, `PRGDP7` = `PRGDP29`, `PRGDP8` = `PRGDP30`, `PRGDP9` = `PRGDP31`, `PRGDP10` = `PRGDP32`, `PRGDP11` = `PRGDP33`)
f10 <- rename(f10, `YEAR FORECAST MADE` = `YEAR`)
f10 <- f10 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 2)

# 2009 Quarter 2 to Present (2019 Quarter 2)
# Fourth Year Forecasts
f11 <- forecast %>%
  filter(YEAR > 2008) %>%
  filter(YEAR != 2009 | QUARTER > 1) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRGDP34, PRGDP35, PRGDP36, PRGDP37, PRGDP38, PRGDP39, PRGDP40, PRGDP41, PRGDP42, PRGDP43, PRGDP44)

f11 <- rename(f11, `PRGDP1` = `PRGDP34`, `PRGDP2` = `PRGDP35`, `PRGDP3` = `PRGDP36`, `PRGDP4` = `PRGDP37`, `PRGDP5` = `PRGDP38`, `PRGDP6` = `PRGDP39`, `PRGDP7` = `PRGDP40`, `PRGDP8` = `PRGDP41`, `PRGDP9` = `PRGDP42`, `PRGDP10` = `PRGDP43`, `PRGDP11` = `PRGDP44`)
f11 <- rename(f11, `YEAR FORECAST MADE` = `YEAR`)
f11 <- f11 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 3)

# Stack current, second, third, and fourth year forecasts
tableRGDPb <- rbind(f8,f9,f10,f11)

tableRGDPb$Year.ID.ForecastYear.Quarter <- paste(tableRGDPb$`YEAR BEING FORECAST`, tableRGDPb$ID, tableRGDPb$`YEAR FORECAST MADE`, tableRGDPb$QUARTER, sep = "-")

tableRGDPb <- rename(tableRGDPb, `FORECASTER ID` = `ID`, `BIN 1` = `PRGDP1`, `BIN 2` = `PRGDP2`, `BIN 3` = `PRGDP3`, `BIN 4` = `PRGDP4`, `BIN 5` = `PRGDP5`, `BIN 6` = `PRGDP6`, `BIN 7` = `PRGDP7`, `BIN 8` = `PRGDP8`, `BIN 9` = `PRGDP9`, `BIN 10` = `PRGDP10`, `BIN 11` = `PRGDP11`)

tableRGDPb$INDICATOR <- "RealGDP"

tableRGDPb <- tableRGDPb %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

tableRGDPb[, "BIN 12"] <- NA
tableRGDPb[, "BIN 13"] <- NA
tableRGDPb[, "BIN 14"] <- NA
tableRGDPb[, "BIN 15"] <- NA

tableRGDPb <- tableRGDPb %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `BIN 1`, `BIN 2`, `BIN 3`, `BIN 4`, `BIN 5`, `BIN 6`, `BIN 7`, `BIN 8`, `BIN 9`, `BIN 10`, `BIN 11`, `BIN 12`, `BIN 13`, `BIN 14`, `BIN 15`, INDICATOR, TDIST)

# ------------------------------------------------------
### Combining nGNP, rGNP, and rGDP

# Creating nGNP
tableNGNP <- rbind(f1, f2, f3)

# Creating rGDP
tableRGDP <- rbind(tableRGDPa, tableRGDPb)

# Creating PRGDP
PRGDP <- rbind(tableNGNP, tableRGNP, tableRGDP)

# Converting the data to be numeric
PRGDP[PRGDP=="#N/A"] <- NA
PRGDP[,colnames(PRGDP)[!(colnames(PRGDP) %in% c("INDICATOR", "Year.ID.ForecastYear.Quarter"))]] <- sapply(PRGDP[,colnames(PRGDP)[!(colnames(PRGDP) %in% c("INDICATOR", "Year.ID.ForecastYear.Quarter"))]], as.numeric)

write.csv(PRGDP,"/Users/Isaac/Downloads/GDP.csv", row.names = FALSE, na ="")
