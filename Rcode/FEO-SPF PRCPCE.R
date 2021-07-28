#######################################################
#
#   R analysis code for the FEO-SPF project
#   analyzing data from the Survey of Professional Forecasters
#   Probability of Core PCE Inflation (PRCPCE)
#   Sandy Campbell 
#   September 18, 2019
#
#######################################################

library(DataComputing)
library(dplyr)
library(readxl)
library(foreign)

# Reading original data and actuals into the R code
# The file we are using was downloaded from the SPF website on July 4th 2019.
forecast <- read_excel("/Users/Isaac/Downloads/Individual_PRCPCE.xlsx", sheet = 1)
# actuals <- read_excel("Actuals PRCPCE_08182019.xlsx", sheet = "Actuals for R", na = c("", "NA"))

# We begin our analysis by reorganizing the probability distribution forecasts for Core PCE Inflation.
# We divide up the file downloaded from the SPF website whenever the ranges, forecast
# horizons, or variables change. 

# 2007 Quarter 1 to Present (2019 Quarter 2)
# Current Year Forecasts
f1 <- forecast %>%
  filter(YEAR > 2006) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRCPCE1, PRCPCE2, PRCPCE3, PRCPCE4, PRCPCE5, PRCPCE6, PRCPCE7, PRCPCE8, PRCPCE9, PRCPCE10)

f1 <- rename(f1, `YEAR FORECAST MADE` = `YEAR`)
f1 <- f1 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

# 2007 Quarter 1 to Present (2019 Quarter 2)
# Second Year Forecasts
f2 <- forecast %>%
  filter(YEAR > 2006) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRCPCE11, PRCPCE12, PRCPCE13, PRCPCE14, PRCPCE15, PRCPCE16, PRCPCE17, PRCPCE18, PRCPCE19, PRCPCE20)

f2 <- rename(f2, `PRCPCE1` = `PRCPCE11`, `PRCPCE2` = `PRCPCE12`, `PRCPCE3` = `PRCPCE13`, `PRCPCE4` = `PRCPCE14`, `PRCPCE5` = `PRCPCE15`, `PRCPCE6` = `PRCPCE16`, `PRCPCE7` = `PRCPCE17`, `PRCPCE8` = `PRCPCE18`, `PRCPCE9` = `PRCPCE19`, `PRCPCE10` = `PRCPCE20`)
f2 <- rename(f2, `YEAR FORECAST MADE` = `YEAR`)
f2 <- f2 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)

# Stack current and second year forecasts

PRCPCE <- rbind(f1, f2)

PRCPCE$Year.ID.ForecastYear.Quarter <- paste(PRCPCE$`YEAR BEING FORECAST`, PRCPCE$ID, PRCPCE$`YEAR FORECAST MADE`, PRCPCE$QUARTER, sep = "-")

PRCPCE <- rename(PRCPCE, `FORECASTER ID` = `ID`, `BIN 1` = `PRCPCE1`, `BIN 2` = `PRCPCE2`, `BIN 3` = `PRCPCE3`, `BIN 4` = `PRCPCE4`, `BIN 5` = `PRCPCE5`, `BIN 6` = `PRCPCE6`, `BIN 7` = `PRCPCE7`, `BIN 8` = `PRCPCE8`, `BIN 9` = `PRCPCE9`, `BIN 10` = `PRCPCE10`)

PRCPCE$INDICATOR <- "Core PCE"

PRCPCE <- PRCPCE %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

PRCPCE <- PRCPCE %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `BIN 1`, `BIN 2`, `BIN 3`, `BIN 4`, `BIN 5`, `BIN 6`, `BIN 7`, `BIN 8`, `BIN 9`, `BIN 10`, INDICATOR, TDIST)

# Converting the data to be numeric
PRCPCE[PRCPCE=="#N/A"] <- NA
PRCPCE[,colnames(PRCPCE)[!(colnames(PRCPCE) %in% c("INDICATOR", "Year.ID.ForecastYear.Quarter"))]] <- sapply(PRCPCE[,colnames(PRCPCE)[!(colnames(PRCPCE) %in% c("INDICATOR", "Year.ID.ForecastYear.Quarter"))]], as.numeric)

write.csv(PRCPCE,"/Users/Isaac/Downloads/CPCE.csv", row.names = FALSE, na ="")
