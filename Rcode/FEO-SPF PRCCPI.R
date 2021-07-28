#######################################################
#
#   R analysis code for the FEO-SPF project
#   analyzing data from the Survey of Professional Forecasters
#   Probability of Core CPI Inflation (PRCCPI)
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
forecast <- read_excel("/Users/Isaac/Downloads/Individual_PRCCPI.xlsx", sheet = 1)
# actuals <- read_excel("Actuals PRCCPI_008182019.xlsx", sheet = "Actuals for R", na = c("", "NA"))

# We begin our analysis by reorganizing the probability distribution forecasts for Core CPI Inflation.
# We divide up the file downloaded from the SPF website whenever the ranges, forecast
# horizons, or variables change. 

# 2007 Quarter 1 to Present (2019 Quarter 2)
# Current Year Forecasts
f1 <- forecast %>%
  filter(YEAR > 2006) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRCCPI1, PRCCPI2, PRCCPI3, PRCCPI4, PRCCPI5, PRCCPI6, PRCCPI7, PRCCPI8, PRCCPI9, PRCCPI10)

f1 <- rename(f1, `YEAR FORECAST MADE` = YEAR)
f1 <- f1 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

# 2007 Quarter 1 to Present (2019 Quarter 2)
# Second Year Forecasts
f2 <- forecast %>%
  filter(YEAR > 2006) %>%
  select(YEAR, QUARTER, ID, INDUSTRY, PRCCPI11, PRCCPI12, PRCCPI13, PRCCPI14, PRCCPI15, PRCCPI16, PRCCPI17, PRCCPI18, PRCCPI19, PRCCPI20 )

f2 <- rename(f2, `PRCCPI1` = `PRCCPI11`, `PRCCPI2` = `PRCCPI12`, `PRCCPI3` = `PRCCPI13`, `PRCCPI4` = `PRCCPI14`, `PRCCPI5` = `PRCCPI15`, `PRCCPI6` = `PRCCPI16`, `PRCCPI7` = `PRCCPI17`, `PRCCPI8` = `PRCCPI18`, `PRCCPI9` = `PRCCPI19`, `PRCCPI10` = `PRCCPI20`)
f2 <- rename(f2, `YEAR FORECAST MADE` = `YEAR`)
f2 <- f2 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)

# Stack current and second year forecasts

PRCCPI <- rbind(f1, f2)

PRCCPI$Year.ID.ForecastYear.Quarter <- paste(PRCCPI$`YEAR BEING FORECAST`, PRCCPI$ID, PRCCPI$`YEAR FORECAST MADE`, PRCCPI$QUARTER, sep = "-")

PRCCPI <- rename(PRCCPI, `FORECASTER ID` = `ID`, `BIN 1` = `PRCCPI1`, `BIN 2` = `PRCCPI2`, `BIN 3` = `PRCCPI3`, `BIN 4` = `PRCCPI4`, `BIN 5` = `PRCCPI5`, `BIN 6` = `PRCCPI6`, `BIN 7` = `PRCCPI7`, `BIN 8` = `PRCCPI8`, `BIN 9` = `PRCCPI9`, `BIN 10` = `PRCCPI10`)

PRCCPI$INDICATOR <- "Core CPI"

PRCCPI <- PRCCPI %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

PRCCPI <- PRCCPI%>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `BIN 1`, `BIN 2`, `BIN 3`, `BIN 4`, `BIN 5`, `BIN 6`, `BIN 7`, `BIN 8`, `BIN 9`, `BIN 10`, INDICATOR, TDIST)

# Converting the data to be numeric
PRCCPI[PRCCPI=="#N/A"] <- NA
PRCCPI[,colnames(PRCCPI)[!(colnames(PRCCPI) %in% c("INDICATOR", "Year.ID.ForecastYear.Quarter"))]] <- sapply(PRCCPI[,colnames(PRCCPI)[!(colnames(PRCCPI) %in% c("INDICATOR", "Year.ID.ForecastYear.Quarter"))]], as.numeric)

write.csv(PRCCPI,"/Users/Isaac/Downloads/CCPI.csv", row.names = FALSE, na ="")
