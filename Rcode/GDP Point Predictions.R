#######################################################
#
#   R analysis code for the FEO-SPF project
#   Analyzing data from the Survey of Professional Forecasters
#   Sandy Campbell
#   May 26, 2020
#
#   This is GDP point predictions
#   https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/rgdp
#######################################################

#library(DataComputing)
library(dplyr)
library(readxl)
library(foreign)
library("ggpubr")
library(tidyr)
library(ggplot2)
library(scales)
library(zoo)
library(lme4)

training <- read.csv("/Users/Isaac/Downloads/fulldata.csv", check.names = FALSE, stringsAsFactors = FALSE)
specialTS <- read_excel("/Users/Isaac/Downloads/RawData/SPF - Annual Historical Values By Survey Date - Selected Variables - Tom Stark.xlsx", sheet = "Data", skip = 3, col_names = c("YEAR", "QUARTER", "RGDPA", "GDPPI"))


# Before 1981 Q3, survey did not include annual forecasts
# And we can't extrapolate percentage change from the quarterly data; we start at 1981 Q3
RGDP <- read_excel("/Users/Isaac/Downloads/RawData/Individual_RGDP.xlsx") 
sapply(RGDP, class)
RGDP[ , ] <- apply(RGDP[ , ], 2,     
                   function(x) as.numeric(as.character(x)))

RGDP <- RGDP %>% 
  filter(YEAR >= 1981) %>%
  filter(YEAR != 1981 | QUARTER != 1) %>%
  filter(YEAR != 1981 | QUARTER != 2) 


############### f3 ######################
# We start from 1981Q3 with fix weighted Real GNP. This goes until 1991Q4. They're asked for current and next year
# NOTE*: for 1985Q1 and 1986 Q1 and 1990Q1, they asked for the previous year and the current year, instead of current and following year. 
# Example 1990 Q1 survey: 1989 Q4, 1990 Q1-4, 1991 Q1, Annual: 1989 1990

f0 <- RGDP %>% 
  filter(YEAR >= 1981 & YEAR < 1992) %>%
  filter(YEAR != 1981 | QUARTER != 1) %>%
  filter(YEAR != 1981 | QUARTER != 2) 

# These are the years where they ask for previous and current
f1 <- f0 %>% 
  filter((YEAR == 1985 & QUARTER == 1) | (YEAR == 1986 & QUARTER == 1)| (YEAR == 1990 & QUARTER == 1))

# These are the years where they ask for current and next year
f2 <- anti_join(f0, f1)

# Clean f1 and f2 up
f1 <- rename(f1, `YEAR FORECAST MADE` = YEAR)
f1 <- f1 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

f1$Year.ID.ForecastYear.Quarter <- paste(f1$`YEAR BEING FORECAST`, f1$ID, f1$`YEAR FORECAST MADE`, f1$QUARTER, sep = "-")

f1 <- f1 %>% 
  mutate(RGDPA = ((RGDPB - RGDPA)/RGDPA)*100)
f1$RGDPB[ ] <- NA


f2 <- rename(f2, `YEAR FORECAST MADE` = YEAR)
f2 <- f2 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)

f2$Year.ID.ForecastYear.Quarter <- paste(f2$`YEAR BEING FORECAST`, f2$ID, f2$`YEAR FORECAST MADE`, f2$QUARTER, sep = "-")

f2 <- f2 %>% 
  mutate(RGDPA = ((RGDPB - RGDPA)/RGDPA)*100)
f2$RGDPB[ ] <- NA

# Bind it and clean it up
f3 <- rbind(f1, f2)

# Renaming the columns
f3 <- rename(f3, `FORECASTER ID` = `ID`)

# Creating a new column: the type of indicator
f3$INDICATOR <- "RealGNP"

# Creating a new column with the TDIST
f3 <- f3 %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Reorganizing the columns
f3 <- f3 %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `RGDP1`, `RGDP2`, `RGDP3`, `RGDP4`, `RGDP5`, `RGDP6`, `RGDPA`, `RGDPB`, `RGDPC`, `RGDPD`, INDICATOR, TDIST)


############### f4 ######################
# 1992Q1-2009Q1 is 2 year forecasts, so let's do that
# NOTE*: Real GDP from 1992 Q1 – 1995Q4, but it’s FIXED WEIGHTED

f4 <- RGDP %>% 
  filter(YEAR >= 1992 & YEAR < 2010) %>%
  filter(YEAR != 2009 | QUARTER != 2) %>%
  filter(YEAR != 2009 | QUARTER != 3) %>%   
  filter(YEAR != 2009 | QUARTER != 4) 

# import the TS data into RGDPA
f4$RGDPC <- f4$RGDPB
f4$RGDPB <- f4$RGDPA
f4 <- f4[,c(1:10,12:14)] # tkae out RGDPA
f4 <- merge(f4, specialTS, by = c("YEAR", "QUARTER"))
f4 <- f4 %>%
  select(c(1:10, 14, 11:13))

# Clean it up
f4 <- rename(f4, `YEAR FORECAST MADE` = YEAR)
f4 <- f4 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

f5 <- f4 %>% 
  mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` +1)

f4$Year.ID.ForecastYear.Quarter <- paste(f4$`YEAR BEING FORECAST`, f4$ID, f4$`YEAR FORECAST MADE`, f4$QUARTER, sep = "-")
f5$Year.ID.ForecastYear.Quarter <- paste(f5$`YEAR BEING FORECAST`, f5$ID, f5$`YEAR FORECAST MADE`, f5$QUARTER, sep = "-")

f4 <- f4 %>% 
  mutate(RGDPA = ((RGDPB - RGDPA)/RGDPA)*100)
f4$RGDPB[ ] <- NA
f4$RGDPC[ ] <- NA

f5 <- f5 %>% 
  mutate(RGDPA = ((RGDPC - RGDPB)/RGDPB)*100)
f5$RGDPB[ ] <- NA
f5$RGDPC[ ] <- NA

f4 <- rbind(f4, f5)

# Renaming the columns
f4 <- rename(f4, `FORECASTER ID` = `ID`)

# Creating a new column: the type of indicator
f4$INDICATOR <- "RealGDP"

# Creating a new column with the TDIST
f4 <- f4 %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Reorganizing the columns
f4 <- f4 %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `RGDP1`, `RGDP2`, `RGDP3`, `RGDP4`, `RGDP5`, `RGDP6`, `RGDPA`, `RGDPB`, `RGDPC`, `RGDPD`, INDICATOR, TDIST)


############### f9 ######################
# 2009Q2-Present is 4 year forecasts, and chain weighted RGDP

f5 <- RGDP %>% 
  filter(YEAR >= 2009) %>%
  filter(YEAR != 2009 | QUARTER != 1)

# import the TS data into RGDPA
f5$RGDPE <- f5$RGDPD
f5$RGDPD <- f5$RGDPC
f5$RGDPC <- f5$RGDPB
f5$RGDPB <- f5$RGDPA
f5 <- select(f5, -c("RGDPA")) # take out RGDPA
f5 <- merge(f5, specialTS, by = c("YEAR", "QUARTER"))
f5 <- f5 %>%
  select(c(1:10, 15, 11:14))

# Clean it up
f5 <- rename(f5, `YEAR FORECAST MADE` = YEAR)
f5 <- f5 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

f6 <- f5 %>% 
  mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)

f7 <- f6 %>% 
  mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 2)

f8 <- f7 %>% 
  mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 3)


f5 <- f5 %>% 
  mutate(RGDPA = ((RGDPB - RGDPA)/RGDPA)*100)
f5$RGDPB[ ] <- NA
f5$RGDPC[ ] <- NA
f5$RGDPD[ ] <- NA
f5$RGDPE[ ] <- NA

f6 <- f6 %>% 
  mutate(RGDPA = ((RGDPC - RGDPB)/RGDPB)*100)
f6$RGDPB[ ] <- NA
f6$RGDPC[ ] <- NA
f6$RGDPD[ ] <- NA
f6$RGDPE[ ] <- NA

f7 <- f7 %>% 
  mutate(RGDPA = ((RGDPD - RGDPC)/RGDPC)*100)
f7$RGDPB[ ] <- NA
f7$RGDPC[ ] <- NA
f7$RGDPD[ ] <- NA
f7$RGDPE[ ] <- NA

f8 <- f8 %>% 
  mutate(RGDPA = ((RGDPE - RGDPD)/RGDPD)*100)
f8$RGDPB[ ] <- NA
f8$RGDPC[ ] <- NA
f8$RGDPD[ ] <- NA
f8$RGDPE[ ] <- NA

f9 <- rbind(f5,f6,f7,f8)

f9$Year.ID.ForecastYear.Quarter <- paste(f9$`YEAR BEING FORECAST`, f9$ID, f9$`YEAR FORECAST MADE`, f9$QUARTER, sep = "-")

# Renaming the columns
f9 <- rename(f9, `FORECASTER ID` = `ID`)

# Creating a new column: the type of indicator
f9$INDICATOR <- "RealGDP"

# Creating a new column with the TDIST
f9 <- f9 %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Reorganizing the columns
f9 <- f9 %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `RGDP1`, `RGDP2`, `RGDP3`, `RGDP4`, `RGDP5`, `RGDP6`, `RGDPA`, `RGDPB`, `RGDPC`, `RGDPD`, INDICATOR, TDIST)


############## RGDP PP ################
# NOTE: re: comparison of two methods, we can compare 1981Q3-1991Q4 Real GNP, then 1996Q1-Present Real GDP

# bind it all together, and RGDPA is your PP
#split into training and validation

RGDPpp <- rbind(f3, f4, f9)
# ------------------------------------------------------
fullGDP <- RGDPpp %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR)
write.csv(fullGDP, "/Users/Isaac/Downloads/TrainingData.csv", row.names = FALSE, na ="")

set.seed(11396)
trainingGDP <- RGDPpp %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR) %>%
  sample_frac(size = 0.5)
validationGDP <- RGDPpp[!(paste0(RGDPpp$Year.ID.ForecastYear.Quarter, RGDPpp$INDICATOR) %in% paste0(trainingGDP$Year.ID.ForecastYear.Quarter, trainingGDP$INDICATOR)),]

# Saving into CSV
write.csv(trainingGDP,"../Training and Validation/trainingGDP.csv", row.names = FALSE, na ="")
write.csv(validationGDP,"../Training and Validation/validationGDP.csv", row.names = FALSE, na ="")

trainingGDP <- read.csv("../Training and Validation/trainingGDP.csv", check.names = FALSE, stringsAsFactors = FALSE)
validationGDP <- read.csv("../Training and Validation/validationGDP.csv", check.names = FALSE, stringsAsFactors = FALSE)


##############
# just take the needed columns

RGDPpp <- select(trainingGDP, c(1:6, 13, 17:18))

actuals_rgnp <- read_excel("../FRED Data/GNPC96.xls", skip = 11, col_names = c("year", "value"))
actuals_rgdp <- read_excel("../FRED Data/GDPC1.xls", skip = 11, col_names = c("year", "value"))

# Creating a function called actual_bin 
# First line: names of the inputs of the function
# Goes row by row through the data, looking for year and quarter, then looking at indicators
# Pulls the actual for the data point by going to the FRED data and filtering it by year being forecast
# Then finds the interval the actual belongs in and returns the bin number

actual_bin <- Vectorize(function(year_forecast_made, quarter, year_being_forecast, indicator, return = c("actual")) {
  yq_forecast_made <- as.yearqtr(paste(year_forecast_made, quarter, sep = "q"))
  if (indicator=="RealGNP") {
    actual <- actuals_rgnp$value[as.Date(actuals_rgnp$year)==as.Date(paste0(year_being_forecast,"-01-01"), format = "%Y-%m-%d")]
  } else if (indicator=="RealGDP") {
    actual <- actuals_rgdp$value[as.Date(actuals_rgdp$year)==as.Date(paste0(year_being_forecast,"-01-01"), format = "%Y-%m-%d")]
  } 
  
  if (all(return=="actual") & length(actual)>0) {
    return(actual)
  } else {
    return(NA)
  }
})


actuals_table <- RGDPpp %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR) %>%
  summarise() %>%
  mutate(actual = actual_bin(year_forecast_made = `YEAR FORECAST MADE`, quarter = QUARTER, year_being_forecast = `YEAR BEING FORECAST`, indicator = INDICATOR, return = "actual"))

RGDPpp <- merge(x = RGDPpp, y = actuals_table, by = c("YEAR FORECAST MADE", "QUARTER", "YEAR BEING FORECAST", "INDICATOR"), all.x = TRUE) 

##############################################
# NOTE*: Real GDP from 1992 Q1 – 1995Q4, but it’s FIXED WEIGHTED
# that's true for the prob dist data too, but maybe not true for FRED data?
RGDPpp <- RGDPpp %>%
   filter(`YEAR FORECAST MADE` < 1992 | `YEAR FORECAST MADE` >= 1996)

RGDPpp <- RGDPpp %>%
  filter(`YEAR FORECAST MADE` >= 1996)


############## START ANALYSIS RGDP PP ################
# think about this one
# it should be %
# so if pp 14% and the actual 21% pessimism
RGDPpp <- RGDPpp[is.na(RGDPpp$actual)==FALSE,]
RGDPpp <- RGDPpp[is.na(RGDPpp$RGDPA)==FALSE,]

mean(RGDPpp$RGDPA)
mean(RGDPpp$actual)
t.test(RGDPpp$RGDPA, RGDPpp$actual, paired = TRUE, alternative = "two.sided")


rgdp_pp_re <- lmer((RGDPA-actual) ~ 1 + (1|`FORECASTER ID`) + (1|`YEAR BEING FORECAST`), RGDPpp)
summary(rgdp_pp_re)


# additional analysis
# If you remove YBF, we observe a positive coefficient (optimism). 
# Treating YBF as a fixed effect might be erroneous because forecasts in the same YBF should be correlated? 
# Stick in recession year as a covariate – the years in which forecasters will look optimistic is when YBF are recession years 

recessionyrs <- read_excel("../FRED Data/RECESSION.xls", skip = 11, col_names = c("RECYEAR", "value"))
recessionyrs$`RECYEAR` <- substr(recessionyrs$`RECYEAR`, 1, 4)
recessionyrs <- recessionyrs %>%
  mutate(RECESSION = ifelse(value == 0, "NO", "YES")) 
recessionyrs <- as.data.frame(recessionyrs)
recessionyrs$RECESSION <- as.factor(recessionyrs$RECESSION)
recessionyrs <- select(recessionyrs, `RECYEAR`, RECESSION)

RGDPpp <- RGDPpp %>% 
  mutate(`RECYEAR` = `YEAR BEING FORECAST`)
RGDPpp <- merge(x = RGDPpp, y = recessionyrs, by = c("RECYEAR"), all.x = TRUE)

rgdp_pp <- lm((RGDPA-actual) ~ 1 + factor(`RECESSION`) + factor(`YEAR BEING FORECAST`) + factor(`FORECASTER ID`), RGDPpp)
summary(rgdp_pp)







##########################################################
# Optimism and pessimism in probability forecasts (histogram)
# Compute a weighted point prediction for each histogram forecast, treating each bin as its midpoint and weighting it by its assigned probability. 
# We already have this: pred_average

RGDPhist <- training %>% 
  filter(INDICATOR == "RealGNP" | INDICATOR == "RealGDP") %>% 
  select(c(1:8, 24:64)) %>% 
  filter(`YEAR BEING FORECAST` != 1981)

# Conduct a t-test comparing weighted hist with outcomes. 
# If this test is significant, conduct a regression on the difference between forecast and outcome, including fixed effects for forecaster and year being forecast. 

RGDPhist <- RGDPhist[is.na(RGDPhist$actual)==FALSE,]
RGDPhist <- RGDPhist[is.na(RGDPhist$pred_average)==FALSE,]
mean(RGDPhist$pred_average)
mean(RGDPhist$actual)
t.test(RGDPhist$pred_average, RGDPhist$actual, paired = TRUE, alternative = "two.sided")


rgdpH_re <- lmer((pred_average-actual) ~ 1 + (1|`FORECASTER ID`) + (1|`YEAR BEING FORECAST`), RGDPhist)
summary(rgdpH_re)


##########################################################
# Do point predictions show more optimism than histogram distributions do? 
# Conduct a paired t-test comparing point-predictions and weighted point prediction forecasts. 
# If this test is significant, conduct a regression on the difference between them, including fixed effects for forecaster and year being forecast. 

# NOTE: re: comparison of two methods, we can compare 1981Q3-1991Q4 Real GNP, then 1996Q1-Present Real GDP
RGDPhistsub <- select(RGDPhist, c(1:5, 7, "TDIST", "actual", "pred_average"))

RGDPall <- merge(x = RGDPpp, y = RGDPhistsub, by = c("YEAR FORECAST MADE", "QUARTER", "YEAR BEING FORECAST", "INDICATOR", "FORECASTER ID", "TDIST", "actual", "Year.ID.ForecastYear.Quarter"), all.x = TRUE) 

# only 2485 observations!!
check <- RGDPall %>% 
  filter(is.na(RGDPA) == FALSE & is.na(pred_average) == FALSE)

# RGDPall <- RGDPall %>% 
#   filter(`YEAR FORECAST MADE` <= 1992 | `YEAR FORECAST MADE` >= 1996) 

# only 3104 observations!!
check <- RGDPall %>% 
  filter(is.na(RGDPA) == FALSE & is.na(pred_average) == FALSE)



RGDPall <- RGDPall[is.na(RGDPall$RGDPA)==FALSE,]
RGDPall <- RGDPall[is.na(RGDPall$pred_average)==FALSE,]
cor(RGDPall$pred_average, RGDPall$RGDPA)
mean(RGDPall$RGDPA)
mean(RGDPall$pred_average)

t.test(RGDPall$pred_average, RGDPall$RGDPA, paired = TRUE, alternative = "two.sided")



rgdpPPhist <- lmer((pred_average-RGDPA) ~ 1 + (1 | `FORECASTER ID`) + (1 | `YEAR BEING FORECAST`), RGDPall)
summary(rgdpPPhist)

##########################################################
# Are forecasters more optimistic during economic booms?  
# Conduct a regression at the level of the forecast. 
# The difference between forecast point prediction and actual outcome is the dependent variable. The key independent variable is a dummy variable for whether the prior year was a recession year. Include fixed effects for forecaster.

# working with RGDPpp


recessionyrs <- read_excel("../FRED Data/RECESSION.xls", skip = 11, col_names = c("RECYEAR", "value"))
recessionyrs$`RECYEAR` <- substr(recessionyrs$`RECYEAR`, 1, 4)
recessionyrs <- recessionyrs %>%
  mutate(RECESSION = ifelse(value == 0, "NO", "YES")) 
recessionyrs <- as.data.frame(recessionyrs)
recessionyrs$RECESSION <- as.factor(recessionyrs$RECESSION)
recessionyrs <- select(recessionyrs, `RECYEAR`, RECESSION)

RGDPpp <- RGDPpp %>% 
  mutate(`RECYEAR` = `YEAR FORECAST MADE` - 1)
RGDPpp <- merge(x = RGDPpp, y = recessionyrs, by = c("RECYEAR"), all.x = TRUE)


RGDPpprec_re <- lmer((RGDPA-actual) ~ RECESSION + (1|`FORECASTER ID`), RGDPpp)
summary(RGDPpprec_re)



# Conduct the above analysis, but using weighted point prediction.
RGDPhist <- RGDPhist %>% 
  mutate(`RECYEAR` = `YEAR FORECAST MADE` - 1)
RGDPhist <- merge(x = RGDPhist, y = recessionyrs, by = c("RECYEAR"), all.x = TRUE)


RGDPhistrec_re <- lmer((pred_average-actual) ~ RECESSION + (1|`FORECASTER ID`), RGDPhist)
summary(RGDPhistrec_re)




#additional analysis
#Conduct analysis with year forecast made as the recession year

RGDPpp <- RGDPpp %>% 
  mutate(`RECYEAR` = `YEAR FORECAST MADE`)
RGDPpp <- merge(x = RGDPpp, y = recessionyrs, by = c("RECYEAR"), all.x = TRUE)

RGDPpprec <- lm((RGDPA-actual) ~ RECESSION.y + factor(`FORECASTER ID`), RGDPpp)
summary(RGDPpprec)



RGDPhist <- RGDPhist %>% 
  mutate(`RECYEAR` = `YEAR FORECAST MADE`)
RGDPhist <- merge(x = RGDPhist, y = recessionyrs, by = c("RECYEAR"), all.x = TRUE)

RGDPhistrec_re <- lmer((pred_average-actual) ~ RECESSION.y + (1|`FORECASTER ID`), RGDPhist)
summary(RGDPhistrec_re)



