#######################################################
#
#   R analysis code for the FEO-SPF project
#   Analyzing data from the Survey of Professional Forecasters
#   Sandy Campbell
#   May 22, 2020
#
#   Run this script after running the scripts to wrangle the data files PRGDP, PRUNEMP, PRCCPI, PRCPCE
#
#######################################################
# # 
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("scales")
# install.packages("zoo")


library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(readxl, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)
library(zoo, warn.conflicts = FALSE)
# 
# library(tidyr)
# library(readxl)
# library(ggplot2)
# library(scales)
# library(zoo)
# library(lmerTest)
# library(lme4)

# This file builds off the reorganized data files for the different economic indicators
# Starting by loading in the cleaned csv files GDP, UNEMP, CCPI, CPCE

GDP <- read.csv("./data/CleanData/Python/GDP.csv", check.names = FALSE, stringsAsFactors = FALSE)
UNEMP <- read.csv("./data/CleanData/Python/UNEMP.csv", check.names = FALSE, stringsAsFactors = FALSE)
CCPI <- read.csv("./data/CleanData/Python/CCPI.csv", check.names = FALSE, stringsAsFactors = FALSE)
CPCE <- read.csv("./data/CleanData/Python/CPCE.csv", check.names = FALSE, stringsAsFactors = FALSE)

actuals_rgdp <- read_excel("./data/RawData/ScrapedData/Actuals/GDP.xls", skip = 11, col_names = c("year", "value"))
actuals_unemp<- read_excel("./data/RawData/ScrapedData/Actuals/UNEMP.xls", skip = 11, col_names = c("year", "value"))
actuals_ccpi <- read_excel("./data/RawData/ScrapedData/Actuals/CCPI.xls", skip = 11, col_names = c("year", "value"))
actuals_cpce <- read_excel("./data/RawData/ScrapedData/Actuals/CPCE.xls", skip = 11, col_names = c("year", "value"))

# Fourth-quarter over fourth-quarter forecasts
# For CPI/PCE, filter for only Q4, divide current year Q4 by preious year's Q4, then change month/date format

actuals_ccpi <- actuals_ccpi[quarters(actuals_ccpi$year)=="Q4",]
actuals_ccpi$value <- ((actuals_ccpi$value/c(NA,actuals_ccpi$value[1:(length(actuals_ccpi$value)-1)]))-1)*100
actuals_ccpi$year <- as.Date(paste0(substr(actuals_ccpi$year, 1, 4),"-01-01"), format = "%Y-%m-%d")

actuals_cpce <- actuals_cpce[quarters(actuals_cpce$year)=="Q4",]
actuals_cpce$value <- ((actuals_cpce$value/c(NA,actuals_cpce$value[1:(length(actuals_cpce$value)-1)]))-1)*100
actuals_cpce$year <- as.Date(paste0(substr(actuals_cpce$year, 1, 4),"-01-01"), format = "%Y-%m-%d")

# Merge all datasets
spf <- bind_rows(GDP, CCPI, CPCE, UNEMP)
rm(GDP, CCPI, CPCE, UNEMP)

# ------------------------------------------------------

# Creating a function called actual_bin that tells you which bin the actual is in accounting for the yearqtr binning arrangement
# first line: names of the inputs of the function
# goes row by row through the data, looking for year and quarter, then looking at indicators
# pulls the actual for the data point by going to the FRED data and filtering it by year being forecast
# then finds the interval the actual belongs in and returns the bin number
here <- NA
actual_bin <- Vectorize(function(year_forecast_made, quarter, year_being_forecast, indicator, return = c("actual", "bin")) {
  yq_forecast_made <- as.yearqtr(paste(year_forecast_made, quarter, sep = "q"))
  if (indicator=="RealGDP") {
    actual <- actuals_rgdp$value[as.Date(actuals_rgdp$year)==as.Date(paste0(year_being_forecast,"-01-01"), format = "%Y-%m-%d")]
    if (yq_forecast_made>=as.yearqtr("1992q1") & yq_forecast_made<=as.yearqtr("2009q1")) {
      bin <- 10-findInterval(actual, c(-2, -1, 0, 1, 2, 3, 4, 5, 6))
    } else if (yq_forecast_made>=as.yearqtr("2009q2")) {
      bin <- 11-findInterval(actual, c(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6))
    }
  } else if (indicator=="Unemployment") {
    actual <- actuals_unemp$value[as.Date(actuals_unemp$year)==as.Date(paste0(year_being_forecast,"-01-01"), format = "%Y-%m-%d")]
    here <- actual
    if (yq_forecast_made>=as.yearqtr("2009q2") & yq_forecast_made<=as.yearqtr("2013q4")) {
      bin <- 10-findInterval(actual, c(6, 7, 7.5, 8, 8.5, 9, 9.5, 10, 11))
    } else if (yq_forecast_made>=as.yearqtr("2014q1")) {
      bin <- 10-findInterval(actual, c(4, 5, 5.5, 6, 6.5, 7, 7.5, 8, 9))
    }
  } else if (indicator=="Core CPI") {
    actual <- actuals_ccpi$value[as.Date(actuals_ccpi$year)==as.Date(paste0(year_being_forecast,"-01-01"), format = "%Y-%m-%d")]
    if (yq_forecast_made>=as.yearqtr("2007q1")) {
      bin <- 10-findInterval(actual, c(0, .5, 1, 1.5, 2, 2.5, 3, 3.5, 4))
    }
  } else if (indicator=="Core PCE") {
    actual <- actuals_cpce$value[as.Date(actuals_cpce$year)==as.Date(paste0(year_being_forecast,"-01-01"), format = "%Y-%m-%d")]
    if (yq_forecast_made>=as.yearqtr("2007q1")) {
      bin <- 10-findInterval(actual, c(0, .5, 1, 1.5, 2, 2.5, 3, 3.5, 4))
    }
  }
  
  if (all(return=="actual") & length(actual)>0) {
    return(actual)
  } else if (all(return=="bin") & length(actual)>0) {
    return(bin)
  } else {
    return(NA)
  }
})

# Apply the function to the data frame to get actuals_table

actuals_table <- spf %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR) %>%
  summarise() %>%
  mutate(actual = actual_bin(year_forecast_made = `YEAR FORECAST MADE`, quarter = QUARTER, year_being_forecast = `YEAR BEING FORECAST`, indicator = INDICATOR, return = "actual"),
         ACTUAL_BIN = actual_bin(year_forecast_made = `YEAR FORECAST MADE`, quarter = QUARTER, year_being_forecast = `YEAR BEING FORECAST`, indicator = INDICATOR, return = "bin"))
spf <- merge(x = spf, y = actuals_table, by = c("YEAR FORECAST MADE", "QUARTER", "YEAR BEING FORECAST", "INDICATOR"), all.x = TRUE) #want to keep all rows in SPF all.X, even if it doesn't merge in SPF table
rm( actuals_rgdp, actuals_unemp, actuals_ccpi, actuals_cpce)

# Analysis
spf$MAX <- apply(spf[,paste("BIN", 1:15)], 1, function(x) if (all(is.na(x))==FALSE) max(x, na.rm = TRUE) else NA)/100

# Number of bins that have the maximum confidence
spf$NUM_OF_MAX <- apply(spf[,paste("BIN", 1:15)], 1, function(x) if (all(is.na(x))==FALSE) sum(x==max(x, na.rm = TRUE), na.rm = TRUE) else NA)
spf$MAX[spf$MAX==0] <- NA
spf$NUM_OF_MAX[spf$MAX==0] <- NA

# Forecasted Confidence level for the actual bin
spf$ACTUAL_CONF <- spf[,paste("BIN", 1:15)][as.matrix(data.frame(row = 1:nrow(spf), col = spf$ACTUAL_BIN))]/100

# Hit 
spf$HIT <- ifelse(spf$ACTUAL_CONF==spf$MAX, 1/spf$NUM_OF_MAX, 0)

# ------------------------------------------------------
# Creating a function called midpoint_bin 
# first line: names of the inputs of the function
# creates a vector called bin_ranges: ranges of the bins according to indicator and yearq
# midpoint: calculates the midpoint of the bins
# nbin: returns the number of bins for that indicator and yearq 

midpoint_bin <- Vectorize(function(year_forecast_made, quarter, bin, indicator, return = c("midpoint", "nbin")) {
  yq_forecast_made <- as.yearqtr(paste(year_forecast_made, quarter, sep = "q"))
  if (indicator=="RealGDP") {
    if (yq_forecast_made>=as.yearqtr("1992q1") & yq_forecast_made<=as.yearqtr("2009q1")) {
      bin_ranges <- rev(c(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7))
      midpoint <- (bin_ranges[bin]+bin_ranges[bin+1])/2
      nbin <- length(bin_ranges)-1
    } else if (yq_forecast_made>=as.yearqtr("2009q2")) {
      bin_ranges <- rev(c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7))
      midpoint <- (bin_ranges[bin]+bin_ranges[bin+1])/2
      nbin <- length(bin_ranges)-1
    }
  } else if (indicator=="Unemployment") {
    if (yq_forecast_made>=as.yearqtr("2009q2") & yq_forecast_made<=as.yearqtr("2013q4")) {
      bin_ranges <- rev(c(5, 6, 7, 7.5, 8, 8.5, 9, 9.5, 10, 11, 12))
      midpoint <- (bin_ranges[bin]+bin_ranges[bin+1])/2
      nbin <- length(bin_ranges)-1
    } else if (yq_forecast_made>=as.yearqtr("2014q1")) {
      bin_ranges <- rev(c(3, 4, 5, 5.5, 6, 6.5, 7, 7.5, 8, 9, 10))
      midpoint <- (bin_ranges[bin]+bin_ranges[bin+1])/2
      nbin <- length(bin_ranges)-1
    }
  } else if (indicator=="Core CPI") {
    if (yq_forecast_made>=as.yearqtr("2007q1")) {
      bin_ranges <- rev(c(-0.5, 0, .5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5))
      midpoint <- (bin_ranges[bin]+bin_ranges[bin+1])/2
      nbin <- length(bin_ranges)-1
    }
  } else if (indicator=="Core PCE") {
    if (yq_forecast_made>=as.yearqtr("2007q1")) {
      bin_ranges <- rev(c(-0.5, 0, .5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5))
      midpoint <- (bin_ranges[bin]+bin_ranges[bin+1])/2
      nbin <- length(bin_ranges)-1
    }
  }
  
  if (all(return=="midpoint") & length(midpoint)>0) {
    return(midpoint)
  } else if (all(return=="nbin") & length(nbin)>0) {
    return(nbin)
  } else {
    return(NA)
  }
})

spf$Indicator.Year.ID.ForecastYear.Quarter <- paste0(spf$INDICATOR, "-", spf$Year.ID.ForecastYear.Quarter)

# There are cases where bin_value_sum adds up to something other than 100 (range: 99 and 101)
# Change data to long form
spfl <- spf %>%
  gather(key = "BIN", value = "bin_value", `BIN 1`:`BIN 15`) %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR, `FORECASTER ID`) %>%
  mutate(bin_value_sum = sum(bin_value, na.rm = TRUE),
         bin_value_sum = ifelse(bin_value_sum==0, NA, bin_value_sum),
         bin_prob = bin_value/bin_value_sum)

# Somebody entered blank (NA) instead of 0 for a bin
# Fix 3 cases were bin value is NA, but bin_prob sums to 100: Core PCE-2014-563-2014-3, RealGDP-2017-568-2015-1, Core CPI-2015-512-2015-1
spfl$bin_prob[is.na(spfl$bin_prob)==TRUE & spfl$bin_value_sum==100] <- 0

# bin_num gets the bin number from e.g., 'BIN 10'
# filter(bin_num<=nbin) selects the rows for which BINs don't have NA
midpoint_table <- spfl %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, BIN, INDICATOR) %>%
  summarise() %>%
  mutate(bin_num = as.numeric(substr(BIN, 5, nchar(BIN))),
         bin_midpoint = midpoint_bin(year_forecast_made = `YEAR FORECAST MADE`, quarter = QUARTER, bin = bin_num, indicator = INDICATOR, return = "midpoint"),
         nbin = midpoint_bin(year_forecast_made = `YEAR FORECAST MADE`, quarter = QUARTER, bin = bin_num, indicator = INDICATOR, return = "nbin")) %>%
  filter(bin_num<=nbin) # Delete the rows for the extra bins (RGNP had 6 bins, 
spfl <- merge(x = spfl, y = midpoint_table, by = c("YEAR FORECAST MADE", "QUARTER", "BIN", "INDICATOR"), all.x = TRUE)

# spf long and predicted average and variances for each forecast
spfl <- spfl %>%
  filter(bin_num<=nbin) %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR, `FORECASTER ID`) %>%
  mutate(weighted_midpoint = bin_prob*bin_midpoint,
         pred_average = sum(weighted_midpoint, na.rm = TRUE),
         pred_average = ifelse(is.na(bin_prob), NA, pred_average),
         pred_var_bin = ((bin_midpoint-pred_average)^2)*bin_prob,
         pred_var = sum(pred_var_bin, na.rm = TRUE),
         pred_var = ifelse(is.na(bin_prob), NA, pred_var)) %>%
  select(-weighted_midpoint, -pred_var_bin) %>%
  arrange(INDICATOR, `YEAR FORECAST MADE`, QUARTER, `FORECASTER ID`, `YEAR BEING FORECAST`, bin_num)

# Converting from long to wide format: take the columns that vary by bin 
    # aka the columns you want to convert from long to wide (e.g., Bin 1 then value, Bin 1 then midpoint)
# unite = paste, making a column 'key' by taking 'BIN and key' (key has bin value, prob, and midpoint)
# gsub = see "X", replace with "Y" 
# spread = converts to wide format
spf2 <- spfl %>%
  select(-bin_num) %>%
  gather(key = "key", value = "value", bin_value, bin_prob, bin_midpoint) %>%
  unite(key, BIN, key) %>%
  mutate(key = gsub("_bin_value", "", key),
         key = gsub("_bin_prob", " prob", key),
         key = gsub("_bin_midpoint", " midpoint", key)) %>%
  spread(key = key, value = value) %>%
  select(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR, `FORECASTER ID`, INDUSTRY, Year.ID.ForecastYear.Quarter, Indicator.Year.ID.ForecastYear.Quarter, paste("BIN", 1:11), paste("BIN", 1:11, "prob"), paste("BIN", 1:11, "midpoint"), nbin, bin_value_sum, TDIST, actual, ACTUAL_BIN, MAX, NUM_OF_MAX, ACTUAL_CONF, HIT, pred_average, pred_var)

# gets the rows in the same order as original SPF file (messed up bc of spread)
spf <- spf2[match(spf$Indicator.Year.ID.ForecastYear.Quarter, spf2$Indicator.Year.ID.ForecastYear.Quarter),]
rm(spf2)

# ------------------------------------------------------
# Save Full Data Frame
final <- spf %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR)
write.csv(final, "./data/CleanedData/RProcessed.csv", row.names = FALSE, na ="")
# 
# set.seed(11396)
# training <- spf %>%
#   group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR) %>%
#   sample_frac(size = 0.5)
# validation <- spf[!(paste0(spf$Year.ID.ForecastYear.Quarter, spf$INDICATOR) %in% paste0(training$Year.ID.ForecastYear.Quarter, training$INDICATOR)),]
# 
# # Saving into CSV
# write.csv(training,"./data/TrainingData/training_fromR.csv", row.names = FALSE, na ="")
# write.csv(validation,"./data/ValidationData/validation_fromR.csv", row.names = FALSE, na ="")
print("Finished Saving CSV!")

