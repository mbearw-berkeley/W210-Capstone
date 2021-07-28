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

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(scales)
library(zoo)
library(lmerTest)
library(lme4)

# This file builds off the reorganized data files for the different economic indicators
# Starting by loading in the cleaned csv files GDP, UNEMP, CCPI, CPCE

GDP <- read.csv("/Users/Isaac/Downloads/GDP.csv", check.names = FALSE, stringsAsFactors = FALSE)
UNEMP <- read.csv("/Users/Isaac/Downloads/UNEMP.csv", check.names = FALSE, stringsAsFactors = FALSE)
CCPI <- read.csv("/Users/Isaac/Downloads/CCPI.csv", check.names = FALSE, stringsAsFactors = FALSE)
CPCE <- read.csv("/Users/Isaac/Downloads/CPCE.csv", check.names = FALSE, stringsAsFactors = FALSE)

actuals_ngnp <- read_excel("/Users/Isaac/Downloads/GNP.xls", skip = 11, col_names = c("year", "value"))
actuals_rgnp <- read_excel("/Users/Isaac/Downloads/GNPC96.xls", skip = 11, col_names = c("year", "value"))
actuals_rgdp <- read_excel("/Users/Isaac/Downloads/GDPC1.xls", skip = 11, col_names = c("year", "value"))
actuals_unemp <- read_excel("/Users/Isaac/Downloads/UNRATE.xls", skip = 11, col_names = c("year", "value"))
actuals_ccpi <- read_excel("/Users/Isaac/Downloads/CPILFESL.xls", skip = 11, col_names = c("year", "value"))
actuals_cpce <- read_excel("/Users/Isaac/Downloads/PCEPILFE.xls", skip = 11, col_names = c("year", "value"))

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

actual_bin <- Vectorize(function(year_forecast_made, quarter, year_being_forecast, indicator, return = c("actual", "bin")) {
  yq_forecast_made <- as.yearqtr(paste(year_forecast_made, quarter, sep = "q"))
  if (indicator=="NominalGNP") {
    actual <- actuals_ngnp$value[as.Date(actuals_ngnp$year)==as.Date(paste0(year_being_forecast,"-01-01"), format = "%Y-%m-%d")]
    if (yq_forecast_made>=as.yearqtr("1968q4") & yq_forecast_made<=as.yearqtr("1973q1")) {
      bin <- 15-findInterval(actual, c(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    } else if (yq_forecast_made>=as.yearqtr("1973q2") & yq_forecast_made<=as.yearqtr("1974q3")) {
      bin <- 15-findInterval(actual, c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    } else if (yq_forecast_made>=as.yearqtr("1974q4") & yq_forecast_made<=as.yearqtr("1981q2")) {
      bin <- 15-findInterval(actual, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
    }
  } else if (indicator=="RealGNP") {
    actual <- actuals_rgnp$value[as.Date(actuals_rgnp$year)==as.Date(paste0(year_being_forecast,"-01-01"), format = "%Y-%m-%d")]
    if (yq_forecast_made>=as.yearqtr("1981q3") & yq_forecast_made<=as.yearqtr("1991q4")) {
      bin <- 6-findInterval(actual, c(-2, 0, 2, 4, 6))
    }
  } else if (indicator=="RealGDP") {
    actual <- actuals_rgdp$value[as.Date(actuals_rgdp$year)==as.Date(paste0(year_being_forecast,"-01-01"), format = "%Y-%m-%d")]
    if (yq_forecast_made>=as.yearqtr("1992q1") & yq_forecast_made<=as.yearqtr("2009q1")) {
      bin <- 10-findInterval(actual, c(-2, -1, 0, 1, 2, 3, 4, 5, 6))
    } else if (yq_forecast_made>=as.yearqtr("2009q2")) {
      bin <- 11-findInterval(actual, c(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6))
    }
  } else if (indicator=="Unemployment") {
    actual <- actuals_unemp$value[as.Date(actuals_unemp$year)==as.Date(paste0(year_being_forecast,"-01-01"), format = "%Y-%m-%d")]
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
rm(actuals_ngnp, actuals_rgnp, actuals_rgdp, actuals_unemp, actuals_ccpi, actuals_cpce)

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
  if (indicator=="NominalGNP") {
    if (yq_forecast_made>=as.yearqtr("1968q4") & yq_forecast_made<=as.yearqtr("1973q1")) {
      bin_ranges <- rev(c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
      midpoint <- (bin_ranges[bin]+bin_ranges[bin+1])/2
      nbin <- length(bin_ranges)-1
    } else if (yq_forecast_made>=as.yearqtr("1973q2") & yq_forecast_made<=as.yearqtr("1974q3")) {
      bin_ranges <- rev(c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
      midpoint <- (bin_ranges[bin]+bin_ranges[bin+1])/2
      nbin <- length(bin_ranges)-1
    } else if (yq_forecast_made>=as.yearqtr("1974q4") & yq_forecast_made<=as.yearqtr("1981q2")) {
      bin_ranges <- rev(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
      midpoint <- (bin_ranges[bin]+bin_ranges[bin+1])/2
      nbin <- length(bin_ranges)-1
    }
  } else if (indicator=="RealGNP") {
    if (yq_forecast_made>=as.yearqtr("1981q3") & yq_forecast_made<=as.yearqtr("1991q4")) {
      bin_ranges <- rev(c(-4, -2, 0, 2, 4, 6, 8))
      midpoint <- (bin_ranges[bin]+bin_ranges[bin+1])/2
      nbin <- length(bin_ranges)-1
    }
  } else if (indicator=="RealGDP") {
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
  select(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR, `FORECASTER ID`, INDUSTRY, Year.ID.ForecastYear.Quarter, Indicator.Year.ID.ForecastYear.Quarter, paste("BIN", 1:15), paste("BIN", 1:15, "prob"), paste("BIN", 1:15, "midpoint"), nbin, bin_value_sum, TDIST, actual, ACTUAL_BIN, MAX, NUM_OF_MAX, ACTUAL_CONF, HIT, pred_average, pred_var)

# gets the rows in the same order as original SPF file (messed up bc of spread)
spf <- spf2[match(spf$Indicator.Year.ID.ForecastYear.Quarter, spf2$Indicator.Year.ID.ForecastYear.Quarter),]
rm(spf2)

# ------------------------------------------------------
# Save Full Data Frame
final <- spf %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR)
write.csv(final, "/Users/Isaac/Downloads/fulldata.csv", row.names = FALSE, na ="")

set.seed(11396)
training <- spf %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR) %>%
  sample_frac(size = 0.5)
validation <- spf[!(paste0(spf$Year.ID.ForecastYear.Quarter, spf$INDICATOR) %in% paste0(training$Year.ID.ForecastYear.Quarter, training$INDICATOR)),]

# Saving into CSV
write.csv(training,"../Training and Validation/training.csv", row.names = FALSE, na ="")
write.csv(validation,"../Training and Validation/validation.csv", row.names = FALSE, na ="")

training <- read.csv("../Training and Validation/training.csv", check.names = FALSE, stringsAsFactors = FALSE)
validation <- read.csv("../Training and Validation/validation.csv", check.names = FALSE, stringsAsFactors = FALSE)
spf <- rbind(training, validation)

# ------------------------------------------------------
table(table(unique(training$`FORECASTER ID`)))
table(table(unique(spf$`FORECASTER ID`)))

numforecasts <- training %>% group_by(`FORECASTER ID`) %>% summarise(n=n())
spfnumforecasts <- spf %>% group_by(`FORECASTER ID`) %>% summarise(n=n())

spftest <- unique(spf[,c("INDUSTRY", "FORECASTER ID")])
table(spftest$INDUSTRY)

trainingna <- training[is.na(training$actual)==TRUE,]
trainingmax <- training[is.na(training$MAX)==TRUE,]
trainingdouble <- training[is.na(training$actual)==TRUE & is.na(training$MAX)==TRUE,]
rm(trainingna, trainingmax, trainingdouble)

training <- training[is.na(training$MAX)==FALSE,]
training <- training[is.na(training$actual)==FALSE,]

# 1. Hit Rate Analysis: Peak confidence vs. hit rate. 
mean(training$MAX)
mean(training$HIT)

t.test(training$MAX, mu = mean(training$HIT), alternative = "two.sided")


trainingtest <- training %>%   
  group_by(`FORECASTER ID`) %>%
  mutate(n=n()) %>% 
  mutate(avgpeak = mean(MAX)) %>% 
  mutate(avghit = mean(HIT)) %>% 
  group_by(`FORECASTER ID`, avgpeak, avghit) %>% 
  summarise()

mean(trainingtest$avgpeak)
mean(trainingtest$avghit)
t.test(trainingtest$avgpeak, trainingtest$avghit, paired = TRUE, alternative = "two.sided")




peak_hit_ave <- training %>%
  group_by(INDICATOR) %>%
  summarise(peak_mean = mean(MAX),
            peak_se = sd(MAX)/sqrt(n()),
            n = n(),
            hit_rate = mean(HIT),
            t_test = t.test(MAX, mu = hit_rate, alternative = "two.sided")$p.value) %>%
  mutate(label = paste0(INDICATOR,'\nN = ', n))

fig <- peak_hit_ave %>%
  ggplot(aes(x = label, y = peak_mean, color = "Peak Confidence")) + 
  geom_point() + geom_point(aes(y = hit_rate, color = "Hit Rate"), size=3) + 
  geom_errorbar(aes(ymin = peak_mean-1.96*peak_se, ymax = peak_mean+1.96*peak_se), width=0.2, size=0.6) + 
  scale_colour_manual(name="", values = c("Peak Confidence"="indianred1", "Hit Rate"="steelblue3")) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size=15), plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), legend.position="bottom") + 
  scale_y_continuous(labels = scales::percent) + ylab("") + ggtitle("Peak Confidence and Hit Rate") 

plot(fig)


# Lump GDP in graph

unique(training$INDICATOR)

lumpind <- function(x) {
  if(x=="NominalGNP" | x=="RealGNP" | x=="RealGDP") {
    "GDP"
} else if (x=="Core CPI") {
  "Core CPI"
} else if (x== "Core PCE") {
  "Core PCE"
} else {
  "Unemployment"
}}

traininggraphs <- training

traininggraphs$INDICATOR <- apply(array(training[["INDICATOR"]]),MARGIN=1, FUN=lumpind)

peak_hit_ave <- traininggraphs %>%
  group_by(INDICATOR) %>%
  summarise(peak_mean = mean(MAX),
            peak_se = sd(MAX)/sqrt(n()),
            n = n(),
            hit_rate = mean(HIT),
            t_test = t.test(MAX, mu = hit_rate, alternative = "two.sided")$p.value) %>%
  mutate(label = paste0(INDICATOR,'\nN = ', n))


fig <- peak_hit_ave %>%
  ggplot(aes(x = label, y = peak_mean, color = "Peak Confidence")) + 
  geom_point() + geom_point(aes(y = hit_rate, color = "Hit Rate"), size=3) + 
  geom_errorbar(aes(ymin = peak_mean-1.96*peak_se, ymax = peak_mean+1.96*peak_se), width=0.2, size=0.6) + 
  scale_colour_manual(name="", values = c("Peak Confidence"="indianred1", "Hit Rate"="steelblue3")) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size=15), plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), legend.position="bottom") + 
  scale_y_continuous(labels = scales::percent) + ylab("") + ggtitle("Peak Confidence and Hit Rate") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.3))

plot(fig)


# Plot the calibration curve

# compute average confidence and hit rate for all bins, conditional on confidence grouped into 10% ranges
# first melt everything into long form, making a data frame that has one row per bin
finalbins <- training[c(9:23,58)] #select just the bins into the "finalbins" data frame
# create a variable "PeakBin" that identifies which bin gets the peak confidence
# identify which bin number 
finalbins$PeakBin <- max.col(finalbins[(c(1:15))],ties.method = "random")
# finalbins$PeakBin <- max.col(replace(finalbins[(c(1:15))], is.na(finalbins[(c(1:15))]), -Inf), ties.method="random")



library(reshape2)
orpb <- melt(finalbins,id=c("ACTUAL_BIN","PeakBin")) #create a "one row per bin" data frame
orpb <- plyr::rename(orpb, c("value" = "conf","variable" = "bin"))

orpb$confcat <- plyr::round_any(orpb$conf,.1,f=floor) #lump into confidence categories
orpb$conf <- orpb$conf/100

# identify hits
orpb$binno <- as.integer(orpb$bin)
orpb$hit <- orpb$binno == orpb$ACTUAL_BIN

# lump into confidence categories
orpb$confcat <- plyr::round_any(orpb$conf,.1,f=floor)

# identify hits
orpb$Hit <- ifelse(orpb$hit == FALSE, 0, 1)

# average hits and confidence over confidence categories
hitsaveraged <- aggregate(Hit~confcat,orpb,mean)
confaveraged <- aggregate(conf~confcat,orpb,mean)
confcats <- merge(hitsaveraged,confaveraged, by="confcat")
confcats <- melt(confcats,id="confcat")

# add frequency counts to the table
bincounts <- as.data.frame(table(orpb$confcat))
bincounts <- plyr::rename(bincounts,c("Var1" = "confcat","Freq"="freq"))
confcats <- merge(confcats,bincounts,id="confcat")

# add standard errors to the table
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))) #define standard error function
hitsstderr <- aggregate(Hit~confcat,orpb,stderr)
confstderr <- aggregate(conf~confcat,orpb,stderr)
confcatserr <- merge(hitsstderr,confstderr, by="confcat")
confcatserr <- reshape2::melt(confcatserr,id="confcat")
confcatserr <- plyr::rename(confcatserr, c("value" = "stderr"))
confcats <- merge(confcats,confcatserr, by=c("confcat","variable"))

fig <- merge(bincounts,confaveraged,by="confcat")
fig <- merge(fig,hitsaveraged,by="confcat")
fig <- merge(fig,hitsstderr,by="confcat")
colnames(fig)[colnames(fig)=="Hit.x"]<-"Hit"
colnames(fig)[colnames(fig)=="Hit.y"]<-"hitse"
colnames(fig)[colnames(fig)=="confcat"]<-"variable"
fig$variable <- c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0)

# plot the calibration curve
ggplot(data=fig,aes(x=conf,y=Hit))+ #defines x, y, and color variable
  geom_line(size=1.2,colour="indianred1")  +                      #sets line width
  geom_errorbar(aes(ymin=Hit-hitse, ymax=Hit+hitse), width=.02) + #adds error bars
  ylab("Accuracy")+              #y-axis label
  xlab("Confidence")  +        #x-axis label 
  geom_line(data = fig, aes(x = variable, y = variable), color = "steelblue3") + 
  ggtitle("Confidence and Hit Rate") + theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5), text = element_text(size=10), plot.title = element_text(hjust = 0.5)) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour = "gray")) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
      text = element_text(size=20, family = "Open Sans"),
      axis.title.x = element_text(hjust = 0.5))









# 2. Variance Analysis
# Compute the variance of each forecast, by computing the variance of the distribution. 
# To do this, compute the distribution’s mean, then sum the squared distance to each bin, weighted by the probability assigned to it. 
# Conduct a one-sample t-test comparing variance for each forecast with the average variance of realized outcomes across the entire epoch covered by the data.

# Calculating variance of the actuals aka "average variance of realized outcomes" 
# run "200526_actualsvariance.R" 
var_actuals <- actuals_wide2 %>% 
  select(INDICATOR, yearqtr, act_average, act_var, act_Area, act_RAW_GINI, act_ADJ_GINI) 

CORE <- training %>% 
  filter(INDICATOR=="Core CPI" | INDICATOR == "Core PCE") %>% 
  filter(`YEAR FORECAST MADE` > 2006) %>%
  mutate(yearqtr = "2007Q1-Present") 
NGNP1 <- training %>% 
  filter(INDICATOR=="NominalGNP") %>% 
  filter(`YEAR FORECAST MADE` < 1974) %>%
  filter(`YEAR FORECAST MADE` != 1973 | QUARTER == 1) %>% 
  mutate(yearqtr = "1968Q4-1973Q1")
NGNP2 <- training %>% 
  filter(INDICATOR=="NominalGNP") %>% 
  filter(`YEAR FORECAST MADE` > 1972 & `YEAR FORECAST MADE` < 1975) %>%
  filter(`YEAR FORECAST MADE` != 1973 | QUARTER != 1) %>%
  filter(`YEAR FORECAST MADE` != 1974 | QUARTER != 4) %>%
  mutate(yearqtr = "1973Q2-1974Q3")
NGNP3 <- training %>% 
  filter(INDICATOR=="NominalGNP") %>% 
  filter(`YEAR FORECAST MADE` > 1973 & `YEAR FORECAST MADE` < 1982) %>%
  filter(`YEAR FORECAST MADE` != 1974 | QUARTER == 4) %>%
  filter(`YEAR FORECAST MADE` != 1981 | QUARTER < 3) %>%
  mutate(yearqtr = "1974Q4-1981Q2")
RGNP <- training %>% 
  filter(INDICATOR=="RealGNP") %>% 
  filter(`YEAR FORECAST MADE` > 1980 & `YEAR FORECAST MADE` < 1992) %>%
  filter(`YEAR FORECAST MADE` != 1981 | QUARTER > 2) %>%
  mutate(yearqtr = "1981Q3-1991Q4")
RGDP1 <- training %>% 
  filter(INDICATOR=="RealGDP") %>% 
  filter(`YEAR FORECAST MADE` > 1991 & `YEAR FORECAST MADE` < 2010) %>%
  filter(`YEAR FORECAST MADE` != 2009 | QUARTER == 1) %>%
  mutate(yearqtr = "1992Q1-2009Q1")
RGDP2 <- training %>% 
  filter(INDICATOR=="RealGDP") %>% 
  filter(`YEAR FORECAST MADE` > 2008) %>%
  filter(`YEAR FORECAST MADE` != 2009 | QUARTER > 1) %>%
  mutate(yearqtr = "2009Q2-Present")
UNEMP1 <- training %>% 
  filter(INDICATOR=="Unemployment") %>% 
  filter(`YEAR FORECAST MADE` > 2008 & `YEAR FORECAST MADE` < 2014) %>%
  filter(`YEAR FORECAST MADE` != 2009 | QUARTER != 1) %>%
  filter(`YEAR FORECAST MADE` != 2014 | QUARTER != 1) %>%
  mutate(yearqtr = "2009Q2-2013Q4")
UNEMP2 <- training %>% 
  filter(INDICATOR=="Unemployment") %>% 
  filter(`YEAR FORECAST MADE` > 2013) %>%
  filter(`YEAR FORECAST MADE` != 2013 | QUARTER > 4) %>%
  mutate(yearqtr = "2014Q1-Present")
training1 <- rbind(CORE, NGNP1, NGNP2, NGNP3, RGNP, RGDP1, RGDP2, UNEMP1, UNEMP2)
rm(CORE, NGNP1, NGNP2, NGNP3, RGNP, RGDP1, RGDP2, UNEMP1, UNEMP2)

training1 <- merge(x = training1, y = var_actuals, by = c("INDICATOR", "yearqtr"), all.x = TRUE)

# Calculating variance of each forecast, by indicator
# this is going to give me a single number for each indicator
var_forecasts <- training1 %>% 
  group_by(INDICATOR, yearqtr) %>% 
  summarise(varforecasts = mean(pred_var))

mean(training1$pred_var)
mean(training1$act_var)
t.test(training1$pred_var, mu=mean(training1$act_var), alternative = "two.sided")


trainingtest <- training1 %>%   
  group_by(`FORECASTER ID`) %>%
  mutate(n=n()) %>% 
  mutate(avgpred_var = mean(pred_var)) %>% 
  mutate(avgact_var = mean(act_var)) %>% 
  group_by(`FORECASTER ID`, avgpred_var, avgact_var) %>% 
  summarise()

mean(trainingtest$avgpred_var)
mean(trainingtest$avgact_var)
t.test(trainingtest$avgpred_var, mu=mean(trainingtest$avgact_var), alternative = "two.sided")



### plots
traininggraphs <- training1

traininggraphs$INDICATOR <- apply(array(training[["INDICATOR"]]),MARGIN=1, FUN=lumpind)

variance_ave <- traininggraphs %>%
  group_by(INDICATOR) %>%
  summarise(forecast_var = mean(pred_var),
            se = sd(pred_var)/sqrt(n()),
            n = n(),
            actual_var = mean(act_var),
            t_test = t.test(pred_var, mu = actual_var, alternative = "two.sided")$p.value) %>%
  mutate(label = paste0(INDICATOR,'\nN = ', n))

fig2 <- variance_ave %>%
  ggplot(aes(x = label, y = forecast_var, color = "Forecast Var")) + 
  geom_point() + geom_point(aes(y = actual_var, color = "Variance of Actuals"), size=3) + 
  geom_errorbar(aes(ymin = forecast_var-1.96*se, ymax = forecast_var+1.96*se), width=0.2, size=0.6) + 
  scale_colour_manual(name="", values = c("Forecast Var"="indianred1", "Variance of Actuals"="steelblue3")) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size=15), plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), legend.position="bottom") + 
  scale_y_continuous() + ylab("") + ggtitle("Variance") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.3))

plot(fig2)


# 3. Gini Coefficient Analysis
# Conduct a one-sample t-test comparing Gini coefficient for each forecast with the average Gini of realized outcomes across the entire epoch covered by the data.

#### Calculating Gini of Forecasts
func <- function(x) {
  vec <- as.numeric(sort(x[25:39], decreasing = TRUE))
  if (x[1] == "Core PCE" & x[2] == "2007Q1-Present") {
    area <- (1/10)*(vec[1]*0.5 + vec[2]*1.5 + vec[3]*2.5 + vec[4]*3.5 + vec[5]*4.5 + vec[6]*5.5 + vec[7]*6.5 + vec[8]*7.5 + vec[9]*8.5 + vec[10]*9.5)
  } else if (x[1] == "NominalGNP" & x[2] == "1968Q4-1973Q1") {
    area <- (1/15)*(vec[1]*0.5 + vec[2]*1.5 + vec[3]*2.5 + vec[4]*3.5 + vec[5]*4.5 + vec[6]*5.5 + vec[7]*6.5 + vec[8]*7.5 + vec[9]*8.5 + vec[10]*9.5 + vec[11]*10.5 + vec[12]*11.5 + vec[13]*12.5 + vec[14]*13.5 + vec[15]*14.5)
  } else if (x[1] == "NominalGNP" & x[2] == "1973Q2-1974Q3") {
    area <- (1/15)*(vec[1]*0.5 + vec[2]*1.5 + vec[3]*2.5 + vec[4]*3.5 + vec[5]*4.5 + vec[6]*5.5 + vec[7]*6.5 + vec[8]*7.5 + vec[9]*8.5 + vec[10]*9.5 + vec[11]*10.5 + vec[12]*11.5 + vec[13]*12.5 + vec[14]*13.5 + vec[15]*14.5)
  } else if (x[1] == "NominalGNP" & x[2] == "1974Q4-1981Q2") {
    area <- (1/15)*(vec[1]*0.5 + vec[2]*1.5 + vec[3]*2.5 + vec[4]*3.5 + vec[5]*4.5 + vec[6]*5.5 + vec[7]*6.5 + vec[8]*7.5 + vec[9]*8.5 + vec[10]*9.5 + vec[11]*10.5 + vec[12]*11.5 + vec[13]*12.5 + vec[14]*13.5 + vec[15]*14.5)
  } else if (x[1] == "RealGNP" & x[2] == "1981Q3-1991Q4") {
    area <- (1/6)*(vec[1]*0.5 + vec[2]*1.5 + vec[3]*2.5 + vec[4]*3.5 + vec[5]*4.5 + vec[6]*5.5)
  } else if (x[1] == "RealGDP" & x[2] == "1992Q1-2009Q1") {
    area <- (1/10)*(vec[1]*0.5 + vec[2]*1.5 + vec[3]*2.5 + vec[4]*3.5 + vec[5]*4.5 + vec[6]*5.5 + vec[7]*6.5 + vec[8]*7.5 + vec[9]*8.5 + vec[10]*9.5)
  } else if (x[1] == "RealGDP" & x[2] == "2009Q2-Present") {
    area <- (1/11)*(vec[1]*0.5 + vec[2]*1.5 + vec[3]*2.5 + vec[4]*3.5 + vec[5]*4.5 + vec[6]*5.5 + vec[7]*6.5 + vec[8]*7.5 + vec[9]*8.5 + vec[10]*9.5+ vec[11]*10.5)
  } else if (x[1] == "Unemployment" & x[2] == "2009Q2-2013Q4") {
    area <- (1/10)*(vec[1]*0.5 + vec[2]*1.5 + vec[3]*2.5 + vec[4]*3.5 + vec[5]*4.5 + vec[6]*5.5 + vec[7]*6.5 + vec[8]*7.5 + vec[9]*8.5 + vec[10]*9.5)
  } else if (x[1] == "Unemployment" & x[2] == "2014Q1-Present") {
    area <- (1/10)*(vec[1]*0.5 + vec[2]*1.5 + vec[3]*2.5 + vec[4]*3.5 + vec[5]*4.5 + vec[6]*5.5 + vec[7]*6.5 + vec[8]*7.5 + vec[9]*8.5 + vec[10]*9.5)
  } else if (x[1] == "Core CPI" & x[2] == "2007Q1-Present") {
    area <- (1/10)*(vec[1]*0.5 + vec[2]*1.5 + vec[3]*2.5 + vec[4]*3.5 + vec[5]*4.5 + vec[6]*5.5 + vec[7]*6.5 + vec[8]*7.5 + vec[9]*8.5 + vec[10]*9.5)
  }
  return(area)
}

area_gini <- lapply(c(1:nrow(training1)), function(i)func(training1[i,]))
training1$Area <- as.numeric(area_gini)
training1$RAW_GINI <- (0.5 - training1$Area)/0.5

func2 <- function (x) {
  if (x[1] == "Core PCE") {
    adj <- as.numeric(x[72]*10/9)
  } else if (x[1] == "NominalGNP") {
    adj <- as.numeric(x[72] * 15/14)
  } else if (x[1] == "RealGNP") {
    adj <- as.numeric(x[72] * 6/5)
  } else if (x[1] == "RealGDP" & x[2] == "1992Q1-2009Q1") {
    adj <- as.numeric(x[72]*10/9)
  } else if (x[1] == "RealGDP" & x[2] == "2009Q2-Present") {
    adj <- as.numeric(x[72]*11/10)
  } else if (x[1] == "Unemployment") {
    adj <- as.numeric(x[72]*10/9)
  } else if (x[1] == "Core CPI") {
    adj <- as.numeric(x[72]*10/9)
  }
  return(adj)
}

adj_gini <- lapply(c(1:nrow(training1)), function(i)func2(training1[i,]))
training1$ADJ_GINI <- as.numeric(adj_gini)

# Check what's up with actuals unemployment gini
training2 <- filter(training1, INDICATOR == "Unemployment")
actuals_wide_check <- select(actuals_wide2, c(1:17, 48, 49, 52:54))

ttest <- training1 %>% 
  filter(INDICATOR=="Unemployment" & yearqtr == "2009Q2-2013Q4") 
mean(ttest$act_ADJ_GINI)
t.test(ttest$ADJ_GINI, mu=mean(ttest$act_ADJ_GINI), alternative = "two.sided")


# Overall gini t-test
mean(training1$act_ADJ_GINI)
mean(training1$ADJ_GINI)
t.test(training1$ADJ_GINI, mu=mean(training1$act_ADJ_GINI), alternative = "two.sided")


trainingtest <- training1 %>%   
  group_by(`FORECASTER ID`) %>%
  mutate(n=n()) %>% 
  mutate(avgadj_gini = mean(ADJ_GINI)) %>% 
  mutate(avgact_gini = mean(act_ADJ_GINI)) %>% 
  group_by(`FORECASTER ID`, avgadj_gini, avgact_gini) %>% 
  summarise()

mean(trainingtest$avgadj_gini)
mean(trainingtest$avgact_gini)
t.test(trainingtest$avgadj_gini, mu=mean(trainingtest$avgact_gini), alternative = "two.sided")











# Get the ginis for the forecasts for each indicator
gini_forecasts <- training1 %>% 
  group_by(INDICATOR, yearqtr) %>% 
  summarise(meanginiforecasts = mean(ADJ_GINI))

## plot
traininggraphs <- training1

traininggraphs$INDICATOR <- apply(array(training[["INDICATOR"]]),MARGIN=1, FUN=lumpind)


gini_ave <- traininggraphs %>%
  group_by(INDICATOR) %>%
  summarise(forecast_gini = mean(ADJ_GINI),
            se = sd(ADJ_GINI)/sqrt(n()),
            n = n(),
            actual_gini = mean(act_ADJ_GINI),
            t_test = t.test(ADJ_GINI, mu = actual_gini, alternative = "two.sided")$p.value) %>%
  mutate(label = paste0(INDICATOR,'\nN = ', n))

fig3 <- gini_ave %>%
  ggplot(aes(x = label, y = forecast_gini, color = "Forecast Gini")) + 
  geom_point() + geom_point(aes(y = actual_gini, color = "Gini of Actuals"), size=3) + 
  geom_errorbar(aes(ymin = forecast_gini-1.96*se, ymax = forecast_gini+1.96*se), width=0.2, size=0.6) + 
  scale_colour_manual(name="", values = c("Forecast Gini"="indianred1", "Gini of Actuals"="steelblue3")) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size=15), plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), legend.position="bottom") + 
  scale_y_continuous() + ylab("") + ggtitle("Gini") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.3))


plot(fig3)


# ------------------------------------------------------
# RECESSION YEARS

recessionyrs <- read_excel("../FRED Data/RECESSION.xls", skip = 11, col_names = c("RECYEAR", "value"))
recessionyrs$`RECYEAR` <- substr(recessionyrs$`RECYEAR`, 1, 4)
recessionyrs <- recessionyrs %>%
  mutate(RECESSION = ifelse(value == 0, "NO", "YES")) 
recessionyrs <- as.data.frame(recessionyrs)
recessionyrs$RECESSION <- as.factor(recessionyrs$RECESSION)
recessionyrs <- select(recessionyrs, `RECYEAR`, RECESSION)

training1 <- training1 %>% 
  mutate(`RECYEAR` = `YEAR FORECAST MADE` - 1)
training1 <- merge(x = training1, y = recessionyrs, by = c("RECYEAR"), all.x = TRUE)

MAXreclm_re <- lmer(MAX ~ RECESSION + (1|`FORECASTER ID`), training1)
summary(MAXreclm_re)

GINIreclm_re <- lmer(ADJ_GINI ~ RECESSION + (1|`FORECASTER ID`), training1)
summary(GINIreclm_re)

VARreclm_re <- lmer(pred_var ~ RECESSION + (1|`FORECASTER ID`), training1)
summary(VARreclm_re)








# ------------------------------------------------------
### QSR

# This is what gives me the qsr for the forecasts

training1 <- training1[c(2:75, 1)]

QSR <- function(x) {
  vec <- as.numeric(sort(x[25:39], decreasing = TRUE))
  if (x[1] == "Core PCE" & x[2] == "2007Q1-Present") {
    qsr <- as.numeric(1 + x[62]*2) - (vec[1]^2 + vec[2]^2 + vec[3]^2 + vec[4]^2 + vec[5]^2 + vec[6]^2 + vec[7]^2 + vec[8]^2 + vec[9]^2 + vec[10]^2)
  } else if (x[1] == "NominalGNP" & x[2] == "1968Q4-1973Q1") {
    qsr <- as.numeric(1 + x[62]*2) - (vec[1]^2 + vec[2]^2 + vec[3]^2 + vec[4]^2 + vec[5]^2 + vec[6]^2 + vec[7]^2 + vec[8]^2 + vec[9]^2 + vec[10]^2 + vec[11]^2 + vec[12]^2 + vec[13]^2 + vec[14]^2 + vec[15]^2)
  } else if (x[1] == "NominalGNP" & x[2] == "1973Q2-1974Q3") {
    qsr <- as.numeric(1 + x[62]*2) - (vec[1]^2 + vec[2]^2 + vec[3]^2 + vec[4]^2 + vec[5]^2 + vec[6]^2 + vec[7]^2 + vec[8]^2 + vec[9]^2 + vec[10]^2 + vec[11]^2 + vec[12]^2 + vec[13]^2 + vec[14]^2 + vec[15]^2)
  } else if (x[1] == "NominalGNP" & x[2] == "1974Q4-1981Q2") {
    qsr <- as.numeric(1 + x[62]*2) - (vec[1]^2 + vec[2]^2 + vec[3]^2 + vec[4]^2 + vec[5]^2 + vec[6]^2 + vec[7]^2 + vec[8]^2 + vec[9]^2 + vec[10]^2 + vec[11]^2 + vec[12]^2 + vec[13]^2 + vec[14]^2 + vec[15]^2)
  } else if (x[1] == "RealGNP" & x[2] == "1981Q3-1991Q4") {
    qsr <- as.numeric(1 + x[62]*2) - (vec[1]^2 + vec[2]^2 + vec[3]^2 + vec[4]^2 + vec[5]^2 + vec[6]^2)
  } else if (x[1] == "RealGDP" & x[2] == "1992Q1-2009Q1") {
    qsr <- as.numeric(1 + x[62]*2) - (vec[1]^2 + vec[2]^2 + vec[3]^2 + vec[4]^2 + vec[5]^2 + vec[6]^2 + vec[7]^2 + vec[8]^2 + vec[9]^2 + vec[10]^2)
  } else if (x[1] == "RealGDP" & x[2] == "2009Q2-Present") {
    qsr <- as.numeric(1 + x[62]*2) - (vec[1]^2 + vec[2]^2 + vec[3]^2 + vec[4]^2 + vec[5]^2 + vec[6]^2 + vec[7]^2 + vec[8]^2 + vec[9]^2 + vec[10]^2 + vec[11]^2)
  } else if (x[1] == "Unemployment" & x[2] == "2009Q2-2013Q4") {
    qsr <- as.numeric(1 + x[62]*2) - (vec[1]^2 + vec[2]^2 + vec[3]^2 + vec[4]^2 + vec[5]^2 + vec[6]^2 + vec[7]^2 + vec[8]^2 + vec[9]^2 + vec[10]^2)
  } else if (x[1] == "Unemployment" & x[2] == "2014Q1-Present") {
    qsr <- as.numeric(1 + x[62]*2) - (vec[1]^2 + vec[2]^2 + vec[3]^2 + vec[4]^2 + vec[5]^2 + vec[6]^2 + vec[7]^2 + vec[8]^2 + vec[9]^2 + vec[10]^2)
  } else if (x[1] == "Core CPI" & x[2] == "2007Q1-Present") {
    qsr <- as.numeric(1 + x[62]*2) - (vec[1]^2 + vec[2]^2 + vec[3]^2 + vec[4]^2 + vec[5]^2 + vec[6]^2 + vec[7]^2 + vec[8]^2 + vec[9]^2 + vec[10]^2)
  }
  return(qsr)
}

forecast_qsr <- lapply(c(1:nrow(training1)), function(i)QSR(training1[i,]))
training1$forecast_qsr <- as.numeric(forecast_qsr)
summary(training1$forecast_qsr1)

# this is just checking why there are 2.01's (bc data not normalized)
max(training1$forecast_qsr)
check <- filter(training1, MAX == 1)

# Saving into CSV
write.csv(training1,"../Training and Validation/training1.csv", row.names = FALSE, na ="")
training1 <- read.csv("../Training and Validation/training1.csv", check.names = FALSE, stringsAsFactors = FALSE)


trainingQSR <- training1

# run 200525_QSR.R!!!!


# ------------------------------------------------------
### Do forecasts get better over time? Presumably technology allows us to use more data, and statistical advancements lead to better models. 
# Conduct a regression analysis at the level of the forecast with accuracy (QSR) as the dependent variable.  
# Independent variable is the year forecast made. Include fixed effects for forecaster. 
# The hypothesis predicts a significant positive coefficient on year.  

QSRlm2 <- lmer(forecast_qsr ~ `YEAR FORECAST MADE` + (1|`FORECASTER ID`), training1)
summary(QSRlm2)
# they get better throughout time


### Does accuracy go up as the forecast distance shrinks? 
# Conduct a regression on forecast accuracy predicted by quarters distance from the moment of truth.  
# Include fixed effects for forecaster and year being forecast.  

QSRtdistlm2 <- lmer(forecast_qsr ~ `TDIST` + (1|`FORECASTER ID`) + (1|`YEAR BEING FORECAST`), training1)
summary(QSRtdistlm2)



##plot Forecast Accuracy (QSR) ~ TDIST
lumpind <- function(x) {
  if(x=="NominalGNP" | x=="RealGNP" | x=="RealGDP") {
    "GDP"
  } else if (x=="Core CPI") {
    "Core CPI"
  } else if (x== "Core PCE") {
    "Core PCE"
  } else {
    "Unemployment"
  }}

traininggraphs <- training1

traininggraphs$INDICATOR <- apply(array(training1[["INDICATOR"]]),MARGIN=1, FUN=lumpind)


qsr_TDIST <- traininggraphs %>%
  group_by(INDICATOR, TDIST) %>%
  summarise(qsr_mean = mean(forecast_qsr),
            qsr_se = sd(forecast_qsr)/sqrt(n()),
            n = n()) %>%
  mutate(label = paste0(INDICATOR,'\nN = ', n))
qsr_TDIST$TDIST <- qsr_TDIST$TDIST + 0.25


fig4 <- qsr_TDIST %>%
  ggplot(aes(x = TDIST, y = qsr_mean, color = INDICATOR)) + 
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = qsr_mean - 1.96*qsr_se, ymax = qsr_mean + 1.96*qsr_se), width=0.2, size=1) +
  theme_bw() + 
  scale_y_continuous(breaks = seq(0, 2, by = 0.1), minor_breaks = seq(0, 2, by = 0.1), position = "right") + ylab("Accuracy (QSR) Score") + 
  scale_x_reverse(breaks = seq(0, 4, by = 0.25), minor_breaks = seq(0, 4, by = 0.25)) + xlab("Quarter Distance to MOT") +
  ggtitle("Distance to Moment of Truth and Accuracy Score") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.3)) +
  theme(legend.position = c(0.15, 0.75),
        legend.background = element_blank())

plot(fig4)


qsr_TDIST_all <- traininggraphs %>%
  group_by(TDIST) %>%
  summarise(qsr_mean = mean(forecast_qsr),
            qsr_se = sd(forecast_qsr)/sqrt(n()),
            qsr_sd = sd(forecast_qsr),
            n = n())
qsr_TDIST_all$TDIST <- qsr_TDIST_all$TDIST + 0.25





wherethefours <- filter(traininggraphs, `YEAR BEING FORECAST` == 2016)
wherethefours <- select(wherethefours, c(1:7, `TDIST`))

# add .25 to the TDIST col
# MOT = type out moment of truth
# add the error bars
fig4_all <- qsr_TDIST_all %>%
  ggplot(aes(x = TDIST, y = qsr_mean)) + 
  # geom_line(size = 1, col = "plum") +
  geom_point(col = "gray") +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, col = "steelblue3") +
  # stat_smooth(method = "lm", formula = y ~ poly(x, 5), size = 1) +
  # stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  # stat_smooth(method = "loess", formula = y ~ x, size = 1, col = "plum") +
  geom_errorbar(aes(ymin = qsr_mean - 1.96*qsr_se, ymax = qsr_mean + 1.96*qsr_se), width=0.2, size=1, col = "gray") +
  theme_bw() + 
  scale_y_continuous(breaks = seq(0, 2, by = 0.1), minor_breaks = seq(0, 2, by = 0.1), position = "right") + ylab("Accuracy \n (QSR) Score") + 
  scale_x_reverse(breaks = seq(0, 4, by = 0.25), minor_breaks = seq(0, 4, by = 0.25)) + xlab("Distance to Moment of Truth (Yrs)") +
  ggtitle("Distance to Moment of Truth and Accuracy Score") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.3)) +
  theme(axis.title.y.right = element_text(angle = 0, vjust = 0.5),
        text = element_text(size=20, family = "Open Sans"),
        axis.title.x = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "gray"))




plot(fig4_all)


##plot Forecast Accuracy (QSR) ~ YEAR FORECAST MADE
qsr_YFM <- traininggraphs %>%
  group_by(`YEAR FORECAST MADE`) %>%
  summarise(qsr_mean = mean(forecast_qsr),
            qsr_se = sd(forecast_qsr)/sqrt(n()),
            n = n())

qsrYFM <- qsr_YFM %>%
  ggplot(aes(x = `YEAR FORECAST MADE`, y = qsr_mean)) + 
  geom_point(col="gray") + 
  geom_smooth(method='lm', col = "steelblue3") +
  geom_errorbar(aes(ymin = qsr_mean - 1.96*qsr_se, ymax = qsr_mean + 1.96*qsr_se), width=0.2, size=1, col = "gray") +
  theme_bw() + 
  scale_y_continuous(breaks = seq(0, 2, by = 0.1), minor_breaks = seq(0, 2, by = 0.1), position = "right") + ylab("Accuracy \n (QSR) Score") + 
  scale_x_continuous(breaks = seq(1968, 2020, by = 10), minor_breaks = seq(0, 2020, by = 10)) + xlab("Year Forecast Made") +
  ggtitle("Accuracy Score Over Time") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.3), panel.grid.minor = element_blank(), text = element_text(size=20, family = "Open Sans")) +
  theme(axis.title.y.right = element_text(angle = 0, vjust = 0.5),
        text = element_text(size=20, family = "Open Sans"),
        axis.title.x = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.15, 0.75),
        legend.background = element_blank()) 
#  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = , b = 0, l = 20))) +

plot(qsrYFM)



###############Additional exploratory analyses####################33


# Leif suggested maybe when there are fewer bins it wouldn't be as taxing, so we would observe less overprecision.  
# I predict we are likely to get the opposite result: that overprecision goes up when there are fewer bins.  This wouldn't be ironclad refutation of the fatigue issue, but it would help. Please conduct this analysis.  Ultimately, we will want to add it to our planned analyses.  
# Compare within GNP, within the different measures 
# Average peak confidence for nominal gdp when there were 15 bins and compare that against average x when there were 10 bins 
# Look at how much area the bin covers?
# Divide the peak by the bin width 
# T test of two means 
levels(factor(training1$INDICATOR))
levels(factor(training1$yearqtr))

# 10 bins, width 1 
rgdp1 <- training1 %>% 
  filter(INDICATOR=="RealGDP" & yearqtr == "1992Q1-2009Q1")

# 11 bins, width 1
rgdp2 <- training1 %>% 
  filter(INDICATOR=="RealGDP" & yearqtr == "2009Q2-Present")

mean(rgdp1$MAX) # 10 bins, .5523
mean(rgdp2$MAX) # 11 bins, .49

t.test(rgdp1$MAX, rgdp2$MAX, alternative = "two.sided")


# let's try different indicators 

# 6 bins

rgnp <- training1 %>% 
  filter(INDICATOR=="RealGNP" & yearqtr == "1981Q3-1991Q4")
  # mutate(MAX1 = MAX)

#compare 10 bins with 6 bins
mean(rgnp$MAX) # 6 bins, .66
t.test(rgdp1$MAX, rgnp$MAX1, alternative = "two.sided")
#compare 11 bins with 6 bins
t.test(rgdp2$MAX, rgnp$MAX1, alternative = "two.sided")

# 15 bins, width 1
ngnp <-  training1 %>% 
  filter(INDICATOR=="NominalGNP") %>% 
  filter(yearqtr == "1968Q4-1973Q1" | yearqtr == "1973Q2-1974Q3" | yearqtr == "1974Q4-1981Q2") 

mean(ngnp$MAX) #.515
t.test(rgdp2$MAX, ngnp$MAX, alternative = "two.sided")
t.test(rgdp1$MAX, ngnp$MAX, alternative = "two.sided")




num = c(34)

trim = as.numeric(substr(num, nchar(num[1])-1, nchar(num[1])))
trim = as.numeric(substr(num, nchar(num[1]), nchar(num[1])))
as.numeric(substr(num, nchar(num), nchar(num)))


# -----------------------------------------------------
trainingnum <- training1

#one point for every 5
trainingnum$roundnum5 <- apply(trainingnum[,paste("BIN", 1:15)], 1, function(x) if (all(is.na(x))==FALSE) sum((as.numeric(substr(x, nchar(x), nchar(x)))==5), na.rm =TRUE)
                            else NA)/trainingnum$nbin

letssee <- trainingnum %>% 
  select(c(10:25, 76:77))

#two points for every ten's (but not 0)
trainingnum$roundnum0 <- apply(trainingnum[,paste("BIN", 1:15)], 1, function(x) if (all(is.na(x))==FALSE) sum((as.numeric(substr(x, nchar(x), nchar(x)))==0) & x!=0, na.rm =TRUE)*2 
                               else NA)/trainingnum$nbin

letssee <- trainingnum %>% 
  select(c(10:25, 78))

#add the columns 
trainingnum$roundnum <- trainingnum$roundnum0 + trainingnum$roundnum5

#correlate
cor.test(trainingnum$roundnum, trainingnum$MAX, method = "pearson")


#the more round numbers they used, the higher their precision was -- fatigue may increase precision
#the more round numbers they used, the less their precision was -- this implies uncertainty, leads to wider distribution and more round numbers

# -----------------------------------------------------
library(ltm)

newdata <- subset(training1, 
                  select=c(MAX, pred_var, ADJ_GINI))

cronbach.alpha(data = subset(training1, 
                             select=c(MAX, pred_var, ADJ_GINI)))
# can't do this bc var goes in the other direction, but this is suggestive

# bi-variate correlations -- three correlations


library("Hmisc")
cor <- rcorr(as.matrix(newdata))
cor

core2 <- cor(as.matrix(newdata))

library(corrplot)
corrplot(core2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


# -----------------------------------------------------

# 6967 obs --> 2149 obs
# only makes sense to take 2009Q2 +
GDPonly <- training1 %>% 
  filter(INDICATOR=="NominalGNP" | INDICATOR == "RealGNP" | INDICATOR == "RealGDP") %>% 
  filter(`YEAR FORECAST MADE` >= 2009) %>% 
  filter(`YEAR FORECAST MADE` != 2009 | QUARTER != 1) %>% 
  filter(`YEAR FORECAST MADE` != 2009 | QUARTER != 2) 

GDPonly$Year.ID.ForecastYear.Quarter <- paste0(GDPonly$Year.ID.ForecastYear.Quarter)

# 2139 obs
UNEMPonly <- training1 %>% 
  filter(INDICATOR == "Unemployment")

UNEMPonly$Year.ID.ForecastYear.Quarter <- paste0(UNEMPonly$Year.ID.ForecastYear.Quarter)


# column in the unemployment data frame called filled_GDP 
# that is a 1 if they filled out the GDP questionnaire, otherwise 0 

z <- merge(x= UNEMPonly, y = GDPonly[ , c("Year.ID.ForecastYear.Quarter", "ADJ_GINI", "MAX", "pred_var")], by = "Year.ID.ForecastYear.Quarter", all.x=TRUE)

# are the values of GDP in unemployment 
z$Year.ID.ForecastYear.Quarter %in% UNEMPonly$Year.ID.ForecastYear.Quarter

z$filled_GDP <- ifelse(z$MAX.y >= 0, 1, 0)
z[["filled_GDP"]][is.na(z[["filled_GDP"]])] <- 0

# limitation is that there are  1000 obs
mean(z[z$filled_GDP == 1,]$MAX.x) # 0.527
mean(z[z$filled_GDP == 0,]$MAX.x) # 0.522

t.test(z[z$filled_GDP == 1,]$MAX.x, z[z$filled_GDP == 0,]$MAX.x, alternative = "two.sided")

t.test(z[z$filled_GDP == 1,]$ADJ_GINI.x, z[z$filled_GDP == 0,]$ADJ_GINI.x, alternative = "two.sided")

t.test(z[z$filled_GDP == 1,]$pred_var.x, z[z$filled_GDP == 0,]$pred_var.x, alternative = "two.sided")


