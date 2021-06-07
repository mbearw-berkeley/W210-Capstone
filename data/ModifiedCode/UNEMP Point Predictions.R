#######################################################
#
#   R analysis code for the FEO-SPF project
#   Analyzing data from the Survey of Professional Forecasters
#   Sandy Campbell, Don Moore
#   May 26, 2020
#
#   This is UNEMP point predictions
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

training <- read.csv("/Users/Isaac/Downloads/fulldata.csv", check.names = FALSE, stringsAsFactors = FALSE)

# Before 1981 Q3, survey did not include annual forecasts
# So 1981 Q2 form: UNEMP 1 - 6 is 1981 Q1-4 and 1982 Q1-2

# 1990 Q1 survey: 1989 Q4, 1990 Q1-4, 1991 Q1, Annual: 1989 1990

# 2001 Q1 survey: 2000 Q4 GIVEN, 2001 Q1-4, 2002 Q1, Annual: 2000 GIVEN, 2001, 2002

# 2006 Q3 survey: 2006 Q2 GIVEN, 2006 Q3-Q4, 2007 Q1-2, 2007 Q1-3, Annual: 2005 GIVEN, 2006, 2007

# 2007 Q1 survey: 2006 Q4 GIVEN, 2007 Q1-4, 2008 Q1, Annual: 2006 GIVEN, 2007, 2008

# 2009 Q2 survey: 2009 Q1 GIVEN, 2009 Q2-4, 2010 Q1-2, Annual: 2008 GIVEN, 2009, 2010 AND 2011, 2012

# 2010 Q1 survey: 2009 Q4 GIVEN, 2010 Q1-4, 2011 Q1, Annual: 2009 GIVEN, 2010-2013

# 2011 Q1 survey: 2010 Q4 GIVEN, 2011 Q1-4, 2012 Q1, Annual: 2010 GIVEN, 2011-2014

# 2014 Q1 survey: 2013 Q4 GIVEN, 2014 Q1-4, 2015 Q1, Annual: 2013 GIVEN, 2014-2017

# Special Notes: Surveys 1985:Q1, 1986:Q1, and 1990:Q1 mistakes, previous year and current year (UNEMPA and UNEMPB)

UNEMP <- read_excel("/Users/Isaac/Downloads/RawData/Individual_UNEMP.xlsx") 
sapply(UNEMP, class)
UNEMP[ , ] <- apply(UNEMP[ , ], 2,     
                    function(x) as.numeric(as.character(x)))

# Let's make sure it's kosher to average quarterly data for the years that don't have annual forecasts
# Filter for Q1 surveys between 1981 Q3 - 2000 Q4, take the average of UNEMP 2-5 and compare it to UNEMPB
# So, say for 1982 Q1, we looked at the correlation between the average of the forecasters’ quarterly forecasts (1982 Q1 – 1982 Q4) and their annual forecast for 1982. 
check1 <- UNEMP %>% 
  filter(YEAR > 1980 & YEAR < 2001) %>%
  filter(YEAR != 1981 | QUARTER != 1) %>%
  filter(YEAR != 1981 | QUARTER != 2) %>%
  filter(QUARTER == 1)

check1mistakes <- check1 %>% 
  filter(YEAR == 1985 | YEAR == 1986 | YEAR == 1990) %>%
  filter(QUARTER == 1) 

check1 <- anti_join(check1, check1mistakes)

check1 <- rename(check1, `YEAR FORECAST MADE` = YEAR)
check1 <- check1 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)
check1$qrt_avg <- rowMeans(check1[6:9], na.rm=FALSE)

check1mistakes <- rename(check1mistakes, `YEAR FORECAST MADE` = YEAR)
check1mistakes <- check1mistakes %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)
check1mistakes$qrt_avg <- rowMeans(check1mistakes[6:9], na.rm=FALSE)


x <- check1$qrt_avg
y <- check1$UNEMPA
z <- check1$UNEMPB 

cor.test(x, y,  method = "pearson", use = "complete.obs")
cor.test(x, z,  method = "pearson", use = "complete.obs")

# So, removing the mistake years makes it so that correlation between x and y is higher, which is right

x1 <- check1mistakes$qrt_avg
y1 <- check1mistakes$UNEMPA
z1 <- check1mistakes$UNEMPB 

cor.test(x1, y1,  method = "pearson", use = "complete.obs")
cor.test(x1, z1,  method = "pearson", use = "complete.obs")

# correlation between x1 and z1 is stronger, which is right (:


#########
# filter for Q1 surveys between 2001 Q1 to Present,
# take the average of UNEMP 2-5 and compared it to UNEMPA
check2 <- UNEMP %>% 
  filter(YEAR > 2000) %>%
  filter(QUARTER == 1)

check2 <- rename(check2, `YEAR FORECAST MADE` = YEAR)
check2 <- check2 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)
check2$qrt_avg <- rowMeans(check2[6:9], na.rm=FALSE)

#this correlation shows that yes, UNEMPA is what we should be looking at
a <- check2$qrt_avg
b <- check2$UNEMPA
c <- check2$UNEMPB 

cor.test(a, b,  method = "pearson", use = "complete.obs")
cor.test(a, c,  method = "pearson", use = "complete.obs")

#########

# f1, f2: for survey years 1968 Q4 – 1981 Q2, we’ll want to extrapolate the annual forecast
# f1 takes all the Q1 surveys (UNEMP 2-5)
# f2a takes all the Q2 surveys (UNEMP 1-4)
# f2b takes all the Q4 surveys (UNEMP 3-6)
# e.g., 2014 Q1 forecasts 2014, 2014 Q2 forecasts 2014, 2014 Q4 forecasts 2015

f1 <- UNEMP %>% 
  filter(YEAR >= 1968 & YEAR < 1982) %>%
  filter(YEAR != 1981 | QUARTER != 3) %>%
  filter(YEAR != 1981 | QUARTER != 4) %>% 
  filter(QUARTER == 1) # kind of odd that 1969 and 1970 don't have UNEMP6's...

f1 <- rename(f1, `YEAR FORECAST MADE` = YEAR)
f1 <- f1 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)
f1$UNEMPA <- rowMeans(f1[6:9], na.rm=FALSE)



f2a <- UNEMP %>% 
  filter(YEAR >= 1968 & YEAR < 1982) %>%
  filter(YEAR != 1981 | QUARTER != 3) %>%
  filter(YEAR != 1981 | QUARTER != 4) %>% 
  filter(QUARTER == 2)  # kind of odd that 1969 and 1970 don't have UNEMP6's...

f2a <- rename(f2a, `YEAR FORECAST MADE` = YEAR)
f2a <- f2a %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)
f2a$UNEMPA <- rowMeans(f2a[5:8], na.rm=FALSE)


f2b <- UNEMP %>% 
  filter(YEAR >= 1968 & YEAR < 1982) %>%
  filter(YEAR != 1981 | QUARTER != 3) %>%
  filter(YEAR != 1981 | QUARTER != 4) %>% 
  filter(QUARTER == 4)

f2b <- rename(f2b, `YEAR FORECAST MADE` = YEAR)
f2b <- f2b %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)
f2b$UNEMPA <- rowMeans(f2b[7:10], na.rm=FALSE)

f0 <- rbind(f1, f2a, f2b)

# Creating a unique ID that combines Year Being Forecasted, Forecaster ID, Year Forecast Made, and Quarter
f0$Year.ID.ForecastYear.Quarter <- paste(f0$`YEAR BEING FORECAST`, f0$ID, f0$`YEAR FORECAST MADE`, f0$QUARTER, sep = "-")

# Renaming the columns
f0 <- rename(f0, `FORECASTER ID` = `ID`)

# Creating a new column: the type of indicator
f0$INDICATOR <- "Unemployment"

# Creating a new column with the TDIST
f0 <- f0 %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Reorganizing the columns
f0 <- f0 %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `UNEMP1`, `UNEMP2`, `UNEMP3`, `UNEMP4`, `UNEMP5`, `UNEMP6`, `UNEMPA`, `UNEMPB`, `UNEMPC`, `UNEMPD`, INDICATOR, TDIST)


#########

# f3: for survey years 1981 Q3 – 2000 Q4, they're asked for the current year and next year
# Special Notes: Surveys 1985:Q1, 1986:Q1, and 1990:Q1 mistakes, previous year and current year (UNEMPA and UNEMPB)

fnote.a <- UNEMP %>% 
  filter(YEAR >= 1985 & YEAR <= 1990) %>%
  filter(YEAR == 1985 | QUARTER == 1) %>% 
  filter(YEAR == 1986 | QUARTER == 1) %>% 
  filter(YEAR == 1990 | QUARTER == 1) %>% 
  filter(YEAR != 1987 & YEAR != 1988 & YEAR != 1989)   

fnote.a <- rename(fnote.a, `YEAR FORECAST MADE` = YEAR)
fnote.a <- fnote.a %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` - 1)

fnote.b <- fnote.a %>% 
  mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

fnote.a$Year.ID.ForecastYear.Quarter <- paste(fnote.a$`YEAR BEING FORECAST`, fnote.a$ID, fnote.a$`YEAR FORECAST MADE`, fnote.a$QUARTER, sep = "-")
fnote.b$Year.ID.ForecastYear.Quarter <- paste(fnote.b$`YEAR BEING FORECAST`, fnote.b$ID, fnote.b$`YEAR FORECAST MADE`, fnote.b$QUARTER, sep = "-")

fnote.b <- fnote.b %>% 
  mutate(UNEMPA = UNEMPB) 
fnote.b$UNEMPB[ ] <- NA
fnote.a$UNEMPB[ ] <- NA

fnote <- rbind(fnote.a, fnote.b)

#######

f3 <- UNEMP %>% 
  filter(YEAR >= 1981 & YEAR < 2001) %>%
  filter(YEAR != 1981 | QUARTER != 1) %>%
  filter(YEAR != 1981 | QUARTER != 2) %>% 
  filter(YEAR != 1985 | QUARTER != 1) %>% 
  filter(YEAR != 1986 | QUARTER != 1) %>% 
  filter(YEAR != 1990 | QUARTER != 1)

f3 <- rename(f3, `YEAR FORECAST MADE` = YEAR)
f3 <- f3 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

f4 <- f3 %>% 
  mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)

f3$Year.ID.ForecastYear.Quarter <- paste(f3$`YEAR BEING FORECAST`, f3$ID, f3$`YEAR FORECAST MADE`, f3$QUARTER, sep = "-")
f4$Year.ID.ForecastYear.Quarter <- paste(f4$`YEAR BEING FORECAST`, f4$ID, f4$`YEAR FORECAST MADE`, f4$QUARTER, sep = "-")

f4 <- f4 %>% 
  mutate(UNEMPA = UNEMPB) 
f4$UNEMPB[ ] <- NA
f3$UNEMPB[ ] <- NA

f5 <- rbind(f3, f4, fnote)

# Renaming the columns
f5 <- rename(f5, `FORECASTER ID` = `ID`)

# Creating a new column: the type of indicator
f5$INDICATOR <- "Unemployment"

# Creating a new column with the TDIST
f5 <- f5 %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Reorganizing the columns
f5 <- f5 %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `UNEMP1`, `UNEMP2`, `UNEMP3`, `UNEMP4`, `UNEMP5`, `UNEMP6`, `UNEMPA`, `UNEMPB`, `UNEMPC`, `UNEMPD`, INDICATOR, TDIST)




#########

# f6: for survey years 2001 Q1 - 2009 Q1, the previous year's annual unemp rate is given
# so they're forecasting current and next year

f6 <- UNEMP %>% 
  filter(YEAR >= 2001 & YEAR <= 2009) %>%
  filter(YEAR != 2009 | QUARTER != 2) %>%
  filter(YEAR != 2009 | QUARTER != 3) %>%
  filter(YEAR != 2009 | QUARTER != 4) 

f6 <- rename(f6, `YEAR FORECAST MADE` = YEAR)
f6 <- f6 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

f7 <- f6 %>% 
  mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` +1)

f6$Year.ID.ForecastYear.Quarter <- paste(f6$`YEAR BEING FORECAST`, f6$ID, f6$`YEAR FORECAST MADE`, f6$QUARTER, sep = "-")
f7$Year.ID.ForecastYear.Quarter <- paste(f7$`YEAR BEING FORECAST`, f7$ID, f7$`YEAR FORECAST MADE`, f7$QUARTER, sep = "-")

f7 <- f7 %>% 
  mutate(UNEMPA = UNEMPB) 
f7$UNEMPB[ ] <- NA
f6$UNEMPB[ ] <- NA

f8 <- rbind(f6, f7)

# Renaming the columns
f8 <- rename(f8, `FORECASTER ID` = `ID`)

# Creating a new column: the type of indicator
f8$INDICATOR <- "Unemployment"

# Creating a new column with the TDIST
f8 <- f8 %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Reorganizing the columns
f8 <- f8 %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `UNEMP1`, `UNEMP2`, `UNEMP3`, `UNEMP4`, `UNEMP5`, `UNEMP6`, `UNEMPA`, `UNEMPB`, `UNEMPC`, `UNEMPD`, INDICATOR, TDIST)







#########

# f9: for survey years 2009 Q2 - Present, the previous year's annual unemp rate is given
# so they're forecasting current and next year, and two years after

f9 <- UNEMP %>% 
  filter(YEAR >= 2009) %>%
  filter(YEAR != 2009 | QUARTER != 1) 

f9 <- rename(f9, `YEAR FORECAST MADE` = YEAR)
f9 <- f9 %>% mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE`)

f10 <- f9 %>% 
  mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 1)

f11 <- f10 %>% 
  mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 2)

f12 <- f11 %>% 
  mutate('YEAR BEING FORECAST' = `YEAR FORECAST MADE` + 3)


f9$Year.ID.ForecastYear.Quarter <- paste(f9$`YEAR BEING FORECAST`, f9$ID, f9$`YEAR FORECAST MADE`, f9$QUARTER, sep = "-")
f10$Year.ID.ForecastYear.Quarter <- paste(f10$`YEAR BEING FORECAST`, f10$ID, f10$`YEAR FORECAST MADE`, f10$QUARTER, sep = "-")
f11$Year.ID.ForecastYear.Quarter <- paste(f11$`YEAR BEING FORECAST`, f11$ID, f11$`YEAR FORECAST MADE`, f11$QUARTER, sep = "-")
f12$Year.ID.ForecastYear.Quarter <- paste(f12$`YEAR BEING FORECAST`, f12$ID, f12$`YEAR FORECAST MADE`, f12$QUARTER, sep = "-")

f10 <- f10 %>% 
  mutate(UNEMPA = UNEMPB) 
f10$UNEMPB[ ] <- NA
f10$UNEMPC[ ] <- NA
f10$UNEMPD[ ] <- NA

f11 <- f11 %>% 
  mutate(UNEMPA = UNEMPC) 
f11$UNEMPB[ ] <- NA
f11$UNEMPC[ ] <- NA
f11$UNEMPD[ ] <- NA

f12 <- f12 %>% 
  mutate(UNEMPA = UNEMPD) 
f12$UNEMPB[ ] <- NA
f12$UNEMPC[ ] <- NA
f12$UNEMPD[ ] <- NA

f9$UNEMPB[ ] <- NA
f9$UNEMPC[ ] <- NA
f9$UNEMPD[ ] <- NA

f13 <- rbind(f9, f10, f11, f12)

# Renaming the columns
f13 <- rename(f13, `FORECASTER ID` = `ID`)

# Creating a new column: the type of indicator
f13$INDICATOR <- "Unemployment"

# Creating a new column with the TDIST
f13 <- f13 %>%
  mutate(TDIST = `YEAR BEING FORECAST` + 1 - (`YEAR FORECAST MADE` + (`QUARTER`/4)))

# Reorganizing the columns
f13 <- f13 %>%
  select(Year.ID.ForecastYear.Quarter, `YEAR FORECAST MADE`, `YEAR BEING FORECAST`, QUARTER, `FORECASTER ID`, INDUSTRY, `UNEMP1`, `UNEMP2`, `UNEMP3`, `UNEMP4`, `UNEMP5`, `UNEMP6`, `UNEMPA`, `UNEMPB`, `UNEMPC`, `UNEMPD`, INDICATOR, TDIST)


##############
# bind it all together, and UNEMPA is your PP
# split into training and validation

UNEMPpp <- rbind(f0, f5, f8, f13)

# ------------------------------------------------------
fullUPP <- UNEMPpp %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR)
write.csv(fullUPP, "/Users/Isaac/Downloads/TrainingData/fullunemploy.csv", row.names = FALSE, na ="")
  

set.seed(11396)
trainingUPP <- UNEMPpp %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR) %>%
  sample_frac(size = 0.5)
validationUPP <- UNEMPpp[!(paste0(UNEMPpp$Year.ID.ForecastYear.Quarter, UNEMPpp$INDICATOR) %in% paste0(trainingUPP$Year.ID.ForecastYear.Quarter, trainingUPP$INDICATOR)),]

# Saving into CSV
write.csv(trainingUPP,"../Training and Validation/trainingUPP.csv", row.names = FALSE, na ="")
write.csv(validationUPP,"../Training and Validation/validationUPP.csv", row.names = FALSE, na ="")

trainingUPP <- read.csv("../Training and Validation/trainingUPP.csv", check.names = FALSE, stringsAsFactors = FALSE)
validationUPP <- read.csv("../Training and Validation/validationUPP.csv", check.names = FALSE, stringsAsFactors = FALSE)



##############
# just take the needed columns

UNEMPpp <- select(trainingUPP, c(1:6, 13, 17, 18))

actuals_unemp <- read_excel("../FRED Data/UNRATE.xls", skip = 11, col_names = c("year", "value"))

# Creating a function called actual_bin 
# First line: names of the inputs of the function
# Goes row by row through the data, looking for year and quarter, then looking at indicators
# Pulls the actual for the data point by going to the FRED data and filtering it by year being forecast
# Then finds the interval the actual belongs in and returns the bin number

actual_bin <- Vectorize(function(year_forecast_made, quarter, year_being_forecast, indicator, return = c("actual")) {
  yq_forecast_made <- as.yearqtr(paste(year_forecast_made, quarter, sep = "q"))
  if (indicator=="Unemployment") {
    actual <- actuals_unemp$value[as.Date(actuals_unemp$year)==as.Date(paste0(year_being_forecast,"-01-01"), format = "%Y-%m-%d")]
  }
  
  if (all(return=="actual") & length(actual)>0) {
    return(actual)
  } else {
    return(NA)
  }
})


# Apply the function to the data frame to get actuals_table

actuals_table <- UNEMPpp %>%
  group_by(`YEAR FORECAST MADE`, QUARTER, `YEAR BEING FORECAST`, INDICATOR) %>%
  summarise() %>%
  mutate(actual = actual_bin(year_forecast_made = `YEAR FORECAST MADE`, quarter = QUARTER, year_being_forecast = `YEAR BEING FORECAST`, indicator = INDICATOR, return = "actual"))

UNEMPpp <- merge(x = UNEMPpp, y = actuals_table, by = c("YEAR FORECAST MADE", "QUARTER", "YEAR BEING FORECAST", "INDICATOR"), all.x = TRUE) 


# left with 6890 after taking out rows for which there's no actual and no PP

UNEMPpp <- UNEMPpp[is.na(UNEMPpp$actual)==FALSE,]
UNEMPpp <- UNEMPpp[is.na(UNEMPpp$UNEMPA)==FALSE,]


##########################################################
# Are forecasts overly optimistic or pessimistic?
# Optimism Point Prediction Analyses: 
# Are forecasters’ point predictions systematically optimistic or pessimistic? 
# Conduct a t-test comparing point-prediction forecasts with outcomes. 

mean(UNEMPpp$UNEMPA)
mean(UNEMPpp$actual)
t.test(UNEMPpp$UNEMPA, UNEMPpp$actual, paired = TRUE, alternative = "two.sided")


library(lme4)
library(lmerTest)
unemp_pp2 <- lmer((UNEMPA-actual) ~ 1 + (1|`FORECASTER ID`) + (1|`YEAR BEING FORECAST`), UNEMPpp)
summary(unemp_pp2)




##########################################################
# Optimism and pessimism in probability forecasts (histogram)
# Compute a weighted point prediction for each histogram forecast, treating each bin as its midpoint and weighting it by its assigned probability. 

UNEMPhist <- training %>% 
  filter(INDICATOR == "Unemployment") %>% 
  select(c(1:8, 24:64))

hist <- function(x) {
  vec1 <- as.numeric(x[9:18])
  vec2 <- as.numeric(x[24:33])
  if (x[4] == "Unemployment") {
    hist <- (vec1[1]*vec2[1] + vec1[2]*vec2[2] + vec1[3]*vec2[3] + vec1[4]*vec2[4]
             + vec1[5]*vec2[5] + vec1[6]*vec2[6] + vec1[7]*vec2[7] + vec1[8]*vec2[8]
             + vec1[9]*vec2[9] + vec1[10]*vec2[10])
  }
  return(hist)
}

unemp_hist <- lapply(c(1:nrow(UNEMPhist)), function(i)hist(UNEMPhist[i,]))
UNEMPhist$unemp_hist <- as.numeric(unemp_hist)
summary(UNEMPhist$unemp_hist)

UNEMPhist <- UNEMPhist[is.na(UNEMPhist$actual)==FALSE,]
UNEMPhist <- UNEMPhist[is.na(UNEMPhist$unemp_hist)==FALSE,]

# Conduct a t-test comparing weighted hist with outcomes. 
# If this test is significant, conduct a regression on the difference between forecast and outcome, including fixed effects for forecaster and year being forecast. 

mean(UNEMPhist$unemp_hist)
mean(UNEMPhist$actual)
t.test(UNEMPhist$unemp_hist, UNEMPhist$actual, paired = TRUE, alternative = "two.sided")


unempH_re <- lmer((unemp_hist-actual) ~ 1 + (1|`FORECASTER ID`) + (1|`YEAR BEING FORECAST`), UNEMPhist)
summary(unempH_re)




##########################################################
# Do point predictions show more optimism than histogram distributions do? 
# Conduct a paired t-test comparing point-predictions and weighted point prediction forecasts. 
# If this test is significant, conduct a regression on the difference between them, including fixed effects for forecaster and year being forecast. 

# NOTE: There's nothing before 2009 Q2 for histogram data so filter out the pp data
UNEMPppsub <- UNEMPpp %>% 
  filter(`YEAR FORECAST MADE` >= 2009) %>% 
  filter(`YEAR FORECAST MADE` != 2009 | QUARTER != 1) 


UNEMPhistsub <- select(UNEMPhist, c(1:5, 7, "TDIST", "actual", "unemp_hist"))

UNEMPall <- merge(x = UNEMPppsub, y = UNEMPhistsub, by = c("YEAR FORECAST MADE", "QUARTER", "YEAR BEING FORECAST", "INDICATOR", "FORECASTER ID", "TDIST", "actual", "Year.ID.ForecastYear.Quarter"), all.x = TRUE) 


UNEMPall <- UNEMPall[is.na(UNEMPall$UNEMPA)==FALSE,]
UNEMPall <- UNEMPall[is.na(UNEMPall$unemp_hist)==FALSE,]

mean(UNEMPall$unemp_hist)
mean(UNEMPall$UNEMPA)
t.test(UNEMPall$unemp_hist, UNEMPall$UNEMPA, paired = TRUE, alternative = "two.sided")

unempPPhist_re <- lmer((unemp_hist-UNEMPA) ~ 1 + (1|`FORECASTER ID`), UNEMPall)
summary(unempPPhist_re)

unempPPhist_re <- lmer((unemp_hist-UNEMPA) ~ 1 + (1|`FORECASTER ID`) + (1|`YEAR BEING FORECAST`), UNEMPall)
summary(unempPPhist_re)

logLik(unempPPhist_re)

test <- update(unempPPhist_re,start=ss,control=lmerControl(optimizer="bobyqa"))

summary(test)
logLik(test)


length(getME(unempPPhist_re,"theta"))

length(fixef(unempPPhist_re))


numcols <- grep("^c\\.",names(UNEMPall))
dfs <- UNEMPall
dfs[,numcols] <- scale(dfs[,numcols])
m1_sc <- update(unempPPhist_re,data=dfs)


tt <- getME(m1_sc,"theta")
ll <- getME(m1_sc,"lower")
min(tt[ll==0])


derivs1 <- m1_sc@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))

max(pmin(abs(sc_grad1),abs(derivs1$gradient)))


library("numDeriv")
library("RCurl") ## to source() from Github
library("ggplot2"); theme_set(theme_bw())
library("reshape2")
library("plyr")
library("RColorBrewer")
dd <- update(m1_sc,devFunOnly=TRUE)
pars <- unlist(getME(m1_sc,c("theta","fixef")))
grad2 <- grad(dd,pars)
hess2 <- hessian(dd,pars)
sc_grad2 <- solve(hess2,grad2)
max(pmin(abs(sc_grad2),abs(grad2)))


ss <- getME(m1_sc,c("theta","fixef"))
m2 <- update(m1_sc,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))


#If we set the number of evaluations large enough so the optimization actually finishes, we do avoid the warning.
m3 <- update(m1_sc,start=ss,control=lmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))

summary(m3)


afurl <- "https://raw.githubusercontent.com/lme4/lme4/master/misc/issues/allFit.R"
eval(parse(text=getURL(afurl)))
aa <- allFit(m2)


##########################################################
# Are forecasters more optimistic during economic booms?  
# Are forecasters more optimistic about the future when the prior year was a recession year?
# recessions are only identified in hindsight -- that's why we use prior year
# Conduct a regression at the level of the forecast. 
# The difference between forecast point prediction and actual outcome is the dependent variable. The key independent variable is a dummy variable for whether the prior year was a recession year. Include fixed effects for forecaster.

# working with UNEMPpp

recessionyrs <- read_excel("../FRED Data/RECESSION.xls", skip = 11, col_names = c("RECYEAR", "value"))
recessionyrs$`RECYEAR` <- substr(recessionyrs$`RECYEAR`, 1, 4)
recessionyrs <- recessionyrs %>%
  mutate(RECESSION = ifelse(value == 0, "NO", "YES")) 
recessionyrs <- as.data.frame(recessionyrs)
recessionyrs$RECESSION <- as.factor(recessionyrs$RECESSION)
recessionyrs <- select(recessionyrs, `RECYEAR`, RECESSION)

UNEMPpp <- UNEMPpp %>% 
  mutate(`RECYEAR` = `YEAR FORECAST MADE` - 1)
UNEMPpp <- merge(x = UNEMPpp, y = recessionyrs, by = c("RECYEAR"), all.x = TRUE)


UNEMPpprec_re <- lmer((UNEMPA-actual) ~ RECESSION + (1|`FORECASTER ID`), UNEMPpp)
summary(UNEMPpprec_re)

# when the prior year was a recession year, they were pessimistic bc the diff between pp and actual was negative


# Conduct the above analysis, but using weighted point prediction.
UNEMPhist <- UNEMPhist %>% 
  mutate(`RECYEAR` = `YEAR FORECAST MADE` - 1)
UNEMPhist <- merge(x = UNEMPhist, y = recessionyrs, by = c("RECYEAR"), all.x = TRUE)


UNEMPhistrec_re <- lmer((unemp_hist-actual) ~ RECESSION + (1|`FORECASTER ID`), UNEMPhist)
summary(UNEMPhistrec_re)








