# This script retreives historical census data for Wisconsin tracts.
# Time series trend models are fit for each tract from 2010-2019
# Visualizations and summaries for the trends are created.

# Clear working directory
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
library(tidycensus)
library(tidyverse)
library(forecast)
library(data.table)
library(tigris)
library(ggplot2)

# Census API key
key <- ##KEY HERE##
census_api_key(key)

# Decennial census data for rural percentage
dec <- get_decennial(geography = "Tract", 
                     state = '55',
                     variables = c('H002002','H002005'), 
                     sumfile = 'sf1',
                     year = 2010,
                     output = 'wide')
names(dec) <- c('Tract','Name','urban_pop','rural_pop')
dec$rural <- dec$rural_pop / (dec$urban_pop+dec$rural_pop)

# Function to get the required census data and transform it taking year as an argument
get_census_data <- function (year) {
  # Create empty data frame
  df <- data.frame()
  
  # Some variables change periodically so need to specify variable names depending on the year
  if (year == 2010) {
    vars <- c('DP02_0066PE','DP02_0064PE','DP02_0092PE','DP03_0006PE','DP03_0009PE','DP03_0066PE','DP03_0128PE','DP04_0045PE','DP04_0088E','DP05_0002PE','DP05_0066PE','DP05_0072PE','DP05_0073PE')
  } else if (year <= 2014) {
    vars <- c('DP02_0066PE','DP02_0067PE','DP02_0092PE','DP03_0006PE','DP03_0009PE','DP03_0066PE','DP03_0128PE','DP04_0045PE','DP04_0088E','DP05_0002PE','DP05_0066PE','DP05_0072PE','DP05_0073PE')
  } else if (year <= 2016) {
    vars <- c('DP02_0066PE','DP02_0067PE','DP02_0092PE','DP03_0006PE','DP03_0009PE','DP03_0066PE','DP03_0128PE','DP04_0046PE','DP04_0089E','DP05_0002PE','DP05_0066PE','DP05_0072PE','DP05_0073PE')
  } else if (year <= 2018) {
    vars <- c('DP02_0066PE','DP02_0067PE','DP02_0092PE','DP03_0006PE','DP03_0009PE','DP03_0066PE','DP03_0128PE','DP04_0046PE','DP04_0089E','DP05_0002PE','DP05_0071PE','DP05_0077PE','DP05_0078PE')
  } else {
    vars <- c('DP02_0067PE','DP02_0068PE','DP02_0093PE','DP03_0006PE','DP03_0009PE','DP03_0066PE','DP03_0128PE','DP04_0046PE','DP04_0089E','DP05_0002PE','DP05_0071PE','DP05_0077PE','DP05_0078PE')
  }
  
  # For each state, retrieve census data using tidycensus 
  temp <- get_acs('tract', 
      variables = c('GEO_ID',vars
      ), 
      state='55', 
      year = year, output='wide') %>% 
    select(-ends_with("M"))
  
  # Append data to df
  df <- rbind(df, temp)
  
  # Rename columns
  names(df) <- c('Tract','Name','hs_or_higher','bachelor_or_higher','foreign_born','military',
                 'unemployed','social_security','poverty',
                 'own_home','MedHomeVal','male','Hispanic','White','Black')
  
  # Divide percentage columns by 100
  for (col in names(df)[-c(1,2,11)]) {
    df[[col]] <- as.numeric(df[[col]])/100
  }
  
  # Combine other race/ethnicities
  df$Other <- 1- df$Hispanic - df$White - df$Black
  
  # Remove NA's
  df <- na.omit(df)
  
  # Create variable for no high school education
  df$no_hs <- 1-df$hs_or_higher
  
  # Adjust median home value https://fred.stlouisfed.org/series/CSUSHPINSA April values
  #hpi <- c('2010'=1.428, '2011'=1.492, '2012'=1.500, '2013'=1.376, '2014'=1.274, '2015'=1.222,'2016'=1.164,'2017'=1.102, '2018'=1.035,'2019'=1)
  #df$MedHomeVal <- hpi[[as.character(year)]]*df$MedHomeVal
  
  # Add rural percentage
  df <- merge(df, dec[,c('Tract','rural')], by='Tract')
  
  return (df)
}

# Get all tracts for WI
tracts <- get_acs('tract', 
                  variables = c('DP02_0064PE'), 
                  state='55', 
                  year = 2010, output='wide') %>% 
  select(-ends_with("M"))
tracts <- tracts[,c('GEOID','NAME')]

# Load economic mobility regression model
mod <- readRDS(file.path('./models', 'em_model'))

# Get census data and predict EMscore for each tract for each year
df <- data.frame()
for (year in 2010:2019) {
  temp <- get_census_data(year)
  temp[[as.character(year)]] <- exp(predict(mod, temp))
  tracts <- merge(tracts, temp[,c(1,19)], by.x='GEOID', by.y='Tract')
  temp[['year']] <- year
  temp[['pred.EMscore']] <- temp[[as.character(year)]]
  df <- rbind(df, temp[,-c(19)])
}

# Create linear trend model for each tract
for (i in 1:nrow(tracts)) {
  tractTS <- ts(t(tracts[i,c(3:12)]), start=2010, end=2019)
  lt <- tslm(tractTS ~ trend) 
  tracts[i,'Trend'] <- lt$coefficients[[2]]
  tracts[i,'Intercept'] <- lt$coefficients[[1]]
  tracts[i, 'R2'] <- summary(lt)$adj.r.squared
  tracts[i, 'p.value'] <- summary(lt)$coefficients[,"Pr(>|t|)"][[2]]
}

# Read data and add actual EM score
data <- fread(file.path('./data', 'final_data.csv'), header = TRUE,colClasses = c(c('character'), rep(NA, 54)))
tracts <- merge(tracts, data[,c('Tract','EMscore')], by.x = 'GEOID', by.y = 'Tract')

# lowest trends
#55079020400 (722)
#55059001900 (421)
#55079001000 (567)

# highest trends
#55025003200 (192)
#55133201203 (1231)
#55025000901 (160)

# Plot lowest and highest trends
plot(2010:2019, tracts[192,c(3:12)], type='l',ylim=c(3.8,32), ylab='Predicted Economic Mobility', xlab='Year', main='Economic Mobility Trends in Wisconsin')
lines(2010:2019, tracts[192,'Intercept']+(1:10)*tracts[192,'Trend'], col='red', lty='dashed')
text(2018.5,29.5,'Tract 55079020400',cex=.6)
lines(2010:2019, tracts[1231,c(3:12)], type='l')
lines(2010:2019, tracts[1231,'Intercept']+(1:10)*tracts[1231,'Trend'], col='red', lty='dashed')
text(2018.5,20.5,'Tract 55133201203',cex=.6)
lines(2010:2019, tracts[160,c(3:12)], type='l')
lines(2010:2019, tracts[160,'Intercept']+(1:10)*tracts[160,'Trend'], col='red', lty='dashed')
text(2018.5,22.75,'Tract 55025000901',cex=.6)
lines(2010:2019, tracts[722,c(3:12)], type='l')
lines(2010:2019, tracts[722,'Intercept']+(1:10)*tracts[722,'Trend'], col='red', lty='dashed')
text(2018.5,11,'Tract 55079020400',cex=.6)
lines(2010:2019, tracts[421,c(3:12)], type='l')
lines(2010:2019, tracts[421,'Intercept']+(1:10)*tracts[421,'Trend'], col='red', lty='dashed')
text(2018.5,13,'Tract 55059001900',cex=.6)
lines(2010:2019, tracts[567,c(3:12)], type='l')
lines(2010:2019, tracts[567,'Intercept']+(1:10)*tracts[567,'Trend'], col='red', lty='dashed')
text(2018.5,7,'Tract 55079001000',cex=.6)

# Histrogram of trends in WI
hist(tracts$Trend, xlab='Trend', main='Histogram of Economic Mobility Trends in WI')

# Percent of tracts that have a positive trend
sum(tracts$Trend > 0)/nrow(tracts)

# Get geometry for WI tracts and merge with data
t <- tigris::tracts(state='55', cb=TRUE, year=2019)
tracts_geo <- merge(tracts, t[,c('GEOID','STATEFP','COUNTYFP','geometry')], by.x='GEOID', by.y='GEOID')

# WI EM scores
ggplot() +
  geom_sf(data = tracts_geo, aes(fill = EMscore, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-93,-87), y=c(42,47)) + 
  theme_minimal()

# WI EM trends
ggplot() +
  geom_sf(data = tracts_geo, aes(fill = Trend, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-93,-87), y=c(42,47)) + 
  theme_minimal()

# MK EM scores
ggplot() +
  geom_sf(data = subset(tracts_geo, COUNTYFP=='079'), aes(fill = EMscore, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-88.1,-87.8), y=c(42.8,43.2)) + 
  theme_minimal()

# MK EM Trends
ggplot() +
  geom_sf(data = subset(tracts_geo, COUNTYFP=='079'), aes(fill = Trend, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-88.1,-87.8), y=c(42.8,43.2)) + 
  theme_minimal()

# Wauk EM scores
ggplot() +
  geom_sf(data = subset(tracts_geo, COUNTYFP=='133'), aes(fill = EMscore, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-88.6,-88), y=c(42.8,43.2)) + 
  theme_minimal()

# Wauk EM trends
ggplot() +
  geom_sf(data = subset(tracts_geo, COUNTYFP=='133'), aes(fill = Trend, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-88.6,-88), y=c(42.8,43.2)) + 
  theme_minimal()

