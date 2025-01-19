### This script loads the financial well-being survey data from the CFPB
### Performs some transformations to prepare the data for a linear regression
### Fits a linear regression model to predict FWB score
### Evaluates model performance and saves the model for future use.

# Clear and set working directory
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load libraries
library(data.table)
library(car)
library(dplyr)
library(MLmetrics)
library(corrplot)
library(ggplot2)

# Load financial well-being survey (FWBS) data
fwb <- fread(file.path('./data', 'NFWBS_PUF_2016_data.csv'), header = TRUE,
             select = c('PPETHM','PPGENDER','agecat','PPINCIMP','VALUERANGES','SAVINGSRANGES',
                        'MILITARY','ABSORBSHOCK','EMPLOY','SNAP','fpl','PRODHAVE_3','SOCSEC1','FWBscore','HOUSERANGES'))

# correlation plot for variables in FWBS data
## Poverty/income, SS/age have somewhat high cors but will be retained
M <- cor(fwb)
corrplot(M, method='number')

# Recode factor levels to make them more intuitive to work with
fwb$race_ethnicity <- dplyr::recode(fwb$PPETHM, `1`='White, Non-Hispanic', `2`='Black, Non-Hispanic', `3`='Other, Non-Hispanic', `4`='Hispanic')
fwb$male <- dplyr::recode(fwb$PPGENDER, `1`=1, `2`=0)
fwb$age <- dplyr::recode(fwb$agecat, `1`='18-24', `2`='25-34', `3`='35-44', `4`='45-54', `5`='55-74', `6`='55-74', `7`='55-74', `8`='75+')
fwb$income <- dplyr::recode(fwb$PPINCIMP, `1`='Less than 20,000', `2`='20,000-29,999', `3`='30,000-39,999', `4`='40,000-49,999',
                     `5`='50,000-59,999', `6`='60,000-74,999', `7`='75,000-99,999', `8`='100,000-149,999', `9`='150,000+')
fwb$savings <- dplyr::recode(fwb$SAVINGSRANGES, `-1`='Unknown', `1`='0', `2`='1-99', `3`='100-999', `4`='1000-4999', `5`='5000-19999',
                      `6`='20000-74999', `7`='75000+', `98`='Unknown', `99`='Unknown')
fwb$military <- dplyr::recode(fwb$MILITARY, `-1`=0, `0`=0, `1`=1)
fwb$home_value <- dplyr::recode(fwb$VALUERANGES, `-2`='NA', `-1`='NA', `1`='<150000', `2`='150000-249999', `3`='250000-399999',`4`='400000+',
                         `98`='NA',`99`='NA')
fwb$absorb_shock <- dplyr::recode(fwb$ABSORBSHOCK, `-1`=0, `1`=0, `2`=0, `3`=1, `4`=1, `8`=0)
fwb$unemployed <- ifelse(fwb$EMPLOY == 7, 1, 0)
fwb$snap <- dplyr::recode(fwb$SNAP, `-1`=0, `0`=0, `1`=1, `8`=0)
fwb$poverty <- dplyr::recode(fwb$fpl, `1`=1, `2`=0, `3`=0)
fwb$social_security <- dplyr::recode(fwb$SOCSEC1, `-2`=0, `-1`=0, `0`=0, `1`=1)
fwb$health_ins <- fwb$PRODHAVE_3

# Select columns
fwb <- fwb[,c('race_ethnicity','male','age','income','savings','military','home_value',
              'absorb_shock','unemployed','snap','poverty','social_security','health_ins','FWBscore')]

# Indicator variables for age ranges
fwb$Age18_24 <- ifelse(fwb$age == '18-24',1,0)
fwb$Age25_34 <- ifelse(fwb$age == '25-34',1,0)
fwb$Age35_44 <- ifelse(fwb$age == '35-44',1,0)
fwb$Age45_54 <- ifelse(fwb$age == '45-54',1,0)
fwb$Age55_74 <- ifelse(fwb$age == '55-74',1,0)

# Indicator variables for race/ethnicity
fwb$Hispanic <- ifelse(fwb$race_ethnicity == 'Hispanic',1,0)
fwb$Black <- ifelse(fwb$race_ethnicity == 'Black, Non-Hispanic',1,0)
fwb$Other <- ifelse(fwb$race_ethnicity == 'Other, Non-Hispanic',1,0)

# Indicator variables for income levels
fwb$`incomelt20000` <- ifelse(fwb$income == 'Less than 20,000', 1, 0)
fwb$`income20000_29999` <- ifelse(fwb$income == '20,000-29,999', 1, 0)
fwb$`income30000_39999` <- ifelse(fwb$income == '30,000-39,999', 1, 0)
fwb$`income40000_49999` <- ifelse(fwb$income == '40,000-49,999', 1, 0)
fwb$`income50000_74999` <- ifelse(fwb$income %in% c('50,000-59,999','60,000-74,999'), 1, 0)
fwb$`income75000_99999` <- ifelse(fwb$income == '75,000-99,999', 1, 0)
fwb$`income100000_149999` <- ifelse(fwb$income == '100,000-149,999', 1, 0)

# Indicator variables for savings ranges
fwb$`savings0` <- ifelse(fwb$savings=='0',1,0)
fwb$`savings1_99` <- ifelse(fwb$savings=='1-99',1,0)
fwb$`savings100_999` <- ifelse(fwb$savings=='100-999',1,0)
fwb$`savings1000_4999` <- ifelse(fwb$savings=='1000-4999',1,0)
fwb$`savings5000_19999` <- ifelse(fwb$savings=='5000-19999',1,0)
fwb$`savings20000_74999` <- ifelse(fwb$savings=='20000-74999',1,0)
fwb$`savingsUnknown` <- ifelse(fwb$savings=='Unknown',1,0)

# Indicator variables for home value ranges
fwb$`hvNA` <- ifelse(fwb$home_value == 'NA', 1,0)
fwb$`hvlt150000` <- ifelse(fwb$home_value == '<150000', 1,0)
fwb$`hv150000_249999` <- ifelse(fwb$home_value == '150000-249999', 1,0)
fwb$`hv250000_399999` <- ifelse(fwb$home_value == '250000-399999', 1,0)

# Drop columns where indicator variables were created
fwb <- fwb[,-c('race_ethnicity','age','income','savings','home_value')]

# Create linear regression model to predict FWBscore
mod <- lm(FWBscore ~ .-social_security, data=fwb)

# Check VIFs. Looks good.
vif(mod)

# Check resids. Look good
plot(mod, which=1)

# Performance eval
preds_lm <- predict(mod, fwb)
summary(mod)
MAPE(preds_lm, fwb$FWBscore)
MAE(preds_lm, fwb$FWBscore)
RMSE(preds_lm, fwb$FWBscore)
mean(residuals(mod))
cor(preds_lm, fwb$FWBscore)
mean(fwb$FWBscore)

# Plot predicted vs actual values
ggplot() + geom_point(aes(x=preds_lm, y=fwb$FWBscore), alpha=.5, color='black') +
  labs(x='Predicted FWB score', y='Actual FWB score',title='Predicted vs. Actual FWB score') +
  theme(plot.title = element_text(hjust = 0.5))

# Save the model. 
saveRDS(mod, file.path('./models', 'fwb_model'))


