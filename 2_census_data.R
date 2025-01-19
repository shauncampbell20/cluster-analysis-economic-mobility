### This script uses the tidycensus package to retrieve census data.
### Data from the 2019 ACS community survey tables are used.
### The data is transformed so the FWB score prediction model can be applied.
### Other data points are also gather for further analysis.

# Clear working director
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
library(data.table)
library(readxl)
library(tidycensus)
library(tidyverse)

############### CENSUS ###############

# Census API key
key <- ##KEY HERE##
census_api_key(key)

# List of FIPS state codes to retrieve data for
states = c('01','02','04','05','06','08','09','10','11','12','13','15','16','17','18','19','20','21','22','23','24','25',
           '26','27','28','29','30','31','32','33','34','35','36','37','38','39','40','41','42','44','45','46','47','48','49',
           '50','51','53','54','55','56')

# Function to retrieve and transform census data by year for each tract
get_census_data <- function (year) {
  # Create empty data frame
  df <- data.frame()
  
  # For each state, retrieve census data using tidycensus 
  for (s in states) {
    temp <- get_acs('tract', 
                    variables = c('GEO_ID','DP03_0062E','DP05_0018E','DP04_0089E','DP04_0134M','DP02_0017E','DP05_0002PE',
                                  'DP05_0005PE','DP05_0006PE','DP05_0007PE','DP05_0008PE','DP05_0009PE','DP05_0010PE',
                                  'DP05_0011PE','DP05_0012PE','DP05_0013PE','DP05_0014PE','DP05_0015PE','DP05_0016PE',
                                  'DP05_0017PE','DP05_0071PE','DP05_0077PE','DP05_0078PE','DP05_0079PE','DP05_0080PE',
                                  'DP05_0081PE','DP05_0082PE','DP05_0083PE','DP05_0021PE','DP03_0006PE','DP03_0009PE',
                                  'DP03_0052PE','DP03_0053PE','DP03_0054PE','DP03_0055PE','DP03_0056PE','DP03_0057PE',
                                  'DP03_0058PE','DP03_0059PE','DP03_0060PE','DP03_0061PE','DP03_0066PE','DP03_0074PE',
                                  'DP03_0096PE','DP03_0128PE','DP04_0046PE','DP02_0002PE','DP02_0093PE','DP02_0067PE',
                                  'DP02_0068PE','DP02_0152PE','DP02_0153PE'
                                  ), 
                                state=s, 
                                year = year, output='wide') %>% 
      select(-ends_with("M"))
    
    # Append data to df
    df <- rbind(df, temp)
    print(s)
  }
  
  # Divide percentage columns by 100
  for (col in names(df)[8:ncol(df)]) {
    df[[col]] <- as.numeric(df[[col]])/100
  }
  
  # Rename columns
  names(df) <- c('Tract','Name','MedIncome','MedAge','MedHomeVal','MedRent','avg_fam_size','male',
                  'Under 5 years','5-9 years','10-14 years','15-19 years','20-24 years',
                  '25-34 years','35-44 years','45-54 years','55-59 years','60-64 years',
                  '65-74 years','75-84 years', '85+ years','Hispanic','White','Black','American Indian',
                  'Asian','Pacific Islander','Other','Two or More races','18 years and over',
                  'military','unemployed','<10000','10000-14999','15000-24999','25000-34999',
                  '35000-49999','income50000_74999','income75000_99999','income100000_149999',
                  '150000-199999','200000+','social_security','snap','health_ins','poverty',
                  'own_home','married','foreign_born','hs_or_higher','bachelor_or_higher',
                  'computer','internet')
  
  # Combine other race/ethnicities
  df$Other <- df$`American Indian`+df$Asian+df$`Pacific Islander`+df$Other+df$`Two or More races`
  
  # Age buckets
  df$`18-19 years` <- df$`5-9 years`+df$`Under 5 years`+df$`10-14 years`+df$`15-19 years` - 1 + df$`18 years and over`
  df$Age18_24 <- (df$`18-19 years`+df$`20-24 years`)/df$`18 years and over`
  df$Age25_34 <- (df$`25-34 years`)/df$`18 years and over`
  df$Age35_44 <- (df$`35-44 years`)/df$`18 years and over`
  df$Age45_54 <- (df$`45-54 years`)/df$`18 years and over`
  df$Age55_74 <- (df$`55-59 years`+df$`60-64 years`+df$`65-74 years`)/df$`18 years and over`
  df$Age_75 <- (df$`75-84 years`+df$`85+ years`)/df$`18 years and over`
  
  # Income buckets
  df$`incomelt20000` <- df$`<10000`+df$`10000-14999`+(df$`15000-24999`/2)
  df$`income20000_29999` <- (df$`15000-24999`/2)+(df$`25000-34999`/2)
  df$`income30000_39999` <- (df$`25000-34999`/2)+(df$`35000-49999`/3)
  df$`income40000_49999` <- ((df$`35000-49999`)*2)/3
  df$`income150000+` <- df$`150000-199999`+df$`200000+`
  
  # Replace some na's with median/mean
  df[which(is.na(df$own_home)),'own_home'] <- mean(na.omit(df$own_home))
  df[which(is.na(df$MedHomeVal)),'MedHomeVal'] <- median(na.omit(df$MedHomeVal))
  df[which(is.na(df$MedRent)),'MedRent'] <- median(na.omit(df$MedRent))
  
  # Home value buckets
  df$hvNA <- 1-df$own_home
  df$hvlt150000 <- ifelse(df$MedHomeVal < 150000, df$own_home, 0)
  df$hv150000_249999 <- ifelse(df$MedHomeVal >= 150000 & df$MedHomeVal < 250000, df$own_home, 0)
  df$hv250000_399999 <- ifelse(df$MedHomeVal >= 250000 & df$MedHomeVal < 400000, df$own_home, 0)
  
  # Remove NA's
  df <- na.omit(df)
  
  # Create variable for no high school education
  df$no_hs <- 1-df$hs_or_higher
  
  # Select final columns
  df <- df[,c('Tract','Name','male','Age18_24','Age25_34','Age35_44','Age45_54','Age55_74','Age_75','Hispanic','White',
              'Black','Other','MedAge','military','unemployed','incomelt20000','income20000_29999','income30000_39999',
              'income40000_49999','income50000_74999','income75000_99999','income100000_149999','income150000+',
              'social_security','snap','health_ins','poverty','MedIncome',
              'hvNA','hvlt150000','hv150000_249999','hv250000_399999','MedHomeVal','own_home','MedRent','married',
              'foreign_born','no_hs','bachelor_or_higher','computer','internet')]
  
  return (df)
}


## !!!! This part takes a while (5-10 mins) !!!!!
# Get census data for 2019. 
df <- get_census_data(2019)

# For each state, retrieve urban and rural populations from 2010 decennial census
df2 <- data.frame()
for (s in states) {
  temp <- get_decennial(geography = "Tract", 
                        state = s,
                        variables = c('H002002','H002005'), 
                        sumfile = 'sf1',
                        year = 2010,
                        output = 'wide')
  df2 <- rbind(df2, temp)
  print(s)
}
names(df2) <- c('Tract','Name','urban_pop','rural_pop')

# Create variable for rural population percentage of the tract
df2$rural <- df2$rural_pop / (df2$urban_pop+df2$rural_pop)

############### PUMA ###############

# Read financial health data, select and rename columns
df3 <- read_excel(file.path('./data', 'puma_financial_health_metrics.xlsx'))
df3 <- df3[,c(1,2,3,4,5,9,10,13,14,15)]
names(df3)[c(6,7,8,9,10)] <- c('absorb_shock','credit_score','net_worth','own_home','home_value')

# Create savings ranges indicators using net worth as a proxy
df3$savings0 <- ifelse(df3$net_worth < 1, 1, 0)
df3$savings1_99 <- ifelse(df3$net_worth < 100 & df3$net_worth > 0, 1, 0)
df3$savings100_999 <- ifelse(df3$net_worth < 1000 & df3$net_worth >= 100, 1, 0)
df3$savings1000_4999 <- ifelse(df3$net_worth < 5000 & df3$net_worth >= 1000, 1, 0)
df3$savings5000_19999 <- ifelse(df3$net_worth < 20000 & df3$net_worth >= 5000, 1, 0)
df3$savings20000_74999 <- ifelse(df3$net_worth < 75000 & df3$net_worth >= 20000, 1, 0)
df3$savingsUnknown <- 0

# Make credit score numeric and replace NA's with mean
df3$credit_score <- as.numeric(df3$credit_score)
df3[which(is.na(df3$credit_score)),'credit_score'] <- mean(na.omit(df3$credit_score))

# Tract to PUMA relation file
t2p <- read.csv(file.path('./data', '2010_Census_Tract_to_2010_PUMA.txt'),colClasses = "character")
t2p$Tract <- paste(t2p$STATEFP, t2p$COUNTYFP, t2p$TRACTCE, sep='')
t2p$state_puma <- paste(t2p$STATEFP, t2p$PUMA5CE, sep='')
t2p$state_puma <- as.numeric(t2p$state_puma)

# Merge PUMA data with tracts 
df3 <- merge(t2p, df3 , by = 'state_puma')
df3<- df3[,c('Tract','absorb_shock','credit_score','net_worth','savings0','savings1_99','savings100_999',
             'savings1000_4999','savings5000_19999','savings20000_74999','savingsUnknown')]

##################################

# Merge all data by tract
dfFinal <-merge(merge(df, df2[,c('Tract','rural')], by='Tract'), df3, by='Tract')

# Add financial well-being score using LM model
mod <- readRDS(file.path('./models', 'fwb_model'))
dfFinal$FWBscore <- predict(mod, dfFinal)

# Read economic mobility data
mobility <- read.csv(file.path('./data', 'tract_kir_top20_rP_gP_p25.csv'), colClasses = 'character')

# Function to fix the tract format
fix_tract <- function(x) {
  if (nchar(x) == 10) {
    return (paste('0',x,sep=''))
  } else {
    return (x)
  }
}

# Rename columns, transform to numeric, fix the tract format
names(mobility) <- c('Tract','Name','EMscore')
mobility$EMscore <- as.numeric(mobility$EMscore)*100
mobility$Tract <- sapply(mobility$Tract, fix_tract)

# Merge EM data, replace NA's with median
dfFinal <- merge(dfFinal, mobility[,c('Tract','EMscore')], by='Tract')
dfFinal[which(is.na(dfFinal$EMscore)),'EMscore'] <- median(dfFinal$EMscore)

# Omit columns with NA and save to CSV
dfFinal <- na.omit(dfFinal)
write.csv(dfFinal, file.path('./data','final_data.csv'), row.names = FALSE)

