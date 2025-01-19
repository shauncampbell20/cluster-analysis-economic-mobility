# This script scales the census data and performs heirarchical and k-means clustering.
# Summaries and various linear models are created for each cluster.
# A linear regression model to predict economic mobility based on census data is created.
# Various map visualizations are created. 

# Clear working directory
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
library(readxl)
library(data.table)
library(cluster)
library(corrplot)
library(car)
library(MLmetrics)
library(tigris)
library(ggplot2)

# Read data
df <- fread(file.path('./data', 'final_data.csv'), header = TRUE,colClasses = c(c('character'), rep(NA, 54)))

# subset of columns for clustering and omit na's
df_subset <- df[,c('male','Hispanic','Black','Other','military','unemployed','social_security','snap','health_ins','poverty',
           'absorb_shock','net_worth','own_home','MedIncome','MedHomeVal','MedAge','MedRent','married','foreign_born','no_hs',
           'bachelor_or_higher','computer','internet','credit_score','rural','Tract')]
df_subset <- na.omit(df_subset)

#######################################
#             Clustering              #
#######################################

# Scale data
df_scaled <- data.frame(scale(df_subset[,1:25]), df_subset[,26])

# !!!! Might take a while !!!!!
# Take random sub sample for hierarchical clustering
set.seed(123)
ind <- sample(1:nrow(df), 5000)
df_sample <- df_scaled[ind,]

# calculate distances and perform AGNES
x <- dist(df_sample[,1:25], method = "euclidean")
aResult <- agnes(x, diss = TRUE, method = "ward")

# Plot dendrogram
plot(aResult)
abline(h=140, col='red')
abline(h=85, col='red')
abline(h=55, col='red')
abline(h=40, col='red')

## Evaluated median and mean values of cut points and decided on 18 ##

# K-means clustering
set.seed(1)
num_clusters <- 18
kResult <- kmeans(df_scaled[,1:25], centers=num_clusters, iter.max = 20)

# Add cluster to original data, scaled data, and FWBscore and EMscore
df_eval <- data.frame(df_subset, kResult$cluster, df[, c('FWBscore','EMscore')])
df_scaled <- data.frame(df_scaled, kResult$cluster, df[, c('FWBscore','EMscore')])

# Create data frame to store median values for each cluster
medians <- data.frame()
for (i in 1:num_clusters) {
  medians <- rbind(medians, apply(subset(df_eval[,-26], kResult.cluster==i),2,median)) 
}
names(medians) <- names(df_eval[,-26])

# Calculate correlation btw FWBscore and EMscore for each cluster
correls <- data.frame()
for (i in 1:num_clusters) {
  crl <- cor(subset(df_eval, kResult.cluster==i)$FWBscore ,
             subset(df_eval, kResult.cluster==i)$EMscore)
  correls <- rbind(correls, data.frame(kResult.cluster=i, correl = crl))
}
medians <- merge(medians, correls)

#scatter plot of Financial well being score and economic mobility score
plot(df_eval$FWBscore, df_eval$EMscore, xlab='Financial Well-Being Score', ylab='Economic Mobility')

# Plot linear relationship between FWBscore and EMscore for each cluster
par(mfrow=c(3,6))
for (i in 1:18) {
  plot(subset(df_eval, kResult.cluster==i)$FWBscore, subset(df_eval, kResult.cluster==i)$EMscore, main=paste('k =',i),xlab='',ylab='')
  abline(lm(EMscore ~ FWBscore, data = subset(df_eval, kResult.cluster==i)), col='red')
}

# Plot linear relationship for cluster 18. One outlier is removed.
ggplot(data=subset(df_eval, kResult.cluster==18 & EMscore<73.31),aes(x=FWBscore, y=EMscore)) + geom_point(alpha=0.5) + geom_smooth(method='lm') +
  labs(x='Predicted Financial Well-Being Score', x='Economic Mobility', title='Financial Well-Being vs Economic Mobility in Cluster 18') +
  theme(plot.title = element_text(hjust = 0.5))

# Linear regression of FWBscore and EMscore for each cluster
mat <- matrix(nrow=18, ncol=5, dimnames = list(c(),c('r.squared','intercept','coef','p.coef','f.stat')))
for (i in 1:18) {
  mod <- summary(lm(EMscore ~ FWBscore, data = subset(df_eval, kResult.cluster==i)))
  mat[i,1] <- mod$r.squared
  mat[i,2] <- mod$coefficients[[1]]
  mat[i,3] <- mod$coefficients[[2]]
  mat[i,4] <- mod$coefficients[[8]]
  mat[i,5] <- mod$fstatistic[[1]]
}

#######################################
# Linear model for predicting EMscore #
#######################################

# Remove rows with 0 EM score
df_scaled<-df_scaled[df_scaled$EMscore!=0,]
df_eval<-df_eval[df_eval$EMscore!=0,]

# Corrplot to investigate correlations and potential collinearity
par(mfrow=c(1,1))
m <- cor(df_scaled[,-c(26,27)])
corrplot(m,method='number')

# high correlations:
#ss/age
#poverty/snap
#net worth/absorb shock
#own_home/married
#medincome/medrent
#medincome/bachelor or higher
#medrent/medhomeval
#internet/computer
#creditscore/absorbshock

# Remove some variables with high correlations
df_eval <- df_eval[,-which(names(df_eval) %in% c('FWBscore','absorb_shock','net_worth','credit_score',
                                      'MedAge','married','MedRent','computer','MedIncome','snap','internet','health_ins'))]

# Create model
mod <- lm(EMscore~.-kResult.cluster-Tract, data=df_eval)
summary(mod)

# Model evaluation
preds_lm <- predict(mod, df_eval)
MAPE(preds_lm, df_eval$EMscore)
MAE(preds_lm, df_eval$EMscore)
RMSE(preds_lm, df_eval$EMscore)
cor(preds_lm, df_eval$EMscore)

# Check VIF. Looks good
vif(mod)

# Check resids. Changing variance appears to be an issue
plot(mod, which=1)

# Try with logged EMscore and log med home val
mod2 <- lm(log(EMscore)~.-MedHomeVal+log(MedHomeVal), data = df_eval[,-which(names(df_eval) %in% c('kResult.cluster','Tract'))])
summary(mod2)

# Check resids. Changing variance may still be an issue but is better. Resids are normal but skewed a bit
plot(mod2, which=1)
plot(density(mod2$residuals))

# Evaluate model
preds_lm <- predict(mod2, df_eval)
plot(preds_lm, log(df_eval$EMscore),xlab='y hat', ylab='y')
MAPE(preds_lm, log(df_eval$EMscore))
MAE(preds_lm, log(df_eval$EMscore))
exp(MAE(preds_lm, log(df_eval$EMscore)))
RMSE(preds_lm, log(df_eval$EMscore))
exp(RMSE(preds_lm, log(df_eval$EMscore)))
cor(preds_lm, log(df_eval$EMscore))

# Save model for future use. 
saveRDS(mod2, file.path('./models', 'em_model'))

# Plot each variable with EM score
par(mfrow=c(4,4))
for (col in c('male','Hispanic','Black','Other','military','unemployed','social_security','poverty','own_home','MedHomeVal',
              'foreign_born','no_hs','bachelor_or_higher','rural')) {
  plot(df_eval[[col]], log(df_eval$EMscore), xlab=col, ylab='EMscore')
}

# Matrix with regression coefficients for each cluster
df_eval$log_em <- log(df_eval$EMscore)
mat2 <- matrix(nrow=18, ncol=17, dimnames = list(c(),c('Intercept','male','Hispanic','Black','Other','military','unemployed','social_security',
                                                       'poverty','own_home','foreign_born','no_hs','bachelor_or_higher',
                                                       'rural','log(MedHomeVal)','adj.r.squared','fstatistic')))
for (i in 1:18) {
  cluster_mod <- lm(log_em ~.-kResult.cluster-EMscore-Tract-MedHomeVal+log(MedHomeVal), data=subset(df_eval, kResult.cluster==i))
  mod_summary <- summary(cluster_mod)
  mat2[i,1:15] <- paste(round(mod_summary$coefficients[,'Estimate'],4),' (', round(mod_summary$coefficients[,"Pr(>|t|)"],4),')', sep='')
  mat2[i,16] <- mod_summary$adj.r.squared
  mat2[i,17] <- mod_summary$fstatistic[[1]]
}

# Create scatter plots for cluster 18
cluster18 <- subset(df_eval, kResult.cluster==18)

# HS graduation rate vs EM score
ggplot(data=subset(cluster18, EMscore<73.31), aes(x=1-no_hs, y=EMscore)) + geom_point(alpha=.5, color='black') + geom_smooth(method='lm', se=TRUE) +
  labs(x='High School Graduation Rate', y = 'Economic Mobility Score', title='HS Graduation Rate vs Economic Mobility in Cluster 18')+
  theme(plot.title = element_text(hjust = 0.5))

# Home ownership vs EM score
ggplot(data=subset(cluster18, EMscore<73.31), aes(x=own_home, y=EMscore)) + geom_point(alpha=.5, color='black') + geom_smooth(method='lm', se=TRUE) +
  labs(x='Home Ownership Rate', y = 'Economic Mobility Score', title='Home Ownership Rate vs Economic Mobility in Cluster 18')+
  theme(plot.title = element_text(hjust = 0.5))

# Bachelor degree or higher vs EM score
ggplot(data=subset(cluster18, EMscore<73.31), aes(x=bachelor_or_higher, y=EMscore)) + geom_point(alpha=.5, color='black') + geom_smooth(method='lm', se=TRUE) +
  labs(x='Bachleor or Higher Attainment Rate', y = 'Economic Mobility Score', title='Post-Secondary Education vs Economic Mobility in Cluster 18')+
  theme(plot.title = element_text(hjust = 0.5))


#######################################
#              Map Viz                #
#######################################

# Get geometry for all census tracts
t <- tigris::tracts(state=NULL, cb=TRUE, year=2019)

# Merge data with geometry
geo <- merge(df_eval, t[,c('GEOID','STATEFP','COUNTYFP','geometry')], by.x='Tract', by.y='GEOID')
geo <- merge(geo, df[,c('Tract','FWBscore')], by='Tract')
geo$kResult.cluster <- as.factor(geo$kResult.cluster)

# Clusters, Whole US
ggplot() +
  geom_sf(data = geo, aes(fill = kResult.cluster, geometry = geometry)) +
  #scale_fill_gradient(low = "red", high = "green") +
  lims(x=c(-125,-66), y=c(24,50)) + 
  theme_minimal()

# EMscore, Whole US
ggplot() +
  geom_sf(data = geo, aes(fill = EMscore, geometry = geometry)) +
  scale_fill_gradient(low = "orange", high = "green") +
  lims(x=c(-125,-66), y=c(24,50)) + 
  theme_minimal()

# FWBscore, Whole US
ggplot() +
  geom_sf(data = geo, aes(fill = FWBscore, geometry = geometry)) +
  scale_fill_gradient(low = "red", high = "green") +
  lims(x=c(-125,-66), y=c(24,50)) + 
  theme_minimal()

# Subset of just Wisconsin
wi <- subset(geo, STATEFP == '55')
wi$kResult.cluster <- as.factor(wi$kResult.cluster)

# Wisconsin clusters
ggplot() +
  geom_sf(data = wi, aes(fill = kResult.cluster, geometry = geometry)) +
  #scale_fill_brewer(palette = "Set3") +
  lims(x=c(-93,-87), y=c(42,47)) + 
  theme_minimal()

# Milwaukee county clusters
ggplot() +
  geom_sf(data = subset(wi, COUNTYFP=='079'), aes(fill = kResult.cluster, geometry = geometry)) +
  #scale_fill_gradient(low='red', high='green') +
  lims(x=c(-88.1,-87.8), y=c(42.8,43.2)) + 
  theme_minimal()

# Waukesha county clusters
ggplot() +
  geom_sf(data = subset(wi, COUNTYFP=='133'), aes(fill = kResult.cluster, geometry = geometry)) +
  #scale_fill_gradient(low='red', high='green') +
  lims(x=c(-88.6,-88), y=c(42.8,43.2)) + 
  theme_minimal()

# Dane county clusters
ggplot() +
  geom_sf(data = subset(wi, COUNTYFP=='025'), aes(fill = kResult.cluster, geometry = geometry)) +
  #scale_fill_gradient(low='red', high='green') +
  lims(x=c(-89.9,-89), y=c(42.8,43.4)) + 
  theme_minimal()

# Wisconsin EM score
ggplot() +
  geom_sf(data = wi, aes(fill = EMscore, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-93,-87), y=c(42,47)) + 
  theme_minimal()

# Milwaukee county EM score
ggplot() +
  geom_sf(data = subset(wi, COUNTYFP=='079'), aes(fill = EMscore, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-88.1,-87.8), y=c(42.8,43.2)) + 
  theme_minimal()

# Waukesha county EM score
ggplot() +
  geom_sf(data = subset(wi, COUNTYFP=='133'), aes(fill = EMscore, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-88.6,-88), y=c(42.8,43.2)) + 
  theme_minimal()

# Dane county EM score
ggplot() +
  geom_sf(data = subset(wi, COUNTYFP=='025'), aes(fill = EMscore, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-89.9,-89), y=c(42.8,43.4)) + 
  theme_minimal()

# Wisconsin FWB score
ggplot() +
  geom_sf(data = wi, aes(fill = FWBscore, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-93,-87), y=c(42,47)) + 
  theme_minimal()

# Milwaukee county FWB score
ggplot() +
  geom_sf(data = subset(wi, COUNTYFP=='079'), aes(fill = FWBscore, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-88.1,-87.8), y=c(42.8,43.2)) + 
  theme_minimal()

# Waukesha county FWB score
ggplot() +
  geom_sf(data = subset(wi, COUNTYFP=='133'), aes(fill = FWBscore, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-88.6,-88), y=c(42.8,43.2)) + 
  theme_minimal()

# Dane county FWB score
ggplot() +
  geom_sf(data = subset(wi, COUNTYFP=='025'), aes(fill = FWBscore, geometry = geometry)) +
  scale_fill_gradient(low='red', high='green') +
  lims(x=c(-89.9,-89), y=c(42.8,43.4)) + 
  theme_minimal()

# Mississippi clusters
ggplot() +
  geom_sf(data = subset(geo, STATEFP=='28'), aes(fill = kResult.cluster, geometry = geometry)) +
  #scale_fill_gradient(low = "red", high = "green") +
  lims(x=c(-92,-88), y=c(30,35)) + 
  theme_minimal()

# Mississippi EM score
ggplot() +
  geom_sf(data = subset(geo, STATEFP=='28'), aes(fill = EMscore, geometry = geometry)) +
  scale_fill_gradient(low = "red", high = "green") +
  lims(x=c(-92,-88), y=c(30,35)) + 
  theme_minimal()

# Mississippi FWB score
ggplot() +
  geom_sf(data = subset(geo, STATEFP=='28'), aes(fill = FWBscore, geometry = geometry)) +
  scale_fill_gradient(low = "red", high = "green") +
  lims(x=c(-92,-88), y=c(30,35)) + 
  theme_minimal()

# New Jersey Ckysters
ggplot() +
  geom_sf(data = subset(geo, STATEFP=='34'), aes(fill = kResult.cluster, geometry = geometry)) +
  #scale_fill_gradient(low = "red", high = "green") +
  lims(x=c(-76,-73.9), y=c(38.9,41.5)) +
  theme_minimal()

# New jersey EM score
ggplot() +
  geom_sf(data = subset(geo, STATEFP=='34'), aes(fill = EMscore, geometry = geometry)) +
  scale_fill_gradient(low = "red", high = "green") +
  lims(x=c(-76,-73.9), y=c(38.9,41.5)) +
  theme_minimal()

# New jersey FWB score
ggplot() +
  geom_sf(data = subset(geo, STATEFP=='34'), aes(fill = FWBscore, geometry = geometry)) +
  scale_fill_gradient(low = "red", high = "green") +
  lims(x=c(-76,-73.9), y=c(38.9,41.5)) +
  theme_minimal()

# California clusters
ggplot() +
  geom_sf(data = subset(geo, STATEFP=='06'), aes(fill = kResult.cluster, geometry = geometry)) +
  #scale_fill_gradient(low = "red", high = "green") +
  lims(x=c(-124,-114), y=c(32,42)) +
  theme_minimal()

# California EM score
ggplot() +
  geom_sf(data = subset(geo, STATEFP=='06'), aes(fill = EMscore, geometry = geometry)) +
  scale_fill_gradient(low = "red", high = "green") +
  lims(x=c(-124,-114), y=c(32,42)) +
  theme_minimal()

# California FWB score
ggplot() +
  geom_sf(data = subset(geo, STATEFP=='06'), aes(fill = FWBscore, geometry = geometry)) +
  scale_fill_gradient(low = "red", high = "green") +
  lims(x=c(-124,-114), y=c(32,42)) +
  theme_minimal()
