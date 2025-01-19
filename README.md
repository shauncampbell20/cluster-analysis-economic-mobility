## 1_fwb_predictive.R
This script first loads the financial well-being survey data from the CFPB,
then performs some transformations to prepare the data for a linear regression.
A linear regression model is fit to predict FWB score.
The model performance is evaluated and the model is saved for future use.

## 2_census_data.R
This script uses the tidycensus package to retrieve census data.
Data from the 2019 ACS community survey tables are used.
The data is transformed so the FWB score prediction model can be applied.
Other data points are also gather for further analysis.

## 3_clustering.R
This script scales the census data and performs heirarchical and k-means clustering.
Summaries and various linear models are created for each cluster.
A linear regression model to predict economic mobility based on census data is created.
Various map visualizations are created. 

## 4_forecasting.R
This script retreives historical census data for Wisconsin tracts.
Time series trend models are fit for each tract from 2010-2019.
Visualizations and summaries for the trends are created.
