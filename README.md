# -R-Predict-Suncor-Stock-Price-MLR-ARIMA

In this project, we will apply Multiple Linear Regression with ARIMA errors to predict the value of Suncor Energy’s stock price (response variable) from WTI Crude Oil price and S&P/TSX Composite Index (explanatory variables). Suncor Energy is a leading Canadian integrated energy company specializing in producing synthetic crude oil and related products from oil sands. Suncor Energy is listed on Toronto Stock Exchange as SU.TO. This research uses publicly available data, sourcing from data files provided by Yahoo Finance. It includes daily price of all trading days from December 2014 to December 2019.

The research starts with having the first overview of the data and splitting three datasets into train/test subsets. Then, it will fit the train data into MLR model and check for multicollinearity problem. Since the regression model’s residuals are autocorrelated, the research continues with applying ARMA model to make the error stationary. Finally, we will use the final model to forecast Suncor’s stock price upon the test period, then compare the forecast result with the real data.

Data source:
- Yahoo Finance. Crude Oil May 21 (CL=F). Retrieved March 25, 2021, from https://ca.finance.yahoo.com/quote/CL%3DF?p=CL%3DF
- Yahoo Finance. Suncor Energy Inc. (SU.TO). Retrieved March 25, 2021, from https://ca.finance.yahoo.com/quote/su.to/
- Yahoo Finance. S&P/TSX Composite index (^GSPTSE). Retrieved March 25, 2021, from https://ca.finance.yahoo.com/quote/%5EGSPTSE?p=%5EGSPTSE
