library(xts)
library(astsa)
library(forecast)
library(car)


#============================================================================================================
# Support Function:
# a) Time conversion function:
date_converter <- function(d) {
  temp_d <- as.Date(paste0(d,"-01-01"))
  return(temp_d)
}


#============================================================================================================
# Step 1: Loading data files:
suncor_file <- read.csv("./datasets/Suncor-2014-2019.csv",na.strings = "")
WTI_oil_file <- read.csv("./datasets/Crude_Oil_WTI_2014-2019.csv",na.strings = "")
SP_TSX_file <- read.csv("./datasets/SPTSX_2014-2019.csv",na.strings = "")


#============================================================================================================
# Step 2: Convert data into time series data
# Note: Cannot use ts() function because it cannot convert original data to time series
#       daily data excluding weekends and holidays correctly
suncor <- xts(suncor_file[,2], order.by=date_converter(suncor_file[,1]))
WTI_oil <- xts(WTI_oil_file[,2], order.by=date_converter(WTI_oil_file[,1]))
SP_TSX <- xts(SP_TSX_file[,2], order.by=date_converter(SP_TSX_file[,1]))


#============================================================================================================
# Step 3: Initial examination of data
# Time series plot overview
time_check <- c(date_converter("2016-01-24"),date_converter("2018-12-24"))
par(mfrow=c(3,1))
tsplot(time(suncor),suncor,main="Suncor's stock price 12/2014 - 12/2019",ylab="price (CAD)",xlab="date")
abline(v=time_check,col = "blue")
tsplot(time(WTI_oil),WTI_oil,main="WTI Crude Oil price 12/2014 - 12/2019",ylab="price (USD)",xlab="date")
abline(v=time_check,col = "blue")
tsplot(time(SP_TSX),SP_TSX,main="S&P/TSX Composite index 12/2014 - 12/2019",ylab="price (CAD)",xlab="date")
abline(v=time_check,col = "blue")

#------------------------------
# Check data correlation
# - There is a positive relationship between TSX index and Suncor stock price
# - There is a positive relationship between Suncor stock price and WTI oil price
# - TSX index seems to be correlated with Oil price. We will need to check for Multicollinearity after fitting model.
cor(suncor,SP_TSX)
cor(suncor,WTI_oil)
cor(WTI_oil,SP_TSX)
pairs(cbind(Suncor=suncor_file[,2], Oil=WTI_oil_file[,2], TSX=SP_TSX_file[,2]))


#============================================================================================================
# Step 4: Split dataset into interested (train/test) time interval
# Set up train/test intervals
suncor_train <- window(suncor, start=date_converter("2014-12-01"), end=date_converter("2019-07-01"))
oil_train <- window(WTI_oil, start=date_converter("2014-12-01"), end=date_converter("2019-07-01"))
TSX_train <- window(SP_TSX, start=date_converter("2014-12-01"), end=date_converter("2019-07-01"))

suncor_test <- window(suncor, start=date_converter("2019-07-01"), end=date_converter("2019-12-31"))
oil_test <- window(WTI_oil, start=date_converter("2019-07-01"), end=date_converter("2019-12-31"))
TSX_test <- window(SP_TSX, start=date_converter("2019-07-01"), end=date_converter("2019-12-31"))

# ------------------------------
# Visualize train/test intervals
par(mfrow=c(3,1))
tsplot(time(suncor),suncor,
       panel.first = c(rect(date_converter("2014-12-01"), -1e6, 
                            date_converter("2019-07-01"), 1e6, col='lightgreen', border=TRUE),
                       rect(date_converter("2019-07-01"), -1e6, 
                            date_converter("2019-12-31"), 1e6, col=2, border=TRUE)),
       main="Suncor's stock price 12/2014 - 12/2019",ylab="price (CAD)",xlab="date")
tsplot(time(WTI_oil),WTI_oil,
       panel.first = c(rect(date_converter("2014-12-01"), -1e6, 
                            date_converter("2019-07-01"), 1e6, col='lightgreen', border=TRUE),
                       rect(date_converter("2019-07-01"), -1e6, 
                            date_converter("2019-12-31"), 1e6, col=2, border=TRUE)),
       main="WTI Crude Oil price 12/2014 - 12/2019",ylab="price (USD)",xlab="date")
tsplot(time(SP_TSX),SP_TSX,
       panel.first = c(rect(date_converter("2014-12-01"), -1e6, 
                            date_converter("2019-07-01"), 1e6, col='lightgreen', border=TRUE),
                       rect(date_converter("2019-07-01"), -1e6, 
                            date_converter("2019-12-31"), 1e6, col=2, border=TRUE)),
       main="S&P/TSX Composite index 12/2014 - 12/2019",ylab="price (CAD)",xlab="date")


#============================================================================================================
# Step 4: Fit multiple linear regression model
oil_train_2 <- oil_train^2
oil_train_3 <- oil_train^3
TSX_train_2 <- TSX_train^2
TSX_train_3 <- TSX_train^3
oil_TSX <- oil_train*TSX_train

model <- lm(suncor_train ~ oil_train + TSX_train, na.action=NULL)
model_1 <- lm(suncor_train ~ oil_train + oil_train_2 
                                               + TSX_train, na.action=NULL)
model_2 <- lm(suncor_train ~ oil_train + TSX_train
                                               + TSX_train_2, na.action=NULL)
model_3 <- lm(suncor_train ~ oil_train + oil_train_2
                                               + TSX_train + TSX_train_2, na.action=NULL)
model_4 <- lm(suncor_train ~ oil_train + oil_train_2
                                              + oil_train_3 + TSX_train, na.action=NULL)
model_5 <- lm(suncor_train ~ oil_train + oil_TSX + TSX_train, na.action=NULL)

# Variance Inflation Factor is small << 10 
# -> we can add TSX index to model with no multicollinearity issue
car::vif(model)        

# Model selection:
# > After checking model summary, anova table, AIC:
#   >> Best model = model_4:
#      suncor_train ~ oil_train + oil_train^2 + oil_train^3 + TSX_train
summary(model_4)       # regression results
summary(aov(model_4))  # ANOVA table
AIC(model,model_1,model_2,model_3,model_4,model_5)

# > Variance Inflation Factor of Oil variables are huge >> 14 -> structural multicollinearity issue
#   >> We can try standardizing Oil variables by subtracting the mean
car::vif(model_4)
oil_train_S <- oil_train - mean(oil_train)
oil_train_2S <- oil_train_S^2
oil_train_3S <- oil_train_S^3
model_4S <- lm(suncor_train ~ oil_train_S + oil_train_2S 
                        + oil_train_3S + TSX_train, na.action=NULL)

# > After standardization, multicollinearity issue is fixed
summary(model_4S)       # regression results
summary(aov(model_4S))  # ANOVA table
car::vif(model_4S)

# > Best Model:
best_model = model_4S
fitted <- suncor_train - resid(best_model)
par(mfrow=c(1,1))
plot.zoo(cbind(suncor_train, fitted), 
         plot.type = "single", 
         col = c("black", "blue"),lty = c(1,2),
         main="Fitted regression model of Suncor's stock price 2015 - 2019",
         ylab="price (CAD)",xlab="date",xy.label=TRUE)


#============================================================================================================
# Step 5: Verify/Transform stationary data
# - There is clear trend in time series plot, and residuals are correlated
#   > We need to do a transformation
#   > After the first difference transformation, residuals are uncorrelated
#   > ACF cuts off after lag 2, and PACF cuts off after lag 2.
#     >> we can try model ARIMA(2,2): p=2, q=2
par(mfrow=c(1,3))
tsplot(time(resid(best_model)),resid(best_model),main="Residual plot before transformation")
acf(resid(best_model),100)
pacf(resid(best_model),100)
tsplot(time(resid(best_model)),diff(resid(best_model)),main="Residual plot after 1st difference")
acf(diff(resid(best_model)),100,na.action=na.pass)
pacf(diff(resid(best_model)),100,na.action=na.pass)


#============================================================================================================
# Step 6: Adding ARIMA model to Multiple Linear Regression model
# - Model selection:
#   > Drop two variables oil_train_S^2 and oil_train_S^3, because they are insignificant in new ARIMA model
#   > After checking p-value of coefficients, comparing residuals plot and AIC:
#     >> Best model = arima(suncor_train,2,1,4,xreg=cbind(oil_train_S,TSX_train))
#   > In all models, residuals are not normally distributed.
oil_test_S <- oil_test - mean(oil_test)
oil_test_2S <- oil_test_S^2
oil_test_3S <- oil_test_S^3

#sarima(suncor_train,2,1,2,xreg=cbind(oil_train_S,TSX_train))
sarima(suncor_train,2,1,4,xreg=cbind(oil_train_S,TSX_train))
suncor_model <- Arima(suncor_train,order=c(2,1,4),xreg=cbind(oil_train_S,TSX_train))
suncor_model
AIC(suncor_model)

#============================================================================================================
# Step 7: Prediction using above model (95% interval)
par(mfrow=c(2,1))  
tsplot(time(suncor),suncor,main="Real data - Suncor's stock price 12/2014 - 12/2019",
       ylab="price (CAD)",xlab="date")
suncor_forecast <- forecast(suncor_model,xreg=cbind(oil_test_S,TSX_test))
tsplot(suncor_forecast,xaxt = 'n',xlab='date',ylab="price (CAD)",
       xm.grid=FALSE,main="Forecast 95% - ARIMA(2,1,4) with regressors - Suncor's stock price 07/2019 - 12/2019")
axis(1, at=seq(20, 1325, by=252), labels = seq(2015, 2020, by=1))

