library(fpp2)    ### for various dataset and forecst package 
library(urca)       ### for KPSS test
library(seasonal)  ### for seas() or seas(x11="") decomposition, if required
library(dplyr)      ### for %>% 
library(tseries)     ### for adf.test() for Augmented Dicky-Fuller test

#######################Evapotranspiration CSV dataset obtain from https://indiawris.gov.in/wris/#/evapotranspiration
data<-read.csv("Evapotranspiration.csv")

#####Whole dataset is converted into a time series data with monthly frequency 12
t<-ts(data$ET, start=c(2018, 6), end=c(2021, 7), frequency = 12)
class(t)

###Autoplot the whole time series data set
autoplot(t)


##Decomposition of whole time series dataset using STL

autoplot(stl(t, s.window = "periodic", robust=TRUE))


#### ACF and PACF plot fo whole time series data set

ggtsdisplay(t)


##Total rows in the whole time series data set
NROW(t)

###Find Boxcox lambda value of the whole time series dataset
BoxCox.lambda(t)



####################################################################################################
############################# Ordinary check whether auto.arima works on the whole time series dataset
####### Auto Arima fitting on whole data set
fit1<-auto.arima(t, stepwise = FALSE, approximation = FALSE)
fit1
checkresiduals(fit1)
fc1<-forecast(fit1, h=12)
autoplot(fc1)


###################################################################################################
####### Ordinary check whether ETS works on the whole time series dataset
fit2<-ets(t)
checkresiduals(fit2)
fc2<-forecast(fit2, h=8)
autoplot(fc2)


##################################################################################################
###### Check of KPSS test for stationarity of whole time series data; if T observed > T critical ; Data is stationary
summary(ur.kpss(t))

###### ReCheck of KPSS test for stationarity of whole data with a first order difference; if T observed > T critical ; Data is stationary
summary(ur.kpss(diff(t)))


###### ReCheck of KPSS test for stationarity of whole data with a lagged value as per frequency difference; if T observed > T critical ; Data is stationary
summary(ur.kpss(diff(t, lag=12)))


#### Check the rquired first order differenceing for the whole data 
ndiffs(t)

#### Check the rquired seasonal differenceing for the whole data 
nsdiffs(t)


##################################################################################################

####################################### Trainig data set creation
train<-window(t, end=c(2020, 11))


###################################################################################################

####Set horizon for forecasting [VERY IMPORTANT] Checng the value accordingly

h<- 8



#####################################################################
### Auto.Arima model forecasting for h horizons on train data

fa<-auto.arima(train)

### Check residuals of the fitted data on the train set; Ljung-Box test: if p value > alpha (.05) then WHITE NOISE, Proceed next
checkresiduals(fa)

### Get detailed information about the fitted model on training set
fa

### Forecast for remaining data (test) using fitted model
a1<-fa%>%forecast(h=h)

### Find the accuracy of the fitted model on the test data
accuracy(a1, t)

### Plot the forecasted data on the test data
autoplot(forecast(fa, h=h))

### Actual Forecasting beyond the dataset on whole data on the auto.arima fitted model for next 8 horizons
t%>%auto.arima()%>%forecast(h=8)%>%autoplot()
t%>%auto.arima()%>%forecast(h=8)%>%autoplot()+autolayer(fa$fitted)


####################################################################
### ETS model forecasting for h horizons on train data
fe<-ets(train)

### Check residuals of the fitted data on the train set; Ljung-Box test: if p value > alpha (.05) then WHITE NOISE, Proceed next
checkresiduals(fe)


### Get detailed information about the fitted model on training set
fe


### Forecast for remaining data (test) using fitted model
a2<-fe%>%forecast(h=h)


### Find the accuracy of the fitted model on the test data
accuracy(a2, t)


### Plot the forecasted data on the test data
autoplot(forecast(fe, h=h))

### Actual Forecasting beyond the dataset on whole data on the ETS fitted model for next h horizons
t%>%ets()%>%forecast(h=h)%>%autoplot()


#######################################################################
### TABTS model forecasting for h horizons on train data

ft<-tbats(train)

### Check residuals of the fitted data on the train set; Ljung-Box test: if p value > alpha (.05) then WHITE NOISE, Proceed next
checkresiduals(ft)


### Get detailed information about the fitted model on training set
ft


### Forecast for remaining data (test) using fitted model
a3<-ft%>%forecast(h=h)


### Find the accuracy of the fitted model on the test data
accuracy(a3, t)


### Plot the forecasted data on the test data
autoplot(forecast(ft, h=h))

### Actual Forecasting beyond the dataset on whole data on the TBATS fitted model for next h horizons
t%>%tbats()%>%forecast(h=h)%>%autoplot()



########################################################################
### NNETAR model forecasting for h horizons on train data

fn<-nnetar(train)

### Check residuals of the fitted data on the train set; Ljung-Box test: if p value > alpha (.05) then WHITE NOISE, Proceed next
checkresiduals(fn)


### Get detailed information about the fitted model on training set
fn


### Forecast for remaining data (test) using fitted model
a4<-fn%>%forecast(PI=TRUE, h=h)


### Find the accuracy of the fitted model on the test data
accuracy(a4, t)


### Plot the forecasted data on the test data
autoplot(forecast(fn, PI=TRUE, h=h))

### Actual Forecasting beyond the dataset on whole data on the NNETAR fitted model for next h horizons
t%>%nnetar()%>%forecast(PI=TRUE, h=h)%>%autoplot()

#####################################################################################

#### Mean model forecasting for h horizons on train data
fm<-meanf(train)

### Check residuals of the fitted data on the train set; Ljung-Box test: if p value > alpha (.05) then WHITE NOISE, Proceed next
checkresiduals(fm)


### Get detailed information about the fitted model on training set
fm


### Forecast for remaining data (test) using fitted model
a5<-fm%>%forecast(h=h)


### Find the accuracy of the fitted model on the test data
accuracy(a5, t)


### Plot the forecasted data on the test data
autoplot(forecast(fm, h=h))

### Actual Forecasting beyond the dataset on whole data on the Mean fitted model for next 8 horizons
t%>%meanf()%>%forecast(PI=TRUE, h=h)%>%autoplot()

####################################################################################
#### Naive model forecasting for h horizons on train data
fn<-naive(train)

### Check residuals of the fitted data on the train set; Ljung-Box test: if p value > alpha (.05) then WHITE NOISE, Proceed next
checkresiduals(fn)


### Get detailed information about the fitted model on training set
fn


### Forecast for remaining data (test) using fitted model
a6<-fn%>%forecast(h=h)


### Find the accuracy of the fitted model on the test data
accuracy(a6, t)


### Plot the forecasted data on the test data
autoplot(forecast(fn, h=h))

### Actual Forecasting beyond the dataset on whole data on the Naive fitted model for next h horizons
t%>%naive()%>%forecast(h=h)%>%autoplot()


####################################################################################
#### Snaive model forecasting for h horizons on train data
fsn<-snaive(train)

### Check residuals of the fitted data on the train set; Ljung-Box test: if p value > alpha (.05) then WHITE NOISE, Proceed next
checkresiduals(fsn)


### Get detailed information about the fitted model on training set
fsn


### Forecast for remaining data (test) using fitted model
a7<-fsn%>%forecast(h=h)


### Find the accuracy of the fitted model on the test data
accuracy(a7, t)


### Plot the forecasted data on the test data
autoplot(forecast(fsn, h=h))

### Actual Forecasting beyond the dataset on whole data on the Snaive fitted model for next h horizons
t%>%snaive()%>%forecast(h=h)%>%autoplot()

####################################################################################
#### Random walk model with Drift (Dift Model) forecasting for h horizons on train data
frwf<-rwf(train, drift=TRUE)

### Check residuals of the fitted data on the train set; Ljung-Box test: if p value > alpha (.05) then WHITE NOISE, Proceed next
checkresiduals(frwf)


### Get detailed information about the fitted model on training set
frwf


### Forecast for remaining data (test) using fitted model
a8<-frwf%>%forecast(h=h)


### Find the accuracy of the fitted model on the test data
accuracy(a8, t)


### Plot the forecasted data on the test data
autoplot(forecast(frwf, h=h))

### Actual Forecasting beyond the dataset on whole data on the Random Walk Dirft model for next h horizons
t%>%rwf(drift=TRUE)%>%forecast(h=h)%>%autoplot()

####################################################################################
#### Random walk model forecasting for h horizons
frw<-rwf(train)

### Check residuals of the fitted data on the train set; Ljung-Box test: if p value > alpha (.05) then WHITE NOISE, Proceed next
checkresiduals(frw)


### Get detailed information about the fitted model on training set
frw


### Forecast for remaining data (test) using fitted model
a9<-frw%>%forecast(h=h)


### Find the accuracy of the fitted model on the test data
accuracy(a9, t)


### Plot the forecasted data on the test data
autoplot(forecast(frw, h=h))

### Actual Forecasting beyond the dataset on whole data on the Random Walk fitted model for next h horizons
t%>%rwf()%>%forecast(h=h)%>%autoplot()



#########################################################################
###########Arima model forecasting for h horizons

f1<-Arima(train, order=c(3,0,3), seasonal = c(0,1,3), lambda=0)

####Check AICc value of the fitted model
f1$aicc

#####Training data residual check; Ljung-Box test: if p value > alpha (.05) then WHITE NOISE, Proceed next
checkresiduals(f1)

#####Plot to check the adjustment with Test data
fr1<-forecast(f1, h=h)

#####Test data and fitted model accuracy comparison
accuracy(fr1, t)

######Test plot based on fitted model
autoplot(forecast(fr1, h=h))

######Actual Forecasting beyond the dataset on whole data on the Arima fitted model for next h horizons
t %>% Arima(order=c(3,0,3), seasonal=c(0,1,3), lambda=0) %>% forecast(h=h) %>% autoplot()





#########################################################################
########### Fit various Arima models based on (p,d,q) and (P, D, Q) for forecasting for h horizons



