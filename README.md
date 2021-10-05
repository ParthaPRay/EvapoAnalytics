# Time Series Forecasting For Evapotranspiration 

## _Partha Pratim Ray_

> **Forecasting for Evapotranspirartion in India**

> **The codes and CSV files are should not be used by any person/organization/entities without prior permission from the Author (Partha Pratim Ray)** [Work under Progress]

> **This work is does forecasting for the Evapotranspiration (ET) in mm level for India. **

**The data set contains monthly data from 8, 2018 to 7, 2021.**

**Following R libraries are used to Forecast the Evapotranspiration in India**

- library(fpp2)    ### for various dataset and forecst package 
- library(urca)       ### for KPSS test
- library(seasonal)  ### for seas() or seas(x11="") decomposition, if required
- library(dplyr)      ### for %>% 
- library(tseries)     ### for adf.test() for Augmented Dicky-Fuller test


> **We used KPSS test and Ljung-Box test at initial phases**


> **We used following models in this forecasting process**

- auto.arima() 
- meanf()
- rwf() Random Walk Model
- rwf(drif=TRUE) i.e., Drift Model
- naive()
- snaive()
- tbats()
- nnetar()
- ets()
- Arima()


> **Arima does better**
