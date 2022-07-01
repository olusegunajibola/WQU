library(tidyquant)  #loads the tidyquant package
library(timetk)  #loads the timetk package
library(rugarch)
#First we load the excel data
AAPL2 <- read_excel("AAPL2.xlsx", 
        + col_types = c("date", "numeric", "numeric", 
        + "numeric", "numeric", "numeric", 
        +   "numeric", "numeric"))
View(AAPL2)
adj_price = AAPL2$'Adj Close'
plot(adj_price)

# To obtain daily stock returns
AppDailyRtn <- AAPL2 %>%
  tq_transmute(select = 'Adj Close',      # this specifies which column to select   
               mutate_fun = periodReturn,   # This specifies what to do with that column
               period = "daily",      # This argument calculates Daily returns
               type = "log",   #does the calculation with natural log
               col_rename = "adj_cls_price") # renames the column

daily_rtn = AppDailyRtn$`adj_cls_price`
plot(daily_rtn)

#MODELING
#We use eGARCH(1,1) model using ugarchspec function.
#garchOrder=c(1,1)
#Constant mean
#armaOrder=c(0,0)
apl_egarch11_spec <- ugarchspec(variance.model = list(model="eGARCH", 
                                                  garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 0)))

#Estimate the GARCH model
apl_egarch11_fit<-ugarchfit(spec=egarch11_spec, data=daily_rtn)

#Coefficients
egarchcoef = coef(apl_egarch11_fit)
egarchcoef


#Forecast future returns
apl_egarch11_forecast<-ugarchforecast(apl_egarch11_fit, nahead=15)
apl_egarch11_forecast

#Plot future returns
plot(apl_egarch11_forecast)