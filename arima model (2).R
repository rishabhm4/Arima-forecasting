library(ggplot2)
library(forecast)
library(tseries)
library(readxl)
library(plotly)
library(tsoutliers)
library(tsoutliers)
library(zoo)
library(lubridate)
library(dplyr)
library(plyr)
library(xlsx)
library(e1071)

library(fracdiff)

setwd("C:/Users/Dinu Level A/Desktop")
data1  <- read.xlsx("Wheat data.xlsx",sheetIndex = 2, header=TRUE, stringsAsFactors=FALSE)      
#as.Date(data1$`Date GMT`)
#DATA BEFORE CLEANING
#plot_ly(data1,x=data1$Date.GMT,y=~data1$Open,type="scatter",mode="lines") %>%
# add_trace(normal_gdp,x=~normal_gdp$Date.GMT,y=~normal_gdp$Open) 


##Removing outliers
#For low
quan <- quantile(data1$Low, probs = c(.25, .75))
range <- 1.5 * IQR(data1$Low)
normal<- subset(data1,
                     data1$Low > (quan[1] - range) & data1$Low< (quan[2] + range))
#For High
quan <- quantile(normal$High, probs = c(.25, .75))




range <- 1.5 * IQR(normal$High)
normal<- subset(normal,
                normal$High > (quan[1] - range) & normal$High< (quan[2] + range))
#For Last
quan <- quantile(normal$Last, probs = c(.25, .75))
range <- 1.5 * IQR(normal$Last)
normal<- subset(normal,
               normal$Last > (quan[1] - range) & normal$Last< (quan[2] + range))

data2 <- normal
#zeta <- 30
#write.xlsx(data2,"wheat lowoutlier.xlsx")#Moving av.
#Weekly moving average


#plot_ly(data1,x=data1$Date.GMT,y=~data1$Open1,type="scatter",mode="lines",name="Open")%>% add_trace(name="Open7",x=data1$Date.GMT,y=data1$Open_ma,mode="lines") %>%
# add_trace(x=data1$Date.GMT,y=data1$Open_ma30,mode="lines",name="Open30")


#wee <- week(strftime((data2$Date.GMT)))
#month <- month(strftime((data2$Date.GMT)))
#b <- cbind(data2,week=wee,month)

#r <- ddply(b,.variables ="week",function(t){  b$Open_ma7 <- ma(b$Open,order=7) })

#Moving average
data2$High_ma <-  ma(data2$High, order=15)
#half month MA
data2$Low_ma <- ma(data2$Low,order=15)
#Monthly moving average
data2$Last_ma <- ma(data2$Last,order=15)

data2$mean <- rowMeans(subset(data2, select = c(High_ma,Low_ma,Last_ma )), na.rm = TRUE) 
#Decomposition
cnt <- ts(na.omit(data2$mean),frequency=15)
decomp =  stl(cnt,s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
#To check stationary augumented dick fuller
adf.test(cnt,alternative = "stationary")
#ACF
Acf(cnt)
#Pacf
Pacf(cnt)

##Repeating due to absence of seasonality
#differencing to see seasonality
count_d1 <- diff(deseasonal_cnt,differences = 1)
#Expand the graph section before plot
plot(count_d1)
adf.test(count_d1,alternative = "stationary")
Acf(count_d1,main="Differenced acf series")
Pacf(count_d1,main="Diff. pacf series")
#Arima 
#auto.arima(deseasonal_cnt,seasonal = TRUE)

#for p=2,d=1,q=2
fit3<-auto.arima(deseasonal_cnt, seasonal=TRUE)
tsdisplay(residuals(fit3), lag.max=45, main='(2,1,2) Model Residuals')
f3 <- forecast(fit3,h=90)
plot(f3)
a <- as.list(f3)
a <- as.data.frame(a)
a
a$`Point Forecast`

#FILE WRITE UP
write.xlsx(a,"w1lastest.xlsx")
accuracy(a$`Point Forecast`,tail(data2$mean,90))


fit2 = arima(deseasonal_cnt, order=c(1,0,7))
f2 <-  forecast(fit2,h=90)
plot(f2)
b <- as.list(f2)
b <- as.data.frame(b)

accuracy(b$`Point Forecast`,tail(data2$mean,zeta))



