rm(list=ls())

## Import packages
library(data.table)
library(sandwich) # For White correction
library(lmtest) # More advanced hypothesis testing tools
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(plm)
library(TSA)
library(forecast)
library(MASS)
library(tseries)
library(broom)
library(vars)
library(psych)
library(e1071)
library(caret)


fred.df=read.csv("C:/Users/Dhruv Sharma PC/Desktop/UTD subjetcs/Semester 2/BA with R/PS 3/fred.csv",header=T)
bitcoin.df=read.csv("C:/Users/Dhruv Sharma PC/Desktop/UTD subjetcs/Semester 2/BA with R/PS 3/bitcoin.csv",header=T)
bitbf17.df=read.csv("C:/Users/Dhruv Sharma PC/Desktop/UTD subjetcs/Semester 2/BA with R/PS 3/bitbf17.csv",header=T)
fred.df
bitcoin.df
bitbf17.df
names(fred.df)
names(bitcoin.df)
x1<-merge(fred.df,bitcoin.df,all=FALSE)
x1
##Q 3.3- data mergers in x1

head(x1)

summary(x1)
x1$DATE<-as.Date(x1$DATE)
ts.plot(x1$DATE ,gpars=list(xlab="day",ylab="bitcoin"))

ts.plot(x1$CBBTCUSD ,gpars=list(xlab="day",ylab="bitcoin"))
ggplot(x1,aes(x=DATE,CBBTCUSD)) +geom_line()

ts.plot(x1$DCOILWTICO ,gpars=list(xlab="day",ylab="DCOILWTICO"))
ggplot(x1,aes(x=DATE,DCOILWTICO)) +geom_line()

ts.plot(x1$SP500 ,gpars=list(xlab="day",ylab="SP500"))
ggplot(x1,aes(x=DATE,SP500)) +geom_line()

ts.plot(x1$Gold ,gpars=list(xlab="day",ylab="Gold"))
ggplot(x1,aes(x=DATE,Gold)) +geom_line()

ts.plot(x1$DEXUSEU ,gpars=list(xlab="day",ylab="DEXUSEU"))
ggplot(x1,aes(x=DATE,DEXUSEU)) +geom_line()


#Q3.4-

model3.5 <- lm(CBBTCUSD~DCOILWTICO+SP500+Gold+DEXUSEU, data = x1)
summary(model3.5)
coeftest(model3.5,vcov.=vcovHC)

model3.5.1 <-lm(log(CBBTCUSD)~log(DCOILWTICO)+log(Gold)+log(SP500)+DEXUSEU,data=x1) 
summary(model3.5.1)
#Q3.5- except DCOILWITCO SP500,GOLD,DEXUSEU ARE significant

kpss.test(x1$DATE,null="Level")
kpss.test(x1$DATE,null="Trend")
kpss.test(x1$DCOILWTICO,null="Level")
kpss.test(x1$DCOILWTICO,null="Trend")
kpss.test(x1$SP500,null="Level")
kpss.test(x1$SP500,null="Trend")
kpss.test(x1$Gold,null="Level")
kpss.test(x1$Gold,null="Trend")
kpss.test(x1$DEXUSEU,null="Level")
kpss.test(x1$DEXUSEU,null="Trend")
kpss.test(x1$CBBTCUSD,null="Level")
kpss.test(x$CBBTCOCUSD,null="Trend")

rep.kpss <- function(series,maxdiffs=5,crit.val=0.05) {
  x <- series
  diffs <- 0
  for(i in 1:maxdiffs) {
    pval <- kpss.test(x,null="Level")$p.value
    if(pval >= crit.val) 
      return(c(diffs,0,pval))
    pval <- kpss.test(x,null="Trend")$p.value
    if(pval >= crit.val) 
      return(c(diffs,1,pval))
    x <- diff(x)
    diffs <- diffs + 1
  }
  return(FALSE)

}
rep.kpss(x1$CBBTCUSD)
rep.kpss(x1$DEXUSEU)
rep.kpss(x1$DCOILWTICO)
rep.kpss(x1$Gold)
rep.kpss(x1$SP500)
#Q3.6

model3.7 <- lm(diff(CBBTCUSD)~diff(SP500)+diff(Gold)+diff(DEXUSEU)+diff(DCOILWTICO), data = x1)
summary(model3.7)
coeftest(model3.7,vcov.=vcovHC)

##Q 3.7-


ts.plot(bitbf17.df$CBBTCUSD ,gpars=list(xlab="day",ylab="bitcoin"))
bitcoin <- bitcoin[date>=as.Date('2017-01-01')]

##Q 3.8-


acf(diff(bitbf17.df$CBBTCUSD))
pacf(diff(bitbf17.df$CBBTCUSD))

##Q 3.9-

outp <- matrix(0L,7^2,3)
row <- 1
# The i and j value are set from 0 to 6 because, from acf and pacf model
# the peak value for me is in between 0 and 6
for(i in 0:6){
  for(j in 0:6){
    aic <- AIC(arima(log(bitbf17.df$CBBTCUSD),c(i,1,j)))
    outp[row,] <- c(i,j,aic)
    row <- row + 1
  }
}
order(outp[,3])
# The below output will give you the p,q value for which the AIC is minimum. 
# Choose that value for the rest of models.
outp[38,]


##Q 3.10-


forcst1 <- forecast(bitcoin.df$CBBTCUSD)
plot(forcst1,include=30,gpars=list(xlab="bitcoin",ylab="day"))



forcst2 <- forecast(fred.df$Gold)
plot(forcst2,include=30,gpars=list(xlab="Gold",ylab="day"))

forcst3 <- forecast(fred.df$SP500)
plot(forcst3,include=30,gpars=list(xlab="SP500",ylab="day"))

forcst6 <- forecast(fred.df$DEXUSEU)
plot(forcst6,include=30,gpars=list(xlab="DEXUSEU",ylab="day"))


forcst5 <- forecast(fred.df$DEXUSEU)
plot(forcst5,include=30,gpars=list(xlab="DEXUSEU",ylab="day"))

#Q3.11-


periodogram(x1$CBBTCUSD)
periodogram(diff(x1$CBBTCUSD))
periodogram(x1$DATE)
periodogram(diff(x1$DATE))
periodogram(x1$DCOILWTICO)
periodogram(diff(x1$DCOILWTICO))
periodogram(x1$SP500)
periodogram(diff(x1$SP500))
periodogram(x1$Gold)
periodogram(diff(x1$Gold))
periodogram(x1$DEXUSEU)
periodogram(diff(x1$DEXUSEU))


##Q 3.12- Yes, the periodogram has changed slightly and this helps to capture seasonality in the price to a small extent.



x1$weekday <- format(as.Date(x1$DATE), "%a")
bitbf17.df$weekday 

model4<- lm(CBBTCUSD~weekday ,data=x1)
x1$res <- residuals(model4)
x1$res
periodogram(x1$res)
periodogram(diff(x1$res))

##Q 3.13-


diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}
x <- x1 %>% dplyr::select(CBBTCUSD,DCOILWTICO,DEXUSEU,Gold,SP500) %>% log %>% diff_jp
VAR(x,p=1,type="both") %>% AIC
VAR(x,p=2,type="both") %>% AIC
VAR(x,p=3,type="both") %>% AIC
model3.14 <- VAR(x,p=2,type="both")
summary(model3.14)
# Employment Granger-causes change in the wage
##Q 3.14-


x <- x1 %>% dplyr::select(SP500,DCOILWTICO,DEXUSEU,Gold,CBBTCUSD) %>% log %>% diff_jp
VAR(x,p=1,type="both") %>% AIC
VAR(x,p=2,type="both") %>% AIC
VAR(x,p=3,type="both") %>% AIC

## Thus we select p=1


model3.15 <- VAR(x,p=1,type="both")
variable <- VAR(x,p=1,type="both")
hh<- 30

forc <- predict(variable, n.ahead = hh, ci = 0.95, dumvar = NULL)
plot(forc)
forc[1]$fcst$CBBTCUSD

##Q 3.15- 
