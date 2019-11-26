library(data.table)

bitcoin <- fread('bitcoin.csv')
fred <- fread('fred.csv')
bitcoin <- merge(bitcoin,fred,all=FALSE)

summary(bitcoin)
head(bitcoin)
bitcoin$date <- as.Date(bitcoin$date)
bitcoin$weekday <- weekdays(bitcoin$date)
bitcoin$weekday <- as.factor(bitcoin$weekday)

ts.plot(bitcoin$bitcoin)
library(ggplot2)
ggplot(bitcoin,aes(x=date,y=bitcoin)) + geom_line()
ggplot(bitcoin,aes(x=date,y=sp500)) + geom_line()

model <-lm(log(bitcoin)~log(gold)+log(oilspot)+log(sp500)+euro,data=bitcoin) 
summary(model)

library(tseries)
rep.kpss <- function(series,maxdiffs=5,crit.val=0.05) {
  x <- series
  diffs <- 0
  for(i in 1:maxdiffs) {
    suppressWarnings(pval <- kpss.test(x,null="Level")$p.value)
    if(pval >= crit.val) return(c(diffs,0,pval))
    suppressWarnings(pval <- kpss.test(x,null="Trend")$p.value)
    if(pval >= crit.val) return(c(diffs,1,pval))
    x <- diff(x)
    diffs <- diffs + 1
  }
  return(FALSE)
}
rep.kpss(bitcoin$bitcoin)
rep.kpss(bitcoin$gold)
rep.kpss(bitcoin$oilspot)
rep.kpss(bitcoin$euro)
rep.kpss(bitcoin$sp500)

model <-lm(diff(log(bitcoin))~diff(log(gold))+diff(log(oilspot))+diff(log(sp500))+diff(euro),data=bitcoin)
model <- step(model)
summary(model)

bitcoin <- bitcoin[date>=as.Date('2017-01-01')]

acf(diff(log(bitcoin$bitcoin)))
pacf(diff(log(bitcoin$bitcoin)))

library(forecast)
auto.arima(log(bitcoin$bitcoin))
model_arima<-stats::arima(log(bitcoin$bitcoin),c(2,1,1))
summary(model_arima)
plot(forecast(model,h=30))

library(TSA)
library(dplyr)
periodogram(diff(bitcoin$bitcoin))
n <- nrow(bitcoin)
bitcoin$res <- rep(NaN,n)
ts.plot(diff(bitcoin$bitcoin))
lm(diff(bitcoin)~weekday[2:n],data=bitcoin)
bitcoin$res[2:n] <- lm(diff(bitcoin)~weekday[2:n],data=bitcoin) %>% residuals
periodogram(bitcoin$res[3:n])
ts.plot(bitcoin$res)

periodogram(diff(bitcoin$bitcoin))
periodogram(bitcoin$res[3:n])

library(vars)
diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}
x <- bitcoin %>% dplyr::select(bitcoin,gold,oilspot,sp500,euro) %>% log %>% diff_jp
VAR(x,p=1,type="both") %>% AIC
VAR(x,p=2,type="both") %>% AIC
VAR(x,p=3,type="both") %>% AIC
model <- VAR(x,p=1,type="both")
summary(model)
?vars::predict
pred <- predict(model,n.ahead=30)

forc <- pred$fcst$bitcoin
forc <- forc[,1]
log(bitcoin$bitcoin)
forc[1] <- forc[1] + 8.836519
forc

forc0 <- forecast(model_arima,h=30)
?forecast
forc0 <- forc0$mean

exp(forc0)
exp(cumsum(forc))
