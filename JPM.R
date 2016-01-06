library(quantmod)
library(tseries)
library(forecast)
library(urca)

getSymbols("JPM", src="yahoo", from='2010-01-01', to='2014-04-14')
chartSeries(JPM, subset='2010::2014')

log.rtn <- na.omit(diff(log(JPM$JPM.Adjusted)))
rtn <- log.rtn[-length(log.rtn)+9:-length(log.rtn)]
f.rtn <- log.rtn[-1: (-length(log.rtn)+10)]
n = length(rtn)
plot(rtn, ylab='daily log returns', xlab='time', main='Daily Log Returns: JP Morgan');abline(h=0)



# Checking residuals for stationary
adf.test(rtn, alternative="stationary")
acf(rtn, lag.max=24, main="Autocorrelation Function")
pacf(rtn, lag.max=24, main="Partial Autocorrelation Function")

Box.test(rtn,lag=24,type='Ljung') 

# As we can reject the null hypothesis (independence) , we assume there is serial correlation

# Check for the trend
summary(ur.df(rtn, type='trend', lags=20, selectlags="BIC"))


# Check for the seasonality
acf(rtn, main="Autocorrelation Function", yaxt="n")
ci=qnorm(c(0.025, 0.975))/sqrt(n)
text(y=ci,par("usr")[1],labels=round(ci,4),pos=2,xpd=TRUE)
pacf(rtn,main="Patial Autocorrelation Function",yaxt="n")
text(y=ci,par("usr")[1],labels=round(ci,4),pos=2,xpd=TRUE)
spec.pgram(rtn,main="Series: Daily Log Return")


# Finding the best ARMA
AIC = matrix(ncol=5, nrow=5, dimnames=list(c("0","1","2","3","4"),
                                           c("0","1","2","3","4")))
for (i in (1:5)){
  for (j in (1:5)){
    model = arima(rtn, c(i-1,0,j-1), method="ML", optim.control=list(maxit=200))
    AIC[i,j] = model$aic
  }
}
print(AIC)
cat('Minimum AIC is',min(AIC),'\n') 


# Finding the best ARIMA
d.rtn <- na.omit(diff(JPM$JPM.Adjusted))
AIC = matrix(ncol=5, nrow=5, dimnames=list(c("0","1","2","3","4"),
                                           c("0","1","2","3","4")))
for (i in (1:5)){
  for (j in (1:5)){
    model = arima(d.rtn, c(i-1,0,j-1), method="ML", optim.control=list(maxit=200))
    AIC[i,j] = model$aic
  }
}
print(AIC)
cat('Minimum AIC is',min(AIC),'\n') 





best.model <- arima(rtn, c(1,0,1))
summary(best.model)

acf(best.model$residuals, lag.max=15, main="Autocorrelation Function")
pacf(best.model$residuals, lag.max=15, main="Partial Autocorrelation Function")


res=residuals(best.model)


hist(res, xlim=c(-0.2,0.2))
lines(density(res))
qqnorm(res)          
qqline(res)


# Forecasts
forecast.model <- forecast.Arima(best.model, h=10)
forecast.model




############ Checking GARCH Effect ################################

# Test if variance is constant or not
var = (rtn-mean(rtn))^2
Box.test(var, lag=20, type='Ljung')

# Box.test shows we can reject the null (independence) on variance,
# so it has significant serial correlation, in other words, ARCH effect.


library(fGarch)

res.best.model <- best.model$res
plot(res.best.model^2, main='Squared Residuals')
acf(res.best.model^2, main='ACF of squared Residuals')
pacf(res.best.model^2, main='PACF of Squared Residuals')

garch.model <- garchFit(~arma(1,1)+garch(1,1), rtn, cond.dist="std")
summary(garch.model)
garch.model@fit$matcoef

p <- predict(garch.model, n.ahead=1, mse="cond")
p.1 <- p$meanForecast

garch.model <- garchFit(~arma(1,1)+garch(1,1), c(rtn, f.rtn[1]), cond.dist="std")
p<- predict(garch.model, n.ahead=1, mse="cond")
p.2 <- p$meanForecast

garch.model <- garchFit(~arma(1,1)+garch(1,1), c(rtn, f.rtn[1:2]), cond.dist="std")
p <- predict(garch.model, n.ahead=1, mse="cond")
p.3 <- p$meanForecast

garch.model <- garchFit(~arma(1,1)+garch(1,1), c(rtn, f.rtn[1:3]), cond.dist="std")
p <- predict(garch.model, n.ahead=1, mse="cond")
p.4 <- p$meanForecast

garch.model <- garchFit(~arma(1,1)+garch(1,1), c(rtn, f.rtn[1:4]), cond.dist="std")
p <- predict(garch.model, n.ahead=1, mse="cond")
p.5 <- p$meanForecast

garch.model <- garchFit(~arma(1,1)+garch(1,1), c(rtn, f.rtn[1:5]), cond.dist="std")
p <- predict(garch.model, n.ahead=1, mse="cond")
p.6 <- p$meanForecast

garch.model <- garchFit(~arma(1,1)+garch(1,1), c(rtn, f.rtn[1:6]), cond.dist="std")
p <- predict(garch.model, n.ahead=1, mse="cond")
p.7 <- p$meanForecast

garch.model <- garchFit(~arma(1,1)+garch(1,1), c(rtn, f.rtn[1:7]), cond.dist="std")
p <- predict(garch.model, n.ahead=1, mse="cond")
p.8 <- p$meanForecast

garch.model <- garchFit(~arma(1,1)+garch(1,1), c(rtn, f.rtn[1:8]), cond.dist="std")
p <- predict(garch.model, n.ahead=1, mse="cond")
p.9 <- p$meanForecast

garch.model <- garchFit(~arma(1,1)+garch(1,1), c(rtn, f.rtn[1:9]), cond.dist="std")
p <- predict(garch.model, n.ahead=1, mse="cond")
p.10 <- p$meanForecast

predict <- c(p.1,p.2,p.3,p.4,p.5,p.6,p.7,p.8,p.9,p.10)
predict

value.1 <- JPM$JPM.Adjusted[1067]*exp(predict[1])
value.2 <- JPM$JPM.Adjusted[1068]*exp(predict[2])
value.3 <- JPM$JPM.Adjusted[1069]*exp(predict[3])
value.4 <- JPM$JPM.Adjusted[1070]*exp(predict[4])
value.5 <- JPM$JPM.Adjusted[1071]*exp(predict[5])
value.6 <- JPM$JPM.Adjusted[1072]*exp(predict[6])
value.7 <- JPM$JPM.Adjusted[1073]*exp(predict[7])
value.8 <- JPM$JPM.Adjusted[1074]*exp(predict[8])
value.9 <- JPM$JPM.Adjusted[1075]*exp(predict[9])
value.10 <- JPM$JPM.Adjusted[1076]*exp(predict[10])

predict.value <- c(value.1,value.2,value.3,value.4,value.5,value.6,value.7,value.8,value.9,value.10)
predict.value



