# oracle price behavio
# hw2 for ams 586
require(quantmod)
require(TTR)
require(par)
require(forecast)
require(urca)
require(tseries)
require(reshape)
require(fGarch)
##loading data

getSymbols('ORCL',from ='2010-01-01',to = '2014-03-31')
data <- ORCL
names(data) <- c('Open','High','Low','Close','Volume','Adjusted')


# orginal data preprocessing
# raw data diagnostic
chartSeries(data,theme='black')

data.close = data$Adjusted
n = length(data.close)
ci=qnorm(c(0.025, 0.975))/sqrt(n)
acf(data.close,main="ACF of raw data",yaxt="n")
text(y=ci,par("usr")[1],labels=round(ci,4),pos=2,xpd=TRUE)
pacf(data.close,main="PACF of raw data",yaxt="n")
text(y=ci,par("usr")[1],labels=round(ci,4),pos=2,xpd=TRUE)
summary(ur.df(data.close, type='trend', lags=20, selectlags="BIC"))


## fitting 

# log return 
r = na.omit(diff(log(data.close)))
plot(r, main = 'scatter plot of log return')
n = length(r)
ci=qnorm(c(0.025, 0.975))/sqrt(n)
acf(r,main="ACF of log return",yaxt="n")
text(y=ci,par("usr")[1],labels=round(ci,4),pos=2,xpd=TRUE)
pacf(r,main="PACF of log return",yaxt="n")
text(y=ci,par("usr")[1],labels=round(ci,4),pos=2,xpd=TRUE)


shapiro.test(as.numeric(r))
Box.test(r,lag=20,type = 'Ljung-Box')
require(fGarch)

res = r
Box.test(res^2,lag=10,type = 'Ljung')

#omega is the intercept
# alpha is the ar part
# beta is the ma part
m1 = garchFit(~arma(1,1) + garch(1,1),data=r,trace=F)

summary(m1)

m3 = garchFit(~garch(1,1),data=r,trace=F)

summary(m3)

predict(m1, n.ahead = 10, trace = FALSE, mse = "cond",plot=FALSE,nx=NULL)
# final model

m2=garchFit(~arma(1,1) +garch(1,1),data=r,cond.dist="std",trace=F)
summary(m2)
#plot(m2)

m4 =garchFit(~garch(1,1),data=r,cond.dist="std",trace=F)
summary(m4)
# structural equation model
struct_fit <- StructTS(r,type='level')
struct_fit


resid = residuals(m1, standardize = FALSE)
ci=qnorm(c(0.025, 0.975))/sqrt(n)
acf(resid,main="ACF of square of log return",yaxt="n")
text(y=ci,par("usr")[1],labels=round(ci,4),pos=2,xpd=TRUE)
pacf(resid,main="PACF of square of log return",yaxt="n")
text(y=ci,par("usr")[1],labels=round(ci,4),pos=2,xpd=TRUE)

# arch test
Box.test(resid,lag = 10,type='Ljung')


write.table(data.frame(ORCL),file = 'C:/Users/LaoZZZZZ/Desktop/rawdata.csv',col.names = TRUE,sep=',',row.names = TRUE)
write.table(data$Adjusted,file = 'C:/Users/LaoZZZZZ/Desktop/rawdata_adjusted.csv',col.names = TRUE,sep=',',row.names = TRUE)


#prediction
pred.step = 10;
getSymbols('ORCL',from='2014-4-1',to = '2014-4-20')

len = length(ORCL$ORCL.Adjusted)
len = min(pred.step,len)
obs = log(ORCL$ORCL.Adjusted)[1:len]
observation = (ORCL$ORCL.Adjusted)[1:len]
mse = c()
pl = c()
pu = c()
fitted =m4
dataset =log(data.close)
start = length(dataset)
predict.value =c()
for(i in 1:len){
  move = length(dataset)
  #pred.ci = forecast(fitted,h=1,level=95)
  #pred = as.numeric(pred.ci$mean)
  pred.ci = predict(fitted, n.ahead = 1, trace = FALSE, mse = "cond",plot=FALSE,nx=NULL)
  pred = pred.ci$meanForecast
  pred = exp(pred + as.numeric(dataset[move]))
  pl = c(pl, pred - exp(pred.ci$standardDeviation*1.96/sqrt(move)))
  pu = c(pu,pred + exp(pred.ci$standardDeviation*1.96/sqrt(move)))
  #print(pred.ci$meanForecast)
  #print(dataset[move])
  predict.value = c(predict.value,pred)
  mse = c(mse,(exp(obs[i])-pred)^2)
  
  dataset = append(dataset,obs[i])
  fitted = garchFit(~garch(1,1),data=na.omit(diff(dataset)),cond.dist="std",trace=F)
  #fitted = garchFit(~arma(1,1) + garch(1,1),data=na.omit(diff(dataset)),trace=F)
  #fitted=garchFit(~arma(1,1) +garch(1,1),data=na.omit(diff(dataset)),cond.dist="std",trace=F)
  #fitted <- StructTS(na.omit(diff(dataset)),type='level')
}

mse = mean(mse)
mse
rmse = sqrt(mse)
rmse

plot(seq(1,len),exp(dataset[seq(start+1,start+len)]),col = 'blue',ylab = 'price',xlab = 'lag',type='l')
lines(1:len,predict.value[1:len],col='red')


legend('topright' , legend = c('obs','predicted'),
       lty=1, col=c('blue','red'), bty='n', cex=.75)
write.table(data.frame(exp(obs),predict.value),file = 'C:/Users/LaoZZZZZ/Desktop/Garch_predict.csv'
            ,col.names = TRUE,sep=',',row.names=TRUE)





#garch_pred = read.table('C:/Users/LaoZZZZZ/Desktop/garch_predict.csv',header = T, sep=',')
garch_pred = c(as.numeric(data.close),predict.value)
require(par)
plot.new()

pl = c(as.numeric(data.close),pl)

pu = c(as.numeric(data.close),pu)

x.axis = cbind(range,range,range,range)
y.axis = cbind(observations[range],garch_pred[range],pl[range],pu[range])

matplot(x.axis,y.axis,type='l',col=c('black','red','brown','brown'),xlab='index',ylab='stock price($)')
lines(1:1067,data.close,type='l',col='black')
abline(v = 1068)

legend('topleft' , legend = c('Observation','Prediction','CI'),
       lty=1, col=c('black','red','brown'), bty='n', cex=.75)


par(mar = (c(2,4,2,2) + 0.1))
plot.new()
plot.window(xlim=c(0,1), ylim=c(5,10))
axis(2,at = seq(20,50,1),labels = seq(20,50,1))
axis(1,at=range,labels = range)

plot(range,garch_pred[range],type='l',col = 'red',xlab = "",ylab=''
     ,yaxt="n", lty=1,axes=FALSE)

lines(range,garch_pred[range],type='l',col = 'red',xlab = "",ylab=''
       ,yaxt="n", lty=3,axes=FALSE)
lines(1068:1077,pl,lty=2,col='brown')
lines(1068:1077,pu,lty=2,col='brown')
lines(range,observations[range],type='l',col='black')

lines(range,struct_data[range],col = 'blue')
lines(range,observations[range],col='black')
abline(v=1067,col='black')
legend('bottomleft' , legend = c('Observation','Garch','Structure equation'),
       lty=1, col=c('black','red','blue'), bty='n', cex=.75)
