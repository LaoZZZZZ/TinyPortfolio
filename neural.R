# neural network

require(quantmod) 
require(nnet)
require(caret)
require(forecast)
library(PerformanceAnalytics)

getSymbols('ORCL',from ='2010-01-01',to = '2014-03-31')
data <- ORCL
names(data) <- c('Open','High','Low','Close','Volume','Adjusted')

data.close = data$Adjusted

#r = na.omit(diff(log(data.close)))
#fit <- nnetar(r,p = 3)

#pred = predict(fit,h= 2)


#pred = predict(fit,h=1)



nnetGrid <- expand.grid(.decay = c(0,0.01,.1),
                        .size =  c(1:10),
                        .bag=F)

r = data.close
set.seed(100)
x = c()
for(i in 1:5){
  x = cbind(x,as.numeric(Lag(r,i)))
  x = cbind(x,r-Lag(r,i))
}
r = data.frame(x,r)
names(r) = c('lag1','err1','lag2','err2','lag3','err3','lag4','err4','lag5','err5','y')
ctrl <- trainControl(method='cv',number=3)

# training funciton

netTraining <- function(crossval,paraGrid,data){
  nnetTune <- train(y ~ lag1 + lag2 + lag3 + lag4 + lag5 +
                        err1 + err2 + err3 + err4 + err5,
                    data,
                    method = 'avNNet',
                    tuneGrid = paraGrid,
                    trControl = crossval,
                    linout=TRUE,
                    trace = F,
                    maxit = 500
                    )
  return(nnetTune)
}

# add data to the dataset
addPoints <- function(dataset,point){
  
  row = dim(dataset)[1]
  dataset[row+1,] = c(dataset[row,11], point - dataset[row,11],
                      dataset[row,1],  point - dataset[row,1],
                      dataset[row,3],  point - dataset[row,3],
                      dataset[row,5],  point - dataset[row,5],
                      dataset[row,7],  point - dataset[row,7],
                      point)
  return(dataset)
}


fit <- netTraining(ctrl,nnetGrid,r)
#Examine results

plot(1:1067,r$y,type="l",col = 2)
lines(6:1067,ps, col='black')

predict(nnetTune,r[1067,])

# prediction
pred.step = 10;
getSymbols('ORCL',from='2014-4-1',to = '2014-4-20')

len = length(ORCL$ORCL.Adjusted)
len = min(pred.step,len)
obs = ORCL$ORCL.Adjusted[1:len]
mse = c()
fitted = nnetTune
dataset =r
start = dim(dataset)[1]
predict.value =c()
for(i in 1:len){
  move = dim(dataset)[1]
  #pred.ci = forecast(fitted,h=1,level=95)
  #pred = as.numeric(pred.ci$mean)
  #pred = predict(fitted, h = 1)$mean
  pred = predict(fitted,dataset[move,])
  
  #pred = pred.ci$meanForecast
  #print(pred.ci$meanForecast)
  #print(dataset[move])
  #pred = exp(pred + as.numeric(dataset[move]))
  #print(pred)
  predict.value = c(predict.value,pred)
  mse = c(mse,(obs[i]-as.numeric(pred))^2)
  
  dataset = addPoints(dataset,obs[i])
  #print(dim(dataset))
  #fitted  <- netTraining(ctrl,nnetGrid,dataset)
  # fitted <- nnetar(data.close,p= 6,size= 3,lambda=0)
  
  #fitted <- nnetar(dataset,p=3,lambda=0)
  
  #fitted = garchFit(~arma(1,1) + garch(1,1),data=na.omit(diff(dataset)),trace=F)
  #fitted=garchFit(~arma(1,1) +garch(1,1),data=na.omit(diff(dataset)),cond.dist="std",trace=F)
  #fitted <- StructTS(na.omit(diff(dataset)),type='trend')
}


mse = mean(mse)
mse
rmse = sqrt(mse)
rmse
plot(seq(1,len),dataset[seq(start+1,start+len),11],col = 'blue',ylab = 'price',xlab = 'lag',type='l')
lines(1:len,predict.value[1:len],col='red')


legend('topright' , legend = c('obs','predicted'),
       lty=1, col=c('blue','red'), bty='n', cex=.75)
write.table(data.frame(as.numeric(obs),predict.value),file = 'C:/Users/LaoZZZZZ/Desktop/neural_predict.csv'
            ,col.name = F,sep=',',row.name=F)

range = 1050:1077
observations = c(as.numeric(data.close),obs)
garch_pred = c(as.numeric(data.close),predict.value)

require(par)
plot.new()
plot(range,observations[range],type = 'l',main='prediction of nnar',xlab='index',ylab='stock price($)')
lines(1068:1077,garch_pred[1068:1077],type='l',lty=2,col='red')
abline(v=1068)
legend('topleft' , legend = c('Observation','Prediction'),
       lty=1, col=c('black','red'), bty='n', cex=.75)


predictions <- read.table('C:/Users/LaoZZZZZ/Desktop/586_project/oracle/predict_all_oracal.csv',header = T, sep=',')
colnames <- predictions[1,]
predics <- predictions[2:11,2:5]

names(predics) <- c('date','observation','garch','ssm','nnet')
garch_pred <- c(as.numeric(data.close),as.vector(predics[,2]))
ssm   <- c(as.numeric(data.close),as.vector(predics[,3]))
nu   <- c(as.numeric(data.close),as.vector(predics[,4]))
predrange = 1068:1077

x.axis = cbind(range,predrange,predrange,predrange)
y.axis = cbind(observations[range],garch_pred[predrange],ssm[predrange],nu[predrange])

plot(range,observations[range],type='l')
lines(predrange,garch_pred[predrange],type='l',col='red')
lines(predrange,ssm[predrange],type='l',col='blue')
lines(predrange,nu[predrange],type='l',col='yellow')


legend('topleft' , legend = c('Observation','Garch','state space','Neural network'),
       lty=1, col=c('black','red','blue','yellow'), bty='n', cex=.75)
lines(1:1067,data.close,type='l',col='black')
abline(v = 1068)
