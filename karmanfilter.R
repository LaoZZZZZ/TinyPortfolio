library(quantmod)
library(PerformanceAnalytics)
require(dlm)

getSymbols('ORCL',from ='2010-01-01',to = '2014-03-31')
data <- ORCL
names(data) <- c('Open','High','Low','Close','Volume','Adjusted')


# orginal data preprocessing
# raw data diagnostic
chartSeries(data)

data.close = data$Adjusted
n = length(data.close)

# get the log return
lnabs.ret = na.omit(diff(log(data.close)))

# training function, no need to see the detail
buildSV = function(parm) {
  # parm[1]=phi, parm[2]=omega, parm[3]=lnsig2n
  parm[3] = exp(parm[3])
  F.mat = matrix(c(1),1,1)
  F.mat[1] = parm[2]
  V.val = pi^2/8
  G.mat = matrix(c(parm[1]),1,1, byrow=TRUE)
  W.mat = diag(0,1)
  W.mat[1] = parm[3]
  m0.vec = 0
  C0.mat = diag(1,1)*1e7
 # C0.mat[1,1] = 1e-7
  #C0.mat[2,2] = 1e-7
 C0.mat[1] = 0
  #C0.mat[1] = parm[3]/(1-parm[1]^2)
  SV.dlm = dlm(FF=F.mat, V=V.val, GG=G.mat, W=W.mat,
               m0=m0.vec, C0=C0.mat)
  return(SV.dlm)
}

# 
# # training function, no need to see the detail
# buildSV = function(parm) {
#   # parm[1]=phi, parm[2]=omega, parm[3]=lnsig2n
#   parm[3] = exp(parm[3])
#   F.mat = matrix(c(1,1),1,2)
#   V.val = pi^2/8
#   G.mat = matrix(c(1,0,0,parm[1]),2,2, byrow=TRUE)
#   W.mat = diag(0,2)
#   W.mat[2,2] = parm[3]
#   m0.vec = c(0,parm[2]/(1-parm[1]))
#   C0.mat = diag(1,2)*1e7
#   C0.mat[1,1] = 1e-7
#   #C0.mat[2,2] = 1e-7
#   C0.mat[2,2] = parm[3]/(1-parm[1]^2)
#   SV.dlm = dlm(FF=F.mat, V=V.val, GG=G.mat, W=W.mat,
#                m0=m0.vec, C0=C0.mat)
#   return(SV.dlm)
# }


# # training function, no need to see the detail
# buildSV = function(parm) {
#   # parm[1]=phi, parm[2]=omega, parm[3]=lnsig2n
#   parm[3] = exp(parm[3])
#   F.mat = matrix(c(1,0,1),1,3)
#   V.val = pi^2/8
#   G.mat = matrix(c(1,0,0,0,1,0,0,1,parm[1]),3,3, byrow=TRUE)
#   W.mat = diag(0,3)
#   W.mat[3,3] = parm[3]
#   m0.vec = c(0, parm[2], parm[2]/(1-parm[1]))
#   C0.mat = diag(1,3)*1e7
#   C0.mat[1,1] = 1e-7
#   C0.mat[2,2] = 1e-7
#   C0.mat[3,3] = parm[3]/(1-parm[1]^2)
#   SV.dlm = dlm(FF=F.mat, V=V.val, GG=G.mat, W=W.mat,
#                m0=m0.vec, C0=C0.mat)
#   return(SV.dlm)
# }

# training the inital model
phi.start = 1
omega.start = 1
lnsig2n.start = 0
start.vals = c(phi.start, omega.start, lnsig2n.start)
SV.mle <- dlmMLE(y=lnabs.ret, parm=start.vals, build=buildSV, hessian=T,
                   lower=c(0, -Inf, -Inf), upper=c(0.999, Inf, Inf))
SV.dlm = buildSV(SV.mle$par)
SV.f <- dlmFilter(lnabs.ret, SV.dlm)
pred <- dlmForecast(SV.f, nAhead = 1)
pred$a[,1]


#prediction
pred.step = 10;
getSymbols('ORCL',from='2014-4-1',to = '2014-4-20')

len = length(ORCL$ORCL.Adjusted)
len = min(pred.step,len)
obs = log(ORCL$ORCL.Adjusted)[1:len]
observation = ORCL$ORCL.Adjusted
mse = c()
pl = c()
pu = c()
fitted = SV.f
dataset =log(data.close)
start = length(dataset)
predict.value =c()
for(i in 1:len){
  move = length(dataset)
  #pred.ci = forecast(fitted,h=1,level=95)
  #pred = as.numeric(pred.ci$mean)
  #pred.ci = predict(fitted, n.ahead = 1, trace = FALSE, mse = "cond",plot=FALSE,nx=NULL)
  pred.ci <- dlmForecast(fitted, nAhead = 1)
  pred = pred.ci$f[,1]

  #print(pred.ci$meanForecast)
  #print(dataset[move])
  pred = exp(pred + as.numeric(dataset[move]))
  pl = c(pl, pred - exp(pred.ci$Q[[1]]*1.96/sqrt(move)))
  pu = c(pu,pred + exp(pred.ci$Q[[1]]*1.96/sqrt(move)))
  predict.value = c(predict.value,pred)
  mse = c(mse,(exp(obs[i])-pred)^2)
  
  dataset = append(dataset,obs[i])
  
  # refit the model
#   SV.mle <- dlmMLE(y=na.omit(diff(dataset)), parm=start.vals, build=buildSV, hessian=T,
#                    lower=c(0, -Inf, -Inf), upper=c(0.999, Inf, Inf))
#   SV.dlm = buildSV(SV.mle$par)
#   

  sv.dlm <-  dlmSmooth(fitted)

  fitted <- dlmFilter(na.omit(diff(dataset)), SV.dlm)
  
  #fitted <-  dlmSmooth(SV.f)
  #fitted = garchFit(~arma(1,1) + garch(1,1),data=na.omit(diff(dataset)),trace=F)
  #fitted=garchFit(~arma(1,1) +garch(1,1),data=na.omit(diff(dataset)),cond.dist="std",trace=F)
  #fitted <- StructTS(na.omit(diff(dataset)),type='trend')
}

mse = mean(mse)
mse
rmse = sqrt(mse)
rmse
plot(seq(1,len),exp(dataset[seq(start+1,start+len)]),col = 'blue',ylab = 'price',xlab = 'lag',type='l')
lines(1:len,predict.value[1:len],col='red')


legend('topright' , legend = c('obs','predicted'),
       lty=1, col=c('blue','red'), bty='n', cex=.75)
write.table(data.frame(exp(obs),predict.value),file = 'C:/Users/LaoZZZZZ/Desktop/state_predict.csv'
            ,col.name = F,sep=',',row.name=F)
# 
# buildFun <- function(x) {
#    m <- dlmModPoly(1, dV = exp(x[1]))
#    m$JW <- matrix(1)
#    m$X <- matrix(exp(x[2]), nc = 1, nr = length(Nile))
#    j <- which(time(Nile) == 1899)
#    m$X[j,1] <- m$X[j,1] * (1 + exp(x[3]))
#    return(m)
#    }
# Nile = ts(data= runif(201),start=1800,end = 2000)
# fit <- dlmMLE(Nile, parm = c(0,0,0), build = buildFun)
# dlmNileJump <- buildFun(fit$par)
# plot(Nile, type = 'o', col = "seagreen")
# points(Nile,col='red')
# points(dlmNileJump$m,col='pink')
# 
# 
# nileJumpFilt <- dlmFilter(Nile, dlmNileJump)
# plot(Nile, type = 'o', col = "seagreen")
# points(Nile,col='red')
# points(nileJumpFilt$m,col='pink')
# lines(dropFirst(nileJumpFilt$m), type = 'o',
#       pch = 20, col = "blue")
# attach(nileJumpFilt)
# v <- unlist(dlmSvd2var(U.C, D.C))
# pl <- dropFirst(nileJumpFilt$m) + qnorm(0.05, sd = sqrt(v[-1]))
# pu <- dropFirst(nileJumpFilt$m) + qnorm(0.95, sd = sqrt(v[-1]))
# detach()
# 
# 
# nileJumpSmooth <- dlmSmooth(nileJumpFilt)
# plot(Nile, type = 'o', col = "seagreen")
# 
# attach(nileJumpSmooth)
# lines(dropFirst(nileJumpSmooth$s), type = 'o', pch = 20, col = "brown")
# v <- unlist(dlmSvd2var(U.S, D.S))
# pl <- dropFirst(s) + qnorm(0.05, sd = sqrt(v[-1]))
# pu <- dropFirst(s) + qnorm(0.95, sd = sqrt(v[-1]))
# detach()
# 
# lines(pl, lty = 2, col = "brown")
# lines(pu, lty = 2, col = "brown")
# 
# lines(pl, lty = 2, col = "brown")
# lines(pu, lty = 2, col = "brown")
# nileJumpSmooth <- dlmSmooth(nileJumpFilt)
# plot(Nile, type = 'o', col = "seagreen")
# 

range = 1050:1077
observations = c(as.numeric(data.close),observation)
#garch_pred = read.table('C:/Users/LaoZZZZZ/Desktop/garch_predict.csv',header = T, sep=',')
garch_pred = c(as.numeric(data.close),predict.value)

pl = c(as.numeric(data.close),pl)

pu = c(as.numeric(data.close),pu)


require(par)
plot.new()


x.axis = cbind(range,range,range,range)
y.axis = cbind(observations[range],garch_pred[range],pl[range],pu[range])

matplot(x.axis,y.axis,type='l',col=c('black','red','brown','brown'),xlab='index',ylab='stock price($)')
lines(1:1067,data.close,type='l',col='black')
abline(v = 1068)

legend('topleft' , legend = c('Observation','Prediction','CI'),
       lty=1, col=c('black','red','brown'), bty='n', cex=.75)