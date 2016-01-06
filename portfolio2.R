setwd('c:/Users/LaoZZZZZ/Desktop/portfolio')
O <- read.delim("ORCL.txt")
O.P <- read.delim("ORCL_Predict.txt") 
B <- read.delim("BP.txt")
B.P <- read.delim("BP_Predict.txt")
P <- read.delim("PFE.txt")
P.P <- read.delim("PFE_Predict.txt")
J <- read.delim("JPM.txt")
J.P <- read.delim("JPM_Predict.txt")

T.O <- c(O$ORCL.Adjusted, O.P$ORCL.Adjusted)
# real return
rtn1 <- na.omit(diff(log(T.O)))
T.B <- c(B$Adj.Close, B.P$Adj.Close)
rtn2 <- na.omit(diff(log(T.B)))
T.P <- c(P$Adj.Close, P.P$Prediction.Value)
rtn3 <- na.omit(diff(log(T.P)))
T.J <- c(J$Adj.Close, J.P$Predict.value)
rtn4 <- na.omit(diff(log(T.J)))

# real future return
ORCL.rtn <- rtn1[-length(rtn1)+9:-length(rtn1)]
BP.rtn <- rtn2[-length(rtn2)+9:-length(rtn2)]
PFE.rtn <- rtn3[-length(rtn3)+9:-length(rtn3)]
JPM.rtn <- rtn4[-length(rtn4)+9:-length(rtn4)]


# real future return
rtn <- cbind(ORCL=ORCL.rtn, BP=BP.rtn, PFE=PFE.rtn, JPM=JPM.rtn)


# real log return

real1 <- na.omit(diff(log(T.O)))
realrtn.ORCL <- exp(real1[-1:-1066])-1
real2 <- na.omit(diff(log(T.B)))
realrtn.BP <- exp(real1[-1:-1066])-1
real3 <- na.omit(diff(log(T.P)))
realrtn.PFE <- exp(real1[-1:-1066])-1
real4 <- na.omit(diff(log(T.J)))
realrtn.JPM <- exp(real1[-1:-1066])-1


ORCL.ER <- c(O.P$Exp.Lret)
BP.ER <- c(B.P$Exp.Lret)
PFE.ER <- c(P.P$Exp.Lret)
JPM.ER <- c(J.P$Exp.Lret)


target.return <- 0.01
r.free <- 0.0001

covmat <- cov(rtn)
inv.covmat <- solve(covmat)
one <- matrix(c(1,1,1,1), nrow=4)


# realrtn <- matrix(nrow=10)
# 
# for (i in (1:10)){
#   er <- c(ORCL.ER[i], BP.ER[i], PFE.ER[i], JPM.ER[i])
#   A <- (target.return-r.free)*(inv.covmat%*%(er-r.free*er))
#   B <- t(er-one*r.free)%*%inv.covmat%*%(er-one*r.free)
#   C <- A%*%(1/B)
#   weights <- rbind(C, RF=(1-(C[1]+C[2]+C[3]+C[4])))
#   realrtn[i] <- matrix(c(realrtn.ORCL[i], realrtn.BP[i], realrtn.PFE[i], realrtn.JPM[i], r.free), nrow=1)%*%weights
# }
# 
# print(realrtn)
# 
# (1+realrtn[1])*(1+realrtn[2])*(1+realrtn[3])*(1+realrtn[4])*(1+realrtn[5])*(1+realrtn[6])*(1+realrtn[7])*(1+realrtn[8])*(1+realrtn[9])*(1+realrtn[10])-1


er = cbind(ORCL.ER,BP.ER,PFE.ER,JPM.ER)
A = (target.return-r.free) * inv.covmat %*% t((er - r.free*er))
r.free.mat = matrix(rep(r.free,40),ncol=4,nrow=10,byrow=T)
B = diag((er - r.free.mat)%*%inv.covmat%*%t(er-r.free.mat))
B = matrix(rep(B,4),ncol = 10,nrow = 4,byrow=TRUE)
c = A / B

weights = rbind(c,apply(c,2,function(x) 1-sum(x)))
realrtn <- diag(cbind(realrtn.ORCL,realrtn.BP,realrtn.PFE,realrtn.JPM,r.free.mat[,1]) %*% weights)

result = 1
for(i in 1:10)
  result = result*(1+realrtn[i])
result - 1

