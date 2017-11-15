N<-6 # Anzahl Experten
n<-1000 # Anzahl Runden

etaopt<-sqrt(8*log(N)/n)

expert.pred<-matrix(0,nrow=N, ncol=n)
expert.loss<-matrix(0,nrow=N, ncol=n)
expert.weights<-matrix(0,nrow=N, ncol=n)

expert.pred[1,]<-rep(0.001,n)                  #schwarz
expert.pred[2,]<-rbinom(n,size=1,prob=0.05)    #rot
expert.pred[3,]<-rbinom(n,size=1,prob=0.01)    #grün
expert.pred[4,]<-rnorm(n,mean=0,sd=0.05)       #blau
expert.pred[5,]<-0.1*((n-1):0)/n               #türkis

target<-rnorm(n,mean=0,sd=0.1)

expert.pred[6,]<-target                        #pink


loss<-function(pred,y) abs(y-pred)

forecaster.pred<-rep(0,n)
forecaster.loss<-rep(0,n)
forecaster.regret<-matrix(0,nrow=N, ncol=n)

weights.calc.poly<-function(regret,p) {
  weights<-apply(cbind(regret,0),1,max)^p
  2*weights/sum(weights)
}

weights.calc.exp<-function(regret,eta) {
  weights<-exp(eta*regret)
  weights/sum(weights)
}

weights.calc.ftl<-function(regret) {
  weights<-rep(0,N)
  weights[which.max(regret)]<-1
  weights
  }

for (t in 1:n) {
  experts<-expert.pred[,t]
  if (t==1) weights<-rep(1/N,N)
  else {
   regret<-forecaster.regret[,(t-1)]
   # weights<-weights.calc.poly(regret,2)  
   weights<-weights.calc.exp(regret,etaopt)  
   #   weights<-weights.calc.ftl(regret)  
  }
  if (sum(is.na(weights))>0) weights<-rep(1/N,N)
  weights<-weights/sum(weights)
  print(weights)
  expert.weights[,t]<-weights
  pred<-sum(weights*experts)
  forecaster.pred[t]<-pred
  y<-target[t]
  forecaster.loss[t]<-loss(pred,y)
  expert.loss[,t]<-loss(experts,rep(y,N))
  for (j in 1:N) forecaster.regret[j,t]<-sum(forecaster.loss[1:t])-sum(expert.loss[j,1:t])
  }

plot(expert.weights[1,],type="l",ylim=c(0,1))
for (i in 2:N) lines(expert.weights[i,],col=i)
title("Weights")

plot(forecaster.regret[1,],type="l",ylim=c(-10,20))
for (i in 2:N) lines(forecaster.regret[i,],col=i)
title("Regret")

