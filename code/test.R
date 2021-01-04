source("signT.R")
## test
set.seed(1)
d=10
a=randortho(d)[,1]
b=randortho(d)[,1]
c=randortho(d)[,1]

##### simulate graphon models
a=seq(from=0,to=1,length=d)
b=seq(from=0,to=1,length=d)
#c=seq(from=0,to=1,length=d)
signal=graphon_to_tensor(a,b,0,type=10)
signal=signal[,,1]
#plot(svd(signal)$d)
#signal=sigmoid(a%o%b,a=1)
#signal=sigmoid(a%o%b%o%c,a=50)
Y=signal+array(rnorm(length(signal),0,0*max(abs(signal))),dim=dim(signal))
truer=2
hist(Y)
missing=array(rbinom(length(signal),1,0),dim=dim(signal))
Y[missing==1]=NA

Lmin=min(Y,na.rm=T)
Lmax=max(Y,na.rm=T)

set.seed(1)
res=SignT(Y,truer,Lmin=min(signal),Lmax=max(signal),H=30,rho=.001,option=1)
plot(res$est,signal)
abline(0,1)
plot(res$est[missing==0],signal[missing==0])
plot(res$est[missing==1],signal[missing==1])
mean(abs(res$est[missing==0]-signal[missing==0])^2)
abline(0,1)


### matrix
est2=fit_continuous(Y,truer)
plot(est2,signal)
plot(est2[missing==0],signal[missing==0])
plot(est2[missing==1],signal[missing==1])
mean(abs(est2[missing==0]-signal[missing==0])^2)
abline(0,1)

