source("signT.R")
## test
set.seed(1)
d=20
a=randortho(d)[,1]
b=randortho(d)[,1]
c=randortho(d)[,1]
error=NULL

##### simulate graphon models
for(d in c(20,40,50)){
a=seq(from=0,to=1,length=d)
b=seq(from=0,to=1,length=d)
c=seq(from=0,to=1,length=d)
signal=graphon_to_tensor(a,b,c,type=10)
Y=signal+array(rnorm(length(signal),0,0.1*max(abs(signal))),dim=dim(signal))
truer=1
hist(Y)
#missing=array(rbinom(length(signal),1,0),dim=dim(signal))
#Y[missing==1]=NA
#Lmin=min(Y,na.rm=T)
#Lmax=max(Y,na.rm=T)

set.seed(1)
res=SignT(Y,truer,Lmin=Lmin,Lmax=Lmax,H=10,option=2) ## recommend option = 2 or 3.
plot(res$est,signal)
abline(0,1)
plot(res$est[missing==0],signal[missing==0])
plot(res$est[missing==1],signal[missing==1])
error=c(error,mean(abs(res$est[missing==0]-signal[missing==0])))
abline(0,1)
}

### continuous
est2=fit_continuous(Y,truer)
plot(est2,signal)
plot(est2[missing==0],signal[missing==0])
plot(est2[missing==1],signal[missing==1])
mean(abs(est2[missing==0]-signal[missing==0])^2)
abline(0,1)

data=readMat("../data/dnations.mat")
data=readMat("../data/alyawarradata.mat")
tensor=data$R

none=sum(tensor==1,na.rm=T)
nzero=sum(tensor==0,na.rm=T)
sum(is.na(tensor))
none_test=sample(none,0.2*none,replace=T) ## test
nzero_test=sample(nzero,0.2*nzero,replace=T) ## test
training=tensor
training[c(none_test,nzero_test)]=NA

set.seed(1)
res=SignT(training,truer=2,0,1,H=10)
res2=fit_continuous(training,2)

mean((res$est[c(none_test,nzero_test)]-tensor[c(none_test,nzero_test)])^2,na.rm=T)
mean((res2[c(none_test,nzero_test)]-tensor[c(none_test,nzero_test)])^2,na.rm=T)


auc(tensor[c(none_test,nzero_test)],res$est[c(none_test,nzero_test)])
auc(tensor[c(none_test,nzero_test)],res2[c(none_test,nzero_test)])

