## Model 1. stochastic tensor block, type=5
source("signT.R")
set.seed(1)
error=error_con=NULL
d_list=seq(from=15,to=60,length=10)

args <- (commandArgs(trailingOnly=TRUE))
cat(args[1])
if(length(args) == 1){
    BATCH <- as.numeric(args[1])
} else {
    stop()
}

truer=3
d=d_list[BATCH]
##### simulate graphon models
a=seq(from=0,to=1,length=d)
signal=graphon_to_tensor(a,a,a,type=5)
for(sim in 1:30){
Y=signal+array(runif(length(signal),-0.5,0.5),dim=dim(signal))
Lmin=min(Y,na.rm=T)
Lmax=max(Y,na.rm=T)
##### our method
res=SignT(Y,truer,Lmin=Lmin,Lmax=Lmax,H=10,option=2)
error=c(error,mean(abs(res$est-signal)))
res_m=SignUnfold(Y,truer,Lmin=Lmin,Lmax=Lmax,H=10,option=2,signal)
##### continuous method
res2=fit_continuous(Y,truer)
error_con=c(error_con,mean(abs(res2$est-signal)))
print(paste("dimension",d,"-simulation",sim," is done",sep = ""))
}

save(error,error_con,file=paste("Figure1-dimension",d,".RData",sep=""))


####################### load output from server#######################
d_list=seq(from=15,to=60,length=10)
con=ours=NULL
load("Figure1-dimension15.RData")
ours=error; con=error_con
for(i in d_list[2:10]){
    load(sprintf("Figure1-dimension%d.RData",i))
    ours=rbind(ours,error)
    con=rbind(con,error_con)
}
error=ours;error_con=con
save(error,error_con,file="Model1.RData")
#######################

##### plot
d_list=seq(from=15,to=60,length=10)
load("Model1.RData")
library(ggplot2)
error_cont=cbind(d_list,apply(error_con,1,mean),apply(error_con,1,sd))
error_ours=cbind(d_list,apply(error,1,mean),apply(error,1,sd))
data=data.frame(rbind(error_cont,error_ours))
data=cbind(data,c(rep("cont",10),rep("ours",10)))
colnames(data)=c("dim","mean","se","method")


pdf("Model1.pdf",width=4,height=3)
figure=ggplot(data,aes(dim,mean))+geom_point(aes(dim,mean,col=method,shape=method),size=2)+geom_line(aes(dim,mean,col=method))+geom_errorbar(aes(ymin=mean-se/sqrt(30), ymax=mean+se/sqrt(30),col=method), width=1,position=position_dodge(0))+labs(x = "dimension")+labs(y = "MAE")+coord_cartesian(ylim = c(0.02, 0.08))

figure
dev.off()

library(Hmisc)
a=seq(from=0,to=1,length=300)
source("../signT.R")
signal=graphon_to_tensor(a,a,a,type=10)
pdf("cdf_model3.pdf",height=2,width=3)
Ecdf(c(signal),xlim=c(-1,1),axes=F)
axis(1,at=c(-1,0,1))
axis(2,at=c(0,0.5,1))
abline(h=1)
abline(h=0)
dev.off()


signal=graphon_to_tensor(a,a,a,type=9)
pdf("cdf_model4.pdf",height=2,width=3)
Ecdf(c(signal),xlim=c(-1,1),axes=F)
axis(1,at=c(-1,0,1))
axis(2,at=c(0,0.5,1))
abline(h=1)
abline(h=0)
dev.off()


signal=graphon_to_tensor(a,a,a,type=5)
pdf("cdf_model1.pdf",height=2,width=3)
Ecdf(c(signal),xlim=c(-1,1),axes=F)
axis(1,at=c(-1,0,1))
axis(2,at=c(0,0.5,1))
abline(h=1)
abline(h=0)
dev.off()

signal=graphon_to_tensor(a,a,a,type=6)
pdf("cdf_model2.pdf",height=2,width=3)
Ecdf(c(signal),xlim=c(-1,1),axes=F)
axis(1,at=c(-1,0,1))
axis(2,at=c(0,0.5,1))
abline(h=1)
abline(h=0)
dev.off()
