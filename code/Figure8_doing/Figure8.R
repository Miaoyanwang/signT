### min hypergraphon, type=9
source("signT.R")
set.seed(1)
error=error_con=NULL
d=40
rho_list=seq(from=0.3,to=1,length=8)

args <- (commandArgs(trailingOnly=TRUE))
cat(args[1])
if(length(args) == 1){
    BATCH <- as.numeric(args[1])
} else {
    stop()
}

truer=2
rho=rho_list[BATCH]
obs=rbinom(d^3,1,rho)
##### simulate graphon models
a=seq(from=0,to=1,length=d)
signal=graphon_to_tensor(a,a,a,type=9)
for(sim in 1:30){
Y=signal+array(rnorm(length(signal),0,0.15),dim=dim(signal)) ###
Y[obs==0]=NA
Lmin=min(Y,na.rm=T)
Lmax=max(Y,na.rm=T)
set.seed(1)
res=SignT(Y,truer,Lmin=Lmin,Lmax=Lmax,H=10+2*(d-15)/10,option=2)
error=c(error,mean(abs(res$est-signal)))
##### continuous method
res2=fit_continuous(Y,truer)
error_con=c(error_con,mean(abs(res2$est-signal)))
print(paste("dimension",d,"-simulation",sim," is done",sep = ""))
}

save(error,error_con,file=paste("Figure8-dimension",rho,".RData",sep=""))


####################### load output from server#######################
rho_list=seq(from=0.3,to=1,length=8)
con=ours=NULL
load("Figure8-dimension0.3.RData")
ours=error; con=error_con
for(i in 2:7){
    load(sprintf("Figure8-dimension%.1f.RData",rho_list[i]))
    ours=rbind(ours,error)
    con=rbind(con,error_con)
}
load("Figure8-dimension1.RData")
ours=rbind(ours,error); con=rbind(con,error_con)
error=ours;error_con=con
save(error,error_con,file="Model8.RData")
#######################

##### plot
load("Model8.RData")
library(ggplot2)
error_cont=cbind(rho_list,apply(error_con,1,mean),apply(error_con,1,sd))
error_ours=cbind(rho_list,apply(error,1,mean),apply(error,1,sd))
data=data.frame(rbind(error_cont,error_ours))
data=cbind(data,c(rep("cont",8),rep("ours",8)))
colnames(data)=c("dim","mean","se","method")


pdf("Model8.pdf",width=4,height=3)
figure=ggplot(data,aes(x=dim,y=mean))+geom_point(aes(x=dim,y=mean,col=method,shape=method),size=2)+geom_line(aes(x=dim,y=mean,col=method))+geom_errorbar(aes(ymin=mean-se/sqrt(30), ymax=mean+se/sqrt(30),col=method), width=.01,position=position_dodge(0))+labs(x = "dimension")+labs(y = "MAE")+coord_cartesian(ylim = c(0, 0.1))

figure
dev.off()



#source("../signT.R")
#truer=2
#error_con=error=matrix(0,nrow=5,ncol=2)
#for(BATCH in 1:5){
#d=d_list[2*BATCH]
##### simulate graphon models
#a=seq(from=0,to=1,length=d)
#signal=graphon_to_tensor(a,a,a,type=9)
#for(sim in 1:2){
#    Y=signal+array(rnorm(length(signal),0,0.15),dim=dim(signal)) ###
#    Lmin=min(Y,na.rm=T)
#    Lmax=max(Y,na.rm=T)
#    set.seed(1)
#res=SignT(Y,truer,Lmin=Lmin,Lmax=Lmax,H=10+2*(d-15)/10,option=2)
#    error[BATCH,sim]=mean(abs(res$est-signal))
##### continuous method#
#    res2=fit_continuous(Y,truer)
#    error_con[BATCH,sim]=mean(abs(res2$est-signal))
#    print(paste("dimension",d,"-simulation",sim," is done",sep = ""))
#}
#}
