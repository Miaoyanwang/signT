source("signT.R")
set.seed(1)
d=30;r=3;
data_full=NULL
for(nsim in 1:10){
a=matrix(rnorm(d*r),ncol=r)
T=tensorize(a,a,a)
data_full=NULL
c=c(0.1,0.5,1,3,5,10,15,20,25,30,35,40,45,50,75,100,125,150,175,200)
rank=rank2=accuracy=NULL
for(i in 1:20){
T_trans=1/(1+exp(-(c[i]*T)))
T_trans=as.tensor(T_trans/sqrt(sum(T_trans^2)))
tensor_rank=appx_rank(T_trans,thresh=90,step=5)
rank2=c(rank2,max(tensor_rank[,1]))
accuracy=c(accuracy,tensor_rank[dim(tensor_rank)[1],2])
}
data=data.frame(c,rank2)
colnames(data)=c("c","rank")
data_full=rbind(data_full,data)
}
save(data,fild="example1.RData")
pdf("example1.pdf",width=3.5,height=3)
ggplot(data,aes(c,rank))+geom_point(aes(c,rank))+geom_line(aes(c,rank))
dev.off
