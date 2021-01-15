########## prediction accuracy via cross validation
load("rNIPS.RData")
source("../../../code/signT.R")
set.seed(1)
fold_data=sample(1:5,length(tensor),replace=TRUE)
error_our=error_con=error_naive=matrix(0,nrow=5,ncol=5)

for(r in 1:5){
truer=3*r
for(fold in 1:5){
hold=which(fold_data==fold)
training=tensor
training[hold]=NA
##### ours
res=SignT(training,truer,min(training,na.rm=T),max(training,na.rm=T),H=20,option=2)
error_our[r,fold]=mean(abs(res$est[hold]-tensor[hold]),na.rm=T) ## ours
##### cp
res2=fit_continuous(training,truer)
error_con[r,fold]=mean(abs(res2$est[hold]-tensor[hold]),na.rm=T)##CP
##### naive
error_naive[r,fold]=mean(abs(mean(training,na.rm=T)-tensor[hold])) ##naive
}
}

error_our
error_con
error_naive

############### signal estimation ###############
load("rNIPS.RData")
source("../../../code/signT.R")
set.seed(1)
truer=10
res=SignT(tensor,truer,min(tensor,na.rm=T),max(tensor,na.rm=T),H=20,option=2)
save(res,tensor,truer,file="est.RData")

############### plot ###############
library(rTensor)
load("est.RData")
dimnames(res$est)=dimnames(tensor)
author_by_year=apply(tensor,c(1,3),mean)
###### set word count to NA if the author publishes no papars in that year
index=which(author_by_year==0,arr.ind=T)
for(i in 1:dim(index)[1]){
    res$est[index[i,1],,index[i,2]]=NA
    tensor[index[i,1],,index[i,2]]=NA
}

###### reorder the index to move the top (author,paper,year) to the conner
index=which(author_by_year==0,arr.ind=T)
denoise=res$est
tensor=tensor[sort(apply(denoise,1,function(x)mean(x,na.rm=T)),index=T,decreasing=T)$ix,sort(apply(denoise,2,function(x)mean(x,na.rm=T)),index=T,decreasing=T)$ix,sort(apply(denoise,3,function(x)mean(x,na.rm=T)),index=T,decreasing=T)$ix]
denoise=denoise[sort(apply(denoise,1,function(x)mean(x,na.rm=T)),index=T,decreasing=T)$ix,sort(apply(denoise,2,function(x)mean(x,na.rm=T)),index=T,decreasing=T)$ix,sort(apply(denoise,3,function(x)mean(x,na.rm=T)),index=T,decreasing=T)$ix]

### top word after removing genetic word
topword=dimnames(denoise)[[2]][c(5,6,7,11,13,14,16,17,18,19)]

### after denoising
pdf("after_denoise.pdf",width=10,height=10)
levelplot(denoise[1:10,rev(topword),c(3,4,2,1)],col.regions=hcl.colors(100, "YlOrRd", rev = TRUE),at=seq(0,2.5, length.out=100),scales=list(x=list(rot=45))) ## reverse the word order b.c. default layout in levelplot
dev.off()
### before denoising
pdf("before_denoise.pdf",width=10,height=10)
levelplot(tensor[1:10,rev(topword),c(3,4,2,1)],col.regions=hcl.colors(100, "YlOrRd", rev = TRUE),at=seq(0,2.5, length.out=100),limscales=list(x=list(rot=45)))
dev.off()

### top word frequency
apply(denoise,2,function(x)mean(x,na.rm=T))[topword]

### top author frequency
apply(denoise,1,function(x)mean(x,na.rm=T))[1:10]

pdf("top_word.pdf",width=6,height=4)
par(mar=c(7,3,1,1))
barplot(apply(denoise,2,function(x)mean(x,na.rm=T))[topword],ylim=c(0.68,0.78),beside=TRUE, xpd=FALSE,las=2)
dev.off()
#library(wordcloud)
#wordcloud(words=dimnames(tensor)[[2]],freq=apply(res$est,2,mean),colors=brewer.pal(8, "Dark2"),rot.per=0.35, min.freq =.5,random.order=FALSE)
