
####################################################
# data preprocessing ###############################
data=read.table("nips.tns.txt")
author_map=read.table("mode-2-authors.map.txt")
word_map=read.table("mode-3-words.map.txt")
year_map=read.table("mode-4-years.map.txt")

authorcount=aggregate(data[,5],by=list(data[,2]),FUN=sum)
wordcount=aggregate(data[,5],by=list(data[,3]),FUN=sum)

authorID=authorcount[sort(authorcount[,2],index=T,decreasing=T)$ix,][1:100,1] ## top 100 authors
wordID=wordcount[sort(wordcount[,2],index=T,decreasing=T)$ix,][1:200,1] ## top 200 words
data=data[(data[,2]%in%authorID)&(data[,3]%in%wordID),]

names(data) = c("papers","authors","words","years","counts")
# reshape dataframe to tensor
library(reshape2)
tensor = acast(data,authors~words~years,fun.aggregate=sum,value.var = "counts") ## sum up word counts for each triplet

tensor=log(tensor+1)
dimnames(tensor)[[1]]=author_map[authorID,]
dimnames(tensor)[[2]]=word_map[wordID,]
dimnames(tensor)[[3]]=year_map[,1]
save(tensor,file="rNIPS.RData")



