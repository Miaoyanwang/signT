source("signT.R")
##########Model 2###########################################################
result = data.frame(matrix(nrow = 90 ,ncol = 4))
names(result) = c("dim","sim","MSE")
result$dim = rep(c(20,40,60),each = 30)
result$sim = rep(1:30,3)

MSE = NULL

for(d in c(20,40,60)){
##### simulate graphon models
  set.seed(1)
  a=seq(from=0,to=1,length=d)
  signal=graphon_to_tensor(a,a,a,type=6)

  for(sim in 1:30){
    Y=signal+array(rnorm(length(signal),0,0.15),dim=dim(signal)) ###
    
    MSE= c(MSE,mean(abs( Spectral(Y,1,c(2,3),threshold = 0.3*d)-signal)))
    
    print(paste("dimension",d,"-simulation",sim," is done",sep = ""))
  }
}

result$MSE = MSE


result = summarySE(result, measurevar="MSE", groupvars=c("dim"));result

load("simulation_figure/Figure2/Figure2-dimension20.RData")
mean(error)
load("simulation_figure/Figure2/Figure2-dimension40.RData")
mean(error)
load("simulation_figure/Figure2/Figure2-dimension60.RData")
mean(error)
##########Model 3###########################################################



result = data.frame(matrix(nrow = 90 ,ncol = 4))
names(result) = c("dim","sim","MSE")
result$dim = rep(c(20,40,60),each = 30)
result$sim = rep(1:30,3)

MSE = NULL

for(d in c(20,40,60)){
  ##### simulate graphon models
  set.seed(1)
  a=seq(from=0,to=1,length=d)
  signal=graphon_to_tensor(a,a,a,type=10)
  
  for(sim in 1:30){
    Y=signal+array(runif(length(signal),-0.1,0.1),dim=dim(signal)) ###
    
    MSE= c(MSE,mean(abs( Spectral(Y,1,c(2,3),threshold = 0.4*d/sqrt(12))-signal)))
    
    print(paste("dimension",d,"-simulation",sim," is done",sep = ""))
  }
}

result$MSE = MSE
result = summarySE(result, measurevar="MSE", groupvars=c("dim"));result

load("simulation_figure/Figure3/Figure3-dimension20.RData")
mean(error)
load("simulation_figure/Figure3/Figure3-dimension40.RData")
mean(error)
load("simulation_figure/Figure3/Figure3-dimension60.RData")
mean(error)


##########Model 1###########################################################




result = data.frame(matrix(nrow = 90 ,ncol = 4))
names(result) = c("dim","sim","MSE")
result$dim = rep(c(20,40,60),each = 30)
result$sim = rep(1:30,3)

MSE = NULL

for(d in c(20,40,60)){
  ##### simulate graphon models
  set.seed(1)
  a=seq(from=0,to=1,length=d)
  signal=graphon_to_tensor(a,a,a,type=5)
  
  for(sim in 1:30){
    Y=signal+array(runif(length(signal),-0.3,0.3),dim=dim(signal)) ###
    
    MSE= c(MSE,mean(abs( Spectral(Y,1,c(2,3),threshold = 1.2*d/sqrt(12))-signal)))
    
    print(paste("dimension",d,"-simulation",sim," is done",sep = ""))
  }
}

result$MSE = MSE
result = summarySE(result, measurevar="MSE", groupvars=c("dim"));result



load("simulation_figure/Figure1/Figure1-dimension20.RData")
mean(error)
sd(error)
mean(error_con)
sd(error_con)
load("simulation_figure/Figure1/Figure1-dimension40.RData")
mean(error)
sd(error)
mean(error_con)
sd(error_con)
load("simulation_figure/Figure1/Figure1-dimension60.RData")
mean(error)
sd(error)
mean(error_con)
sd(error_con)



##########Model 4###########################################################


result = data.frame(matrix(nrow = 90 ,ncol = 4))
names(result) = c("dim","sim","MSE")
result$dim = rep(c(20,40,60),each = 30)
result$sim = rep(1:30,3)

MSE = NULL

for(d in c(20,40,60)){
  ##### simulate graphon models
  set.seed(1)
  a=seq(from=0,to=1,length=d)
  signal=graphon_to_tensor(a,a,a,type=9)
  
  for(sim in 1:30){
    Y=signal+array(rnorm(length(signal),0,0.15),dim=dim(signal)) ###
    
    MSE= c(MSE,mean(abs( Spectral(Y,1,c(2,3),threshold = 0.3*d)-signal)))
    
    print(paste("dimension",d,"-simulation",sim," is done",sep = ""))
  }
}

result$MSE = MSE
result = summarySE(result, measurevar="MSE", groupvars=c("dim"));result



load("simulation_figure/Figure4/Figure4-dimension20.RData")
mean(error)
sd(error)
mean(error_con)
sd(error_con)
load("simulation_figure/Figure4/Figure4-dimension40.RData")
mean(error)
sd(error)
mean(error_con)
sd(error_con)
load("simulation_figure/Figure4/Figure4-dimension60.RData")
mean(error)
sd(error)
mean(error_con)
sd(error_con)

