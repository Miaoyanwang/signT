library(pracma)
library(rTensor)
library(quadprog)
library(Matrix)
min.thresh=10^(-3)
max.thresh=10^10

binaryloss=function(Ybar,W,Yfit){
    return(mean(W*abs(Ybar-sign(Yfit)),na.rm=T))
}

#################### main function for nonparametric tensor completion  ####################
SignT=function(Y,truer,H=5,Lmin,Lmax,rho=0.1,lambda=10^(-3),option=2){

    B_fitted=result=list()
    pi_seq=seq(from=Lmin,to=Lmax,length=2*H+1)
    for(h in 2:(2*H)){
        pi=pi_seq[h]
        if(option==1){
            res=ADMM(sign(Y-pi),abs(Y-pi),r=truer,rho=rho,lambda=lambda)}
        else if(option==2){
            res=Alt(sign(Y-pi),abs(Y-pi),r=truer,type="logistic") ## recommend
        }else if(option==3){
            res=Alt(sign(Y-pi),abs(Y-pi),r=truer,type="hinge") ## recommend
        }
        result[[h]]=res
        B_fitted[[h]]=res$fitted
    }
    B_fitted=array(unlist(B_fitted),dim=c(dim(Y),2*H-1));
    res=list();
    res$result=result;
    res$fitted=B_fitted
    res$est=1/2*(apply(sign(B_fitted),1:length(dim(Y)),mean)+1)*(Lmax-Lmin)+Lmin
    return(res)
}
### Alternating optimization for classification
Alt=function(Ybar,W,r,type=c("logistic","hinge")){
    result=list()
    d=dim(Ybar)
    ini=cp(as.tensor(fit_continuous(Ybar,r)),r);
    A1 = ini$U[[1]];
    A2 = ini$U[[2]];
    scale=matrix(0,nrow=r,ncol=r)
    diag(scale)=ini$lambda
    A3 = ini$U[[3]]%*%scale;
    
    #A1 = cbind(randortho(d[1])[,1:r]);
    #A2 = cbind(randortho(d[2])[,1:r]);
    #A3 = cbind(randortho(d[3])[,1:r]);
    obj=cost(A1,A2,A3,Ybar,W,type);
    binary_obj=binaryloss(Ybar,W,tensorize(A1,A2,A3))
    
    error=1;iter=1;
 
 while((error>0.01)&(binary_obj[iter]>0.01)&(iter<20)){
     
 
 #tic()
 optimization=optim(c(A3),function(x)cost(A1,A2,matrix(x,ncol=r),Ybar,W,type),function(x)gradient(A1,A2,matrix(x,ncol=r),3,Ybar,W,type),method="BFGS")
 A3=matrix(optimization$par,ncol=r)
 optimization=optim(c(A2),function(x)cost(A1,matrix(x,ncol=r),A3,Ybar,W,type),function(x)gradient(A1,matrix(x,ncol=r),A3,2,Ybar,W,type),method="BFGS")
 A2=matrix(optimization$par,ncol=r)
 optimization=optim(c(A1),function(x)cost(matrix(x,ncol=r),A2,A3,Ybar,W,type),function(x)gradient(matrix(x,ncol=r),A2,A3,1,Ybar,W,type),method="BFGS")
 A1=matrix(optimization$par,ncol=r)
 #toc()

 #tic()
 #l=lapply(1:nrow(A3),function(i){optim(c(A3[i,]),function(x)cost(A1,A2,matrix(x,ncol=r),array(Ybar[,,i],dim=c(d[1],d[2],1)),array(W[,,i],dim=c(d[1],d[2],1)),type),function(x)gradient(A1,A2,matrix(x,ncol=r),3,array(Ybar[,,i],dim=c(d[1],d[2],1)),array(W[,,i],dim=c(d[1],d[2],1)),type),method="BFGS")$par}) ## perhaps faster than the other approach?
 #A3=matrix(unlist(l),nrow=nrow(A3),byrow=T)
 #l=lapply(1:nrow(A2),function(i){optim(c(A2[i,]),function(x)cost(A1,matrix(x,ncol=r),A3,array(Ybar[,i,],dim=c(d[1],1,d[3])),array(W[,i,],dim=c(d[1],1,d[3])),type),function(x)gradient(A1,matrix(x,ncol=r),A3,2,array(Ybar[,i,],dim=c(d[1],1,d[3])),array(W[,i,],dim=c(d[1],1,d[3])),type),method="BFGS")$par}) ## perhaps faster than the other approach?
 #A2=matrix(unlist(l),nrow=nrow(A2),byrow=T)
 #l=lapply(1:nrow(A1),function(i){optim(c(A1[i,]),function(x)cost(matrix(x,ncol=r),A2,A3,array(Ybar[i,,],dim=c(1,d[2],d[3])),array(W[i,,],dim=c(1,d[2],d[3])),type),function(x)gradient(matrix(x,ncol=r),A2,A3,1,array(Ybar[i,,],dim=c(1,d[2],d[3])),array(W[i,,],dim=c(1,d[2],d[3])),type),method="BFGS")$par}) ### perhaps faster than the other approach?
 #A1=matrix(unlist(l),nrow=nrow(A1),byrow=T)
 #toc()
        obj=c(obj,cost(A1,A2,A3,Ybar,W,type))
        binary_obj=c(binary_obj,binaryloss(Ybar,W,tensorize(A1,A2,A3)))
        iter=iter+1
error=(obj[iter-1]-obj[iter])

 }
 result$binary_obj=binary_obj;
 result$obj=obj;
 result$iter=iter;
 result$error=error;
 result$fitted=tensorize(A1,A2,A3); ## exact low-rank
 return(result)
}

gradient=function(A1,A2,A3,mode,Ybar,W,type=c("logistic","hinge")){
    d=dim(Ybar)
    margin=Ybar*tensorize(A1,A2,A3)
    R=dim(A3)[2]
    
    if(type=="logistic"){
        tem=-W*Ybar*exp(-margin)/(1+exp(-margin))
    }else if(type=="hinge"){
        tem=-W*Ybar*(margin<1)
    }
    tem[is.na(tem)]=0
    
    if(mode==3){
    Grad=matrix(0,nrow=dim(A3)[1],ncol=R)
    for(r in 1:R){
Grad[,r]=ttl(as.tensor(tem),list(as.matrix(t(A1[,r])),as.matrix(t(A2[,r]))),ms=c(1,2))@data
    }}else if(mode==2){
    Grad=matrix(0,nrow=dim(A2)[1],ncol=R)
    for(r in 1:R){
Grad[,r]=ttl(as.tensor(tem),list(as.matrix(t(A1[,r])),as.matrix(t(A3[,r]))),ms=c(1,3))@data
    }}else if(mode==1){
    Grad=matrix(0,nrow=dim(A1)[1],ncol=R)
    for(r in 1:R){
Grad[,r]=ttl(as.tensor(tem),list(as.matrix(t(A2[,r])),as.matrix(t(A3[,r]))),ms=c(2,3))@data
            }}
     return(Grad)
}

cost=function(A1,A2,A3,Ybar,W,type=c("logistic","hinge")){
    return(sum(W*loss(tensorize(A1,A2,A3)*Ybar,type),na.rm=T))
}
loss=function(y,type=c("logistic","hinge")){
    if(type=="hinge") return(ifelse(1-y>0,1-y,0))
    if(type=="logistic") return(log(1+exp(-y)))
}

### ADMM for classification
ADMM=function(Ybar,W,r,rho=0.1,lambda=10^(-3)){
  result=list();
  
  Lambda=array(0,dim=dim(Ybar))
  PQ=0; iter=0; obj =residual=error=max.thresh;
  
  rho_list=NULL;
  while(((iter < 10)|(error > 10^-3))){

    PQ_prev=PQ
    
    ### update B
    if((rho+lambda)!=0){
    res=SVM_offset(Ybar,W,OffsetC=(2*rho*PQ-Lambda)/(2*(lambda+rho)),cost=1/(2*(rho+lambda)))
    }else if((rho+lambda)==0){
         res=SVM_offset(Ybar,W,OffsetC=array(0,dim=dim(W)),cost=max.thresh)
    }
    obj=c(obj,res$hinge) ## minimize objective
    B=res$coef
    
    ## Update PQ
    if(rho==0){PQ=B}
    else{
    if(length(dim(Ybar))>2){
    PQ=cp(as.tensor(B+1/(2*rho)*Lambda),r)
        PQ=PQ$est@data
    }else if(length(dim(Ybar))==2){
        PQ=svd(B+1/(2*rho)*Lambda)
        if(r==1){
            PQ=cbind(PQ$u[,1:r])%*%diag(as.matrix(PQ$d[1:r]))%*%t(cbind(PQ$v[,1:r]))
        }else{
            PQ=PQ$u[,1:r]%*%diag(PQ$d[1:r])%*%t(PQ$v[,1:r])
        }
    }
    }
    
    
    residual=c(residual,sqrt(sum((B-PQ)^2)))
    
    ## update Lambda
    Lambda=Lambda+2*rho*(B-PQ)
    
    ## geometric step size
    if(iter>=10){
        rho=rho*1.1;
        lambda=lambda*1.1;
    }
    

    rho_list=c(rho_list,rho)
    iter=iter+1;

    error=abs(-residual[iter+1]+residual[iter])
    if(iter>=200) break
  }
  
  result$obj=obj[-1];
  result$iter=iter;
  result$error=error;
  result$fitted=PQ; ## exact low-rank
  result$B=B; ## approximate low-rank from SVM
  result$residual=residual[-1];result$rho=rho_list;
  result$alpha=res$res$solution
  return(result)
}

SVM_offset=function(Ybar,W,OffsetC,cost=1){
  n=length(Ybar)
  missing=which(is.na(Ybar)==T)
  nonmissing=setdiff(1:n,missing)
  
  m=length(Ybar[nonmissing])
  dvec = 1-c(Ybar[nonmissing]*OffsetC[nonmissing])
  Dmat = diag(1,m)
  Amat = cbind(c(Ybar[nonmissing]),diag(1,m),-diag(1,m))
  bvec = c(rep(0,1+m),-c(cost*W[nonmissing]))
  res = solve.QP(Dmat,dvec,Amat,bvec,meq =1)
  
  ## calculate coefficient
  coef=OffsetC
  coef[nonmissing]=coef[nonmissing]+res$solution*Ybar[nonmissing]

  return(list("res"=res,"coef"=coef,"hinge"=objective(coef[nonmissing],Y[nonmissing],W[nonmissing])))
}

hinge = function(y) ifelse(1-y>0,1-y,0)

objective=function(yfit,Y,W){
    return(sum(hinge(Y*yfit)*W))
}

likelihood = function(data,theta){
    index=which(is.na(data)==F & is.na(theta)==F)
   return(sqrt(sum((data[index]-theta[index])^2)))
}

################################### normalize each column of X to be unit-1 ###################################
normalize_tensor=function(A,B,C){
    scale_A=apply(A,2,function(x) sqrt(sum(x^2)))
    scale_B=apply(B,2,function(x) sqrt(sum(x^2)))
    scale_C=apply(C,2,function(x) sqrt(sum(x^2)))
    scale=scale_A*scale_B*scale_C
    
    
    ind=sort(scale,decreasing=T,index=T)$ix
    A=normalize(A)[,ind]
    B=normalize(B)[,ind]
    C=normalize(C)[,ind]%*%diag(scale[ind])
    parameter=tensorize(A,B,C)
    Fnorm=sqrt(sum(parameter^2))
    return(list(A,B,C,Fnorm))
}

################################### normalize each column of X to be unit-one. ###################################
normalize=function(X){
    d=dim(X)[2]
    for(i in 1:d){
        X[,i]=X[,i]/sqrt(sum(X[,i]^2))
    }
    return(X)
}

##################### construct CP tensor using factor matrices X, Y, Z ###################################
tensorize=function(X,Y,Z){
    r=dim(X)[2]
    tensor=0
    if(is.matrix(X)==0){
        tensor=X%o%Y%o%Z
        return(tensor)
    }
    
    for(i in 1:r){
        tensor=tensor+X[,i]%o%Y[,i]%o%Z[,i]
    }
    return(tensor)
}

fit_continuous=function(data,r){
    original_data=data
    index=which(is.na(data)==T)
    data[index]=mean(data,na.rm=T)
    if(length(dim(data))>=3){
    decomp=cp(as.tensor(data),r)
    res0=1
    res=0
    thresh=10^(-3)
    error=NULL
    
    while((res0-res)>thresh){
    res0=likelihood(original_data,decomp$est@data)
    decomp=cp(as.tensor(data),r)
    res=likelihood(original_data,decomp$est@data)
    data[index]=decomp$est@data[index]
    error=c(error,res)
    }
    return(decomp$est@data)
    }else if(length(dim(data))==2){
        PQ=svd(data)
        if(r==1){
            decomp=cbind(PQ$u[,1:r])%*%diag(as.matrix(PQ$d[1:r]))%*%t(cbind(PQ$v[,1:r]))
        }else{
            decomp=PQ$u[,1:r]%*%diag(PQ$d[1:r])%*%t(PQ$v[,1:r])
        }
        res0=1
        res=0
        thresh=10^(-3)
        error=NULL
        
        while((res0-res)>thresh){
            res0=likelihood(original_data,decomp)
            if(r==1){
                decomp=cbind(PQ$u[,1:r])%*%diag(as.matrix(PQ$d[1:r]))%*%t(cbind(PQ$v[,1:r]))
            }else{
                decomp=PQ$u[,1:r]%*%diag(PQ$d[1:r])%*%t(PQ$v[,1:r])
            }
            res=likelihood(original_data,decomp)
            data[index]=decomp[index]
            error=c(error,res)
        }
        return(decomp)
}
}

#################### simulation model ####################
graphon_to_tensor=function(a,b,c,type){
    d1=length(a);d2=length(b);d3=length(c)
    M=array(0,dim=c(d1,d2,d3))
    if(type==10){
        for(i in 1:d1){
            for(j in 1:d2){
                for(k in 1:d3){
                    M[i,j,k]=log(1+0.5*max(a[i],b[j],c[k]))
                }
            }
        }
    }
    if(type==9){
        for(i in 1:d1){
            for(j in 1:d2){
                for(k in 1:d3){
                    M[i,j,k]=exp(-0.5*(min(a[i],b[j],c[k]))) ##
                }
            }
        }
    }
    if(type==6){
        for(i in 1:d1){
            for(j in 1:d2){
                for(k in 1:d3){
                    M[i,j,k]=abs(a[i]-b[j]) ## full rank
                }
            }
        }
    }
    if(type==7){
        for(i in 1:d1){
            for(j in 1:d2){
                for(k in 1:d3){
                    M[i,j,k]=1/(1+exp(-max(a[i],b[j],c[k])^2-min(a[i],b[j],c[k])^4))
                }
            }
        }
    }
    if(type==8){
        for(i in 1:d1){
            for(j in 1:d2){
                for(k in 1:d3){
                    M[i,j,k]=exp(-max(a[i],b[j],c[k])^(3/4))
                }
            }
        }
    }
    return(M)
}
appx_rank=function(tensor,thresh=95,step=5){
    size=dim(tensor)
    size=sort(size)
    min=which(sqrt(cumsum(svd(unfold(tensor,1:2,3)@data)$d^2))>thresh*0.01)[1]
    res=test=NULL
    r=min
    while(r<=(size[1]*size[2])){
        r=r+step;
        rank=c(r,cp(tensor,r)$norm_percent)
        res=rbind(res,rank)
        if(rank[2]>thresh) break
    }
    return(res)
}
