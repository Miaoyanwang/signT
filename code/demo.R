
source("signT.R")
library(rgl)
library("RColorBrewer")
marker = list(color = brewer.pal(11, "RdBu"))$color
d=9
par(mfrow = c(1, 2))
x <- y <- seq(0, 1, length=d)
z <- outer(x, y, function(x,y)log(1+0.5*pmax(x,y)))

pdf("image3.pdf",width=30,height=10)
set.seed(1)
par(mfrow=c(1,3))
data=z+rnorm(length(z),0,0.013)
persp3D(z = z,col= marker,zlim=c(0,max(data)+0.02),border="gray",phi = 30,theta=10)

res=signT(data,2,Lmin=min(z),Lmax=max(z),H=5,option=2)

image3D(z = 0, colvar = sign(res$fitted[,,2]),zlim=c(-3,20),col=c("gray","white"), border="black",alpha=0.7,theta=10)
image3D(z = 5, colvar = sign(res$fitted[,,4]),add=TRUE,col=c("gray","white"),border="black")
image3D(z = 10, colvar = sign(res$fitted[,,6]),add=TRUE,col=c("gray","white"),border="black")
image3D(z = 15, colvar = sign(res$fitted[,,8]),add=TRUE,col=c("gray","white"),border="black")
image(sign(res$fitted[,,8]))


image3D(z=0.5,colvar=z,col=marker,zlim=c(0,1),phi = 25,theta=30)
dev.off()

image(data,col=marker)


#z[c(3,8),c(4,7)]=NA
pdf("image3.pdf",width=30,height=10)
set.seed=20
par(mfrow=c(1,3))
#image3D(z = 0,colvar = z,col= hcl.colors(d^2, "YlOrRd", rev = TRUE),zlim=c(0,1))

data=z+rnorm(length(z),0,0.01)
persp3D(z = data,col= hcl.colors(d^2, "YlOrRd", rev = TRUE))

res=nonMAR(data,2,Lmin=min(z),Lmax=max(z),H=5,rho=.001)

image3D(z = 0, colvar = sign(res$fitted[,,2]),zlim=c(-1,6.5),col=c("gray","white"),theta=10, border="black",alpha=0.7)
image3D(z = 2, colvar = sign(res$fitted[,,4]),add=TRUE,col=c("gray","white"),border="black")
image3D(z = 4, colvar = sign(res$fitted[,,6]),add=TRUE,col=c("gray","white"),border="black")
image3D(z = 6, colvar = sign(res$fitted[,,8]),add=TRUE,col=c("gray","white"),border="black")

#image(res$est,col= hcl.colors(d^2, "YlOrRd", rev = TRUE))
image3D(z=0,colvar=res$est,col= hcl.colors(d^2, "YlOrRd", rev = TRUE),zlim=c(0,1))
dev.off()
