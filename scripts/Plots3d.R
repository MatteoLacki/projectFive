require(rgl)
load("data/Data.RData")
#r3dDefaults$windowRect <- c(0,0, 1221, 806)
#r3dDefaults$userMatrix <- rotationMatrix(-pi/2.4,1.4,0.3,0.35)
r3dDefaults$family <- "sans"


ellipsoid3d <- function(rx=1,ry=1,rz=1,n=30,ctr=c(0,0,0),
                        qmesh=FALSE,
                        trans = par3d("userMatrix"),...) {
  if (missing(trans) && !rgl.cur()) trans <- diag(4)
  degvec <- seq(0,2*pi,length=n)
  ecoord2 <- function(p) {
    c(rx*cos(p[1])*sin(p[2]),ry*sin(p[1])*sin(p[2]),rz*cos(p[2])) }
  v <- apply(expand.grid(degvec,degvec),1,ecoord2)
  if (qmesh) v <- rbind(v,rep(1,ncol(v))) ## homogeneous
  e <- expand.grid(1:(n-1),1:n)
  i1 <- apply(e,1,function(z)z[1]+n*(z[2]-1))
  i2 <- i1+1
  i3 <- (i1+n-1) %% n^2 + 1
  i4 <- (i2+n-1) %% n^2 + 1
  i <- rbind(i1,i2,i4,i3)
  if (!qmesh)
    quads3d(v[1,i],v[2,i],v[3,i],...)
  else return(rotate3d(qmesh3d(v,i,material=...),matrix=trans))
}

cigarette <- function(x=0, y=0, h=1, r=NULL, s=c(0.3,0.95)) {
  if (is.null(r)) { r <- 0.1*h }
  theta <- t(matrix(seq(0, 1, len=50), 50, 50))
  z1 <- h*matrix(seq(0, s[1], len=50), 50, 50)
  z2 <- h*matrix(seq(s[1], s[2], len=50), 50, 50)
  z3 <- h*matrix(seq(s[2], 1, len=50), 50, 50)
  a <- r*cos(theta*2*pi)
  b <- r*sin(theta*2*pi)
  persp3d(a+x,b+y,z1,,col="white", add=T,texture="picts/cigTexture1.png")  
  persp3d(a+x,b+y,z2,col="white", add=T, texture="picts/cigTexture2.png")
  persp3d(a+x,b+y,z3,col=rgb(0.5,0.5,0.5), add=T, texture="picts/cigTexture2.png")
  s <- ellipsoid3d(qmesh=TRUE,trans=diag(4))
  plot3d(translate3d(scale3d(s,r,r,0.1*r), x, y, h) ,col="grey",add=TRUE)
}

cigHist <- function(v, v2=NULL, v3 = NULL,norm=F, legend=NULL) {
  d <- !is.na(v)
  v <- v[d]
  m <- max(sapply(levels(v), function(x) { sum(na.omit(v == x)) }))
  hgh <- m
  if (is.null(v2)) { v2 <- vector(length=length(v)) }
  else { v2 <- v2[d] }
  if (!is.null(v3)) { v3 <- v3[d] }
  open3d(zoom=0.72,windowRect=c(0,0, 1221, 806), userMatrix=rotationMatrix(-pi/2.4,1.4,0.3,0.35))
  for (i in 1:length(levels(v))) {
    w <- v == levels(v)[i]
    h1 <- sum(w)
    h2 <- sum(w & v2)
    if (is.null(v3)) { 
      h3 <- 0.5*m 
    } else {
      h3 <- sum(w & v3)
    }
    if (!norm) { hgh <- h1 }
    cigarette(0.25*i*m,0,hgh, 0.075*m, c(h3/h1,(h1-h2)/h1))
    text3d(0.25*i*m,0,-0.075*m,levels(v)[i])
  }
  N <- 10
  ll <-0.25*length(levels(v))*m
  oz=floor(seq(0,m,length.out=N)/1000)*1000
  segments3d(x=rep(c(0,ll),N),y=rep(c(0,0),N),z=c(sapply(oz, function(x) {c(x,x)})))
  text3d(x=rep(0,N),y=rep(0,N),z=oz,oz,adj=1)
  if (!is.null(legend)) {
    text3d(rep(ll+0.22*m,3),rep(0,3),seq(m/2-0.1*m,m/2+0.1*m,length.out=3),legend,adj=c(0,0))
    spheres3d(rep(ll+0.15*m,3),rep(0,3),seq(m/2-0.1*m,m/2+0.1*m,length.out=3),radius=0.04*m,col=c("orange","white",rgb(0.5,0.5,0.5)))
  }
}

cigHist(Data$Employment, Data$Smokes=='yes', Data$Smoker_Group=='never smoked',legend=c("Never smoked","Quit Smoking","Smoker"))
rgl.snapshot("picts/plots/EmploymentHistogram3d.png")
cigHist(Data$Education, Data$Smokes=='yes', Data$Smoker_Group=='never smoked',legend=c("Never smoked","Quit Smoking","Smoker"))
rgl.snapshot("picts/plots/EducationHistogram3d.png")

b_income <- round(seq(0,4000,length.out=20))
bl <- length(b_income)
Cut_Income <- cut(Data$Income, b_income,right=F,labels=F)
b_income2 <- (b_income[-bl]+b_income[-1])/2
Cut_Income <- sapply(Cut_Income, function(x) { b_income2[x] } )
b_age <- round(seq(15,99,length.out=20))
bl <- length(b_age)
Cut_Age <- cut(Data$Age, b_age,right=F,labels=F)
b_age2 <- (b_age[-bl]+b_age[-1])/2
Cut_Age <- sapply(Cut_Age, function(x) { b_age2[x] } )
m <- matrix(nrow=length(b_income2),ncol=length(b_age2))
for (i in 1:length(b_income2)) {
  for (j in 1:length(b_age2)) {
    m[i,j] <- mean(Data$Daily_Smokes[Data$Income>=b_income[i] & Data$Income<b_income[i+1] &
                                  Data$Age >=b_age[j] & Data$Age< b_age[j+1]],na.rm=T)
  }
}
Smoking <- matrix(nrow=length(b_income2),ncol=length(b_age2))
for (i in 2:length(b_income2)-1) {
  for (j in 2:length(b_age2)-1) {
    Smoking[i,j] <- mean(m[i-1:i+1,j-1:j+1],na.rm=T)
  }
}
drawSurface <- function() {
  colorlut <- heat.colors(6)
  col <- colorlut[floor(Smoking)+1]
  open3d(windowRect=c(0,0, 670, 728),userMatrix=rotationMatrix(-pi/3,2,-1,-2))
  persp3d(b_age2,b_income2,Smoking,color=col,box=T,xlab="",ylab="",zlab="",specular="black")
  text3d(20,4000,2.3,"Smoking",adj=0.4)
  text3d(50,4000,5.3,"Age",adj=0.2)
  text3d(90,2200,5.3,"Income",adj=0.5)
  segments3d(rep(c(15,95,95,95),5),rep(c(3950,3950,3950,0),5),sapply(1:5,function(x) { c(x,x,x,x)}),alpha=0.5,add=T)
  segments3d(c(20,20,40,40,60,60,80,80),rep(c(3950,3950),4),rep(c(5,1),4),alpha=0.5,add=T)
  segments3d(rep(95,4), c(0,0,1000,1000,2000,2000,3000,3000),rep(c(5,1),4),alpha=0.5,add=T)
  spheres3d(rep(15,5),rep(3950,5),1:5,col=colorlut, radius=50,add=T)
}
drawSurface()
rgl.snapshot('picts/plots/Surface3d.png')