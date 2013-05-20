require(rgl)

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
  persp3d(a+x,b+y,z3,col="darkgrey", add=T, texture="picts/cigTexture2.png")
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
  open3d()
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
  segments3d(x=rep(c(0,ll),N),y=rep(c(0,0),N),z=c(sapply(oz, function(x) {c(x,x)})) )
  text3d(x=rep(0,N),y=rep(0,N),z=oz,oz,adj=1)
  if (!is.null(legend)) {
    text3d(rep(ll+0.22*m,3),rep(0,3),seq(m/2-0.1*m,m/2+0.1*m,length.out=3),legend,adj=c(0,0))
    spheres3d(rep(ll+0.15*m,3),rep(0,3),seq(m/2-0.1*m,m/2+0.1*m,length.out=3),radius=0.04*m,col=c("orange","white","darkgrey"))
  }
}

# cigHist(Data$Employment, Data$Smokes=='yes', Data$Smoker_Group=='never smoked',legend=c("Never smoked","Quit Smoking","Smoker"))
# cigHist(Data$Education, Data$Smokes=='yes', Data$Smoker_Group=='never smoked')
# cigHist(Data$Education, Data$Smokes=='yes', Data$Smoker_Group=='never smoked', norm=T)
# cigHist(Data$Employment, Data$Smokes=='yes', Data$Smoker_Group=='never smoked')
# cigHist(Data$Employment, Data$Smokes=='yes', Data$Smoker_Group=='never smoked', norm=T)