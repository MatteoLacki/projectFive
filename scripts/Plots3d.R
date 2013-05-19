require(rgl)

cigarette <- function(x=0, y=0, h=1) {
  r <- 0.1
  theta <- t(matrix(seq(0, 1, len=50), 50, 50))
  z1 <- h*matrix(seq(0, 0.2, len=50), 50, 50)
  z2 <- h*matrix(seq(0.2, 0.95, len=50), 50, 50)
  z3 <- h*matrix(seq(0.95, 1, len=50), 50, 50)
  a <- r*cos(theta*2*pi)
  b <- r*sin(theta*2*pi)
  persp3d(a+x,b+y,z1,col="yellow", add=T)  
  persp3d(a+x,b+y,z2,col="white", add=T)
  persp3d(a+x,b+y,z3,col="darkgrey", add=T)
}