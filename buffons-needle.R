#BUFFON'S NEEDLE
buffon <- function(n,k,d){
  # k: the length of needle
  # d: distance between lines
  # n: number of repeating
  # x: condition
  # y: distance of needle midpoint to nearest line
  # teta: angle of needle with nearest line
  if(k>d){
    print("There is a problem. k should be smaller than d")
  }else{
    i=1
    sumx=0
    Estimation <- vector()
    for(i in 1:n)
    {
      randomy <- runif(1,0,d)
      randomteta <- runif(1,0,pi)
      y <- d*randomy
      teta <- pi*randomteta
      if(y<((k/2)*sin(teta))||d-y<((k/2)*sin(teta))){
        x <- 1
      }else
      {
        x <- 0
      }
      sumx <- sumx + x
      Estimation[i] <- sumx/i
    }
    plot(Estimation[])
    print(Estimation[n])
    pi <- (2*k*n)/(sumx*d)
    cat("The estimation of pi:",pi)
  }
}