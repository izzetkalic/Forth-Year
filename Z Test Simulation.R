# N: Monte Carlo sample size
# n: Sample size
# Mu: Given value of population mean
# Type: Willing value of error types(e.g. Type 1, Type 2)
project_8 <- function(N,n,Mu,alfa,type){
  get <- function(){
    
  }
  xi <- vector()
  j=0
  for(j in 1:N){
    i=0
    for(i in 1:n){
      xi[i] <- get()
      Sumx <- Sumx + xi
    }
    Avgx <- Sumx/n
    i=0
    for(i in 1:n){
      Diff <- (xi[i]-Avgx)^2
      SumDiff <- SumDiff + Diff
    }
    Varx <- SumDiff/n
    stdz <- (Avgx-Mu)/(Varx/sqrt(n))
    if(type=1){
      if(stdz > abs(qnorm(alfa/2))){
        wi <- 0
      }else{
        wi <- 1
      }
      
    }else if(type=2){
      if(stdz > abs(qnorm(alfa/2))){
        wi <- 1
      }else{
        wi <- 0
      }
    }
    Sumw <- Sumw + wi
  }
  Avgw <- Sumw/N
}