#KOLMOGROV-SIMIRNOV TEST IN R
# There could be rounding difference between your values and mine.
# But results doesn't change. If it does please let me know (izzetkalic@gmail.com).
# You can use ks.test() function for finding Dc and p-values
# but it doesn't have the columns which we have Zi,Fo,Di values.
# There is two type of algoritm as known case and unknown case.
KNOWNNORMALITY <- function(y,mean,sigma,dtable){
  is.vector(y)
  standartZ <- vector()
  Fo <- vector()
  D1 <- vector()
  x <- sort(y,decreasing = FALSE)
  n <- length(x)
  for(i in 1:n)
  {
    F[i] <- i/n
    standartZ[i] <- (x[i]-mean)/sigma
    Fo[i] <- pnorm(standartZ[i])
    D1[i] <- abs(F[i]-Fo[i])
  }
  table <- t(rbind(x,F,standartZ,Fo,D1))
  table <- round(table,digits=4)
  colnames(table) <- c("Xi"," Step 1: F(Xi)"," Step 2: Zi"," Step 3: F(Zi)"," Step 4: Di")
  cat("\nMean: ",mean)
  cat("\nStandart Deviation: ",sigma)
  cat("\n")
  print(table)
  #Step 5:
  cat("\nDc: ", max(D1))
  #Step 4:
  if(max(D1)>dtable){
    cat("\nSince Dc>Dt, Ho is rejected. The distribution of data hasn't been drawn from normal distribution.")
  }else{
    cat("\nSince Dc<Dt, Ho is not rejected. The distribution of data has been drawn from normal distribution.")
  }
}

UNKNOWNNORMALITY <- function(y,value,dtable,alpha){
  # End of the test we test the sample size wheter is small or not, 
  # to compare the sample size there has been used "value" variable
  is.vector(y)
  standartZ <- vector()
  Fo <- vector()
  D1 <- vector()
  D2 <- vector()
  S <- vector()
  x <- sort(y,decreasing = FALSE)
  n <- length(x)
  #Step 1:
  cat("\nMean: ",mean(x))
  cat("\nStandart Error",sqrt(var(x)))
  i=0
  for(i in 1:n)
  {
    F[i] <- i/n
    standartZ[i] <- (x[i]-mean(x))/sqrt(var(x))
    Fo[i] <- pnorm(standartZ[i])
    D1[i] <- abs(F[i]-Fo[i])
    
  }
  D2[1] <- 0
  for(i in 2:n)
    D2[i] <- abs(F[i-1]-Fo[i])
  dvalues <- rbind(D1,D2)
  table <- t(rbind(x,F,standartZ,Fo,D1,D2))
  table <- round(table,digits=4)
  colnames(table) <- c("Xi"," Step 1: F(Xi)"," Step 3: Zi"," Step 4: F(Zi)"," Step 5: D1i","D2i")
  cat("\n")
  print(table)
  #Step 6:
  cat("\nDc: ",max(dvalues))
  #Step 7:
  if(length(x)<value){
    if(max(dvalues)>dtable){
      cat("\nSince Dc>Dt, Ho is rejected. Sample data has not been drawn from normal")
    }else{
      cat("\nSince Dc<Dt, Ho is not rejected. Sample data has been drawn from normal")
    }
  }else{
    a <- sqrt(length(x))*max(dvalues)
    if(0<=a&0.27>a){
      p <- 1
    }else if(0.27<=a&1>a){
      Q <- exp(-1.23370*(a^(-2)))
      p <- 1-((2.506628/2)*(Q+Q^9+Q^25))
    }else if(1<=a&3.1>a){
      Q <- exp(-2*a^2)
      p <- 2*(Q-Q^4+Q^9-Q^16)
    }else if(3.1<=a){
      p <- 0
    }
    cat("\n",a)
    cat("\n",p)
    if(p<alpha){
      cat("\nSince p<alpha, Ho is rejected. Sample data has not been drawn from normal")
    }else{
      cat("\nSince p>alpha, Ho is not rejected. Sample data has been drawn from normal")
    }
  }
}