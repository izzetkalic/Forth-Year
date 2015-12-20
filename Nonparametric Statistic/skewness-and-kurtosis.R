#SKEWNESS AND KURTOSIS FOR SAMPLE NORMALITY
# There could be rounding difference between your values and mine.
# But results doesn't change. If it does please let me know (izzetkalic@gmail.com).
install.packages("agricolae")
library(agricolae)
SKEWNESSANDKURTOSIS <- function(x,z){
  is.vector(x)
  #STEP 1:
  print('Step 1:')
  cat("Sample Mean =",mean(x))
  cat("\nSample Standart Error =", sqrt(var(x)))
  #STEP 2:
  cat("\n")
  print("Step2:")
  cat("Kurtosis Coefficient =",kurtosis(x))
  cat("\nSkewness Coefficient =",skewness(x))
  #STEP 3:
  cat("\n")
  print('Step 3:')
  n <- length(x)
  SEKurttosis <- sqrt((24*n*(n-1)^2)/((n-2)*(n-3)*(n+5)*(n+3)))
  SESkewness <- sqrt((6*n*(n-1))/((n-2)*(n+1)*(n+3)))
  cat("Standart Error of Kurtosis =",SEKurttosis)
  cat("\nStandart Error of Skewness =",SESkewness)
  #STEP 4:
  cat("\n")
  print('Step 4:')
  StandartKurtosis <- (kurtosis((x)-0))/(SEKurttosis)
  StandartSkewness <- (skewness(x)-0)/(SESkewness)
  cat("Standardized Kurtosis =",StandartKurtosis)
  cat("\nStandardized Skewness =",StandartSkewness)
  #Step 5:
  cat("\n")
  print('Step 5:')
  if(-z<StandartKurtosis&StandartKurtosis<z&-z<StandartSkewness&StandartSkewness<z){
    cat("Decision: Since z-scores of skewness and kurtosis coefficients fell down in range of standart z-scores 
        distribution of data has been drawn from normal distribution")
  }else{
    cat("Decision: Since z-scores of skewness and kurtosis coefficients fell down in range of standart z-scores 
        distribution of data hasn't been drawn from normal distribution")
  }
  }