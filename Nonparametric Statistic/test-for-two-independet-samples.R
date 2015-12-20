# Two Independent Samples Median Test
  median.test <- function(x,y){
    i=1
    A=0
    B=0
    z <- c(x,y)
    for(i in i:length(x)){
      if(x[i]>median(z)){
        A=A+1
      }
    }
    i=1
    for(i in i:length(y)){
      if(y[i]>median(z)){
        B=B+1
      }
    }
    zh <- ((A/length(x))-(B/length(y)))/sqrt(((A+B)/length(z))*(1-((A+B)/length(z)))*((1/length(x))+(1/length(y))))
    pval <- pnorm(zh)
    cat("Test Statistic Zh:",zh)
    cat("\np.value:",pval)
  }
  # Example 6.1 page 150
  x <- c(18,42,67,18,28,27,11,24,18,16,27,18,7,9,41,67,71,18,24,61,60,18,24,32,31,61,43,30,18,21)
  y <- c(24,21,7,18,16,40,37,60,70,32,30,27,18,29,38,37,40,90,18,71)
  median.test(x,y)
  
  # Example in notebook 01.12.2015
  median.test(c(10,10,10,12,15,17,17,19,20,22,25,26),c(6,7,8,8,12,16,19,19,22))
  
# Two Independent Samples Mann-Whitney Test

  # Example 6.3 page 160
  wilcox.test(c(36,48,61,70,81,67,78,90,70,14),c(28,36,90,17,29,30,21,8,10),alternative = "greater",correct = FALSE)
  
  # Example in notebook 09.12.2015
  wilcox.test(c(53,41,17,45,44,12,49,50),c(91,18,14,21,23,99,16,10),alternative = "greater",correct = FALSE)
  
# Two Independent Sample Kolmogrov-Smirnov Test

  #Example 6.8 page 176
  ks.test(c(40,50,54,60,70,84),c(45,56,59,62,64,75,90))
  
  #Example in notebook 08.12.2015
  ks.test(c(8,9,15,14,12,16,13,18),c(5,4,1,10,11,6,3))
  