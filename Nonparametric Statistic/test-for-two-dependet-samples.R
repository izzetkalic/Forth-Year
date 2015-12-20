# Two Relaterd Sample Sign Test
library(BSDA)
  
  # Example 7.1 page 213
  SIGN.test(c(82,81,80,68,70,71,68,67,66,61),c(80,82,80,63,65,66,62,60,59,59),alternative = "greater")
  
  # Example in notebook 02.12.2015
  SIGN.test(c(8,16,18,17,21,19,20,14,15,19),c(9,17,20,20,21,18,21,19,17,21),alternative = "less")

# Two Relaterd Sample Willcoxon Signed Rank Test
  
  # Example 7.5 page 226
  wilcox.test(c(31,34,26,21,18,17,19,15),c(36,32,30,26,28,27,23,20),alternative = "less",paired = TRUE)
  
  # Example in notebook 02.12.2015
  wilcox.test(c(2.7,4.2,3.3,5.3,4.2,3.5,6.5,4.8,3.7,7.1),
              c(4.5,2.6,1.4,2.5,2.5,2.3,3.0,2.6,1.9,0.4),alternative = "greater",paired = TRUE)
  