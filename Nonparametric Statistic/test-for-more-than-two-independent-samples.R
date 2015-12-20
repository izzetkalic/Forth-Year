# Kruskal Wallis Test
  
  # Library for Post-Hoc Kruskal Nemenyi Test
  install.packages("PMCMR")
  library(PMCMR)
  # Library for kruskalmc() function
  install.packages("pgirmess")
  library(pgirmess)
  
  # Example 9.4 page 291
  Sunflower_A <- c(200,160,140,145,180)
  Sunflower_B <- c(182,174,142,150,166,168,171)
  Sunflower_C <- c(210,195,220,215,204,193)
  Sunflower_D <- c(219,217,230,250,202,240,245)
  kruskal.test(list(Sunflower_A,Sunflower_B,Sunflower_C,Sunflower_D))
    # Pairwise Comparison
    Yield <- c(200,160,140,145,180,182,174,142,150,166,168,171,210,195,220,215,204,193,219,217,230,250,202,240,245)
    Sunflowers<-c("A","A","A","A","A","B","B","B","B","B","B","B","C","C","C","C","C","C","D","D","D","D","D","D","D")
    kruskalmc(Yield~Sunflowers)
    
    Sunflowerss <- c(1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,4)
    posthoc.kruskal.nemenyi.test(Yield~Sunflowerss)
    
  # Example in notebook 09.12.2015
  MethodA <- c(90,87,89,90)
  MethodB <- c(75,88,97,99,78,85)
  MethodC <- c(72,76,77,79,70)
  kruskal.test(list(MethodA,MethodB,MethodC))
    # Pairwise Comparison
    Spelling_Grades <- c(90,87,89,90,75,88,97,99,78,85,72,76,77,79,70)
    Methods <- c("A","A","A","A","B","B","B","B","B","B","C","C","C","C","C")
    kruskalmc(Spelling_Grades~Methods)
    
    Methodss <- c(1,1,1,1,2,2,2,2,2,2,3,3,3,3,3)
    posthoc.kruskal.nemenyi.test(Spelling_Grades~Methodss) 
  
  # Example Large Sample Approximation in notebook 09.12.2015
  City1 <- c(81,92,123,82,108,122)
  City2 <- c(119,116,101,103,113,84)
  City3 <- c(70,56,55,73,68,69)
  City4 <- c(61,54,59,67,80,71)
  kruskal.test(list(City1,City2,City3,City4))
    # Pairwise Comparison
    Amount_of_rain <- c(81,93,123,82,108,122,119,116,101,103,113,81,70,56,55,73,68,69,61,54,59,67,80,71)
    Cities <- c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4)
    kruskalmc(Amount_of_rain~Cities)
    posthoc.kruskal.nemenyi.test(Amount_of_rain~Cities)