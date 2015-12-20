# Friedman S Test

  # Example 
  Healing_Time <- c(12,14,11,14,15,16,20,21,25,14,15,13,16,18,17,22,22,24,17,18,12,17,19,19,28,24,27,
                    16,20,14,19,22,22,26,30,30,18,21,19,20,24,26,29,29,32)
  Drug_Type <- c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5)
  Ages <- c(1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9)
  friedman.test(Healing_Time~Drug_Type|Ages)
    # Pairwise Comparison
    friedmanmc(Healing_Time,Drug_Type,Ages)
    posthoc.friedman.nemenyi.test(Healing_Time,Drug_Type,Ages)
  
  # Example in notebook
  Rates <- c(70,77,76,80,78,79,61,66,69,63,71,68,72,74,73,75,81,83,82,84,91,96,98,91)
  Raters <- c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4)
  Characteristics <- c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6)
  friedman.test(Rates~Raters|Characteristics)
    # Pairwise Comparison
    friedmanmc(Rates,Raters,Characteristics)
    posthoc.friedman.nemenyi.test(Rates,Raters,Characteristics)
    