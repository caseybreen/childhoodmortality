calculate_component_survival_probabilities <- function(df) {
  #Calculate component survival probabilities
  df$csp <- (1 - df$cdpw)

  #Create calculate the product of all component survival probabilities
  cspw <- data.matrix(df$csp)
  cspw_prod <- colProds(cspw)

  #Subtract product from 1 and multiply by 1,000
  return(abs(cspw_prod - 1) * 1000)

}
