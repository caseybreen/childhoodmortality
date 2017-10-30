compute_coweights <- function(df, lower_age_segment, upper_age_segment) {
  #Set lower and upper limits of age interval
  df$al <- lower_age_segment
  df$au <- upper_age_segment

  #Set lower and upper limits of of time period
  df$tu <- df$intdatecmc
  df$tl <- df$intdatecmc - 60

  #Calculate cohort limits
  df$tlau <- df$tl - df$au
  df$tlal <- df$tl - df$al
  df$tuau <- df$tu - df$au
  df$tual <- df$tu - df$al

  #Create the 3 cohorts by full exposure (1) or partial exposure (0.5)
  df$coweight[df$kiddobcmc >= df$tlau & df$kiddobcmc < df$tlal] <- 0.5
  df$coweight[df$kiddobcmc >= df$tlal & df$kiddobcmc < df$tuau] <- 1
  df$coweight[df$kiddobcmc >= df$tuau & df$kiddobcmc < df$tual] <-
    ifelse(upper_age_segment == 1, 1, 0.5)

  #Weight numerator by person weight
  df$coweight2 <- df$coweight * df$perweight

  return(df)
}
