#######################################################
# Utility Scripts
# Casey Breen
# 10/20/2017
#######################################################


calculate_component_survival_probabilities <- function(df) {
  #Calculate component survival probabilities
  df$csp <- (1 - df$cdpw)

  #Create calculate the product of all component survival probabilities
  cspw <- data.matrix(df$csp)
  cspw_prod <- matrixStats::colProds(cspw)

  #Subtract product from 1 and multiply by 1,000
  return(abs(cspw_prod - 1) * 1000)
}


compute_cdpw <- function(df, lower_age_segment, upper_age_segment) {

  #Numerator Calculation
  #Select sub-sample where age at death > lower age segment < upper age segment
  T_num <- df[which(df$kidagediedimp >= lower_age_segment &
                      df$kidagediedimp < upper_age_segment), ]

  #Only calculate CDPs if dataset is populated
  if (nrow(T_num) == 0) return(list(cdp = 0, cdpw = 0))

  T_num <- compute_coweights(T_num, lower_age_segment, upper_age_segment)

  T_den <- df[which(!df$kidagediedimp < (lower_age_segment + 1) |
                      is.na(df$kidagediedimp)), ]
  T_den <- compute_coweights(T_den, lower_age_segment, upper_age_segment)

  cdpw <- sum(T_num$coweight2, na.rm = TRUE) /
    (sum(T_den$coweight2, na.rm = TRUE))
  cdp <- sum(T_num$coweight, na.rm = TRUE) /
    (sum(T_den$coweight, na.rm = TRUE))

  out <- (list(cdpw = cdpw, cdp = cdp))
  if (!"cdpw" %in% names(out)) print (cdpw)

  return(list(cdpw = cdpw, cdp = cdp))
}


compute_for_all_age_segments <- function(df, age_segments) {

  cdpw_sample <- numeric(length = length(age_segments))
  names(cdpw_sample) <- names(age_segments)
  cdp_sample <- numeric(length = length(age_segments))
  names(cdp_sample) <- names(age_segments)

  for (i in seq_along (age_segments)) {
    age_seg <- age_segments[[i]]
    age_seg_label <- names(age_segments)[i]

    lower_age_segment <- age_seg[1]
    upper_age_segment <- age_seg[2]

    .cdpw <- compute_cdpw(df, lower_age_segment, upper_age_segment)

    cdpw_sample[age_seg_label] <- .cdpw$cdpw
    cdp_sample[age_seg_label] <- .cdpw$cdp
  }

  return(list(cdpw_sample = cdpw_sample, cdp_sample = cdp_sample))

}

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
