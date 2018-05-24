#######################################################
# Utility Scripts
# Casey Breen
# 10/20/2017
#######################################################


calculate_component_survival_probabilities <- function(df, grouping) {
  #Calculate component death probabilities
  df <- dplyr::summarise(
    df,
    cdpw = sum(cdpw_num) / sum(cdpw_denom)
  )

  #Calculate component survival probabilities
  df <- dplyr::mutate(df, csp = 1 - cdpw)
  df <- dplyr::group_by_at(df, grouping)
  dplyr::summarise(
    df,
    mortality_rate = abs(prod(csp) - 1) * 1000
  )
}


compute_for_all_age_segments <- function(df, grouping) {

  df <- dplyr::group_by_at(df, c(grouping, "age_segment", "psu"))
  out <- dplyr::summarise(
    df,
    cdpw_num = sum(coweight_num_weight[numerator], na.rm = TRUE),
    cdpw_denom = sum(coweight_den_weight[denominator], na.rm = TRUE),
    cdp_num = sum(coweight_num[numerator], na.rm = TRUE),
    cdp_denom = sum(coweight_den[denominator], na.rm = TRUE)
  )
  dplyr::mutate(
    out,
    neonatal = age_segment == "0-0",
    postneonatal = age_segment %in% c("1-2", "3-5", "6-11"),
    infant = age_segment %in% c("0-0", "1-2", "3-5", "6-11"),
    child = age_segment %in% c("12-23", "24-35", "36-47", "48-59"),
    underfive = TRUE
  )
}

compute_coweights <- function(df, lower_age_segment, upper_age_segment) {
  #Set lower and upper limits of age interval
  df$al <- lower_age_segment
  df$au <- upper_age_segment

  #Set lower and upper limits of of time period
  df$tu <- df$intdatecmc
  df$tl <- df$intdatecmc - df$period

  #Calculate cohort limits
  df$tlau <- df$tl - df$au
  df$tlal <- df$tl - df$al
  df$tuau <- df$tu - df$au
  df$tual <- df$tu - df$al

  #Create the 3 cohorts by full exposure (1) or partial exposure (0.5)
  df$coweight_num[df$kiddobcmc >= df$tlau - 1 & df$kiddobcmc < df$tlal] <- 0.5
  df$coweight_num[df$kiddobcmc >= df$tlal & df$kiddobcmc < df$tuau-1] <- 1
  df$coweight_num[df$kiddobcmc >= df$tuau - 1 & df$kiddobcmc < df$tual] <-
    ifelse(upper_age_segment == 0, 1, 1)


  df$coweight_den[df$kiddobcmc >= df$tlau - 1 & df$kiddobcmc < df$tlal] <- 0.5
  df$coweight_den[df$kiddobcmc >= df$tlal & df$kiddobcmc < df$tuau-1] <- 1
  df$coweight_den[df$kiddobcmc >= df$tuau - 1 & df$kiddobcmc < df$tual] <- 0.5

  #Weight numerator by person weight
  df$coweight_num_weight <- df$coweight_num * (df$perweight*1000000)
  df$coweight_den_weight <- df$coweight_den * (df$perweight*1000000)


  df$numerator <- !is.na(df$kidagediedimp) & df$kidagediedimp >= lower_age_segment &
    df$kidagediedimp <= upper_age_segment

  df$denominator <- is.na(df$kidagediedimp) | df$kidagediedimp >= lower_age_segment


  df$age_segment <- paste0(lower_age_segment, "-", upper_age_segment)

  df[ , c("unique_id", "age_segment", "coweight_num", "coweight_den",
          "coweight_num_weight", "coweight_den_weight", "numerator",
          "denominator")]
}
