compute_cdpw <- function(df, lower_age_segment, upper_age_segment) {

  #Numerator Calculation
  #Select sub-sample where age at death > lower age segment < upper age segment
  T_num <- df[which(df$kidagediedimp >= lower_age_segment &
                    df$kidagediedimp < upper_age_segment), ]

  #Only calculate CDPs if dataset is populated
  if (nrow(T_num) == 0) return(list(cdp = 0, cdp = 0))

  T_num <- compute_coweights(T_num, lower_age_segment, upper_age_segment)

  T_den <- df[which(!df$kidagediedimp < (lower_age_segment + 1) |
                     is.na(df$kidagediedimp)), ]
  T_den <- compute_coweights(T_den, lower_age_segment, upper_age_segment)

  cdpw <- sum(T_num$coweight2, na.rm = TRUE) /
        (sum(T_den$coweight2, na.rm = TRUE))
  cdp <- sum(T_num$coweight, na.rm = TRUE) /
        (sum(T_den$coweight, na.rm = TRUE))

  return(list(cdpw = cdpw, cdp = cdp))
}
