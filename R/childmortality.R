########################################################
# Master Function
# 10/30/2017
# Casey Breen
########################################################

#' Calculates childhood mortality
#'
#' calculates childhood mortality & SE's
#'
#' @param data dataframe object containing DHS variables YEAR, PSU, PERWEIGHT, KIDDOBCMC, INTDATECMC, KIDAGEDIEDIMP, and grouping var.
#'
#' @param grouping This character string gives the variable name of the variable denoting groups.
#'
#' @param rate_type This character string gives the type of mortality rate to be calculated (neonatal, postneonatal, infant, child, under-five)
#'
#' @export
childhoodmortality <- function(data, grouping, rate_type="underfive") {

  if (!rate_type %in% c("neonatal", "postneonatal", "infant", "child", "underfive") stop("Please specify a valid mortality rate type. Valid options are neonatal, postneonatal, infant, child, underfive")

  #generate master table
  mortality_rates <- data.frame(a =c(), rate =c(), IFM = c())

  data<- data[, c("YEAR", "GEOALT_NG2008_2013", "PSU", "PERWEIGHT", "KIDDOBCMC", "INTDATECMC", "KIDAGEDIEDIMP")]

  age_segments <- list(c(0, 1),
                       c(1, 2),
                       c(3, 5),
                       c(6, 11),
                       c(12, 23),
                       c(24, 35),
                       c(36, 47),
                       c(48, 59)
  )

  # What's happening here?
  names(age_segments) <-
    sapply(
      age_segments,
      function(x) {
        paste0("age_", x[1], "_to_", x[2])
      }
    )

  #Select observations for year 2008
  group_levels <- unique(data[[grouping]])
  for (group in group_levels) {
    sub_sample <- data[which(data[[grouping]] == group),]

    # Calculates Component death probabilities for each age interval
    cdpw_sample <- compute_for_all_age_segments(sub_sample, age_segments)

    # Calculates under-Five mortality rate from component death probablities

    # Estimates based on rate type
    if(rate_type == "neonatal") {
      cdpw <- data.frame(cdpw=cdpw_sample$cdpw_sample[1:1])
    } else if (rate_type == "postneonatal") {
      cdpw <- data.frame(cdpw=cdpw_sample$cdpw_sample[2:4])
    } else if (rate_type == "infant") {
      cdpw <- data.frame(cdpw=cdpw_sample$cdpw_sample[1:4])
    } else if (rate_type == "child") {
      cdpw <- data.frame(cdpw=cdpw_sample$cdpw_sample[5:8])
    } else cdpw <- data.frame(cdpw=cdpw_sample$cdpw_sample)


    # Call Utility Functions
    mortality_type_rate <- calculate_component_survival_probabilities(cdpw)

    append <- data.frame(group, mortality_type_rate)
    mortality_rates <- rbind(mortality_rates, append)


  }

  ###############################################################
  # Jack Knife Script for Standard Error Calculation
  # Casey Breen
  # 5/2/2017
  #
  # This portion of the script uses the Jackknife repeated replication
  # method to derive estimates for standard errors for under-five
  # mortality rates. This script replicates methods found in DHS
  # Final Reports for estimating sampling error.
  #################################################################

  #Calculation of repeated replication of parent sample  ommiting obervations in "ith" PSU. Delete-one jackknife method used by DHS.

  SE_rates <- data.frame(group =c(), SE =c())

  #### update
  for (group in group_levels) {
    sub_sample <- data[which(data[[grouping]] == group),]
    #Generate Vector
    jack <- c()

    #Find unique PSUs and create replications
    PSU <- sub_sample$PSU
    for (i in unique(PSU)) {

      sub_sample_delete_i <-  sub_sample[which(!sub_sample$PSU == i),]

      # Iterate over age segments
      cdpw_sample <- compute_for_all_age_segments(sub_sample_delete_i, age_segments)

      # Put all individual CDP values into dataframe

      # Estimates based on rate type
      if(rate_type == "neonatal") {
        cdpw <- data.frame(cdpw=cdpw_sample$cdpw_sample[1:1])
      } else if (rate_type == "postneonatal") {
        cdpw <- data.frame(cdpw=cdpw_sample$cdpw_sample[2:4])
      } else if (rate_type == "infant") {
        cdpw <- data.frame(cdpw=cdpw_sample$cdpw_sample[1:4])
      } else if (rate_type == "child") {
        cdpw <- data.frame(cdpw=cdpw_sample$cdpw_sample[5:8])
      } else cdpw <- data.frame(cdpw=cdpw_sample$cdpw_sample)


      mortality_type_rate <- calculate_component_survival_probabilities(cdpw)

      jack <- append(jack, mortality_type_rate)

    }

    #Jack Knife Calculation using formula found in DHS final reports

    #Set values
    r <- mean(jack)
    k <- length(jack)

    #Import vector as data frame
    jack_frame <- data.frame('r_replication'=jack)
    jack_frame$r_i <- (k*r)-(k-1)*(jack_frame$r_replication)

    #Perform jack knife calculations
    jack_frame$ri_r <- (jack_frame$r_i-r)^2
    diff_sums <- jack_frame$ri_r
    diff_sums2 <- sum(diff_sums)
    SE <- sqrt ((1/(k*(k-1))*diff_sums2))

    append <- data.frame(group, SE)

    SE_rates <- rbind(SE_rates, append)

  }

  #################################################################################################################


  #Merge U5 Mortality Rate with the U5 Standard Errors

  disaggregate_mortality <- merge(mortality_rates, SE_rates, by="group", all=TRUE)


  disaggregate_mortality <- plyr::rename(disaggregate_mortality, c("group" = grouping))
  disaggregate_mortality$Lower_confidence_interval <- disaggregate_mortality$mortality_type_rate-2*disaggregate_mortality$SE
  disaggregate_mortality$Upper_confidence_interval <- disaggregate_mortality$mortality_type_rate+2*disaggregate_mortality$SE

  if(rate_type == "neonatal") {
    disaggregate_mortality <- plyr::rename(disaggregate_mortality, c(mortality_type_rate = "neonatal"))
  } else if (rate_type == "postneonatal") {
    disaggregate_mortality <- plyr::rename(disaggregate_mortality, c(mortality_type_rate = "postneonatal"))
  } else if (rate_type == "infant") {
    disaggregate_mortality <- plyr::rename(disaggregate_mortality, c(mortality_type_rate = "infant"))
  } else if (rate_type == "child") {
    disaggregate_mortality <- plyr::rename(disaggregate_mortality, c(mortality_type_rate = "child"))
  }
  else disaggregate_mortality <- plyr::rename(disaggregate_mortality, c(mortality_type_rate = "underfive"))

  return(disaggregate_mortality)
}

