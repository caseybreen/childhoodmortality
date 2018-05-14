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
#' @examples
#' data("model_ipums_dhs_dataset")
#' underfive_mortality_rates <- childhoodmortality(
#'  model_ipums_dhs_dataset,
#'  grouping ="wealthq",
#'  rate_type = "underfive"
#' )
#'
#' @export
childhoodmortality <- function(data, grouping = "year", rate_type="underfive") {

  # Convert all input to lower
  names(data) <- tolower(names(data))
  grouping    <- tolower(grouping)
  rate_type    <- tolower(rate_type)

  if (!rate_type %in% c("neonatal", "postneonatal", "infant", "child", "underfive")) stop("Please specify a valid mortality rate type. Valid options are neonatal, postneonatal, infant, child, underfive")

  if (haven::is.labelled(data[[grouping]])) {
    cat(paste0("Variable ", grouping, " will be converted from class labelled to factor to preserve labels.\n"))
    data <- dplyr::mutate_at(data, grouping, ~ haven::as_factor(., levels = "both"))
  }

  #generate master table
  group_levels <- unique(data[[grouping]])
  group        <- rep(NA, length((group_levels)))
  rate         <- rep(NA, length((group_levels)))

  mortality_rates <- cbind(group, rate)
  varnames <- unique(c("year", grouping, "psu", "perweight", "kiddobcmc", "intdatecmc", "kidagediedimp"))
  data <- data[,varnames]
  class(data) <- "data.frame"

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
        paste0(x[1], "-", x[2])
      }
    )

  data <- dplyr::mutate(data, unique_id = 1:nrow(data))
  coweights <- purrr::map_dfr(
    age_segments,
    ~ compute_coweights(data, .[1], .[2])
  )
  data <- dplyr::left_join(data, coweights, by = "unique_id")
  # data <- tidyr::nest(data, age_segment, coweight, coweight2, numerator, denominator, .key = "coweights")
  cdpw_all <- compute_for_all_age_segments(data, grouping)
  cdpw <- dplyr::filter_at(cdpw_all, rate_type, dplyr::all_vars(.))
  cdpw <- dplyr::group_by_at(cdpw, c(grouping, "age_segment"))
  mortality_rates <- calculate_component_survival_probabilities(cdpw, grouping)

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

  grouped_data <- dplyr::group_by_at(data, c(grouping, "psu"))
  grouped_data <- dplyr::summarize(grouped_data)
  p            <- dplyr::progress_estimated(nrow(grouped_data))


  jack <- suppressWarnings(
    purrr::map_dfr(
      group_levels,
      function(g) {
        sub_sample <- dplyr::filter_at(cdpw, grouping, dplyr::all_vars(. == g))
        #Generate Vector
        psu <- unique(sub_sample$psu)

        out <- purrr::map_dfr(
          psu,
          function(i) {
            sub_sample_delete_i <- sub_sample[which(!sub_sample$psu == i),]
            p$tick()
            p$print()
            out <- calculate_component_survival_probabilities(
              sub_sample_delete_i,
              grouping
            )
          }
        )
        dplyr::mutate(
          out,
          r = mean(mortality_rate),
          k = nrow(out)
        )
      }
    )
  )


  #Jack Knife Calculation using formula found in DHS final reports

  #Set values
  jack <- dplyr::mutate(
    jack,
    r_i = (k * r) - (k - 1) * mortality_rate,
    r_i_r = (r_i - r)^2
  )
  jack <- dplyr::group_by_at(jack, grouping)
  #Perform jack knife calculations
  SE_rates <- dplyr::summarise(
    jack,
    diff_sums = sum(r_i_r),
    SE = sqrt((1 / (k[1] * (k[1] - 1)) * diff_sums))
  )
  SE_rates <- dplyr::select_at(SE_rates, c(grouping, "SE"))


  #################################################################################################################

  #Merge U5 Mortality Rate with the U5 Standard Errors

  disaggregate_mortality <- dplyr::left_join(mortality_rates, SE_rates, by = grouping)


  # disaggregate_mortality <- plyr::rename(disaggregate_mortality, c("group" = grouping))
  disaggregate_mortality <- dplyr::mutate(
    disaggregate_mortality,
    lower_confidence_interval = mortality_rate - 2 * SE,
    upper_confidence_interval = mortality_rate + 2 * SE
  )

  disaggregate_mortality <- dplyr::rename_at(
    disaggregate_mortality,
    "mortality_rate",
    ~ rate_type
  )

  return(disaggregate_mortality)

}

