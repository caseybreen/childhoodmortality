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
#' @param rate_type This character string gives the type of mortality rate to be calculated (neonatal, postneonatal, infant, child, under-five).
#'
#' @param period This numeric value gives the period in years for which the mortality rate will be calculated (default 5 years).
#'
#' @examples
#' data("model_ipums_dhs_dataset")
#' underfive_mortality_rates <- childhoodmortality(
#'  model_ipums_dhs_dataset,
#'  grouping ="wealthq"
#' )
#'
#'
#' @export
childhoodmortality <- function(data, grouping = "sample", rate_type="underfive", period = 5) {

  . <- "null"

  # Convert all input to lower
  names(data) <- tolower(names(data))
  grouping    <- tolower(grouping)
  rate_type    <- tolower(rate_type)
  period <- period

  if (!rate_type %in% c("neonatal", "postneonatal", "infant", "child", "underfive")) stop("Please specify a valid mortality rate type. Valid options are neonatal, postneonatal, infant, child, underfive")




  # Check if inout data comes from IPUMS-DHS or DHS
  if ("kiddobcmc" %in% colnames(data)) {
    data <- dplyr::select(data, grouping , psu, perweight, kiddobcmc, intdatecmc, kidagediedimp)}
  else {
    data <- dplyr::rename(data, sample = v000)
    data <- dplyr::select(data, grouping, psu = v021, perweight = v005, kiddobcmc = b3, intdatecmc = v008, kidagediedimp = b7)}

  if (haven::is.labelled(data[[grouping]])) {
    #cat(paste0("Variable ", grouping, " will be converted from class labelled to factor to preserve labels.\n"))
    data <- dplyr::mutate_at(data, grouping, ~ haven::as_factor(., levels = "both"))
  }


  #generate master table
  group_levels <- unique(data[[grouping]])
  group        <- rep(NA, length((group_levels)))
  rate         <- rep(NA, length((group_levels)))
  data <- dplyr::mutate(data, period = period * 12)
  class(data) <- "data.frame"

  mortality_rates <- cbind(group, rate)

  age_segments <- list(c(0, 0),
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
  data <- suppressWarnings(dplyr::left_join(data, coweights, by = "unique_id", ))
  # data <- tidyr::nest(data, age_segment, coweight, coweight2, numerator, denominator, .key = "coweights")
  cdpw_all <- compute_for_all_age_segments(data, grouping)
  cdpw <- tidyr::unnest(cdpw_all, rate_type)
  cdpw <- dplyr::group_by_at(cdpw, c(grouping, "age_segment", "rate_type"))
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
            calculate_component_survival_probabilities(
              sub_sample_delete_i,
              grouping
            )
          }
        )
        out <- dplyr::group_by(out, rate_type)
        dplyr::mutate(
          out,
          r = mean(mortality_rate),
          k = n()
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
  jack <- dplyr::group_by_at(jack, c(grouping, "rate_type"))
  #Perform jack knife calculations
  SE_rates <- dplyr::summarise(
    jack,
    diff_sums = sum(r_i_r),
    SE = sqrt((1 / (k[1] * (k[1] - 1)) * diff_sums))
  )
  SE_rates <- dplyr::select_at(SE_rates, c(grouping, "SE", "rate_type"))


  #################################################################################################################

  #Merge U5 Mortality Rate with the U5 Standard Errors
  attr(mortality_rates[[grouping]], "label") <- NULL
  attr(mortality_rates[[grouping]], "var_desc") <- NULL
  disaggregate_mortality <- suppressWarnings(dplyr::left_join(mortality_rates, SE_rates, by = c(grouping, "rate_type")))


  # disaggregate_mortality <- plyr::rename(disaggregate_mortality, c("group" = grouping))
  disaggregate_mortality <- dplyr::mutate(
    disaggregate_mortality,
    lower_confidence_interval = mortality_rate - 2 * SE,
    upper_confidence_interval = mortality_rate + 2 * SE
  )

  rate_type_filter <- rate_type
  if (rate_type != "all") {
    disaggregate_mortality <- dplyr::filter(disaggregate_mortality, rate_type == rate_type_filter)
  }

  disaggregate_mortality <- dplyr::arrange(disaggregate_mortality, factor(rate_type, levels = c("neonatal", "postneonatal", "infant", "child", "underfive")))

  return(as.data.frame(disaggregate_mortality))

}

globalVariables(".")
