compute_for_all_age_segments <- function(df, age_segments) {

  cdpw_2008 <- numeric(length = length(age_segments))
  names(cdpw_2008) <- names(age_segments)
  cdp_2008 <- numeric(length = length(age_segments))
  names(cdp_2008) <- names(age_segments)

  for (i in seq_along (age_segments)) {
    age_seg <- age_segments[[i]]
    age_seg_label <- names(age_segments)[i]

    lower_age_segment <- age_seg[1]
    upper_age_segment <- age_seg[2]

    .cdpw <- compute_cdpw(df, lower_age_segment, upper_age_segment)

    cdpw_2008[age_seg_label] <- .cdpw$cdpw
    cdp_2008[age_seg_label] <- .cdpw$cdp
  }

  return(list(cdpw_2008 = cdpw_2008, cdp_2008 = cdp_2008))

}
