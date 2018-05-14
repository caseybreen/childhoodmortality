context("Computing neonatal, postneonatal, infant, child, or underfive
        mortality rates")

data("model_ipums_dhs_dataset")

test_that("We can calculate the neonatal rate", {
  neonat <- childhoodmortality(
    model_ipums_dhs_dataset,
    grouping = "wealthq",
    rate_type = "neonatal"
  )
  should_be <- data.frame(
    wealthq = c(1L, 2L),
    neonatal = c(31.1427321440335, 24.3486921534002),
    SE = c(12.7836577602188, 13.7750866051571),
    lower_confidence_interval = c(5.57541662359598, -3.201481056914),
    upper_confidence_interval = c(56.710047664471, 51.8988653637144)
  )
  expect_equivalent(neonat, should_be)
})

test_that("We can calculate the postneonatal rate", {
  postneonat <- childhoodmortality(
    model_ipums_dhs_dataset,
    grouping = "wealthq",
    rate_type = "postneonatal"
  )
  should_be <- data.frame(
    wealthq = c(1L, 2L),
    postneonatal = c(32.7166400925034, 44.448588812974),
    SE = c(9.95625699915569, 16.5902419616456),
    lower_confidence_interval = c(12.804126094192, 11.2681048896829),
    upper_confidence_interval = c(52.6291540908148, 77.6290727362651)
  )
  expect_equivalent(postneonat, should_be)
})

test_that("We can calculate the infant rate", {
  infant <- childhoodmortality(
    model_ipums_dhs_dataset,
    grouping = "wealthq",
    rate_type = "infant"
  )
  should_be <- data.frame(
    wealthq = c(1L, 2L),
    infant = c(62.8404866774834, 67.7150159607139),
    SE = c(14.5961688937916, 19.9707261153864),
    lower_confidence_interval = c(33.6481488899002, 27.7735637299412),
    upper_confidence_interval = c(92.0328244650667, 107.656468191487)
  )
  expect_equivalent(infant, should_be)
})

test_that("We can calculate the child rate", {
  child <- childhoodmortality(
    model_ipums_dhs_dataset,
    grouping = "wealthq",
    rate_type = "child"
  )
  should_be <- data.frame(
    wealthq = c(1L, 2L),
    child = c(42.343099105013, 70.5230459237924),
    SE = c(18.4045992737969, 30.8881871605915),
    lower_confidence_interval = c(5.53390055741924, 8.7466716026095),
    upper_confidence_interval = c(79.1522976526068, 132.299420244975)
  )
  expect_equivalent(child, should_be)
})

test_that("We can calculate the underfive rate", {
  underfive <- childhoodmortality(
    model_ipums_dhs_dataset,
    grouping = "wealthq",
    rate_type = "underfive"
  )
  should_be <- data.frame(
    wealthq = c(1L, 2L),
    underfive = c(102.522724827305, 133.462592704179),
    SE = c(21.1758368149082, 32.6586561301917),
    lower_confidence_interval = c(60.1710511974882, 68.1452804437953),
    upper_confidence_interval = c(144.874398457121, 198.779904964562)
  )
  expect_equivalent(underfive, should_be)
})
