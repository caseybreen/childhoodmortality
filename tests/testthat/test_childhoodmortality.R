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
    wealthq = c(1, 2),
    neonatal = c(30.7517397785223, 24.2009365148407),
    SE = c(12.299727083382, 13.4682943141511),
    lower_confidence_interval = c(6.15228561175826, -2.7356521134616),
    upper_confidence_interval = c(55.3511939452864, 51.137525143143)
  )
  expect_equal(as.data.frame(neonat), should_be)
})

test_that("We can calculate the postneonatal rate", {
  postneonat <- childhoodmortality(
    model_ipums_dhs_dataset,
    grouping = "wealthq",
    rate_type = "postneonatal"
  )
  should_be <- data.frame(
    wealthq = c(1, 2),
    postneonatal = c(38.8478038650554, 58.9227696496323),
    SE = c(12.118324788531, 18.8277349032995),
    lower_confidence_interval = c(14.6111542879935, 21.2672998430333),
    upper_confidence_interval = c(63.0844534421173, 96.5782394562312)
  )
  expect_equal(as.data.frame(postneonat), should_be)
})

test_that("We can calculate the infant rate", {
  infant <- childhoodmortality(
    model_ipums_dhs_dataset,
    grouping = "wealthq",
    rate_type = "infant"
  )
  should_be <- data.frame(
    wealthq = c(1L, 2L),
    infant = c(68.4049060881524, 81.6977199569037),
    SE = c(15.467926457812, 21.4301764659382),
    lower_confidence_interval = c(37.4690531725284, 38.8373670250273),
    upper_confidence_interval = c(99.3407590037764, 124.55807288878)
  )
  expect_equal(as.data.frame(infant), should_be)
})

test_that("We can calculate the child rate", {
  child <- childhoodmortality(
    model_ipums_dhs_dataset,
    grouping = "wealthq",
    rate_type = "child"
  )
  should_be <- data.frame(
    wealthq = c(1L, 2L),
    child = c(42.4963743878745, 90.2625432999845),
    SE = c(18.335496568309, 34.1300574503225),
    lower_confidence_interval = c(5.82538125125643, 22.0024283993396),
    upper_confidence_interval = c(79.1673675244925, 158.522658200629)
  )
  expect_equal(as.data.frame(child), should_be)
})

test_that("We can calculate the underfive rate", {
  underfive <- childhoodmortality(
    model_ipums_dhs_dataset,
    grouping = "wealthq",
    rate_type = "underfive"
  )
  should_be <- data.frame(
    wealthq = c(1L, 2L),
    underfive = c(107.994319976937, 164.586019271768),
    SE = c(21.3588493281453, 34.3981955680069),
    lower_confidence_interval = c(65.2766213206468, 95.7896281357543),
    upper_confidence_interval = c(150.712018633228, 233.382410407782)
  )
  expect_equal(as.data.frame(underfive), should_be)
})
