# This data was downoaded from IPUMS-DHS and provides all variables necessary to compute childhood mortality rates.

# Find more harmonized DHS data at https://www.idhsdata.org/idhs/

model_ipums_dhs_dataset <- read.csv("data-raw/model_ipums_dhs_dataset.csv", stringsAsFactors = FALSE)

devtools::use_data(model_ipums_dhs_dataset, overwrite = TRUE)

