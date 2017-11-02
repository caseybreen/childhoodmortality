# This data was downoaded from IPUMS-DHS and provides all variables necessary to compute childhood mortality rates.

# Find more harmonized DHS data at https://www.idhsdata.org/idhs/

NG2008_IPUMS_DHS_extract <- read.csv("data_raw/NG2008_IPUMS_DHS_extract.csv")

devtools::use_data(NG2008_IPUMS_DHS_extract, overwrite = TRUE)



