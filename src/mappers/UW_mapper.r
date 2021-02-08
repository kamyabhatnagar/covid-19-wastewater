


###########################################
# Read in the generic mappers
# this has several base functions
source("src/mappers/generic_mapper.r")

#wbe_delete_whole_db()

x <- wbe_mapper_generator$new(nm ="UW",
                              #map_fn_pattern = "^u_w_wwtp_data_V2\\.xlsx$",
                              tbln_nm_wide2long = "SampleMeasurementWide"
                              )

dfs <- x$map_data()
x$append_mapped_data()


