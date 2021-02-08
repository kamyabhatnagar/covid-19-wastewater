


###########################################
# Read in the generic mappers
# this has several base functions
source("src/mappers/generic_mapper.r")



x <- wbe_mapper_generator$new(nm ="NML",
                              map_fn_pattern = "^Pilot mastersheet.*\\s+-\\s+.*\\.xlsx",
                                  tbln_nm_wide2long = "DataRethreshold",clean_names_before_rename = TRUE

    )





dfs <- x$read_data()
lapply(dfs,function(x)(clean_names(x))) %>% extract2("DataRethreshold") %>% colnames()
x$prep_data()

x$map_data()
dfs$DataRethreshold
dfs <- x$map_data()

x$fns_list
x$most_recent_fn

, x = ., invert = T, value = T)
x$fns_list
.*(\\.xlsx)$
#######################################################
#'
#' Map a dataframe from NML into sample and measurement tables of the database
#' returns a list of dataframes
#'
wbe_mapper_nml_sample_measure <- function(df_raw){
        df_raw %>%
        clean_names() %>%
        rename(Sample.ID := sample_id,
               Sample.Site.ID := location,
               Sample.dateTime := date_sampled,
               Measurement.analysisDate :=  date_received,
               Measurement.temp_c_1 :=  arrival_temp,
               Measurement.passFreezeSpotSeen_boolean_1 :=  spot_see_freeze,
               Measurement.passSpotSeenWarm_boolean_1 :=   spot_see_warm,
               Measurement.notes := sample_condition,
               Measurement._analysisDate := q_pcr_date,
               Measurement.covidN1_ml_1 := n1_1_cp_m_l,
               Measurement.covidN1_ml_2 := n1_2_cp_m_l,
               Measurement.covidN2_ml_1 := n2_1_cp_m_l,
               Measurement.covidN2_ml_2 := n2_2_cp_m_l,
               Measurement.PPMV_ml_1 := pmmv_1_cp_m_l,
               Measurement.PPMV_ml_2 := pmmv_2_cp_m_l
               ) %>%
        select(-any_of(c("week",
                         "n1_1", "n1_2", "n2_1", "n2_2",
                         "mhv_1", "mhv_2",
                         "pmmv_1", "pmmv_2", "pmmv1_10_1", "pmmv1_10_2",
                         "avg_n1_cp_m_l", "avg_n2_cp_m_l", "avg_pmmv_cp_m_l"
                         ))) %>%
        mutate(Sample.dateTime = as.POSIXct(Sample.dateTime),
               Measurement.analysisDate = as.Date(Measurement.analysisDate),
               Measurement._analysisDate = as.Date(Measurement._analysisDate),
               Measurement.temp_c_1 = as.double(Measurement.temp_c_1),
               Measurement.passFreezeSpotSeen_boolean_1 = as.logical(Measurement.passFreezeSpotSeen_boolean_1),
               Measurement.passSpotSeenWarm_boolean_1 = as.logical(Measurement.passSpotSeenWarm_boolean_1),
               Measurement._analysisDate =as.Date(Measurement._analysisDate)
               ) %>%
    wbe_sample_measurments_wide_2_long()
}

#######################################################
#'
#' Map a dataframe from NML into site table of the database
#' returns a dataframe
#'
wbe_mapper_nml_site <- function(df, nm){
    df %>%
        clean_names() %>%
        rename(ID := pilot_code_names,
               name := x2) %>%
        mutate(repoter.ID = nm) %>%
        filter(!is.na(ID))
}

###########################################
#'
#' Map NML submitted Data to the database.
#'
#' @param dfs takes a list of dataframes
#'
wbe_mapper_nml <- function(x){
    dfs <- read_data(x)
    dfs <- wbe_mapper_nml_sample_measure(dfs$DataRethreshold) %>%
           c(dfs)

    dfs[["Site"]] <- wbe_mapper_nml_site(dfs$PilotCodes, x$nm)
    dfs[["Reporter"]] <- tibble(ID = x$nm, contactName = "Chand")

    dfs[intersect(wbe_tbl_list(),names(dfs))]
}

##################################
#'
#' create a new NML Mapper object
#'
x <- new_wbe_mapper(nm = "NML",
                     map_fn_pattern = "^Pilot mastersheet\\s+-\\s+.*\\.xlsx",
                     map_data = wbe_mapper_nml)


#nml_mapper$list_files
#list_files(nml_mapper)
#most_recent_fn(nml_mapper)
#read_data(nml_mapper)

dfs <- map_data(x)
wbe_append_from_dfs(dfs)
# names(dfs)
# wbe_tbl("Site")
# wbe_tbl("Measurement")
# wbe_tbl("Reporter")
# wbe_tbl("Sample")
