


source("src/wbe_db_func.r")

library(tidyverse)
library(magrittr)
library(roxygen2)
#wbe_delete_whole_db()



#################################
#'
#' wbe_mapper_generator  creates a refClass
#' with wbe_mapper_generator$new(nm, ...)
#' basically you need this class to make mappers for different data sets
#'
wbe_mapper_generator <-
    setRefClass(Class = "wbe_mapper",
                fields = list(
                    nm = "character" ,
                    decreasing = "logical" ,
                    full.names = "logical",
                    map_fn_pattern = "character",
                    dfs_raw = class(list()),
                    dfs_trans = class(list()),
                    data_dir = "character",
                    fns_list = "character",
                    most_recent_fn = "character",
                    renamings = class(list()),
                    tbln_nm_wide2long = "character",
                    conn = "SQLiteConnection",
                    clean_names_before_rename = "logical"
                ),
                methods = list(
                    initialize = function(nm,
                                          decreasing = TRUE,
                                          full.names = FALSE,
                                          a_data_dir = file.path("data", nm),
                                          map_fn_pattern = "^[^~]*.\\.xlsx$",
                                          tbln_nm_wide2long = "SampleMeasurementWide",
                                          clean_names_before_rename = FALSE,
                                          conn = wbe_conn()){
                        nm <<- nm
                        decreasing <<- decreasing
                        full.names <<- full.names
                        map_fn_pattern <<- map_fn_pattern
                        data_dir <<- a_data_dir
                        tbln_nm_wide2long <<- tbln_nm_wide2long
                        clean_names_before_rename <<- clean_names_before_rename
                        conn <<- conn
                        print(glue("map_fn_pattern = {map_fn_pattern}"))

                        fns_list <<- a_data_dir %>%
                            list.files(full.names = full.names) %>%
                            grep(x = ., pattern = map_fn_pattern, ignore.case = TRUE, value = TRUE, perl = TRUE) %>%
                            grep(x = ., pattern = glue("{nm}_rename.xlsx"), ignore.case = TRUE, value = TRUE, perl = TRUE, invert = TRUE) %>%
                            sort(decreasing = decreasing)


                            #grep(x = ., pattern = map_fn_pattern, ignore.case = TRUE, value = TRUE, perl = TRUE) %>%
                            #grep(pattern = glue("{nm}_rename.xlsx"), x = ., ignore.case = TRUE, value = TRUE, invert = TRUE) %>%
                            #sort(decreasing = decreasing)

                        most_recent_fn <<-
                            fns_list %>%
                            extract2(1)





                    },
                    show = function(){
                        cat(glue("mapper for {nm}"))
                    }
                )
            )
x$data_dir

#################################
#'
#' x$read_data()
#'
#' reads in data for this mapper
#' then sets df_raw, and returns df_raw
#'
wbe_mapper_generator$methods(read_data = function(
                                    data_dir = .self$data_dir,
                                    most_recent_fn= .self$most_recent_fn
                                ){

    dfs_raw <<- file.path(data_dir, most_recent_fn) %>% wbe_xlsx_to_dfs
    dfs_raw
})

#################################
#'
#' x$read_renaming()
#'
#' reads the file that contains all the renamings we need
#'
wbe_mapper_generator$methods(read_renaming = function(
                                                    dir = .self$data_dir,
                                                    fn = file.path(dir, paste0(.self$nm,"_rename.xlsx"))
                                                    ){
    #dir = x$data_dir
    #fn = file.path(dir, paste0(x$nm,"_rename.xlsx"))
    if (! file.exists(fn) ){
        warning(glue("no renaming file found for {nm}, \n`{fn}`"))
        renamings <<-
            list(
                Tbls = tibble(tbl_nm_orig = character(), tbl_nm_dest = character()),
                Cols = tibble(tbl_nm_orig = character(), col_nm_orig = character(), col_nm_dest = character()),
                Vals = tibble(tbl_nm_orig = character(), col_nm_orig = character(), val_nm_orig = character(), val_nm_dest = character())
            )
    }else{
        renamings <<- wbe_xlsx_to_dfs(fn)
    }
    renamings
})

#################################
#'
#' x$prep_data()
#' does some simple prep of the data including renaming, normally a map_data function in derived class will call this function then do specific mappings
#'
#'
wbe_mapper_generator$methods(prep_data = function(
    clean_names_before_rename = .self$clean_names_before_rename
){
    #dfs_raw <- x$read_data()
    #renamings <- x$read_renaming()
    if(length(dfs_raw) == 0){
        read_data()
    }
    if(is_null(renamings)){
        read_renaming()
    }

    dfs_prep <- dfs_raw
    if(nrow(renamings) == 0){
        return(dfs_prep)
    }

    if(clean_names_before_rename){
        dfs_prep <- lapply(dfs_prep,function(x)(clean_names(x)))
    }


    for(i in 1:nrow(renamings)){
        renaming <- renamings %>% slice(i) %>% as.list()


        if (is.na(renaming$col_nm_dest)){
            #print(glue("skipping {i}"))
            next
        }

        #print(glue("not skipping {i}"))
        dfs_prep[[renaming$tbl_nm_dest]] <-
            dfs_prep[[renaming$tbl_nm_orig]] %>%
            rename(!!sym(renaming$col_nm_dest) := !!sym(renaming$col_nm_orig))
    }

    dfs_prep
})


#################################
#'
#' x$map_data()
#' Map data from the read in format to the database format
#' often a specific mapper class will overide this function, with its own
#'
wbe_mapper_generator$methods(map_data = function(
    tbln_nm_wide2long = .self$tbln_nm_wide2long
    ){
    #dfs_prep <- x$prep_data()
    #tbln_nm_wide2long <- x$tbln_nm_wide2long
    dfs_prep <- prep_data()
    smpl_mesur <- wbe_sample_measurments_wide_2_long(df = dfs_prep[[tbln_nm_wide2long]])

    dfs_prep <- c(smpl_mesur, dfs_prep)

    tbls <- dfs_prep %>% names() %>% unique() %>%
        intersect(wbe_tbl_list(conn = conn))

    dfs_trans <<- dfs_prep[tbls]

    dfs_trans
})



#################################
#'
#' x$append_mapped_data()
#' Map data from the read in format to the database format
#' often a specific mapper class will overide this function, with its own
#'
wbe_mapper_generator$methods(append_mapped_data = function(conn = .self$conn){

    if(length(dfs_trans) == 0){
        map_data()
    }

    wbe_append_from_dfs(dfs = dfs_trans, conn = conn)

})

#################################
#'
# x <- wbe_mapper_generator$new(nm ="UW" )





