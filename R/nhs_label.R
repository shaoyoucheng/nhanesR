#' label for variable
#'
#' @param ... one(suggest) or more variable names
#' @param files missing(suggest) or file path of nhs_files_pc()
#'
#' @return variable label
#' @export
#
nhs_label <- function(...,files){
    variable <- c(...) |> tolower()
    if (missing(files)) files <- list.files(get_config_path(),'label',full.names = TRUE,
                                            recursive = TRUE)
    ck <- tools::file_ext(files) != 'label'
    if (any(ck)){
        ext <- tools::file_ext(files[ck]) |> unique() |> sprintf(fmt = '\\.%s')
        files[ck] <- do::Replace0(files[ck],ext) |> sprintf(fmt = '%s.label')
    }
    lapply(files, function(i){
        (Year <- prepare_years(i))
        (Item <- prepare_items(i))
        labeli <- read.delim(i,comment.char = '#')
        if (length(variable) >0){
            ck <- labeli$name %in% variable
            labeli <- labeli[ck,]
        }
        if (nrow(labeli)==0) return()
        cbind(Year=Year,Item=Item,labeli)
    }) |> do.call(what = rbind)
}
