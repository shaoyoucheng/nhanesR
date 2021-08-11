#' prepare years from config
#'
#' @param years one or more years
#' @param range logical.
#'
#' @return years of config
#' @export
#'
prepare_years <- function(years,range=TRUE){
    ys <- get_config_years()

    if (missing(years)) years <- ys
    if (any(grepl(get_config_path(),years))){
        x <- sapply(years,function(i) do::Replace0(i,do::formal_dir(get_config_path(),TRUE),'/.*'))
        names(x) <- NULL
        x
    }else{
        if (!missing(years)){
            years <- do::Replace0(years,'-.*')
            years <- ys[sapply(strsplit(ys,'-'),function(i) any(i %in% years))]
        }

        if (!range) years <- do::Replace0(years,'-.*')
        years |> do::increase()
    }

}
#' prepare items from config
#' ignore capital and little letters and left match
#' @param items one or more items
#'
#' @return items of config
#' @export
#'
prepare_items <- function(items){
    d5 <- get_config_items()
    if (missing(items)){
        items <- d5
    }
    if (any(grepl(get_config_path(),items))){
        x <- sapply(items,function(i) do::Replace0(i,
                                                   do::formal_dir(get_config_path(),TRUE),
                                                   do::formal_dir(prepare_years(i),TRUE),
                                                   '/.*'))
        names(x) <- NULL
        x
    }else{


        d5[tolower(do::left(d5,max(nchar(items)))) %in% tolower(do::left(items,max(nchar(items))))]

    }
}
