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
    if (!missing(years)){
        years <- do::Replace0(years,'-.*')
        years <- ys[sapply(strsplit(ys,'-'),function(i) any(i %in% years))]
    }
    if (missing(years)) years <- ys
    if (!range) years <- do::Replace0(years,'-.*')
    years |> do::increase()
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
    if (missing(items)) items <- d5
    d5[tolower(do::left(d5,max(nchar(items)))) %in% tolower(do::left(items,max(nchar(items))))]
}
