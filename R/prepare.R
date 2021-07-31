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
prepare_data <- function(data){
    d5 <- get_config_data()
    if (missing(data)) data <- d5
    d5[tolower(do::left(d5,max(nchar(data)))) %in% tolower(do::left(data,max(nchar(data))))]
}
