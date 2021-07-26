prepare_years_pc <- function(years){
    ys <- nhs_year_pc()
    if (!missing(years)){
        years <- do::Replace0(years,'-.*')
        years <- ys[sapply(strsplit(ys,'-'),function(i) any(i %in% years))]
    }
    if (missing(years)) years <- ys
    years
}

prepare_years_web <- function(years){
    ys <- webyears
    if (!missing(years)){
        years <- do::Replace0(years,'-.*')
        years <- ys[sapply(strsplit(ys,'-'),function(i) any(i %in% years))]
    }
    if (missing(years)) years <- ys
    years
}
prepare_data <- function(data){
    if (missing(data)) data <- d5
    d5[tolower(do::left(d5,max(nchar(data)))) %in% tolower(do::left(data,max(nchar(data))))]
}
