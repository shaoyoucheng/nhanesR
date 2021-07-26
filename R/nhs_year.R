#' available Year in NHANES database
#' @param range whether to show year interval
#' @name nhs_year
#' @return available year, two years interval
#' @export
#'
#' @examples
#' nhs_year_web()
#' nhs_year_pc()
#' nhs_year_pg()
nhs_year_web <- function(range=TRUE){

    home_url = 'https://wwwn.cdc.gov/nchs/nhanes'
    html = xml2::read_html(home_url)
    years <- html |>
        rvest::html_nodes(xpath = '//div[@class="col-md-3 d-flex"]/a') |>
        rvest::html_text() |>
        do::Replace0(c('\t','\n','\r','NHANES'))

    if (!range) years <- do::Replace0(years,'-.*')
    years
}

#' @rdname nhs_year
#' @export
nhs_year_pc <- function(range=TRUE){
    if (do::cnOS()){
        nolocal <- tmcn::toUTF8("\u6CA1\u6709\u672C\u5730\u6570\u636E\u5E93,\u8BF7\u4F7F\u7528nhs_path()\u914D\u7F6E\u672C\u5730\u6570\u636E\u5E93")
    }else{
        nolocal <- 'No local database, please use nhs_path() to config'
    }
    if (is.null(nhs.path)){
        message(nolocal)
    }else{
        years <- list.files(nhs.path) |>
            stringr::str_extract('[0-9]{4}-[0-9]{4}') |>
            do::complete.data()
    }
    if (!range) years <- do::Replace0(years,'-.*')
    years
}

#' @rdname nhs_year
#' @export

nhs_year_pg <- function(range=TRUE){

    # if (!range) years <- do::Replace0(years,'-.*')
    # years
}
