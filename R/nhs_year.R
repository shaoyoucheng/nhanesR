#' available Year in NHANES database
#' @param range whether to show year interval
#' @name nhs_year
#' @return available year, two years interval
#' @export
#'
#' @examples
#' nhs_years_web()
nhs_years_web <- function(range=TRUE){
    home_url = 'https://wwwn.cdc.gov/nchs/nhanes'
    html = xml2::read_html(home_url)
    years <- html |>
        rvest::html_nodes(xpath = '//div[@class="col-md-3 d-flex"]/a') |>
        rvest::html_text() |>
        do::Replace0(c('\t','\n','\r','NHANES'))
    if (!range) years <- do::Replace0(years,'-.*')
    years
}


# #' @rdname nhs_year
# #' @export
#
# nhs_year_pg <- function(range=TRUE){
#
#     # if (!range) years <- do::Replace0(years,'-.*')
#     # years
# }
