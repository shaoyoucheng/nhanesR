#' available data in NHANES database
#'
#' @param years one or more years
#' @param cat logical, wheter to show the process
#' @name nhs_data
#' @return available data
#' @export
#'
#' @examples
#' nhs_data_web(years=1999)
#' nhs_data_web(years=c(1999,2001))
#' nhs_data_web(nhs_year(range = F))
nhs_data_web <- function(years,cat=TRUE){

    if (do::cnOS()){
        retrieve <-tmcn::toUTF8("\u63D0\u53D6\u6570\u636E(\u5E74):")
    }else{
        retrieve <-'retrieve data (year):'
    }
    ys <- nhs_year_web()
    if (!missing(years)){
        years <- ys[sapply(strsplit(ys,'-'),function(i) any(i %in% years))]
    }
    if (missing(years)) years <- ys
    urls <- paste0('https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=',do::Replace0(years,'-.*'))
    for (i in 1:length(urls)) {
        if (cat) cat('\n',retrieve,years[i])
        if (i==1) res <- list()
        year_html = xml2::read_html(urls[i])
        data_href = year_html |>
            rvest::html_elements(xpath = '//a[@class="list-title td-none td-ul-hover"]') |>
            set::grep_and('Component=') |>
            do::attr_herf() |>
            do::Replace0('.*datapage\\.aspx\\?Component=') |>
            do::Replace0('&CycleBeginYear.*')
        data_href = set::grep_not_and(data_href,'LimitedAccess')
        if (length(data_href)==0) data_href <- rep(NA,5)
        res <- c(res,list(data_href))
        names(res)[i] <- years[i]
    }
    if (cat) cat('\n\n')
    do.call(rbind,res) |> data.frame()
}


#' @rdname nhs_data
#' @export
nhs_data_pc <- function(years,cat=TRUE){

    if (do::cnOS()){
        retrieve <-tmcn::toUTF8("\u63D0\u53D6\u6570\u636E(\u5E74):")
        nolocal <- tmcn::toUTF8("\u6CA1\u6709\u672C\u5730\u6570\u636E\u5E93,\u8BF7\u4F7F\u7528nhs_path()\u914D\u7F6E\u672C\u5730\u6570\u636E\u5E93")
    }else{
        retrieve <-'retrieve data (year):'
        nolocal <- 'No local database, please use nhs_path() to config'
    }
    if (is.null(nhs.path)){
        message(nolocal)
    }else{
        if (!missing(years)){
            ys <- nhs_year_pc()
            years <- ys[sapply(strsplit(ys,'-'),function(i) any(i %in% years))]
        }
        if (missing(years)) years <- nhs_year_pc()


        urls <- paste0(nhs.path,'/',years)
        for (i in 1:length(urls)) {
            # if (cat) cat('\n',retrieve,years[i])
            if (i==1) res <- list()
            data_href = list.files(urls[i])
            data_href = data_href[data_href %in% d5]
            if (length(data_href)==0) data_href <- rep(NA,5)
            res <- c(res,list(data_href))
            names(res)[i] <- years[i]
        }
        if (cat) cat('\n\n')
        do.call(rbind,res)  |> data.frame()
    }

}

#' @rdname nhs_data
#' @export
nhs_data_pg <- function(years,cat=TRUE){

}
