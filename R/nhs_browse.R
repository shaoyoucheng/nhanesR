#' Browse "NHANES" webpage
#'
#' @param years one or more years
#' @param items one or more items
#'
#' @return browse webpage
#' @export
#'
nhs_browse <- function(years,items){
    if (do::cnOS()){
        misys <- tmcn::toUTF8("\u6307\u5B9Aitems\u7684\u65F6\u5019,\u5FC5\u987B\u6307\u5B9Ayears")
        itemswrong <- tmcn::toUTF8("items\u6307\u5B9A\u4E0D\u5BF9,\u5FC5\u987B\u8981\u662F\u4E0B\u5217\u503C: ")
    }else{
        misys <- 'years must be given when items is specified.'
        itemswrong <- 'items is wrong, which should be as follows: '
    }
    if (missing(years) & missing(items)){
        browseURL('https://wwwn.cdc.gov/nchs/nhanes/')
    }else if (!missing(years) & missing(items)){
        years <- prepare_years(years) |>
            do::Replace0('-.*')
        for (i in years) {
            urls <- sprintf('https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=%s',i)
            browseURL(urls)
        }
    }else if (!missing(years) & !missing(items)){
        years <- prepare_years(years) |>
            do::Replace0('-.*')
        items <- prepare_items(items)
        if (length(items)==0) stop(itemswrong, paste0(d5,collapse = ', '))
        for (i in years) {
            for (j in items) {
                urls <- sprintf('https://wwwn.cdc.gov/nchs/nhanes/search/itemspage.aspx?Component=%s&CycleBeginYear=%s',j,i)
                browseURL(urls)
            }
        }
    }else if (missing(years) & !missing(items)){
        years <- prepare_years(years) |>
            do::Replace0('-.*')
        items <- prepare_items(items)
        if (length(items)==0) stop(itemswrong, paste0(d5,collapse = ', '))
        for (i in years) {
            for (j in items) {
                urls <- sprintf('https://wwwn.cdc.gov/nchs/nhanes/search/itemspage.aspx?Component=%s&CycleBeginYear=%s',j,i)
                browseURL(urls)
            }
        }
    }
}
