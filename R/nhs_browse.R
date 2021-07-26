nhs_browse <- function(years,data){
    if (do::cnOS()){
        misys <- tmcn::toUTF8("\u6307\u5B9Adata\u7684\u65F6\u5019,\u5FC5\u987B\u6307\u5B9Ayears")
        datawrong <- tmcn::toUTF8("data\u6307\u5B9A\u4E0D\u5BF9,\u5FC5\u987B\u8981\u662F\u4E0B\u5217\u503C: ")
    }else{
        misys <- 'years must be given when data is specified.'
        datawrong <- 'data is wrong, which should be as follows: '
    }
    if (missing(years) & missing(data)){
        browseURL('https://wwwn.cdc.gov/nchs/nhanes/')
    }else if (!missing(years) & missing(data)){
        years <- prepare_years_web(years) |>
            do::Replace0('-.*')
        for (i in years) {
            urls <- sprintf('https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=%s',i)
            browseURL(urls)
        }
    }else if (!missing(years) & !missing(data)){
        years <- prepare_years_web(years) |>
            do::Replace0('-.*')
        data <- prepare_data(data)
        if (length(data)==0) stop(datawrong, paste0(d5,collapse = ', '))
        for (i in years) {
            for (j in data) {
                urls <- sprintf('https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=%s&CycleBeginYear=%s',j,i)
                browseURL(urls)
            }
        }
    }else if (missing(years) & !missing(data)){
        years <- prepare_years_web(years) |>
            do::Replace0('-.*')
        data <- prepare_data(data)
        if (length(data)==0) stop(datawrong, paste0(d5,collapse = ', '))
        for (i in years) {
            for (j in data) {
                urls <- sprintf('https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=%s&CycleBeginYear=%s',j,i)
                browseURL(urls)
            }
        }
    }
}
