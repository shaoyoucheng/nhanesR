#' Download data from 'NHANES' database
#'
#' @param years one or more years
#' @param data one or more data
#' @param files which files to be download
#' @param mode character. The mode with which to write the file. Useful values are "wb" (binary), "w" and "ab".
#' @return download 'NHANES' database data into local computer and keep the same order.
#' @export
#'
#' @examples
#' \donttest{
#' nhs_download(years = c(2017,2019),data = c('d','l'))
#' }
nhs_download <- function(years,data,files,mode){
    if (do::cnOS()){
        data0 <- tmcn::toUTF8("data\u8D4B\u503C\u4E0D\u5BF9,\u5E94\u8BE5\u662F\u4E0B\u5217\u503C: ")
        start <- tmcn::toUTF8("=====\u5F00\u59CB\u4E0B\u8F7D=====")
    }else{
        data0 <- 'data is not right, which should be: '
        start <- '=====starting====='
    }
    years <- prepare_years_web(years)|> do::increase()
    cat('\nyear: ',paste(years,collapse = ', '))
    data <- prepare_data(data)
    cat('\n\ndata: ',paste(data,collapse = ', '))
    if (missing(mode)) mode <- test_mode()
    cat(paste0('\n\nmode = "',mode,'"'))
    cat('\n\n',crayon::red(start),'\n')
    # urls for data
    urls <- do::Replace0(years,'-.*') |>
        sapply(function(i) sprintf('https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=%s&CycleBeginYear=%s',
                                      data,i)) |>
        as.vector()
    testfile(urls = urls,mode=mode,files=files)
}

urlyear <- function(url){
    do::Replace0(url,c('.*=','.*Nhanes/','/.*'))
}
urlComponet <- function(url){
    do::Replace0(url,c('.*\\?Component=','\\&CycleBeginYear.*'))
}



