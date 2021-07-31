#' Get config information of "NHANES" database
#' @param range logical for years
#' @return config information of "NHANES" database
#' @export
#' @name get_config
#' @examples
#' \donttest{
#' get_config_path()
#' get_config_years()
#' get_config_data()
#'
#' }
get_config_path <- function(){
    if (do::cnOS()){
        msg <- tmcn::toUTF8("\u6CA1\u6709\u914D\u7F6ENHANES\u8DEF\u5F84  \n\u5728\u4F7F\u7528nhanesR\u5305\u4E4B\u524D,\u8BF7\u5148\u5B8C\u6210\u4EE5\u4E0B3\u9879\u914D\u7F6E\n     1.\u4F7F\u7528config_path()\u547D\u4EE4\u914D\u7F6E\u6570\u636E\u5E93\u8DEF\u5F84\n     2.\u4F7F\u7528config_years()\u547D\u4EE4\u914D\u7F6E\u6570\u636E\u5E93\u5E74\u4EFD\n     3.\u4F7F\u7528config_data()\u547D\u4EE4\u914D\u7F6E\u6570\u636E\u5E93\u6587\u4EF6\u7C7B\u578B")
    }else{
        msg <- 'No path of NHANES configed'
    }
    (temp <- tempdir() |> do::upper.dir() |> paste0('nhanesR'))
    nhs.path <- paste0(temp,'/path.nhanes')
    if (!file.exists(nhs.path)){
        message(msg)
    }else{
        read.table(nhs.path)[1,1] |> gsub(pattern = '//',replacement = '/')
    }
}

#' @rdname get_config
#' @export
get_config_data <- function(){
    if (do::cnOS()){
        retrieve <-tmcn::toUTF8("\u63D0\u53D6\u6570\u636E(\u5E74):")
        nolocal <- tmcn::toUTF8("\u6CA1\u6709\u672C\u5730\u6570\u636E\u5E93,\u8BF7\u4F7F\u7528config_data()\u914D\u7F6E\u672C\u5730\u6570\u636E\u5E93")
    }else{
        retrieve <-'retrieve data (year):'
        nolocal <- 'No local database, please use config_data() to config'
    }
    temp <- config_temp()
    if (!dir.exists(temp)) dir.create(temp,showWarnings = FALSE,recursive = TRUE)
    (nhs_data <- paste0(temp,'/data.nhanes'))
    if (!file.exists(nhs_data)){
        message(nolocal)
    }else{
        read.table(nhs_data)[,1]
    }
}

#' @rdname get_config
#' @export
get_config_years <- function(range=TRUE){
    if (do::cnOS()){
        nolocal <- tmcn::toUTF8("\u6CA1\u6709\u672C\u5730\u6570\u636E\u5E93,\u8BF7\u4F7F\u7528config_path()\u914D\u7F6E\u672C\u5730\u6570\u636E\u5E93")
    }else{
        nolocal <- 'No local database, please use config_path() to config'
    }
    (temp <- tempdir() |> do::upper.dir() |> paste0('nhanesR'))
    if (!dir.exists(temp)) dir.create(temp,showWarnings = FALSE,recursive = TRUE)
    (nhs_years <- paste0(temp,'/years.nhanes'))
    if (!file.exists(nhs_years)){
        message(nolocal)
    }else{
        years <- read.table(nhs_years)[,1]
        if (!range) years <- do::Replace0(years,'-.*')
        years
    }
}
