#' temporary config directory
#'
#' @return temporary config directory
#' @export
#'
#' @examples
#' \donttest{
#' config_temp()
#' }
config_temp <- function(){
    tempdir() |> do::upper.dir() |> paste0('nhanesR')
}


#' Config path of 'NHANES' database
#'
#' @param path path of 'NHANES' database
#'
#' @return change the internal path
#' @export
#' @examples
#' \donttest{
#' config_path('f/nhanes')
#' }
config_path <- function(path){
    if (do::cnOS()){
        setpath <- tmcn::toUTF8("\u8BBE\u7F6ENHANES\u6570\u636E\u8DEF\u5F84\u81F3: ")
    }else{
        setpath <- 'set NHANES path to: '
    }
    temp <- config_temp()
    if (!dir.exists(temp)) dir.create(temp,showWarnings = FALSE,recursive = TRUE)
    nhs_path <- paste0(temp,'/path.nhanes')
    path <- do::formal_dir(path)
    write.table(x = path,file = nhs_path,row.names = FALSE,col.names = FALSE)
    cat(setpath,path)
    cat('\n\n',nhs_path)
}


#' Config years of 'NHANES' database
#'
#' @param years years of 'NHANES' database. If years is missing, nhs_years_web() will be
#'     used to be added.
#'
#' @return add "years.nhanes" file to "nhanesR" directory
#' @export
#'
config_years <- function(years){
    if (do::cnOS()){
        setyears <- tmcn::toUTF8("\u6210\u529F\u914D\u7F6E\u4EE5\u4E0B\u5E74\u4EFD")
    }else{
        setyears <- 'config years: '
    }
    if (missing(years)) years <- nhs_years_web()
    years <- do::increase(years)
    temp <- config_temp()
    if (!dir.exists(temp)) dir.create(temp,showWarnings = FALSE,recursive = TRUE)
    (nhs_years <- paste0(temp,'/years.nhanes'))
    write.table(x = years,file = nhs_years,row.names = FALSE,col.names = FALSE,append = FALSE)
    message(setyears)
    years <- read.table(nhs_years)[,1]
    for (i in 1:length(years)) {
        if (i==1) cat('    ')
        cat(years[i],'')
        if (i %% 3 ==0) cat('\n    ')
    }
    cat('\n\n',nhs_years)
}





#' Config items of 'NHANES' database
#'
#' @param items items of 'NHANES' database. If items is missing, Demographics,
#'     Dietary, Examination, Laboratory and Questionnaire will be used.
#'
#' @return add "items.nhanes" file to "nhanesR" directory
#' @export
#' @examples
#' \donttest{
#' config_items(c("Demographics", "Dietary", "Examination",  "Laboratory", "Questionnaire"))
#' }
config_items <- function(items){
    if (do::cnOS()){
        setitems <- tmcn::toUTF8("\u6210\u529F\u914D\u7F6E\u4EE5\u4E0B\u6570\u636E")
    }else{
        setitems <- 'config items: '
    }
    if (missing(items)) items <- c("Demographics", "Dietary", "Examination",  "Laboratory", "Questionnaire")
    names(items) <- NULL
    temp <- config_temp()
    if (!dir.exists(temp)) dir.create(temp,showWarnings = FALSE,recursive = TRUE)
    (nhs_items <- paste0(temp,'/items.nhanes'))
    write.table(x = items,file = nhs_items,row.names = FALSE,col.names = FALSE,append = FALSE)
    message(setitems)
    items <- read.table(nhs_items)[,1]
    for (i in 1:length(items)) {
        if (i==1) cat('    ')
        cat(items[i],'')
        if (i %% 1 ==0) cat('\n    ')
    }
    cat('\n\n',nhs_items)
}


