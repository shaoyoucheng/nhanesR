#' available data in NHANES database
#'
#' @param years one or more years
#' @param cat logical, wheter to show the process
#' @return available data
#' @name nhs_file
#' @export
#'
#' @examples
#' # nhs_file_web(years=1999,data=c('d','e','l','q'))
#' # nhs_file_web(years=2019,data=c('d','e','l','q'))
#' # nhs_file_web(nhs_year(range = F),'demo')
nhs_file_web <- function(years,data,cat=TRUE){

    if (do::cnOS()){
        retrieve <-tmcn::toUTF8("\u63D0\u53D6\u6570\u636E(\u5E74):")
        data0 <- tmcn::toUTF8("data\u8D4B\u503C\u4E0D\u5BF9,\u5E94\u8BE5\u662F\u4E0B\u5217\u503C: ")
    }else{
        retrieve <-'retrieve data (year):'
        data0 <- 'data is not right, which should be: '
    }
    years <- prepare_years_web(years)
    data <- prepare_data(data)
    if (length(data)==0) stop(data0, paste0(d5,collapse = ', '))

    (dt <- rep(data,each=length(years)))
    (ys <- rep(do::Replace0(years,'-.*'),length(data)))
    urls <- sprintf('https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=%s&CycleBeginYear=%s',
                    dt,ys)
    urls <- urls[order(ys)]
    dt <- dt[order(ys)]
    ys <- ys[order(ys)]
    for (i in 1:length(urls)) {

        if (i==1){
            res <- list()

            if (cat) cat('\n',years[i])
        }else{
            if (ys[i] != ys[i-1]) if (cat) cat('\n',years[i])
        }
        if (cat) cat('  ',dt[i])
        html <- xml2::read_html(urls[i])
        tbl <- html |>
            rvest::html_table()
        if (length(tbl)==0){
            res <- c(res,list(data.frame(cbind(year=ys[i],data=dt[i]))))
        }else{
            df <-  tbl |>
                listn(1) |>
                as.data.frame() |>
                df.tolower()

            url1 <- html |>
                rvest::html_elements(xpath = '//tbody//tr/td[2]') |>
                sapply(function(i) i |> rvest::html_elements(xpath = 'a') |> rvest::html_attr('href'))
            url1[sapply(url1, length) == 0] <- NA
            url1 <- unlist(url1)
            ck <- do::left(url1,2) == '..' & !is.na(url1)
            url1[ck] <- do::Replace(url1[ck],'\\.\\.','/nchs/nhanes')

            url2 <- html |>
                rvest::html_elements(xpath = '//tbody//tr/td[3]') |>
                sapply(function(i) i |> rvest::html_elements(xpath = 'a') |> rvest::html_attr('href'))
            url2[sapply(url2, length) == 0] <- NA
            url2 <- unlist(url2)
            ck <- do::left(url2,2) == '..' & !is.na(url2)
            url2[ck] <- do::Replace(url2[ck],'\\.\\.','/nchs/nhanes')

            dfi <- cbind(year=ys[i],
                         data=dt[i],
                         df,
                         'DOC  url'=paste0('https://wwwn.cdc.gov',url1),
                         'Data url'=paste0('https://wwwn.cdc.gov',url2))
            res <- c(res,list(dfi))
        }
    }
    x <- do.call(plyr::rbind.fill,res)
    class(x) <- c('nhs_file','data.frame')
    x
}

listn <- function(x,n=1){
    x[[n]]
}
df.tolower <- function(x){
    for (i in 1:ncol(x)) {
        x[,i] <- tolower(x[,i])
    }
    x
}


#' @rdname nhs_file
#' @export
#'
nhs_file_pc <- function(data,years,type,cat=TRUE){

    if (do::cnOS()){
        nolocal <- tmcn::toUTF8("\u6CA1\u6709\u672C\u5730\u6570\u636E\u5E93,\u8BF7\u4F7F\u7528nhs_path()\u914D\u7F6E\u672C\u5730\u6570\u636E\u5E93")
    }else{
        nolocal <- tmcn::toUTF8("\u6CA1\u6709\u672C\u5730\u6570\u636E\u5E93,\u8BF7\u4F7F\u7528nhs_path()\u914D\u7F6E\u672C\u5730\u6570\u636E\u5E93")
    }
    if (is.null(nhs.path)){
        message(nolocal)
    }else{
        years <- prepare_years_pc(years)
        data <- prepare_data(data)
        if (missing(type)){
            lapply(years, function(i) paste0(nhs.path,'/',i,'/',data) |>
                       list.files(full.names = TRUE) |>
                       file.info2(i)) |>
                do.call(what = rbind)
        }else{
            lapply(years, function(i) paste0(nhs.path,'/',i,'/',data) |>
                       list.files(type,full.names = TRUE) |>
                       file.info2(i)) |>
                do.call(what = rbind)
        }
    }
}

select_df <- function(x,...){
    x[,c(...)]
}

file.info2 <- function(file,i){
    info <<- file.info(file)
    yeari <- i
    datai <- rownames(info) |>
        do::Replace0(paste0('.*',i,'/')) |>
        do::Replace0('/.*')
    filei <- rownames(info) |>
        do::Replace0('.*/')
    size <- sapply(info$size,size_bt2unit)
    mtime <- as.character(info$mtime)
    cbind(year=yeari,data=datai,file=filei,size=size,mtime=mtime) |>
        data.frame()

}


#' @rdname nhs_file
#' @export
#'
nhs_file_pg <- function(data,years,type,cat=TRUE){

}
