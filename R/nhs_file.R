#' available data in NHANES database
#'
#' @param years one or more years
#' @param cat logical, wheter to show the process
#' @param withdrawn logical, whether include withdrawned file
#' @return available data
#' @name nhs_files
#' @export
#'
#' @examples
#' # nhs_files_web(years=1999,items=c('d','e','l','q'))
#' # nhs_files_web(years=2019,items=c('d','e','l','q'))
#' # nhs_files_web(nhs_year(range = F),'demo')
nhs_files_web <- function(years,items,cat=TRUE, withdrawn=FALSE){

    if (do::cnOS()){
        retrieve <-tmcn::toUTF8("\u63D0\u53D6\u6570\u636E(\u5E74):")
        items0 <- tmcn::toUTF8("items\u8D4B\u503C\u4E0D\u5BF9,\u5E94\u8BE5\u662F\u4E0B\u5217\u503C: ")
    }else{
        retrieve <-'retrieve items (year):'
        items0 <- 'items is not right, which should be: '
    }
    (years <- prepare_years(years))
    (items <- prepare_items(items))
    (dt <- rep(items,each=length(years)))
    (ys <- rep(do::Replace0(years,'-.*'),length(items)))
    urls <- sprintf('https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=%s&CycleBeginYear=%s',
                    dt,ys)
    (urls <- urls[order(ys)])
    (dt <- dt[order(ys)])
    (ys <- ys[order(ys)])
    for (i in 1:length(urls)) {

        if (i==1){
            res <- list()

            if (cat) cat('\n',years[i])
        }else{
            if (ys[i] != ys[i-1]) if (cat) cat('\n',years[i])
        }
        if (cat) cat('  ',dt[i])
        wait <- TRUE
        while (wait) {
            html <- tryCatch(xml2::read_html(urls[i]),error=function(e) 'e')
            wait <- ifelse(is.character(html),TRUE,FALSE)
        }
        tbl <- html |>
            rvest::html_table()
        if (length(tbl)==0){
            res <- c(res,list(data.frame(cbind(year=ys[i],items=dt[i]))))
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

            dfi <- cbind(year=prepare_years(ys[i]),
                         items=dt[i],
                         df,
                         'DOC  url'=paste0('https://wwwn.cdc.gov',url1),
                         'Data url'=paste0('https://wwwn.cdc.gov',url2))
            ck <- do::file.name(url2)  |> tolower() == "dxa.aspx"
            ck[is.na(ck)] <- FALSE
            if (any(ck)){
                dx <- dxa.aspx(url = dfi$`DOC  url`[ck],
                               years = ys[i],
                               items=dt[i])
                dfi[ck,] <- dx
            }
            ck <- dfi$`Data url` |> do::duplicated_last()
            if (any(ck)){
                dfi <- dfi[!ck,]
            }
            row.names(dfi) <- NULL
            res <- c(res,list(dfi))
        }
    }
    x <- do.call(plyr::rbind.fill,res)
    class(x) <- c('nhs_file','data.frame')
    if (!withdrawn) x <- x[tolower(x$`Date Published`) != 'withdrawn',]
    rownames(x) <- NULL
    x
}

dxa.aspx <- function(url,years,items){
    wait <- TRUE
    while (wait) {
        html <- tryCatch(xml2::read_html(url), error=function(e) 'e')
        wait <- ifelse(is.character(html),TRUE,FALSE)
    }
    # xpturl
    xpturl <- html |>
        rvest::html_elements(xpath = '//table[@id="GridView1"]/tbody/tr') |>
        set::grep_and(years) |>
        rvest::html_elements(xpath = 'td[4]/a') |>
        do::attr_href() |>
        sprintf(fmt = 'https://wwwn.cdc.gov%s')
    # docurl
    docurl <- html |>
        rvest::html_elements(xpath = '//table[@id="GridView1"]/tbody/tr') |>
        set::grep_and(years) |>
        rvest::html_elements(xpath = 'td[3]/a') |>
        do::attr_href() |>
        sprintf(fmt = 'https://wwwn.cdc.gov%s')
    # ftablej
    ftablej <- html |>
        rvest::html_elements(xpath = '//table[@id="GridView1"]') |>
        rvest::html_table() |>
        do::select(1,drop=TRUE) |>
        as.data.frame()
    ftablej <- ftablej[grepl(years,ftablej$Years),]
    cbind(years=ftablej[,1],items,ftablej[,-1],docurl,xpturl)
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


#' @rdname nhs_files
#' @export
#' @param pattern for nhs_files_pc()
#' @param file_ext file extensions for nhs_files_pc(), default is NULL to list all files
#'
nhs_files_pc <- function(pattern=NULL,items,years,exclude=NULL,file_ext=NULL,cat=TRUE){
    if (missing(years)) years <- nhs_year_pc()
    years <- prepare_years(years)
    items <- prepare_items(items)
    d1 <- get_config_path() %+% '/' %+% years %+% '/'
    d2 <- lapply(d1, function(i) i %+% items)
    d3 <- do.call(c,d2)
    if (is.null(file_ext)) file_ext <- c("sas7bdat","codebook","label","tsv","update","xpt")
    if (!is.null(pattern)) pattern <- paste0(pattern,collapse = '|')
    f1 <- list.files(path = d3,pattern = pattern,full.names = TRUE)
    ck <- tools::file_ext(f1) %in% file_ext
    f1[ck]
    f2 <- f1[ck]
    if (!is.null(exclude)) f2 <- set::grep_not_or(f2,exclude)
    f2
}

select_df <- function(x,...){
    x[,c(...)]
}

file.info2 <- function(file,i){
    info <<- file.info(file)
    yeari <- i
    itemsi <- rownames(info) |>
        do::Replace0(paste0('.*',i,'/')) |>
        do::Replace0('/.*')
    filei <- rownames(info) |>
        do::Replace0('.*/')
    size <- sapply(info$size,size_bt2unit)
    mtime <- as.character(info$mtime)
    cbind(year=yeari,items=itemsi,file=filei,size=size,mtime=mtime) |>
        data.frame()

}


#' @rdname nhs_files
#' @param exclude exclude files
#' @export
#'
nhs_files_tsv <- function(pattern=NULL,items,years,exclude=NULL, cat=TRUE){
    file_ext='tsv'
    if (missing(years)) years <- nhs_year_pc()
    years <- prepare_years(years)
    items <- prepare_items(items)
    d1 <- get_config_path() %+% '/' %+% years %+% '/'
    d2 <- lapply(d1, function(i) i %+% items)
    d3 <- do.call(c,d2)
    if (is.null(file_ext)) file_ext <- c("sas7bdat","codebook","label","tsv","update","xpt")
    if (!is.null(pattern)) pattern <- paste0(pattern,collapse = '|')
    f1 <- list.files(path = d3,pattern = pattern,full.names = TRUE)
    ck <- tools::file_ext(f1) %in% file_ext
    f2 <- f1[ck]
    if (!is.null(exclude)) f2 <- set::grep_not_or(f2,exclude)
    f2
}
