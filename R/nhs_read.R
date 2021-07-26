#' Read data from 'NHANES' database in local PC
#'
#' @param ... one or more data file name.
#' @param years one or years
#' @param exclude exclude some file
#' @param var.label logical, whether to add label for variable
#' @param cookbook logical, whether to decode variable
#'
#' @return a list contains dataframe
#' @export
#'
#' @examples
#' \donttest{
#' nhs_read('demo')
#' }
nhs_read <- function(...,years,exclude=NULL,var.label=FALSE,cookbook=FALSE){
    fileg <- c(...)
    file <- fileg
    if (do::cnOS()){
        ymsg <- tmcn::toUTF8("\u5E74: ")
    }else{
        ymsg <- 'years: '
    }
    # years
    yslong <- list.files(nhs.path,full.names = TRUE)
    ys <- do::Replace0(yslong,'.*/')
    if (missing(years)){
        ck <- rep(TRUE,length(ys))
    }else{
        ck <- sapply(strsplit(ys,'-'),function(i) any(i %in% years))
    }
    ys <- ys[ck]
    (yslong <- yslong[ck])

    # file
    dfi <- lapply(yslong, function(i){

        allfileslong <- list.files(path = i,pattern = '.tsv',full.names = TRUE,recursive = TRUE,ignore.case = TRUE)
        allfiles <- do::Replace0(allfileslong,'.*/')
        ck <- grepl(paste0(tolower(file),collapse = '|'),
                    do::Replace0(tolower(allfiles),'.tsv'))

        fileread <- allfileslong[ck]
        if (!is.null(exclude)){
            ck <- grepl(paste0(tolower(exclude),collapse = '|'),
                        do::Replace0(tolower(fileread),c('.*/','.tsv')))
            fileread <- fileread[!ck]
        }
        cat(paste0('\n\n',crayon::red(do::Replace0(i,'.*/')),'(',length(fileread),')'))
        for (j in 1:length(fileread)) {
            (filej <- do::file.name(fileread[j]))
            dataj <- do::Replace0(fileread[j],i,filej,'/')

            datap <- paste0(dataj,do::rep_n(' ',13-nchar(dataj)))
            if (j == 1){
                cat(paste0(' ',datap))
                cat(paste0(' ',crayon::blue(filej)))
                dfj <- data.table::fread(fileread[j],data.table = FALSE,showProgress = FALSE)
                head(dfj)
                # cookbook
                if (cookbook){
                    (ckbkf <- do::Replace(fileread[j],'.tsv','.cookbook'))
                    if (file.exists(ckbkf)){
                        ckbk <- read.delim(ckbkf,comment.char = '#')
                        head(ckbk)
                        for (k in unique(ckbk$variable)) {
                            code <- ckbk[ckbk$variable == k,]
                            for (cd in 1:nrow(code)) {
                                cdjd <- dfj[,k] == code[cd,'code']
                                cdjd[is.na(cdjd)] <- FALSE
                                dfj[cdjd,k] <- code[cd,'label']
                            }
                        }
                    }

                }
                # add label
                if (var.label){
                    (labefile <- do::Replace(fileread[j],'.tsv','.label'))
                    if (file.exists(labefile)){
                        label <- read.delim(labefile,comment.char = '#')
                        dfj <- sprintf('%s = "%s"',label$name,label$label) |>
                            paste0(collapse = ', ') |>
                            sprintf(fmt = 'expss::apply_labels(dfj,%s)') |>
                            parse(file='',n=NULL) |>
                            eval()
                    }

                }
            }else{
                dataj0 <- do::Replace0(fileread[j-1],c(paste0(i,'/'),'/.*'))
                if (dataj0 != dataj) cat(paste0('\n             ',datap))
                cat(paste0(' ',crayon::blue(filej)))
                dfji <- data.table::fread(fileread[j],showProgress = FALSE,data.table = FALSE)
                # cookbook
                if (cookbook){
                    (ckbkf <- do::Replace(fileread[j],'.tsv','.cookbook'))
                    head(ckbk)
                    if (file.exists(ckbkf)){
                        ckbk <- read.delim(ckbkf,comment.char = '#')
                        for (k in unique(ckbk$variable)) {
                            code <- ckbk[ckbk$variable == k,]
                            for (cd in 1:nrow(code)) {
                                cdjd <- dfji[,k] == code[cd,'code']
                                cdjd[is.na(cdjd)] <- FALSE
                                dfji[cdjd,k] <- code[cd,'label']
                            }
                        }
                    }

                }
                if (var.label){
                    # add label
                    (labefile <- do::Replace(fileread[j],'.tsv','.label'))
                    if (file.exists(labefile)){
                        label <- read.delim(labefile,comment.char = '#')
                        dfji <- sprintf('%s = "%s"',label$name,label$label) |>
                            paste0(collapse = ', ') |>
                            sprintf(fmt = 'expss::apply_labels(dfji,%s)') |>
                            parse(file='',n=NULL) |>
                            eval()
                    }

                }
                dfj <- data.table::merge.data.table(x = dfj,y = dfji,by = 'seqn',all = TRUE)
            }
        }
        dfj
    })
    names(dfi) <- ys
    dfi
}
