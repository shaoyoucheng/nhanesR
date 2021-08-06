#' Read data from 'NHANES' database in local PC
#'
#' @param ... one or more data file path
#' @param label logical, whether to add label for variable
#' @param codebook logical, whether to decode variable
#' @param nrows The maximum number of rows to read.
#'
#' @return a list contains dataframe
#' @export
#'
#' @examples
#' \donttest{
#'
#' demo <- nhs_files_pc(pattern = 'demo',file_ext = 'tsv') |>
#'         set::grep_not_or('p_demo')
#' nhs_read(demo)
#' }
nhs_read <- function(...,label=FALSE,codebook=FALSE,nrows=Inf){
    files <- c(...)
    if (do::cnOS()){
        tsv <- tmcn::toUTF8("\u5FC5\u987B\u662Ftsv\u6587\u4EF6\n     ")
    }else{
        tsv <- 'must be tsv file\n     '
    }
    if (any(tools::file_ext(files) != 'tsv')){
        files <- paste0(files[tools::file_ext(files) != 'tsv'],collapse = '\n     ')
        tsv <- paste0(tsv,files)
        stop(tsv)
    }
    years <- files |>
        do::Replace0(get_config_path()) |>
        stringr::str_extract('[0-9]{4}-[0-9]{4}')
    (yearu <- years |> unique() |> do::increase())

    # file
    dfi <- lapply(yearu, function(i){
        (filesi <- files[years==i])

        cat(paste0('\n\n',crayon::red(do::Replace0(i,'.*/')),'(',length(filesi),')'))
        for (j in 1:length(filesi)) {
            (filej <- do::file.name(filesi[j]))
            (dataj <- filesi[j] |> do::upper.dir(end.slash = FALSE) |> do::file.name(extension = FALSE))

            if (j == 1){
                cat(paste0(' ',dataj))
                cat(paste0(' ',crayon::blue(filej)))
                dfj <- data.table::fread(filesi[j],data.table = FALSE,showProgress = FALSE,nrows=nrows)
                head(dfj)
                # codebook
                if (codebook){
                    (ckbkf <- do::Replace(filesi[j],'\\.tsv','.codebook'))
                    if (file.exists(ckbkf)){
                        ckbk <- read.delim(ckbkf,comment.char = '#')
                        head(ckbk)
                        if (nrow(ckbk)>1){
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
                }
                # add label
                if (label){
                    (labefile <- do::Replace(filesi[j],'\\.tsv','.label'))
                    if (file.exists(labefile)){
                        labelj <- read.delim(labefile,comment.char = '#')
                        if (nrow(labelj)>1){
                            dfj <- sprintf('%s = "%s"',labelj$name,labelj$label) |>
                                paste0(collapse = ', ') |>
                                sprintf(fmt = 'expss::apply_labels(dfj,%s)') |>
                                parse(file='',n=NULL) |>
                                eval()
                        }
                    }

                }
            }else{
                (dataj0 <- filesi[j-1] |> do::upper.dir(end.slash = FALSE) |> do::file.name(extension = FALSE))
                if (dataj0 != dataj) cat(paste0('\n             ',dataj))
                cat(paste0(' ',crayon::blue(filej)))

                dfji <- data.table::fread(filesi[j],showProgress = FALSE,data.table = FALSE,nrows=nrows)
                # codebook
                if (codebook){
                    (ckbkf <- do::Replace(filesi[j],'.tsv','.codebook'))
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
                if (label){
                    # add label
                    (labefile <- do::Replace(filesi[j],'.tsv','.label'))
                    if (file.exists(labefile)){
                        labelj <- read.delim(labefile,comment.char = '#')
                        dfji <- sprintf('%s = "%s"',labelj$name,labelj$label) |>
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
    names(dfi) <- yearu
    dfi
}
