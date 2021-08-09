#' update "NHANES" database
#'
#' @param years one or more years
#' @param items one or more items
#' @param download logical. Whether to download
#' @importFrom do  %+%
#' @return update local "NHANES" database
#' @export
#'
#' @examples
#' \donttest{
#' nhs_update()
#' }
nhs_update <- function(years,items,download=TRUE,redown=TRUE){
    (years <- prepare_years(years))
    (items <- prepare_items(items))
    for (i in 1:length(years)) {
        yeari <- years[i]
        cat('\n',yeari)
        for (j in 1:length(items)) {
            (itemsi <- items[j])
            cat('\n     ',itemsi)
            (pathj <- get_config_path() %+% '/' %+% yeari %+% '/' %+% itemsi %+% '/')
            filetable <- nhs_files_web(yeari,itemsi,FALSE)
            if (nrow(filetable)==0) next(j)
            for (k in 1:nrow(filetable)) {
                (filek <- filetable[k,])
                filek[1,1] <- prepare_years(filek[1,1])
                (fns <- do::file.name(filek$`Data url`) |> tolower())
                (ext <- paste0('\\.',tools::file_ext(fns)))
                (upk <- do::Replace(fns,ext,'.update'))
                (update <- pathj %+% upk)
                (ck <- file.exists(update))
                if (ck){
                    (upread <- read.table(update,header = TRUE))
                    upread[1,1] <- prepare_years(upread[1,1])
                    (ck <- all(upread == filek))
                    if (ck) next(k)
                    cat('\n       ',fns,crayon::red('update'),filek[,"Date Published"])
                    # ------- update
                    (sizej <- filek$`Data File` |> do::Replace0(c('.*- {0,}','\\].*')))
                    cat(crayon::blue(paste0('(size: ',sizej)))
                    if (download) filepage(yeari = yeari,itemsi = itemsi,filetable=filek,redown = TRUE,cat=FALSE,update = TRUE)
                }else{
                    cat('\n       ',fns,crayon::red('new'),filek[,"Date Published"])
                    # ------ update
                    (sizej <- filek$`Data File` |> do::Replace0(c('.*- {0,}','\\].*')))
                    cat(crayon::blue(paste0('(size: ',sizej)))
                    if (download) filepage(yeari = yeari,itemsi = itemsi,filetable=filek,redown = TRUE,cat=FALSE,update = TRUE)
                }
            }
            (fns <- do::file.name(filetable$`Data url`) |> tolower())
            ck <- tools::file_ext(fns) == 'zip'
            if (any(ck)){
                cki <- gsub('\\.zip','.sas7bdat',fns[ck])
                sas7 <- pathj %+% cki
                if (file.exists(sas7)){
                    fns[ck] <- cki
                }else{
                    cki <- gsub('\\.zip','.xpt',fns[ck])
                    xpt <- pathj %+% cki
                    if (file.exists(xpt)){
                        fns[ck] <- cki
                    }
                }
            }
            fns
            fnr <- list.files(pathj,'\\.xpt|\\.sas7bdat')
            fnn <- set::not(fnr,fns)
            ck <- length(fnn) == 0
            if (ck >0){
                for (k in fnn) {
                    cat('\n       ',k,crayon::red('withdrawn'))
                }
            }
        }
    }
    cat('\n')
}
