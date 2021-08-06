

testfile <- function(urls,files,mode,redown=TRUE){
    for (i in 1:length(urls)) {
        (yeari <- do::Replace0(urls[i],'.*='))
        (datai <- urlComponet(urls[i]))
        if (i==1){
            cat('\n',prepare_years(yeari),'\n')
            cat('      ',datai)
        }else{
            if (urlyear(urls[i]) != urlyear(urls[i-1])){
                cat('\n',prepare_years(yeari))
            }
            if (datai != urlComponet(urls[i-1])){
                cat('\n      ',datai)
            }
        }
        filepage(yeari,datai,mode,files,redown=redown)
        if (i == length(urls)) cat('\n')
    }
}


filepage <- function(yeari,datai,mode,files,filetable=NULL,cat=TRUE,redown=TRUE,update=FALSE){
    if (missing(mode)) mode <- test_mode()
    # file table
    if (is.null(filetable)){
        filetable <- nhs_files_web(yeari,datai,FALSE)
    }
    if (nrow(filetable)==0) return(invisible())

    if (!missing(files)){
        ckf <- do::file.name(filetable$`Data url`)  |> tolower() |> set::grepl_or(tolower(files))
        filetable <- filetable[ckf,]
    }
    if (cat) cat(paste0(' (',nrow(filetable),')'))
    if (nrow(filetable)==0) return(invisible())
    (fd <- paste0(get_config_path(),'/',prepare_years(yeari),'/',datai))
    if (!dir.exists(fd))  dir.create(path = fd,showWarnings = FALSE,recursive = TRUE)
    if (cat) cat('-->',fd)
    # (j=which(tools::file_ext(tolower(filetable$`Data url`))=='zip'))
    for (j in 1:nrow(filetable)) {
        (tablej <- filetable[j,])
        (xptj <- tablej$`Data url`)
        (docj <- tablej$`DOC  url`)
        (sizej <- tablej$`Data File` |> do::Replace0(c('.*- {0,}','\\].*')))
        (fj <- tolower(do::file.name(xptj)))
        (fn <- sprintf('%s/%s',fd,fj))
        if (cat) cat('\n')
        if (cat) cat(crayon::blue(paste0('           ',j,': ',fj,' (size:',sizej)))
        if (file.exists(fn)){
            (pattern <- paste0(paste0(do::Replace0(fj,'\\.xpt|\\.zip'),c(".sas7bdat",".codebook",".label",".tsv",".update",".xpt")),collapse = '|'))
            (f5 <- list.files(fd,pattern))
            (ck <- length(f5) == 5)
            if (update) ck <- FALSE
            if (!ck){
                # download
                if (redown){
                    nullcon <- file(nullfile(), open = "wb")
                    sink(nullcon, type = "message")
                    wait <- TRUE
                    while (wait) {
                        download <- tryCatch(download.file(xptj, destfile = fn, quiet = FALSE,
                                                           mode=mode),error=function(e) 'e')
                        wait <- ifelse(download=='e',TRUE,FALSE)
                    }
                    sink(type = "message")
                    close(nullcon)
                    if (tools::file_ext(fn) == 'zip'){
                        oldwd <- getwd()
                        zip=fn
                        setwd(do::Replace0(fn,fj))
                        unzip(zipfile = fn,overwrite = TRUE)
                        (fn <- paste0(do::knife_right(fn,3),
                                      unzip(zipfile = fn,overwrite = TRUE,list = TRUE)[,'Name'] |> tools::file_ext()))
                        # unlink(zip)
                        setwd(oldwd)
                    }
                    cat(crayon::red(paste0(' download: ',filesize(fn),')')))
                }else{
                    if (tools::file_ext(fn) == 'zip'){
                        (zip=fn)
                        # xpt
                        (fn <- paste0(do::knife_right(fn,3),
                                      unzip(zipfile = zip,overwrite = TRUE,list = TRUE)[,'Name'] |> tools::file_ext()))
                        if (!file.exists(fn)){
                            oldwd <- getwd()
                            setwd(do::Replace0(zip,fj))
                            unzip(zipfile = zip,overwrite = TRUE)
                            # unlink(zip)
                            setwd(oldwd)
                        }else{
                            if (cat) cat(crayon::blue(paste0(' Exist: ',filesize(zip),')')))
                        }
                    }else{
                        if (cat) cat(crayon::blue(paste0(' Exist: ',filesize(fn),')')))
                    }
                }
            }else{
                if (cat) cat(crayon::blue(paste0(' Exist: ',filesize(fn),')')))
                next(j)
            }
        }else{
            nullcon <- file(nullfile(), open = "wb")
            sink(nullcon, type = "message")
            # download
            wait <- TRUE
            while (wait) {
                download <- tryCatch(download.file(xptj, destfile = fn, quiet = FALSE,
                                                   mode=mode),error=function(e) 'e')
                wait <- ifelse(download=='e',TRUE,FALSE)
            }
            sink(type = "message")
            close(nullcon)
            if (tools::file_ext(fn) == 'zip'){
                oldwd <- getwd()
                zip=fn
                setwd(do::Replace0(fn,fj))
                unzip(zipfile = fn,overwrite = TRUE)
                (fn <- paste0(do::knife_right(fn,3),
                              unzip(zipfile = fn,overwrite = TRUE,list = TRUE)[,'Name'] |> tools::file_ext()))
                # unlink(zip)
                setwd(oldwd)
            }
            cat(crayon::red(paste0(' download: ',filesize(fn),')')))
        }
        # add tsv and label
        # if (cat) cat('-')
        xpt2tsv_label(xpt = fn)
        # codebook
        # if (cat) cat('-')
        (file <- do::Replace(fn,tools::file_ext(fn),'codebook'))
        codebook_url(url=docj,
                     data=datai,
                     file=file)
        # add update
        # if (cat) cat('-')
        (file <- do::Replace(fn,tools::file_ext(fn),'update'))
        write.table(tablej,file,row.names = FALSE,sep = '\t')
    }
}


