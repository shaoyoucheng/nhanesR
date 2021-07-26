

testfile <- function(urls,files,mode){
    for (i in 1:length(urls)) {
        if (i==1){
            cat('\n',urlyear(urls[i]),'\n')
        }else{
            if (urlyear(urls[i]) != urlyear(urls[i-1])){
                cat('\n',do::Replace0(urls[i],'.*='))
            }
        }
        if (i==1){
            cat('      ',urlComponet(urls[i]))
        }else{
            if (urlComponet(urls[i]) != urlComponet(urls[i-1])){
                cat('\n      ',urlComponet(urls[i]))
            }
        }
        # file table
        wait <- TRUE
        while (wait) {
            filetable <- tryCatch(nhs_file_web(urlyear(urls[i]),urlComponet(urls[i]),FALSE),error=function(e) 'e')
            wait <- ifelse(is.character(filetable),TRUE,FALSE)
        }
        if (!missing(files)){
            ckf <- do::file.name(filetable$`Data url`)  |> tolower() |> set::grepl_or(tolower(files))
            filetable <- filetable[ckf,]
        }
        links <- filetable$`Data url`
        size <- filetable$`Data File` |>
            do::Replace0(c('.*- {0,}','\\].*'))
        cat(paste0(' (',length(links),')'))
        if (length(links)==0) next(i)
        for (j in 1:length(links)) {
            fd <- paste0('NHANES/',urlyear(links[j]),'/',urlComponet(urls[i]))
            if (!dir.exists(fd))  dir.create(path = fd,showWarnings = FALSE,recursive = TRUE)
            fj <- tolower(do::file.name(links[j]))
            fn <- sprintf('%s/%s',fd,fj)
            cat('\n')
            cat(crayon::blue(paste0('           ',j,': ',fj,' (size:',size[j])))
            if (file.exists(fn)){
                (pattern <- do::Replace0(fj,'\\..*'))
                (f5 <- list.files(fd,pattern))
                if (length(f5)!=5 & any(grepl('\\.xpt',f5))){
                    nullcon <- file(nullfile(), open = "wb")
                    sink(nullcon, type = "message")
                    # download
                    wait <- TRUE
                    while (wait) {
                        download <- tryCatch(download.file(links[j], destfile = fn, quiet = FALSE,
                                                           mode=mode),error=function(e) 'e')
                        wait <- ifelse(download=='e',TRUE,FALSE)
                    }
                    sink(type = "message")
                    close(nullcon)
                    cat(crayon::blue(paste0(' download: ',filesize(fn),')')))
                }else{
                    cat(crayon::blue(paste0(' Exist: ',filesize(fn),')')))
                    next(j)
                }
            }else{
                nullcon <- file(nullfile(), open = "wb")
                sink(nullcon, type = "message")
                # download
                wait <- TRUE
                while (wait) {
                    download <- tryCatch(download.file(links[j], destfile = fn, quiet = FALSE,
                                                       mode=mode),error=function(e) 'e')
                    wait <- ifelse(download=='e',TRUE,FALSE)
                }
                sink(type = "message")
                close(nullcon)
                cat(crayon::blue(paste0(' download: ',filesize(fn),')')))
            }

            # add update
            cat('-')
            file <- do::Replace(fn,'.xpt','.update')
            write.table(filetable[j,],file,row.names = FALSE,sep = '\t')
            # add tsv
            cat('-')
            if (do::right(fj,3)=='xpt') xpt2tsv(xpt = fn)
            # add label
            cat('-')
            if (do::right(fj,3)=='xpt') xpt2label(xpt = fn)
            # cookbook
            cat('-')
            (file <- do::Replace(fn,'.xpt','.cookbook'))
            wait <- TRUE
            while (wait) {
                cook <- tryCatch(cookbook_url(url=filetable$`DOC  url`[j],
                                            data=filetable$data[1],
                                            file=file),
                                 error=function(e) 'e')
                wait <- ifelse(cook == 'e',TRUE,FALSE)
            }

        }
    }
}
