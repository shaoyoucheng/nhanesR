.onAttach <- function(...){
    (temp <- tempdir() |> do::upper.dir() |> paste0('nhanesR'))
    if (!dir.exists(temp)) dir.create(temp,showWarnings = FALSE,recursive = TRUE)
    nhs.path <- list.files(path = temp,pattern = 'path.nhanes',full.names = TRUE)
    if (do::cnOS()){
        restart <- tmcn::toUTF8("\u5DF2\u66F4\u65B0,\u8BF7\u91CD\u65B0\u542F\u52A8R\u6216\u8005RStudio")
        checkyears <- tmcn::toUTF8("\u68C0\u67E5nhanesR\u5305\u5185\u7684\u5E74\u4EFD\u548C\u7F51\u7AD9\u7684\u5E74\u4EFD\u662F\u5426\u4E00\u81F4")
        concord <- tmcn::toUTF8("\u5305\u5185\u5E74\u4EFD\u548C\u7F51\u7AD9\u5E74\u4EFD\u76F8\u540C")
        incon <- tmcn::toUTF8("\u5305\u5185\u5E74\u4EFD\u548C\u7F51\u7AD9\u5E74\u4EFD\u4E0D\u4E00\u81F4,\u66F4\u65B0\u5305\u5185\u5E74\u4EFD ...")
        if (length(nhs.path)==0){
            pathmsg <- tmcn::toUTF8("\u6CA1\u6709\u8BBE\u7F6E\u672C\u5730\u6570\u636E\u5E93\u8DEF\u5F84nhs.path\n\u8BF7\u4F7F\u7528nhs_download()\u51FD\u6570\u4E0B\u8F7D\u6570\u636E\u5E93\n\u4F7F\u7528nhs_path()\u51FD\u6570\u8FDB\u884C\u9996\u6B21\u914D\u7F6E\u6216\u8005\u91CD\u65B0\u914D\u7F6E\u6570\u636E\u5E93\u5730\u5740\n\u914D\u7F6E\u5B8C\u6210\u624D\u80FD\u4F7F\u7528nhs_read()\u8BFB\u53D6\u672C\u5730\u6570\u636E")
        }else{
            pathmsg <- paste0(tmcn::toUTF8("\u6570\u636E\u5E93\u8DEF\u5F84\u662F: "),read.table(nhs.path)[1,1])
        }
    }else{
        restart <- 'Updated, please restart R or RStudio'
        incon <- 'The year in the package and the year on the website do not match, update the year in the package ...'
        checkyears <- 'Check that the year in the nhanesR package matches the year of the website'
        concord <- 'The year in the package is the same as the year of the website'
        if (length(nhs.path)==0){
            pathmsg <- 'No nhs.path set, please use the nhs_download() function to download the database, use the nhs_path() function to configure the database address for the first time or reconfigure it'
        }else{
            pathmsg <- paste0('The database path is: ',read.table(nhs.path)[1,1])
        }
    }
    packageStartupMessage(checkyears)
    packageStartupMessage('\nPackage: ',paste0(webyears,collapse = ', '))
    wby <- nhs_year_web()
    packageStartupMessage('\nWebsite: ',paste0(wby,collapse = ', '))
    if (all(webyears == wby)){
        packageStartupMessage('\n',concord)
    }else{
        packageStartupMessage(incon)

        libpath <- .libPaths()
        path <- libpath[sapply(libpath,function(i) "nhanesR" %in% list.files(i))]
        syspath <- paste0(path,'/nhanesR/R/sysdata')
        x <- list(d5=d5,webyears=wby,nhs.path=nhs.path)
        for (i in syspath) {
            makeLazyLoadDB(x,i)
        }
        packageStartupMessage(restart)
    }
    packageStartupMessage('\n',pathmsg)
}
filesize <- function(file){
    size <- file.size(file)
    if (size < 1024){
        paste(round(size,2),'b')
    }else if (size < 1024*1024){
        paste(round(size/1024,2),'kb')
    }else if (size < 1024*1024*1024){
        paste(round(size/1024/1024,2),'mb')
    }else if (size < 1024*1024*1024*1024){
        paste(round(size/1024/1024/1024,2),'Gb')
    }else if (size < 1024*1024*1024*1024*1024){
        paste(round(size/1024/1024/1024/1024,2),'TB')
    }
}


size_bt2unit <- function(bt){
    if (bt < 1024){
        paste(round(bt,2),'B')
    }else if (bt < 1024*1024){
        paste(round(bt/1024,2),'KB')
    }else if (bt < 1024*1024*1024){
        paste(round(bt/1024/1024,2),'MB')
    }else if (bt < 1024*1024*1024*1024){
        paste(round(bt/1024/1024/1024,2),'GB')
    }else if (bt < 1024*1024*1024*1024*1024){
        paste(round(bt/1024/1024/1024/1024,2),'TB')
    }
}
