.onAttach <- function(...){
    (temp <- config_temp())
    loc_config <- suppressMessages(get_config_years())
    if (!is.null(loc_config)){
        if (!dir.exists(temp)) dir.create(temp,showWarnings = FALSE,recursive = TRUE)
        nhs.path <- list.files(path = temp,pattern = 'path.nhanes',full.names = TRUE)
        if (do::cnOS()){
            restart <- tmcn::toUTF8("\u5DF2\u66F4\u65B0,\u8BF7\u91CD\u65B0\u542F\u52A8R\u6216\u8005RStudio")
            checkyears <- tmcn::toUTF8("\u68C0\u67E5nhanesR\u5305\u5185\u7684\u5E74\u4EFD\u548C\u7F51\u7AD9\u7684\u5E74\u4EFD\u662F\u5426\u4E00\u81F4")
            concord <- tmcn::toUTF8("\u5305\u5185\u5E74\u4EFD\u548C\u7F51\u7AD9\u5E74\u4EFD\u76F8\u540C")
            incon <- tmcn::toUTF8("\u5305\u5185\u5E74\u4EFD\u548C\u7F51\u7AD9\u5E74\u4EFD\u4E0D\u4E00\u81F4,\u8BF7\u91CD\u65B0\u914D\u7F6E\u5E74\u4EFD\u6216\u66F4\u65B0\u6570\u636E\u5E93")
            if (length(nhs.path)==0){
                pathmsg <- tmcn::toUTF8("\u6CA1\u6709\u8BBE\u7F6E\u672C\u5730\u6570\u636E\u5E93\u8DEF\u5F84nhs.path\n\u8BF7\u4F7F\u7528nhs_download()\u51FD\u6570\u4E0B\u8F7D\u6570\u636E\u5E93\n\u4F7F\u7528config_path()\u51FD\u6570\u8FDB\u884C\u9996\u6B21\u914D\u7F6E\u6216\u8005\u91CD\u65B0\u914D\u7F6E\u6570\u636E\u5E93\u5730\u5740\n\u914D\u7F6E\u5B8C\u6210\u624D\u80FD\u4F7F\u7528nhs_read()\u8BFB\u53D6\u672C\u5730\u6570\u636E")
            }else{
                pathmsg <- paste0(tmcn::toUTF8("\u6570\u636E\u5E93\u8DEF\u5F84\u662F: "),get_config_path())
            }
        }else{
            restart <- 'Updated, please restart R or RStudio'
            incon <- 'The year in the package and the year on the website do not match, please re-config years or update database'
            checkyears <- 'Check that the year in the nhanesR package matches the year of the website'
            concord <- 'The year in the package is the same as the year of the website'
            if (length(nhs.path)==0){
                pathmsg <- 'No nhs.path set, please use the nhs_download() function to download the database, use the config_path() function to configure the database address for the first time or reconfigure it'
            }else{
                pathmsg <- paste0('The database path is: ',get_config_path())
            }
        }

        localyears <- loc_config |> do::increase()
        packageStartupMessage(checkyears)
        packageStartupMessage('\nPackage: ',paste0(localyears,collapse = ', '))
        wby <- 'get_years_web() |> do::increase()'
        packageStartupMessage('\nWebsite: ',paste0(wby,collapse = ', '))
        if (all(localyears == wby)){
            packageStartupMessage('\n',concord)
        }else{
            packageStartupMessage(incon)
        }
        packageStartupMessage('\n',pathmsg)
    }else{
        if (do::cnOS()){
            msg <- tmcn::toUTF8("\u5728\u4F7F\u7528nhanesR\u5305\u4E4B\u524D,\u8BF7\u5148\u5B8C\u6210\u4EE5\u4E0B3\u9879\u914D\u7F6E\n     1.\u4F7F\u7528config_path()\u547D\u4EE4\u914D\u7F6E\u6570\u636E\u5E93\u8DEF\u5F84\n     2.\u4F7F\u7528config_years()\u547D\u4EE4\u914D\u7F6E\u6570\u636E\u5E93\u5E74\u4EFD\n     3.\u4F7F\u7528config_data()\u547D\u4EE4\u914D\u7F6E\u6570\u636E\u5E93\u6587\u4EF6\u7C7B\u578B")
        }else{
            msg <- 'No path of NHANES configed'
        }
        packageStartupMessage(msg)
    }

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
