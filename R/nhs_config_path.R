#' Config path of 'NHANES' database
#'
#' @param nhs.path path of 'NHANES' database
#'
#' @return change the internal path
#' @export
#'
nhs_config_path <- function(nhs.path){
    if (do::cnOS()){
        setpath <- tmcn::toUTF8("\u8BBE\u7F6ENHANES\u6570\u636E\u8DEF\u5F84\u81F3: ")
    }else{
        setpath <- 'set NHANES path to: '
    }
    (temp <- tempdir() |> do::upper.dir() |> paste0('nhanesR'))
    if (!dir.exists(temp)) dir.create(temp,showWarnings = FALSE,recursive = TRUE)
    nhs_path <- paste0(temp,'/path.nhanes')
    if (do::right_equal(nhs.path,'nhanes')){
        write.table(x = nhs.path,file = nhs_path,row.names = FALSE,col.names = FALSE)
        cat(setpath,nhs.path)
    }else if(do::right_equal(nhs.path,'nhanes/')){
        write.table(x = nhs.path,file = nhs_path,row.names = FALSE,col.names = FALSE)
        cat(setpath,nhs.path)
    }else{
        path <- list.files(nhs.path,full.names = TRUE)[tolower(list.files(nhs.path)) == 'nhanes']
        if (length(path)==0){
            nhs.path <- paste0(nhs.path,'/','NHANES')
            dir.create(nhs.path,showWarnings = FALSE,recursive = TRUE)
            write.table(x = nhs.path,file = nhs_path,row.names = FALSE,col.names = FALSE)
            cat(setpath,nhs.path)
        }else{
            write.table(x = path,file = nhs_path,row.names = FALSE,col.names = FALSE)
            cat(setpath,path)
        }
    }
}



