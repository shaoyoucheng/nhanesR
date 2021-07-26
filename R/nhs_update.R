nhs_update <- function(years,data){
    ck <- ! '.update' %in% ls(envir = .GlobalEnv,all.names = TRUE)
    if (ck) do::exec('.update <- new.env()',envir = .GlobalEnv)
    if (! 'data' %in% ls(envir = .update)) .update$data=c()
    years <- prepare_years_pc(years)
    data <- prepare_data(data)
    for (i in years) {
        message(i)
        for (j in data) {
            cat('   ',j,'\n')
            ij <- paste0(i,'/',j)
            if (ij %in% .update$data) next(j)
            wait <- TRUE
            while (wait) {
                info <- tryCatch(nhs_file_web(do::Replace0(i,'-.*'),j,cat = FALSE),
                         error=function(e) 'e')
                if (is.character(info)){
                    wait <- TRUE
                }else{
                    wait <- FALSE
                }
            }
            if (anyNA(info)) info <- suppressMessages(do::complete.data(x))
            fn <- do::file.name(info$`Data url`) |>
                tolower() |>
                do::Replace0('.xpt')
            for (k in 1:nrow(info)) {
                file <- do::fmt('/ // // // .update',nhs.path,i,j,fn[k])
                write.table(info[k,],file,row.names = FALSE,sep = '\t')
            }
            .update$data <- c(.update$data,ij)
        }
    }
}
