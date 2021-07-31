# codebook <- function(years,data,files,nhs_dir='./Nhanes'){
#     years <- prepare_years_pc(years)
#     data <- prepare_data(data)
#     for (i in years) {
#         for (j in data) {
#             x <- nhs_file_web(2001,'die',FALSE)
#             doc_url <- x$`DOC  url`
#             if (!missing(files)){
#                 fn <- do::file.name(x$`DOC  url`)
#                 doc_url <- doc_url[set::grepl_or(tolower(fn),tolower(files))]
#             }
#             for (k in doc_url) {
#                 codebook_url(url = k,data= x$data[1],nhs_dir=nhs_dir)
#             }
#         }
#     }
# }
codebook_url <- function(url,data,file){
    if (tools::file_ext(url)=='pdf'){
        pdf <- paste0(do::Replace0(file,tools::file_ext(file)),'pdf')
        cat(crayon::bgWhite(' pdf'))
        nullcon <- file(nullfile(), open = "wb")
        sink(nullcon, type = "message")
        # download
        download.file(url,pdf)
        sink(type = "message")
        close(nullcon)
        if (!file.exists(file)){

            df <- data.frame(x=1,y=2,z=3)
            df <- df[-c(1:nrow(df)),]
            colnames(df) <- c('variable','code','label')
            write.table("#firs_publish:pdf",file,row.names = FALSE,col.names = FALSE,quote = FALSE)
            write.table('#last_revise:pdf',file,row.names = FALSE,col.names = FALSE,append = TRUE,quote = FALSE)
            suppressWarnings(write.table(df,file,append = TRUE,sep = '\t',row.names = FALSE))

        }

        return(invisible('pdf'))
    }
    wait <- TRUE
    while (wait) {
        html <- tryCatch(xml2::read_html(url), error=function(e) 'e')
        wait <- ifelse(is.character(html),TRUE,FALSE)
    }
    firs_publish <- html |>
        rvest::html_elements(xpath = '//div[@id="PageHeader"]//h5') |>
        set::grep_and('First Published') |>
        rvest::html_text() |>
        do::fmt(x = '#/ ') |>
        do::Replace0('\t','\n','\r')
    last_revise <- html |>
        rvest::html_elements(xpath = '//div[@id="PageHeader"]//h5') |>
        set::grep_and('Last Revised') |>
        rvest::html_text() |>
        do::fmt(x = '#/ ') |>
        do::Replace0('\t','\n','\r')
    codebook <- html |>
        rvest::html_elements(xpath = '//div[@id="Codebook"]//div[@class="pagebreak"]')|>
        set::grep_and(c('dl','table'))
    if (length(codebook)==0){
        df <- data.frame(x=1,y=2,z=3)
        df <- df[-c(1:nrow(df)),]
        colnames(df) <- c('variable','code','label')
        write.table(firs_publish,file,row.names = FALSE,col.names = FALSE,quote = FALSE)
        write.table(last_revise,file,row.names = FALSE,col.names = FALSE,append = TRUE,quote = FALSE)
        suppressWarnings(write.table(df,file,append = TRUE,sep = '\t',row.names = FALSE))
        return('no codebook')
    }
    df <- codebook |>
        lapply(label_table) |>
        do.call(what = plyr::rbind.fill) |>
        do::select(j=1:3)
    colnames(df) <- tolower(colnames(df))
    df
    # delete missing
    df <- df[tolower(df$"value description") != 'missing',]
    # delete continuous variable
    df <- df[tolower(df$"value description") != 'range of values',]
    df
    if (nrow(df)==0 | all(table(df[,'varibale'])==1)){
        df <- df[-c(1:nrow(df)),]
        colnames(df) <- c('variable','code','label')
        write.table(firs_publish,file,row.names = FALSE,col.names = FALSE,quote = FALSE)
        write.table(last_revise,file,row.names = FALSE,col.names = FALSE,append = TRUE,quote = FALSE)
        suppressWarnings(write.table(df,file,append = TRUE,sep = '\t',row.names = FALSE))
        invisible('ok')
    }else{
        cato <- c('sddsrvyr',names(table(df$varibale)[table(df$varibale)>1]))
        df <- df[df$varibale %in% cato,]
        colnames(df) <- c('variable','code','label')
        id <- as.character(factor(df$variable,levels = unique(df$variable),labels = 1:length(unique(df$variable))))
        df <- cbind(id,df)
        write.table(firs_publish,file,row.names = FALSE,col.names = FALSE,quote = FALSE)
        write.table(last_revise,file,row.names = FALSE,col.names = FALSE,append = TRUE,quote = FALSE)
        suppressWarnings(write.table(df,file,append = TRUE,sep = '\t',row.names = FALSE))
        invisible('ok')
    }
}





label_table <- function(div){
    varibale <- div |>
        rvest::html_elements(xpath = 'dl/dd[1]') |>
        rvest::html_text() |>
        tolower()

    label <- div |>
        rvest::html_elements(xpath = 'table') |>
        rvest::html_table() |>
        listn() |>
        as.data.frame()

    cbind(varibale,label)
}







