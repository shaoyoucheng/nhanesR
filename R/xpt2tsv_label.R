



xpt2tsv_label <- function(xpt){
    ext <- tools::file_ext(xpt)
    if (ext=='xpt'){
        sas <- haven::read_xpt(xpt)
    }else{
        sas <- haven::read_sas(xpt)
    }

    colnames(sas) <- tolower(colnames(sas))
    (tsv <- do::Replace(xpt,paste0('\\.',ext),'.tsv'))
    data.table::fwrite(sas,tsv,sep = '\t')

    # label
    label <- purrr::map_chr(sas,purrr::attr_getter('label'))
    labeldf <- data.frame(name=tolower(names(label)),
               label=label)
    (label <- do::Replace(xpt,paste0('\\.',ext),'.label'))
    write.table(x = labeldf,file = label,sep = '\t',eol = '\n',row.names = FALSE)
}

