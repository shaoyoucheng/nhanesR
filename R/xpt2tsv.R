



xpt2tsv <- function(xpt){
    sas <- foreign::read.xport(xpt)
    colnames(sas) <- tolower(colnames(sas))
    file <- do::Replace0(xpt,c('.*/','.*\\\\','.xpt','.XPT'))
    path <- do::knife_right(xpt,nchar(file)+4)
    tsv <- paste0(path,tolower(file),'.tsv')
    data.table::fwrite(sas,tsv,sep = '\t')
}

