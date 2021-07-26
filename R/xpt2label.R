xpt2label <- function(xpt){
    lkup <- foreign::lookup.xport(xpt)[[1]]
    label <- data.frame(name=tolower(lkup$name),
                        label=lkup$label)
    file <- do::Replace0(xpt,c('.*/','.*\\\\','.xpt','.XPT'))
    path <- do::knife_right(xpt,nchar(file)+4)
    to <- paste0(path,tolower(file),'.label')
    write.table(x = label,file = to,sep = '\t',eol = '\n',row.names = FALSE)
}

# x <- list.files(nhs.path,full.names = TRUE,recursive = T,pattern = '.XPT',ignore.case = TRUE)
# pb <- txtProgressBar(max = length(x),width = 30,style = 3)
# for (i in 1:length(x)) {
# setTxtProgressBar(pb,i)
# xpt2label(x[i])
# }
