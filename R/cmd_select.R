#' Select column for results of nhs_read() function
#'
#' @param x results of nhs_read() function
#' @param ... one ore more columns
#'
#' @return selected column data
#' @export
#'
nhs_select <- function(x,...){
    col_names <- c(...)
    if (is.list(x)){
        for (i in do::seq_range(x)) {
            dfi <- x[[i]]
            x[[i]] <- dfi[,colnames(dfi) %in% col_names]
        }
    }else if(is.data.frame(x)){
        x[,colnames(x) %in% col_names]
    }
    x
}
