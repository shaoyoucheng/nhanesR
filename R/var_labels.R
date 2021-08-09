#' Show labels of colnames
#'
#' @param df one dataframe
#' @param order logical, whether order by colname
#' @return a dataframe contains colname and label
#' @export
#'
var_labels <- function(df,order=FALSE){
    x <- sapply(df, expss::var_lab)
    df <- data.frame(colname=names(x),
                     label=x)
    if (order) df <- df[order(df[,1]),]
    row.names(df)=NULL
    df
}
