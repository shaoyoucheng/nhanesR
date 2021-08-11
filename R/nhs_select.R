#' Rename column names for result of nhs_read()
#'
#' @param data result of nhs_read()
#' @param ... paired names, connected by colon.
#' @param select logical. whether to select the
#' @param force_bind logical.
#'
#' @return
#' @export
#'
nhs_select <- function(data,...,force_bind=FALSE){
    names <- c(...) |> tolower()
    # return(names)
    (from <- strsplit(names,' {0,}: {0,}') |> do::select(j=1) |> unlist())
    (select <- strsplit(from,',') |> unlist() |> unique())
    (to <- strsplit(names,' {0,}: {0,}') |> do::select(j=2) |> unlist())
    for (i in 1:length(data)) {
        di <- data[[i]]
        # select
        di <- di[,colnames(di) %in% select,drop=FALSE]
        # rename
        for (j in 1:length(from)) {
            if (is.na(to[j])) next(j)
            fromj <- from[j] |> strsplit(' {0,}, {0,}') |> unlist() |> unique()
            colnames(di)[colnames(di) %in% fromj] <- to[j]
        }
        data[[i]] <- di
    }
    if (length(data)==1){
        data
    }else{
        (ck <- sapply(2:length(data), function(i) suppressWarnings(do::increase(colnames(data[[1]])) == do::increase(colnames(data[[i]])))))
        if (all(ck)){
            for (i in 1:length(data)) {
                di <- cbind(Year=names(data)[i],data[[i]])
                data[[i]] <- di
            }
            names(data)=NULL
            do.call(rbind,data) |> as.data.frame()
        }else{
            if (force_bind){
                for (i in 1:length(data)) {
                    data[[i]] <- cbind(Year=names(data)[i],data[[i]])
                }
                do.call(plyr::rbind.fill,data)
            }else{
                data
            }
        }
    }


}
