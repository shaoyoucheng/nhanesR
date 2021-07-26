create_table <- function(year,data,int=NULL,char=NULL,time=NULL){
    if (missing(year)) stop('year must not be missing')
    if (missing(data)) stop('data must not be missing')
    if (year != make.names(year)) year <- paste0('"',year,'"')
    if (data != make.names(data)) data <- paste0('"',data,'"')
    x <- list(INT=int,'VARCHAR(50)'=char,'TIMESTAMP(0)'=time)
    x <- x[sapply(x, length) > 0]
    if (length(x)==0) stop('no variable input')
    for (i in 1:length(x)) {
        x[[i]] <- do::Replace0(x[[i]],'\'"')
        x[[i]] <- paste0('"',x[[i]],'"')
    }
    var <- sapply(1:length(x), function(i) paste0(paste(x[[i]], names(x)[i]),collapse = ', \n')) |>
        paste0(collapse = ', \n') |>
        sprintf(fmt = '\n(\n%s\n);')

    head <- 'SET client_min_messages TO WARNING;
    set search_path to %s;
    DROP TABLE IF EXISTS %s CASCADE;
    CREATE TABLE %s%s'
    stmt <<- sprintf(head,year,data,data,var)

    DBI::dbGetQuery(conn = conn$con,
                    statement = stmt)
}

