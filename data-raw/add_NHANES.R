#' add functions, schema and tables
#' create 13 funtions, shema mimic3 and 26 tables
#' @param conn connection, if missing, it will be get 'connectiontopsql' from global environment.
#'
#' @return 13 funtions, shema mimic3 and 26 tables in PostgreSQL.
#' @export
#'
#' @examples
#' \donttest{
#' addInfo_3()
#' }
add_NHANES <- function(path,year.begin=1,user='postgres',password = 'pg'){
    con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                          user = user,
                          password = password)
    conn <- dbplyr::src_dbi(con, auto_disconnect = TRUE)
    # create database
    datname <- as.data.frame(dplyr::tbl(conn,dbplyr::sql('SELECT datname FROM pg_database')))[,1]
    if (! 'nhanes' %in% tolower(datname)){
        message('\ncreate database nhanes')
        DBI::dbGetQuery(conn = conn$con,
                        statement = "CREATE DATABASE nhanes;")
    }
    con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                          user = user,
                          password = password,
                          dbname='nhanes')
    conn <- dbplyr::src_dbi(con, auto_disconnect = TRUE)
    # add schema -------
    years <- list.files(path)
    for (i in years) {
        message('add schema ',i)
        statement <- sprintf('CREATE SCHEMA IF NOT EXISTS "%s"',i)
        DBI::dbGetQuery(conn = conn$con,
                        statement = statement)

    }
    message('\nadd tables names, columns and comments')

    yearslong <- list.files(path,full.names = TRUE)
    for (i in 1:length(years)) {
        cat('\n',crayon::bgWhite(i,': ',years[i]))
        data <- list.files(yearslong[i])
        datalong <- list.files(yearslong[i],full.names = TRUE)
        for (j in 1:length(datalong)) {
            cat('\n',crayon::red(data[j]),'\n')
            xpt <- list.files(datalong[j],pattern = '.xpt',ignore.case = TRUE) |> do::Replace0(c('.xpt','.XPT')) |> tolower()
            xptlong <- list.files(datalong[j],full.names = TRUE,pattern = '.xpt',ignore.case = TRUE)
            pb <- txtProgressBar(max = length(xptlong),width = 30,style = 3)
            for (k in 1:length(xptlong)) {
                setTxtProgressBar(pb,k)
                lkup <- foreign::lookup.xport(xptlong[k])[[1]]
                type <- lkup$type
                name <- lkup$name |> tolower()
                Encoding(lkup$label) <- 'latin1'
                (label <-  lkup$label |>
                        do::Replace0(c("'",'"',hriline)) |>
                        do::Replace(u,'u'))
                # usethis::use_data(hriline,u,d5,internal = T,overwrite = T)
                tbl <- paste0(data[j],'---',xpt[k])
                (nms <- name[type=='numeric'])
                (chr <- name[type!='numeric'])

                # add table
                create_table(year = years[i],
                             data = tbl,
                             int = nms,
                             char = chr)
                # add commet for variable
                comment <- sprintf('COMMENT ON COLUMN "%s"."%s"."%s" IS \'%s\';',
                                   years[i],
                                   tbl,
                                   name,
                                   label) |>
                    paste0(collapse = '\n')

                DBI::dbGetQuery(conn = conn$con,
                                statement = comment)


                if (k==length(xpt)) cat(crayon::red(sprintf('(%s)',length(xpt))))
            }

        }
    }
    message('\nDONE !!! ')
}




