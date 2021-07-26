

#' Connect 'NHANES' database from 'PostgreSQL'
#'
#' @param user user name
#' @param password password
#' @param dbname database name
#' @param ... ignore
#'
#' @return connection to 'PostgreSQL'
#' @export
#'
#' @examples
#' \donttest{
#' connect_NHANES <- connect_NHANES()
#' }
connect_NHANES <- function(user='postgres',password = 'pg',
                           dbname='nhanes',...){
    con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                          user = user,
                          password = password,
                          dbname=dbname,
                          ...)
    dbplyr::src_dbi(con, auto_disconnect = TRUE)
}
