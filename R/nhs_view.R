
#' View result of nsh_datafile()
#'
#' @param x result of nsh_datafile()
#' @param ... highlight
#' @return data in viewer pannel in RStudio
#' @export
#'
nhs_view <- function(x,...) UseMethod('nhs_view')

#' @method nhs_view nhs_file
#' @export
#'
nhs_view.nhs_file <- function(x,...){
    kableExtra::kbl(x[,1:6],
                    escape = FALSE,
                    align=c('c','l','l','l','l','l')) |>
        kableExtra::kable_paper("striped") |>
        kableExtra::column_spec(4,link = x$`DOC  url`) |>
        kableExtra::column_spec(5,link = x$`Data url`) |>
        kableExtra::row_spec(0,align = 'c')

}

