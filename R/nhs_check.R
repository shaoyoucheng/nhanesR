#' Check error files
#' The error file is defined by the absence of the following five formats: xpt or sas7bdat,
#' tsv, label, codebook and update.
#' @param years one or more years
#' @param data one or more data
#'
#' @return if error files exist, green text will be print.
#' @export
#'
#' @examples
#' \donttest{
#' nhs_check()
#' }
nhs_check <- function(years,data){
    (years <- prepare_years(years))
    (data <- prepare_data(data))
    (dt <- rep(data,each=length(years)))
    (ys <- rep(years,length(data)))
    fmt <- paste0(get_config_path(),'/%s/%s')
    (nhs_dir <- sprintf(fmt,ys,dt) |> do::increase())
    ext <- c("\\.codebook", "\\.label", "\\.tsv", "\\.update", "\\.xpt","\\.sas7bdat")
    for (i in 1:length(nhs_dir)) {
        yeari <- nhs_dir |>
            do::select(i) |>
            do::Replace0(get_config_path()) |>
            stringr::str_extract_all('[0-9]{4}-[0-9]{4}') |>
            do::select(1,drop=TRUE)
        yeari
        datai <- nhs_dir |>
            do::select(i) |>
            do::Replace0(get_config_path()) |>
            do::Replace0(yeari) |>
            do::Replace0('/{2,}')
        datai
        if (i==1){
            cat('\n',yeari)
        }else{
            yeari1 <- nhs_dir |>
                do::select(i-1) |>
                do::Replace0(get_config_path()) |>
                stringr::str_extract_all('[0-9]{4}-[0-9]{4}') |>
                do::select(1,drop=TRUE)
            if (yeari != yeari1){
                cat('\n',yeari)
            }
        }
        fn <- list.files(nhs_dir[i],paste0(ext,collapse = '|')) |>
            do::Replace0(ext) |>
            unique()
        fn
        which(fn=='paxraw_c')
        if (length(fn)==0) next(i)
        for (j in 1:length(fn)) {
            fnj <- fn[j]
            len <- list.files(nhs_dir[i]) %in% do::Replace0(paste0(fnj,ext),'\\\\') |>
                sum()
            len
            if (len==5) next(j)
            if (i==1){
                cat('\n     ',datai)
            }else{
                datai1 <- nhs_dir |>
                    do::select(i-1) |>
                    do::Replace0(get_config_path()) |>
                    do::Replace0(yeari) |>
                    do::Replace0('/{2,}')
                if (datai != datai1){
                    cat('\n     ',datai)
                }
            }
            cat(crayon::green(paste0('\n          ',fnj,' ',len)))
        }
    }
}
