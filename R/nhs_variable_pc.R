#' exact variable name
#'
#' @param files file names
#' @param order logical. whether to order by variable
#' @param grep filter variable by grep() function
#' @param exact filter variable by exact match
#' @param ex_grep exclude by grep() function
#' @param ex_exact exclude by exact match
#' @param combine combine variables together
#' @param view logical. Whether to view
#'
#' @return
#' @export
#'
#' @examples
#' \donttest{
#' # variable for demo
#' demo <- nhs_files_pc(pattern = 'demo',file_ext = 'tsv') |>
#'     set::grep_not_or('p_demo')
#'
#' nhs_variable_pc(files = demo,view = T,
#'                 ex_grep = 'wtirep|wtmrep',
#'                 combine = c('aialang&aialanga','dmdborn&dmdborn2&dmdborn4',
#'                             'dmdhrage&dmdhragz','dmdhrbrn&dmdhrbr2&dmdhrbr4',
#'                             'dmdhredu&dmdhredz','dmdhrmar&dmdhrmaz','dmdhsedu&dmdhsedz',
#'                             'dmqmilit&dmqmiliz','indfminc&indfmin2','indhhinc&indhhin2'))
#'
#' }
nhs_variable_pc <- function(files,order=FALSE,
                            grep=NULL,exact=NULL,
                            ex_grep=NULL,ex_exact=NULL,
                            combine=NULL,view=FALSE){

    yeari <- files |>
        do::Replace0(get_config_path() |> do::formal_dir(end.slash = TRUE)) |>
        do::Replace0('/.*')
    df <- lapply(files,function(i){
        namei <- data.table::fread(file = i,check.names = FALSE,showProgress = FALSE,data.table = FALSE,nrows = 1) |>
            colnames()
        matrix(namei,nrow = 1,dimnames = list(NULL,namei)) |>
            as.data.frame()
    }) |>
        do.call(what = plyr::rbind.fill) |>
        t()
    colnames(df) <- yeari
    if (order) df <- df[order(row.names(df)),]
    if (!is.null(grep)){
        ck <- grepl(grep,row.names(df),ignore.case = TRUE)
        df <- df[ck,]
    }
    if (!is.null(ex_grep)){
        ck <- !grepl(ex_grep,row.names(df),ignore.case = TRUE)
        df <- df[ck,]
    }
    if (!is.null(exact)){
        ck <- tolower(row.names(df)) %in% tolower(exact)
        df <- df[ck,]
    }
    if (!is.null(ex_exact)){
        ck <- ! tolower(row.names(df)) %in% tolower(ex_exact)
        df <- df[ck,]
    }
    if (!is.null(combine)){
        for (i in combine) {
            vari <- strsplit(i,' {0,}& {0,}')[[1]]
            dfi <- df[vari,]
            dfi[is.na(dfi)] <- ''
            (dfi <- t(dfi) |> do::paste0_columns(collapse = '/'))
            dfi <- do::Trim(dfi,'/')
            dfi[nchar(dfi)==0] <- NA
            df[vari[1],] <- dfi
            row.names(df)[row.names(df) %in% vari[1]] <- paste0(vari,collapse = '/')
            df <- df[set::not(row.names(df),vari[-1]),]
        }
    }
    if (view) View(df)
    df
}
