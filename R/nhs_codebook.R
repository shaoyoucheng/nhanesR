#' Codebook for variable
#'
#' @param files missing(suggest) or file path of nhs_files_pc() or dataframe of nhs_select()
#' @param ... one(suggest) or more variable names
#'
#' @return variable codebook
#' @export
#
nhs_codebook <- function(files,...){
    if (missing(files)){
        nhs_codebook.character(files,...)
    }else if (is.character(files)){
        nhs_codebook.character(files,...)
    }else if (is.data.frame(files)){
        nhs_codebook.dataframe(files,...)
    }
}

nhs_codebook.character <- function(files,...){
    variable <- c(...)
    if (missing(files)) files <- list.files(get_config_path(),'codebook',full.names = TRUE,
                                            recursive = TRUE)
    ck <- tools::file_ext(files) != 'codebook'
    if (any(ck)){
        ext <- tools::file_ext(files[ck]) |> unique() |> sprintf(fmt = '\\.%s')
        files[ck] <- do::Replace0(files[ck],ext) |> sprintf(fmt = '%s.codebook')
    }
    lapply(files, function(i){
        Year <- do::Replace0(i,get_config_path() |> do::formal_dir(T)) |>
            stringr::str_extract('[0-9]{4}-[0-9]{4}')
        Item <- do::Replace0(i,
                             get_config_path() |> do::formal_dir(T),
                             do::formal_dir(Year,T),
                             '/.*')
        codei <- read.delim(i,comment.char = '#')
        ck <- codei$variable %in% variable
        codei <- codei[ck,c('variable','label')]
        if (nrow(codei)==0) return()
        cbind(Year=Year,Item=Item,codei)
    }) |> do.call(what = rbind) |>
        reshape2::dcast(Year+Item+variable~label,value.var = 'label')
}



nhs_codebook.dataframe <- function(files,...){
    variable <- c(...)
    codei <- files[,c('Year',variable)]
    formu <- as.formula(sprintf('Year~%s',paste0(variable,collapse = '+')))
    reshape2::dcast(codei,formu,value.var = variable,fun.aggregate = length)
}
