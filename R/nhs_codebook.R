#' Codebook for variable
#'
#' @param ... one(suggest) or more variable names
#' @param files missing(suggest) or file path of nhs_files_pc() or dataframe of nhs_select()
#'
#' @return variable codebook
#' @export
#
nhs_codebook <- function(...,files){
    if (missing(files)){
        nhs_codebook.character(...,files)
    }else if (is.character(files)){
        nhs_codebook.character(...,files)
    }else if (is.data.frame(files)){
        nhs_codebook.dataframe(...,files)
    }
}

nhs_codebook.character <- function(...,files){
    if (do::cnOS()){
        miss2 <- tmcn::toUTF8("\u6CA1\u6709\u8F93\u5165\u6570\u636E\u6216\u8005\u53D8\u91CF\u540D")
    }else{
        miss2 <- 'No input of data or variable names'
    }
    variable <- c(...) |> tolower()
    if (length(variable)==0 & missing(files)) stop(miss2)
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



nhs_codebook.dataframe <- function(...,files){
    variable <- c(...) |> tolower()
    codei <- files[,c('Year',variable)]
    formu <- as.formula(sprintf('Year~%s',paste0(variable,collapse = '+')))
    reshape2::dcast(codei,formu,value.var = variable,fun.aggregate = length)
}
