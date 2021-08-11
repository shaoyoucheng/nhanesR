#' Codebook for variable
#'
#' @param ... one(suggest) or more variable names
#' @param files missing(suggest) or file path of nhs_files_pc() or dataframe of nhs_select()
#' @param ignore.case logical
#' @return variable codebook
#' @export
#
nhs_codebook <- function(...,files,ignore.case=FALSE){
    if (missing(files)){
        nhs_codebook.character(...,files=files,ignore.case=ignore.case)
    }else if (is.character(files)){
        nhs_codebook.character(...,files=files,ignore.case=ignore.case)
    }else if (is.data.frame(files)){
        nhs_codebook.dataframe(...,files=files,ignore.case=ignore.case)
    }
}

nhs_codebook.character <- function(...,files,ignore.case=FALSE){
    variable <- c(...) |> tolower()
    if (missing(files)) files <- list.files(get_config_path(),'codebook',full.names = TRUE,
                                            recursive = TRUE)
    ck <- tools::file_ext(files) != 'codebook'
    if (any(ck)){
        ext <- tools::file_ext(files[ck]) |> unique() |> sprintf(fmt = '\\.%s')
        files[ck] <- do::Replace0(files[ck],ext) |> sprintf(fmt = '%s.codebook')
    }
    if (length(variable)==0){
        lapply(files, function(i){
            (Year <- prepare_years(i))
            (Item <- prepare_items(i))
            codei <- read.delim(i,comment.char = '#')
            if (nrow(codei)==0) return()
            codei <- codei[,c('variable','code','label')]
            if (ignore.case) codei$label <- tolower(codei$label)
            cbind(Year=Year,Item=Item,codei)
        }) |> do.call(what = plyr::rbind.fill)
    }else{
        lapply(files, function(i){
            (Year <- prepare_years(i))
            (Item <- prepare_items(i))
            codei <- read.delim(i,comment.char = '#')
            ck <- codei$variable %in% variable
            codei <- codei[ck,c('variable','code','label')]
            if (ignore.case) codei$label <- tolower(codei$label)
            codei <- codei |>
                reshape2::dcast(variable~label,value.var = 'code')
            if (nrow(codei)==0) return()
            cbind(Year=Year,Item=Item,codei)
        }) |> do.call(what = plyr::rbind.fill)
    }
}



nhs_codebook.dataframe <- function(...,files,ignore.case=FALSE){
    variable <- c(...) |> tolower()
    codei <- files[,c('Year',variable)]
    if (ignore.case) codei$variable <- tolower(codei$variable)
    formu <- as.formula(sprintf('Year~%s',paste0(variable,collapse = '+')))
    reshape2::dcast(codei,formu,value.var = variable,fun.aggregate = length)
}
