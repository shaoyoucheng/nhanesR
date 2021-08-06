to_sql <- function(x,name,before='',after=''){
    (name <- sprintf('LOWER(%s)',name))
    x <- x |>   # left quote
        do::Replace(' {0,}\\| {0,}','\\|') |>
        do::Replace(' {0,}\\& {0,}','\\&') |>
        do::Replace(' {0,}\\( {0,}','\\(') |>
        do::Replace(' {0,}\\) {0,}','\\)') |>
        do::Trim() |>        tolower()
    x
    lq=0
    while (do::left(x,1) == '('){
        lq <- lq + 1
        x <- do::knife_left(x,1)
    }
    rq=0
    while (do::right(x,1) == '('){
        rq <- rq + 1
        x <- do::knife_right(x,1)
    }
    lq_in <- stringr::str_extract_all(x,'[\\|\\&]\\({0,}')[[1]] |> # for OR AND
        nchar_max() |>         qoute_n('left')
    rq_in <- stringr::str_extract_all(x,'\\){0,}[\\|\\&]')[[1]] |>
        nchar_max() |>        qoute_n('right')
    for (i in lq_in:0) {
        for (j in rq_in:0) {
            from <- paste0(paste0(rep(')',j),collapse = ''), # OR
                           '\\|',
                           paste0(rep('(',i),collapse = ''))
            to <- paste0(paste0(after,"%'"),
                         paste0(rep(')',j),collapse = ''),
                         ' OR ',
                         paste0(rep(')',i),collapse = ''),
                         paste0(' ',name," like '%",before))
            x <- gsub(from,to,x)
            # AND
            from <- paste0(paste0(rep(')',j),collapse = ''), '\\&', paste0(rep('(',i),collapse = ''))
            to <- paste0(paste0(after,"%'"),
                         paste0(rep(')',j),collapse = ''), ' AND ', paste0(rep(')',i),collapse = ''),
                         paste0(' ',name," like '%",before))
            x <- gsub(from,to,x)
        }
    }
    paste0(
        paste0(paste0(rep('(',lq),collapse = ''),name," like '%",before),
        x,
        paste0(paste0(rep(')',rq),collapse = ''),after,"%'")

    )
}




qoute_n <- function(x,pos='left'){
    if (length(x) ==0 ) return(0)
    if (pos == 'left'){
        sum(strsplit(x,'')[[1]] == '(')
    }else{
        sum(strsplit(x,'')[[1]] == ')')
    }
}
nchar_max <- function(x){
    x[which.max(nchar(x))]
}
