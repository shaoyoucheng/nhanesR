

test_mode <- function(years=1999,items='die'){
    fl <- nhs_files_web(years,items,FALSE)
    ck <- fl$`Data File` |> set::grepl_and('kb')
    fl <- fl[ck,]
    url <- fl$`Data url`[1]
    xpt <- tempfile(fileext = '.xpt')
    mode <- c("wb", "w", "ab")
    for (i in mode) {
        x <- tryCatch(download.file(url = url,
                                    destfile = xpt,
                                    mode = i,
                                    quiet = TRUE),error=function(e) 'e',warning=function(w)'w')
        if (x==0){
            x <- tryCatch(haven::read_xpt(xpt),error = function(e) 'e')
            if (is.data.frame(x)) return(i)
        }
    }
}
