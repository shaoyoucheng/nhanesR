

yearslong <- list.files(path,full.names = TRUE)
for (i in 1:length(years)) {
    cat('\n',crayon::bgWhite(years[i]))
    data <- list.files(yearslong[i])
    datalong <- list.files(yearslong[i],full.names = TRUE)
    for (j in 1:length(datalong)) {
        cat('\n',crayon::red(data[j]),'\n')
        xpt <- list.files(datalong[j]) |> do::Replace0(c('.xpt','.XPT')) |> tolower()
        xptlong <- list.files(datalong[j],full.names = TRUE)
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


year <- years[1]
data <- d5[1]
datafiles <- list.files(paste0(path,'/',year,'/',data),full.names = TRUE)


x <- foreign::read.xport(datafiles[1])
colnames(x) <- tolower(colnames(x))
nms <- do::numeric.nms(x)
cms <- set::not(colnames(x),nms)


tbl <- paste0(d5[1], '-',tolower(do::Replace0(datafiles[1],c('.*/','.xpt','.XPT'))))
create_table(year = schema[1],
             data = tbl,
             int = nms,char = cms)

label <- do::Replace0(foreign::lookup.xport(datafiles[1])$DEMO$label,"'")
name <- tolower(foreign::lookup.xport(datafiles[1])$DEMO$name)

