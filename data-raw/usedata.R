nhs.path <- 'f:/Nhanes/'
d5 <- c("Demographics", "Dietary", "Examination", "Laboratory", "Questionnaire")
webyears <- c("2019-2020", "2017-2018", "2015-2016", "2013-2014", "2011-2012",
              "2009-2010", "2007-2008", "2005-2006", "2003-2004", "2001-2002",
              "1999-2000")
nhs.path <- NULL
usethis::use_data(d5,webyears,nhs.path,internal = TRUE,overwrite = TRUE)

