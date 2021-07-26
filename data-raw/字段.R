demo <- nhs_read('demo')
demo$`1999-2000` |> dim()

demo$`1999-2000` |> colnames_labels(T) |> View()


demo$`1999-2000`$seqn |> unique()
demo$`1999-2000`$seqn |> range()


demo$`1999-2000`$sddsrvyr |> unique()
demo$`1999-2000`$sddsrvyr |> table(useNA = 'i')

demo$`1999-2000`$ridstatr |> unique()
demo$`1999-2000`$ridstatr |> table(useNA = 'i')

demo$`1999-2000`$ridexmon |> unique()
demo$`1999-2000`$ridexmon |> table(useNA = 'i')

demo$`1999-2000`$riagendr |> unique()
demo$`1999-2000`$riagendr |> table(useNA = 'i')

demo$`1999-2000`$ridageyr |> unique()
demo$`1999-2000`$ridageyr |> table()
demo$`1999-2000`$ridageyr |> hist(breaks = seq(0,85,5))


demo$`1999-2000`$ridagemn |> unique()
demo$`1999-2000`$ridagemn |> table(useNA = 'i')

demo$`1999-2000`$ridageex |> unique()


demo$`1999-2000`$ridreth1 |> unique()
demo$`1999-2000`$ridreth1 |> table()

demo$`1999-2000`$ridreth2 |> unique()
demo$`1999-2000`$ridreth2 |> table()

demo$`1999-2000`$dmqmilit |> unique()
demo$`1999-2000`$dmqmilit |> table(useNA = 'i')

demo$`1999-2000`$dmdborn |> unique()
demo$`1999-2000`$dmdborn |> table()


demo$`1999-2000`$indfminc |> unique()
demo$`1999-2000`$indfminc |> table()


demo$`1999-2000`$indhhinc |> unique()
demo$`1999-2000`$indhhinc |> table(useNA = 'ifany')



demo$`1999-2000`$dmdmartl |> unique()
demo$`1999-2000`$dmdmartl |> table(useNA = 'ifany')


demo$`1999-2000`$ridexprg |> unique()
demo$`1999-2000`$ridexprg |> table(useNA = 'i')


demo$`1999-2000`$dmdcitzn |> unique()
demo$`1999-2000`$dmdcitzn |> table(useNA = 'i')



demo$`1999-2000`$dmdyrsus |> unique()
demo$`1999-2000`$dmdyrsus |> table(useNA = 'i')

demo$`1999-2000`$dmdhhsiz |> unique()
demo$`1999-2000`$dmdhhsiz |> table(useNA = 'i')

demo$`1999-2000`$dmdeduc3 |> unique()
demo$`1999-2000`$dmdeduc3 |> table(useNA = 'i')

demo$`1999-2000`$dmdeduc2 |> unique()
demo$`1999-2000`$dmdeduc2 |> table(useNA = 'i')

demo$`1999-2000`$dmdschol |> unique()
demo$`1999-2000`$dmdschol |> table(useNA = 'i')


demo$`1999-2000`$dmdeduc |> unique()
demo$`1999-2000`$dmdeduc |> table(useNA = 'i')


demo$`1999-2000`$dmdhredu |> unique()
demo$`1999-2000`$dmdhredu |> table(useNA = 'i')


demo$`1999-2000`$dmdhsedu |> unique()
demo$`1999-2000`$dmdhsedu |> table(useNA = 'i')

demo$`1999-2000`$wtint2yr
demo$`1999-2000`$wtint4yr




