# 有哪些年分
nhs_year_web() # 网站 web
nhs_year_pc() # 本地 local pc
nhs_year_pg() # PostgreSQL

nhs_year_web(range = FALSE)
nhs_year_pc(range = FALSE)
nhs_year_pg(range = FALSE)

# 有哪些数据类型
#-- ONE year data
nhs_data_web(years = 2017)
nhs_data_pc(years = 2017)
nhs_data_pg(years = 2017)
#-- two years data
nhs_data_web(years = c(2015,2017))
nhs_data_pc(years = c(2015,2017))
nhs_data_pg(years = c(2015,2017))
#-- all years, if years if missing
nhs_data_web()
nhs_data_pc()
nhs_data_pg()



# 有哪些数据文件
x <- nhs_file_web(data = 'demo')
nhs_view(x)

nhs_file_pc(data = 'demo')


# 读取demo数据，提取重要的列，合并
demo <- nhs_read('demo',cookbook = TRUE,var.label = TRUE)

demo_column <- c("seqn", "sddsrvyr", "riagendr", "ridageyr", "ridagemn", "ridexprg",
                 "ridreth1", "sdmvpsu", "sdmvstra", "wtint2yr", "wtmec2yr")
demo2 <- nhs_select(demo,demo_column)
demo_df <- data.table::rbindlist(demo2,use.names = TRUE,fill = TRUE)








