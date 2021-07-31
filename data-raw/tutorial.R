# 有哪些年分
nhs_year_web() # 网站 web

nhs_year_web(range = FALSE)

# 有哪些数据类型
#-- ONE year data
nhs_data_web(years = 2017)
#-- two years data
nhs_data_web(years = c(2015,2017))
#-- all years, if years if missing
nhs_data_web()

# 有哪些数据文件
x <- nhs_file_web(years = 1999:2011,data = 'demo')
nhs_view(x)

x <- nhs_file_web(data = 'demo')
nhs_view(x)


# *******配置*******
config_temp()
config_path('F:/NHANES')
config_years()
config_data()

get_path_config()
get_years_config()
get_data_config()

# download data
nhs_download(mode = 'wb')

# 读取demo数据，提取重要的列，合并
demo <- nhs_read('demo',cookbook = TRUE,var.label = TRUE)

demo_column <- c("seqn", "sddsrvyr", "riagendr", "ridageyr", "ridagemn", "ridexprg",
                 "ridreth1", "sdmvpsu", "sdmvstra", "wtint2yr", "wtmec2yr")
demo2 <- nhs_select(demo,demo_column)
demo_df <- data.table::rbindlist(demo2,use.names = TRUE,fill = TRUE)








