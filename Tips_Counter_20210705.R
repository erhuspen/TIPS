df_2021 = read_xlsx('C:/Users/ehusp/OneDrive/Documents/MSA/TIPS/master_tips_sheet.xlsx')
df_2020 = read_xlsx('C:/Users/ehusp/OneDrive/Documents/MSA/TIPS/master_tips_sheet.xlsx', sheet = '2020')
df_2019 = read_xlsx('C:/Users/ehusp/OneDrive/Documents/MSA/TIPS/master_tips_sheet.xlsx', sheet = '2019')
df_2018 = read_xlsx('C:/Users/ehusp/OneDrive/Documents/MSA/TIPS/master_tips_sheet.xlsx', sheet = '2018')
df_2017 = read_xlsx('C:/Users/ehusp/OneDrive/Documents/MSA/TIPS/master_tips_sheet.xlsx', sheet = '2017')
df_2016 = read_xlsx('C:/Users/ehusp/OneDrive/Documents/MSA/TIPS/master_tips_sheet.xlsx', sheet = '2016')

m_2021 = as.matrix(df_2021)  
m_2020 = as.matrix(df_2020)
m_2019 = as.matrix(df_2019)
m_2018 = as.matrix(df_2018)
m_2017 = as.matrix(df_2017)
m_2016 = as.matrix(df_2016)

m_2021[is.na(m_2021)] <- 'zxzxzx'   
m_2020[is.na(m_2020)] <- 'zxzxzx'
m_2019[is.na(m_2019)] <- 'zxzxzx'
m_2018[is.na(m_2018)] <- 'zxzxzx'
m_2017[is.na(m_2017)] <- 'zxzxzx'
m_2016[is.na(m_2016)] <- 'zxzxzx'


yr_list = list(m_2021, m_2020, m_2019, m_2018, m_2017, m_2016)
#yr_list = c(m_2021, m_2020, m_2019, m_2018, m_2017, m_2016)
yrs = list('2021', '2020', '2019', '2018', '2017', '2016')

search_list = c('interview', 'search', 'career', 'network')
df_count <- data.frame(matrix(ncol=5, nrow= 0))
colnames(df_count) <- c("year","word","count","na_count", "total_tips")
count_list = c()


for(i in 1:length(yr_list)) {
  x = yr_list[[i]]
  yr = yrs[[i]] 
  for(sw in search_list) {
    counter = 0 
    na_count = 0
    for (i in 1:nrow(x)) { 
      for (j in 1:ncol(x)) {
        lotip = tolower(x[i,j]) 
        if(str_detect(lotip, sw)== TRUE){ 
          counter = counter + 1 
        } 
        if(str_detect(lotip, 'zxzxzx')==TRUE){
          na_count <- na_count + 1
    }
    }
    }
    cell_count = nrow(x)*ncol(x)
    count_list <- c(yr, sw, counter, na_count, cell_count)
    df_count = rbind(df_count, list(year=as.integer(count_list[1]), word=count_list[2], count=as.integer(count_list[3]), 
                                    na_count = as.integer(count_list[4]), numtips = as.integer(count_list[5])))
  }
}



df_count <- df_count %>%
  mutate(valid_tips = numtips - na_count) %>%
  mutate(normalized_tips = count/valid_tips)


ggplot(df_count, aes(x=year,y=normalized_tips, color=word, size=.05)) + geom_line()


str(df_count)


