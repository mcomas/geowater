library(dplyr)
library(stringr)
library(lubridate)

data = read.csv('data/database_Antonella.csv', 
                sep = ';', stringsAsFactors = F) %>% tbl_df

v_year = data$Sample_Date
for(y in sprintf("%4d", 1900:2015)){
  v_year = ifelse( str_detect(v_year, y), y, v_year)
}
for(y in sprintf("%4d", 1900:2015)){
  v_year = ifelse( str_sub(v_year, -2) == str_sub(y, -2), y, v_year)
}
v_year = as.numeric(v_year)

v_month_1 = data$Sample_Date %>% as.Date(format='%m/%d/%Y') %>% month
v_month_2 = str_sub(data$Sample_Date, 1, 10) %>% as.Date(format='%d_%m_%Y') %>% month
v_month_3 = data$Sample_Date %>% as.Date(format='%d/%m/%y') %>% month
v_month_4 = data$Sample_Date %>% as.Date(format='%d-%m-%y') %>% month
MONTHS = c('gennaio' = 1, 'febbraio' = 2, 'marzo' = 3, 'aprile' = 4, 'apr' = 4, 'maggio' = 5, 'may' = 5, 
           'giugno' = 6, 'luglio' = 7, 'aug' = 8,'settembre' = 9, 'ottobre' = 10, 'novembre' = 11, 'dicembre' = 12)
v_month_5 = tolower(data$Sample_Date)
recovered = rep(FALSE, length(v_month_5))
for(im in seq_along(MONTHS) ){
  recovered = recovered | str_detect(v_month_5, names(MONTHS)[im])
  v_month_5 = ifelse( str_detect(v_month_5, names(MONTHS)[im]), MONTHS[im], v_month_5)
}
v_month_5[!recovered] = NA
v_month_5 = as.numeric(v_month_5)

## Estiu: Juny, Juliol, Agost
isna_num = function(v) as.numeric(!is.na(v))

(df <- data_frame(text = data$Sample_Date,
                  year = v_year,
                  v_length = nchar(apply(str_split_fixed(text, '/', 3), 1, function(v) v[3])),
                  v_month_1,
                  v_month_2,
                  v_month_3,
                  v_month_4,
                  v_month_5,
                  n = isna_num(v_month_1) + 
                    isna_num(v_month_2) + 
                    isna_num(v_month_3) + 
                    isna_num(v_month_4) + 
                    isna_num(v_month_5)
                  ) %>% 
  transmute(
    text = text,
    year = year,
    month = ifelse(v_length == 4, 
                   v_month_1, 
                   ifelse(v_length == 2, 
                          v_month_3, 
                          pmin(v_month_2, v_month_4, v_month_5, na.rm=T)))) ) %>% View

table(df[['year']], useNA='ifany')
table(df[['month']], useNA='ifany')

barplot(table(df[['year']]), main = 'Years')
barplot(table(df[['month']]), main = 'Months')


