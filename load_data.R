library(dplyr)
library(stringr)
library(lubridate)

data = read.csv('data/database_Antonella.csv', 
                sep = ';', stringsAsFactors = F)
data$id = 1:NROW(data)

v_year = data$Sample_Date
for(y in sprintf("%4d", 1981:2015)){
  v_year = ifelse( str_detect(v_year, y), y, v_year)
}
for(y in sprintf("%4d", 1981:2015)){
  v_year = ifelse( str_sub(v_year, -2) == str_sub(y, -2), y, v_year)
}
v_year = as.numeric(v_year)
v_year = ifelse(v_year > 35000, year(as.Date(v_year, origin='1899-12-30')), v_year)



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
v_month_6 = as.numeric(data$Sample_Date)
v_month_6 = ifelse(v_month_6 > 35000, v_month_6, NA)
v_month_6 = month(as.Date(v_month_6, origin='1899-12-30'))

## Estiu: Juny, Juliol, Agost
isna_num = function(v) as.numeric(!is.na(v))

df.date <- data_frame(id = data$id,
                 text = data$Sample_Date,
                 year = v_year,
                 v_length = nchar(apply(str_split_fixed(text, '/', 3), 1, function(v) v[3])),
                  v_month_1,
                  v_month_2,
                  v_month_3,
                  v_month_4,
                  v_month_5,
                  v_month_6,
                  n = isna_num(v_month_1) + 
                    isna_num(v_month_2) + 
                    isna_num(v_month_3) + 
                    isna_num(v_month_4) + 
                    isna_num(v_month_5) + isna_num(v_month_6)
                  ) %>% 
  transmute(
    id = id,
    text = text,
    year = year,
    month = ifelse(v_length == 4, 
                   v_month_1, 
                   ifelse(v_length == 2, 
                          v_month_3, 
                          pmin(v_month_2, v_month_4, v_month_5, v_month_6, na.rm=T))))

df.date %>% group_by(text) %>% 
  summarise(
    year = first(year),
    month = first(month),
    n = n()
  ) %>% ungroup %>% 
  arrange(year, month) #%>% View

table(df.date[['year']], useNA='ifany')
table(df.date[['month']], useNA='ifany')

barplot(table(df.date[['year']]), main = 'Years')
barplot(table(df.date[['month']]), main = 'Months')
barplot(table(ifelse(df.date[['month']] %in% 6:8, 'estiu', 'no estiu'), df.date[['year']]), main = 'Months')

### Coordinates are pulished
library(rgdal)
df.coord <- data_frame(id = data$id,
                       est.gb = data$EST_GB,
                       nord.gb = data$NORD.GB) %>% 
  mutate(
    est.gb = ifelse(str_count(est.gb, '\\.') == 2, gsub('\\.', '', est.gb), est.gb),
    nord.gb = ifelse(str_count(nord.gb, '\\.') == 2, gsub('\\.', '', nord.gb), nord.gb),
    est.gb = as.numeric(est.gb),
    nord.gb = as.numeric(nord.gb))

data.map = df.coord %>% subset(!is.na(est.gb) & !is.na(nord.gb)) %>% select(id, est.gb, nord.gb) %>% data.frame
coordinates(data.map) = c('est.gb', 'nord.gb')
proj4string(data.map) = CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=0.9996 +x_0=1500000 +y_0=0 +ellps=intl +units=m +no_defs")

data.map = spTransform(data.map, CRS("+init=epsg:4326")) %>% 
  data.frame %>% select(-optional) %>%
  setNames(c('id', 'lon', 'lat')) %>% tbl_df
df.coord = left_join(df.coord, data.map, by='id')

## Composition
nms = c('id' = 'id',
        'pH' = 'ph',
        'Conducibilità_uS_cm_20C_' = 'conduct',
        'Bicarbonati_mg.L_' = 'HCO3',
        'Calcio_mg.L_' = 'Ca',
        'Cloruri_mg.L_'= 'Cl',
        'Magnesio_mg.L_' = 'Mg',
        'Potassio_mg.L_' = 'K',
        'Sodio_mg.L_' = 'Na',
        'Solfati_mg.L_' = 'SO4')
df.chem = data %>%
  select(one_of(names(nms))) %>% setNames(nms) %>% 
  mutate( 
    conduct = suppressWarnings(as.numeric(conduct)) ) %>%
  tbl_df


###
save(df.date, df.coord, df.chem, file='data/geowater.RData')
