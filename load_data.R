library(dplyr)

data = read.csv('data/database_Antonella.csv', sep = ';', stringsAsFactors = F) %>% 
  tbl_df %>%
  transmutate( )
