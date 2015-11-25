library(dplyr)
library(tidyr)

load('data/geowater.RData')
source('functions.R')

data = df.chem %>%                                                        # 4804
  mutate(
    vmin = pmin(HCO3, Ca, Cl, Mg, K, Na, SO4),
    no.zeros = !is.na(vmin) & vmin != 0,
    no.na = !is.na(vmin)) %>%
  inner_join(df.coord %>%
               mutate(
                 with.location = !is.na(lat) & !is.na(lon),
                 location = sprintf('(%f %f)', lat, lon)), 
             by='id') %>%
  inner_join(df.date, 'id') %>%
  filter(with.location & no.zeros & no.na) %>%                           # 4444
  select(location, lat, lon, HCO3:SO4) %>% 
  gather(key=component, value=value, -location, -lat, -lon) %>%
  group_by(location, component) %>%
  summarise(
    lat = first(lat),
    lon = first(lon),
    value = prod(value)^(1/n())) %>%
  spread(key=component, value=value) %>%                                # 2263
  mutate(
    b1 = sqrt(4*3/(4+3)) * log(gmean(Ca, Mg, Na, K) / gmean(HCO3, SO4, Cl)),
    b2 = sqrt(2*2/(2+2)) * log(gmean(Ca, Mg) / gmean(Na, K)),
    b3 = sqrt(1*1/(1+1)) * log(Ca/Mg),
    b4 = sqrt(1*1/(1+1)) * log(Na/K),
    b5 = sqrt(1*2/(1+2)) * log(gmean(HCO3)/gmean(SO4, Cl)),
    b6 = sqrt(1*1/(1+1)) * log(SO4/Cl) )

#save(data, file=sprintf('data/geowater_dataset_%04d.RData', SEED))

# 
# 
# data %>% 
#   mutate(
#     total = HCO3+Ca+Cl+Mg+K+Na+SO4,
#     HCO3  = HCO3/total,
#     Ca    = Ca/total,
#     Cl    = Cl/total,
#     Mg    = Mg/total,
#     K     = K/total,
#     Na    = Na/total,
#     SO4   = SO4/total ) %>%
#   select(-total)
# 
# data %>% filter(location == '(42.932814 10.788894)')
# 
# 
# %>%
#   group_by(location) %>% 
# 
# ilr_coordinates(data %>% compo) %>% tbl_df
