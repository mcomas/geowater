if(!exists('OMEGA')) OMEGA = 'prop'
if(!exists('LAMBDA')) LAMBDA = 'coda.norm'

library(dplyr)

# load('data/geowater_dataset.RData')
# load('data/mixture_model.RData')

data = data %>%
  mutate(
    g0 = sprintf('%02d', m@bestResult@partition),
    post = apply(m@bestResult@proba, 1, max) )

source('hierarchical_merging_functions.R')

POST = data.frame(m@bestResult@proba) %>% tbl_df
POST[POST==0] = .Machine$double.xmin

library(mixpack)
hp = get_hierarchical_partition(POST %>% data.frame %>% as.matrix, omega = OMEGA, lambda=LAMBDA)
S.values = attr(hp,'S.value')
hp = lapply(hp, function(hp_lvl) setNames(hp_lvl, sapply(hp_lvl, paste, collapse=',') ) )
attr(hp, 'S.value') = S.values

#save(hp, file=sprintf('data/hierarchical_merging-%s_%s.RData', OMEGA, LAMBDA))