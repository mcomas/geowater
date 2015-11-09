if(!exists('OMEGA')) OMEGA = 'prop'
if(!exists('LAMBDA')) LAMBDA = 'coda.norm'

library(dplyr)

load('data/geowater_dataset.RData')
load('data/mixture_model.RData')

data = data %>%
  mutate(
    g0 = sprintf('%02d', m@bestResult@partition),
    post = apply(m@bestResult@proba, 1, max) )

source('hierarchical_merging_functions.R')

POST = data.frame(m@bestResult@proba) %>% tbl_df
POST[POST==0] = .Machine$double.xmin

library(mixpack)
hp = get_hierarchical_partition(POST,
                                omega = l_omega[[OMEGA]],
                                lambda = l_lambda[[LAMBDA]])

save(hp, file=sprintf('data/hierarchical_merging-%s_%s.RData', OMEGA, LAMBDA))