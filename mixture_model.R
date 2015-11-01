library(dplyr)
library(Rmixmod)

load('data/geowater_dataset.RData')

Y = data %>% select(b1:b6)
m = mixmodCluster(Y, nbCluster = 12, 
                  models = mixmodGaussianModel(listModels = 'Gaussian_pk_Lk_Ck'),
                  strategy = mixmodStrategy(seed=2, nbTry = 20, initMethod='SEMMax'))

save(m, file = 'data/mixture_model.RData')