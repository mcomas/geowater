library(dplyr)
library(Rmixmod)

#load(sprintf('data/geowater_dataset_%04d.RData', SEED))

Y = data %>% select(b1:b6)
m = mixmodCluster(Y, nbCluster = 12, 
                  models = mixmodGaussianModel(listModels = 'Gaussian_pk_Lk_Ck'),
                  strategy = mixmodStrategy(seed=2, nbTry = 20, initMethod='SEMMax'))

#save(m, file = sprintf('data/mixture_model_%04d.RData', SEED))