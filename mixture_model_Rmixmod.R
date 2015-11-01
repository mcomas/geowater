library(Rmixmod)
library(ggmap)
library(dplyr)

load('data/geowater_dataset.RData')
load('data/geowater_maps.RData')
source('functions.R')

Y = data %>% select(b1:b6)
m = mixmodCluster(Y, nbCluster = 12, 
                  models = mixmodGaussianModel(listModels = 'Gaussian_pk_Lk_Ck'),
                  strategy = mixmodStrategy(seed=2, nbTry = 20, initMethod='SEMMax'))

data = data %>%
  mutate(
    g0 = sprintf('%02d', m@bestResult@partition),
    post = apply(m@bestResult@proba, 1, max)
  )

ggmap(ggmap = map_bw) +
  geom_point(data = data %>% arrange(-post), aes(x=lon, y=lat, col=post), size=3) +
  facet_wrap(~g0)

library(mixpack)
xlog = function(x) x * log(x)
l_lambda = list(
  'entr' = function(v_tau, a, b) xlog(v_tau[a] + v_tau[b]) - xlog(v_tau[a]) - xlog(v_tau[b]),
  'demp' = function(v_tau, a, b) if(which.max(v_tau) == b) 1 else 0,
  'demp.mod' = function(v_tau, a, b) v_tau[b] * (v_tau[a] + v_tau[b])^-1,
  'coda' = function(v_tau, a, b) log(v_tau[b] / v_tau[a]),
  'coda.norm' = function(v_tau, a, b) -log(v_tau[b] / v_tau[a])^2,
  'prop' = function(v_tau, a, b) v_tau[b] )

# Weitghing functions
l_omega = list(
  'cnst' = function(v_tau, a) 1,
  'prop' = function(v_tau, a) v_tau[a],
  'dich' = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0
)
POST = m@bestResult@proba
POST[POST==0] = .Machine$double.xmin
hp = get_hierarchical_partition(POST,
                                omega = l_omega[['prop']],
                                lambda = l_lambda[['coda.norm']])
hp.mod = hp

df = m@bestResult@parameters@mean %>% data.frame %>% tbl_df %>% 
  setNames(balance_nms) %>%
  mutate(g0 = sprintf('%02d', 1:m@bestResult@nbCluster)) %>% 
  gather(key='balance', value='value', -g0)

df.mean = df %>% group_by(balance) %>% summarise(value = mean(value))

##########

HP  = hp.mod[[12]]
hp_temp = lapply(HP, function(i) sprintf('%02d', i))

df$g = ''
for(i in names(hp_temp)){
  df$g[df$g0 %in% hp_temp[[i]]] = i
}

lvls = df %>% filter(balance == '(Ca·Mg·Na·K)/(HCO3·SO4·Cl)') %>% 
  group_by(g) %>% 
  summarise( value.m = mean(value) ) %>% 
  arrange(value.m) %>% select(g) %>% .[[1]]
data$g = factor(cluster_partition(m@bestResult@proba, HP), levels = lvls)
df$g = factor(df$g, lvls)

ggmap12 <- ggmap(map_bw) + 
  geom_point(data = data, aes(x = lon, y = lat, col = g0), alpha=0.4) +
  xlab(NULL) + ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank()) + 
  facet_wrap(~g, nrow=2) + theme(legend.position = 'none')

gg_bal12 <- ggplot() + 
  geom_point(data=df, 
             aes(x=g, y=value, colour=g0), size=3) +
  geom_hline(data=df.mean, aes(yintercept=value), col='blue') + 
  facet_wrap(~balance, ncol=2, scale='free') + 
  theme_bw() + theme(legend.position = 'none', 
                     axis.text.x=element_text(angle=-90)) + 
  scale_x_discrete(breaks=lvls, labels=ifelse(nchar(lvls) > 5, paste0(substr(lvls, 1, 4), '...'), lvls))

library(grid)
grid.newpage()
vpa_ <- viewport(width = 1, height = 0.6, x = 0.5, y = 0.3)
print(gg_bal12, vp = vpa_)
vpb_ <- viewport(width = 1, height = 0.4, x = 0.5, y = 0.8)
print(ggmap12, vp = vpb_)

######

HP  = hp.mod[[6]]
hp_temp = lapply(HP, function(i) sprintf('%02d', i))

df$g = ''
for(i in names(hp_temp)){
  df$g[df$g0 %in% hp_temp[[i]]] = i
}

lvls = df %>% filter(balance == '(Ca·Mg·Na·K)/(HCO3·SO4·Cl)') %>% 
  group_by(g) %>% 
  summarise( value.m = mean(value) ) %>% 
  arrange(value.m) %>% select(g) %>% .[[1]]
data$g = factor(cluster_partition(m@bestResult@proba, HP), levels = lvls)
df$g = factor(df$g, lvls)

ggmapT <- ggmap(map_bw) + 
  geom_point(data = data, aes(x = lon, y = lat, col = g0), alpha=0.4) +
  xlab(NULL) + ylab(NULL) +
  theme(axis.ticks = element_blank(), axis.text = element_blank()) + 
  facet_wrap(~g, nrow=2) + theme(legend.position = 'none')

gg_balT <- ggplot() + 
  geom_point(data=df, 
             aes(x=g, y=value, colour=g0), size=3) +
  geom_hline(data=df.mean, aes(yintercept=value), col='blue') + 
  facet_wrap(~balance, ncol=2, scale='free') + 
  theme_bw() + theme(legend.position = 'none', 
                     axis.text.x=element_text(angle=-90)) + 
  scale_x_discrete(breaks=lvls, labels=ifelse(nchar(lvls) > 5, paste0(substr(lvls, 1, 4), '...'), lvls))

library(grid)
grid.newpage()
vpa_ <- viewport(width = 1, height = 0.6, x = 0.5, y = 0.3)
print(gg_balT, vp = vpa_)
vpb_ <- viewport(width = 1, height = 0.4, x = 0.5, y = 0.8)
print(ggmapT, vp = vpb_)

