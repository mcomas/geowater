load('data/geowater.RData')
library(ggmap)

bbox <- make_bbox(lon = lon, lat = lat, data = df.coord, f = 0.5)

map_color = get_map(location = bbox, maptype='terrain')
map_bw = get_map(location = bbox, maptype='terrain', color='bw')

save(map_color, map_bw, file='data/geowater_maps.RData')