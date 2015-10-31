DATA = data/geowater.RData data/geowater_maps.RData data/geowater_dataset.RData
HTML = www/descriptives.html www/recovering.html www/matching_diff.html

all: $(DATA) $(HTML)

data/geowater.RData : load_data.R data/database_Antonella.csv
	Rscript $<

data/geowater_maps.RData : save_map.R data/geowater.RData
	Rscript $<

data/geowater_dataset.RData : build_dataset.R data/geowater.RData
	Rscript $<

www/descriptives.html : descriptives.Rmd data/geowater.RData data/geowater_maps.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/recovering.html : recovering.Rmd data/geowater.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/matching_diff.html : matching_diff.Rmd data/geowater.RData data/geowater_maps.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

