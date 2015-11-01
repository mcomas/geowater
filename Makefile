DATA = data/geowater.RData data/geowater_maps.RData data/geowater_dataset.RData \
       data/mixture_model.RData
HTML = www/descriptives.html www/recovering.html www/matching_diff.html \
       www/mixture_model.html www/hierarchical_merging.html

all: $(DATA) $(HTML)

data/geowater.RData : load_data.R data/database_Antonella.csv
	Rscript $<

data/geowater_maps.RData : save_map.R data/geowater.RData
	Rscript $<

data/geowater_dataset.RData : build_dataset.R data/geowater.RData
	Rscript $<

data/mixture_model.RData : mixture_model.R data/geowater_dataset.RData
	Rscript $<

www/descriptives.html : descriptives.Rmd data/geowater.RData data/geowater_maps.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/recovering.html : recovering.Rmd data/geowater.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/matching_diff.html : matching_diff.Rmd data/geowater.RData data/geowater_maps.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/mixture_model.html : mixture_model.Rmd data/geowater.RData data/geowater_maps.RData data/mixture_model.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/hierarchical_merging.html : hierarchical_merging.Rmd data/geowater.RData data/geowater_maps.RData data/mixture_model.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@", params = list(merging = "prop|coda.norm"))'

