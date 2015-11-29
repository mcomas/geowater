OMEGA = cnst prop dich
LAMBDA = entr demp demp.mod coda coda.norm prop
LVL = 2 3 4 5 6 7 8 9 10 11 12
HIERAR = $(foreach omega,$(OMEGA),$(foreach lambda,$(LAMBDA), $(shell printf 'data/hierarchical_merging-%s_%s.RData' $(omega) $(lambda))))
DATA = data/geowater.RData data/geowater_maps.RData data/geowater_dataset.RData \
       data/mixture_model.RData data/hierarchical_merging-prop_coda.norm.RData \
       $(HIERAR) \
       $(foreach omega,$(OMEGA),$(foreach lambda,$(LAMBDA),$(foreach lvl,$(LVL),$(shell printf 'www/hierarchical_merging_at_level-%s_%s_%02d.html' $(omega) $(lambda) $(lvl)))))
HTML = www/descriptives.html www/recovering.html www/matched.html www/matched2.html www/matching_diff.html \
       www/mixture_model.html

all: $(DATA) $(HTML)

data/geowater.RData : load_data.R data/database_Antonella.csv
	Rscript $<

data/geowater_maps.RData : save_map.R data/geowater.RData
	Rscript $<

data/geowater_dataset.RData : build_dataset.R data/geowater.RData
	Rscript $<

data/mixture_model.RData : mixture_model.R data/geowater_dataset.RData
	Rscript -e 'load("data/geowater_dataset.RData"); source("$<"); save(m, file="$@")'

data/hierarchical_merging-%.RData : hierarchical_merging.R data/mixture_model.RData data/geowater_dataset.RData
	Rscript -e 'v=strsplit("$*", "_")[[1]]; OMEGA=v[1]; LAMBDA=v[2]; load("data/geowater_dataset.RData"); load("data/mixture_model.RData"); source("$<"); save(hp, file="$@")'

www/hierarchical_merging_at_level-%.html : hierarchical_merging_at_level.Rmd data/mixture_model.RData $(HIERAR)
	mkdir -p $(@F)
	Rscript -e 'rmarkdown::render("$<", output_dir="www", output_file="$(@F)", params = list(merging = "$*"), intermediates_dir = "$(@F)")'
	rm -r $(@F)

www/descriptives.html : descriptives.Rmd data/geowater.RData data/geowater_maps.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/recovering.html : recovering.Rmd data/geowater.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/matched.html : matched.Rmd data/geowater.RData data/geowater_maps.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/matched2.html : matched2.Rmd data/geowater.RData data/geowater_maps.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/matching_diff.html : matching_diff.Rmd data/geowater.RData data/geowater_maps.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/mixture_model.html : mixture_model.Rmd data/geowater.RData data/geowater_maps.RData data/mixture_model.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'

www/hierarchical_merging.html : hierarchical_merging.Rmd data/geowater.RData data/geowater_maps.RData data/mixture_model.RData
	Rscript -e 'rmarkdown::render("$<", output_file="$@", params = list(merging = "prop|coda.norm"))'



