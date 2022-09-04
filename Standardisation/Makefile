# Makefile
# Time-stamp: <2018-04-08 11:08:30 (slane)>
.PHONY: all install-packages manuscripts clean-manuscripts clobber example-data \
	compile-models

all: install-packages data/cpue.RData manuscripts compile-models

example-data: data/cpue.RData

manuscripts: manuscripts/harvest-strategy-package.pdf \
	manuscripts/candidate-outliers.pdf \
	analysis-outlines/cpue-standardisation.html \
	manuscripts/cpue-standardisation.pdf

compile-models: stan/fixed-smu-year.rds \
	stan/smu-year.rds \
	stan/smu-year-reef.rds \
	stan/smu-year-reef-diver.rds \
	stan/cliffy-year.rds \
	stan/cliffy-year-diver.rds

################################################################################
# Rules to make data
data/cpue.RData: Rmd/create-demo-data.Rmd \
	$(call s-, "../data-raw/HS_DRAFT_180131_Data for reference points_AWG.xlsx")
	cd $(<D); \
	Rscript --no-save --no-restore -e \
		"rmarkdown::render('$(<F)', output_dir = '../analysis-outlines')"

################################################################################
# Rules to compile stan models
stan/%.rds: R/compile-model.R stan/%.stan
	cd $(<D); \
	Rscript --no-save --no-restore $(<F) mname=$(basename $(@F) .rds)

################################################################################
# Rules to make manuscripts
manuscripts/harvest-strategy-package.tex: \
	manuscripts/harvest-strategy-package.Rnw
	cd $(<D); \
	Rscript --no-save --no-restore -e "knitr::knit('$(<F)')"

manuscripts/harvest-strategy-package.pdf: \
	manuscripts/harvest-strategy-package.tex
	cd $(<D); \
	latexmk -pdf $(<F)

manuscripts/candidate-outliers.tex: \
	manuscripts/candidate-outliers.Rnw
	cd $(<D); \
	Rscript --no-save --no-restore -e "knitr::knit('$(<F)')"

manuscripts/candidate-outliers.pdf: \
	manuscripts/candidate-outliers.tex
	cd $(<D); \
	latexmk -pdf $(<F)

analysis-outlines/cpue-standardisation.html: Rmd/cpue-standardisation.Rmd \
	$(call s-, "data-raw/RawCatchEffortUnfiltered_1979_2016_DiverPFN.xlsx")
	cd $(<D); \
	Rscript --no-save --no-restore -e \
		"rmarkdown::render('$(<F)', output_dir = '../analysis-outlines')"

manuscripts/cpue-standardisation.tex: \
	manuscripts/cpue-standardisation.Rnw
	cd $(<D); \
	Rscript --no-save --no-restore -e "knitr::knit('$(<F)')"

manuscripts/cpue-standardisation.pdf: \
	manuscripts/cpue-standardisation.tex
	cd $(<D); \
	latexmk -pdf $(<F)

################################################################################
# Cleaning up
clean-manuscripts:
	cd manuscripts/; \
	rm -f *.aux *.bbl *.bcf *.blg *.fdb_latexmk *.fls *.lof *.log *.lot \
		*.code *.loe *.toc *.rec *.out *.run.xml *~ *.tex

clobber: clean-manuscripts
	cd manuscripts/; \
	rm -rf auto/ cache/ figure/

################################################################################
# Rule to install packages (from script extraction).
# This has been provided for the case where library/require calls are made in
# separate scripts. If the packages are not installed, make should return an
# error, saying that the packages are not available. This is politic; don't
# assume the user wants to install stuff without asking.
install-packages: scripts/installs.txt
scripts/installs.txt: scripts/strip-libs.sh R/ipak.R
	cd scripts; \
	chmod u+x strip-libs.sh; \
	./strip-libs.sh ../R/ installs.txt; \
	./strip-libs.sh ../Rmd/ installs.txt; \
	Rscript --no-save --no-restore ../R/ipak.R insts=installs.txt

################################################################################
# Resubstitutions to deal with spaces in filenames
s-= $(subst \ ,-,$1)
-s= $(subst -,\ ,$1)
