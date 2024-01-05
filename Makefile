PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file
FILE_TARGET := "R/${FILE}.R"

# .PHONY is used so that make will not treat the target as a filename
.PHONY: install build doc docs eg check test readme

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

doc:
	${RSCRIPT} -e "devtools::document()"

eg:
	${RSCRIPT} -e "devtools::run_examples(run_dontrun = TRUE)"

check: build
	_R_CHECK_CRAN_INCOMING_=FALSE CROMWELLURL=http://localhost:8000 \
		R CMD CHECK --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

test:
	${RSCRIPT} -e "devtools::test()"

readme:
	${RSCRIPT} -e "knitr::knit('README.Rmd')"

vign_getting_started:
	cd vignettes;\
	${RSCRIPT} -e "Sys.setenv(NOT_CRAN='true'); knitr::knit('${PACKAGE}.Rmd.og', output = '${PACKAGE}.Rmd')";\
	cd ..

docs:
	${RSCRIPT} -e "pkgdown::build_site(); pkgdown::preview_site(preview=TRUE)"

lint_package:
	${RSCRIPT} -e "lintr::lint_dir('R')"

style_file:
	${RSCRIPT} -e 'styler::style_file(${FILE_TARGET})'

style_package:
	${RSCRIPT} -e "styler::style_pkg()"
