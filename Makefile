PKGNAME=checkglobals
PKGVERS=$(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PWD=$(shell pwd)

all: check clean

doc:
	Rscript -e "roxygen2::roxygenize()"

globals:
	Rscript -e "checkglobals::checkglobals()"

test:
	Rscript -e "source(file.path('inst', 'unit_tests', 'unit_tests_checkglobals.R'), local = TRUE)"

build: doc
	R CMD build .

install: build
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

rchk: build
	docker run --rm -v $(PWD):/rchk/$(PKGNAME) kalibera/rchk:latest /rchk/$(PKGNAME)/$(PKGNAME)_$(PKGVERS).tar.gz

clean:
	$(RM) -r $(PKGNAME).Rcheck/
