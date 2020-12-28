.PHONY: sphinx-start sphinx-stop sphinx-download sphinx-install sphinx-compile sphinx-index sphinx-clean

SPHINX_VERSION="2.2.5"

sphinx/sphinx.conf:
	perl -MCwd -ne 'BEGIN { $$apppath = getcwd();} next if /^\s*#/ or /^\s*$$/; s/\Q%APPPATH%/$$apppath/g; print;' sphinx/sphinx-csv.conf.dist > sphinx/sphinx.conf

sphinx-start: sphinx/sphinx.conf sphinx/bin/searchd
	cd sphinx && ./bin/searchd

sphinx-stop: sphinx/bin/searchd
	cd sphinx && ./bin/searchd --stop

tmp/sphinx/sphinx-$(SPHINX_VERSION)-release.tar.gz:
	test -e tmp/sphinx || mkdir -p tmp/sphinx
	cd tmp/sphinx && wget http://sphinxsearch.com/files/sphinx-$(SPHINX_VERSION)-release.tar.gz

tmp/sphinx/sphinx-$(SPHINX_VERSION)-release/configure: | tmp/sphinx/sphinx-$(SPHINX_VERSION)-release.tar.gz tmp/sphinx/libstemmer_c.tgz
	cd tmp/sphinx && tar xf sphinx-$(SPHINX_VERSION)-release.tar.gz
	cd tmp/sphinx/sphinx-$(SPHINX_VERSION)-release && patch -p1 < ../../../sphinx/misc/sphinx-$(SPHINX_VERSION)_sqlite.diff
	cd tmp/sphinx/sphinx-$(SPHINX_VERSION)-release && tar xf ../libstemmer_c.tgz

sphinx/bin/searchd:
	# aptitude install autoconf automake libstemmer-dev g++
	cd tmp/sphinx/sphinx-$(SPHINX_VERSION)-release && autoreconf
	cd tmp/sphinx/sphinx-$(SPHINX_VERSION)-release && ./configure --with-libstemmer --with-sqlite --without-mysql
	cd tmp/sphinx/sphinx-$(SPHINX_VERSION)-release && make
	cp -f tmp/sphinx/sphinx-$(SPHINX_VERSION)-release/src/indexer sphinx/bin
	cp -f tmp/sphinx/sphinx-$(SPHINX_VERSION)-release/src/searchd sphinx/bin

tmp/sphinx/libstemmer_c.tgz:
	cd tmp/sphinx/libstemmer_c.tgz && wget http://snowball.tartarus.org/dist/libstemmer_c.tgz

sphinx-compile: sphinx/bin/searchd

sphinx-install: tmp/sphinx/sphinx-$(SPHINX_VERSION)-release/configure

sphinx-download: tmp/sphinx/sphinx-$(SPHINX_VERSION)-release.tar.gz

sphinx-index: sphinx/sphinx.conf sphinx/bin/indexer
	@mkdir -p sphinx/indexes || true
	cd sphinx && ./bin/indexer --config sphinx.conf --all --rotate

sphinx-clean:
	rm -fr tmp/sphinx/sphinx-$(SPHINX_VERSION)-release
	rm -f sphinx/bin/*

