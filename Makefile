PY?=python
PY3?=python3
PELICAN?=pelican
PELICANOPTS=

BASEDIR=$(CURDIR)
INPUTDIR=$(BASEDIR)/content
OUTPUTDIR=$(BASEDIR)/output
CONFFILE=$(BASEDIR)/pelicanconf.py
PUBLISHCONF=$(BASEDIR)/publishconf.py

APACHE_PREFIX=/usr/local/apache2
APACHE_CTRL=$(APACHE_PREFIX)/bin/apachectl
APACHE_HTDOCS=$(APACHE_PREFIX)/htdocs

DEBUG ?= 0
ifeq ($(DEBUG), 1)
	PELICANOPTS += -D
endif

RELATIVE ?= 0
ifeq ($(RELATIVE), 1)
	PELICANOPTS += --relative-urls
endif

-include Makefile.private

help:
	@echo 'Makefile for a pelican Web site                                           '
	@echo '                                                                          '
	@echo 'Usage:                                                                    '
	@echo '   make draft                          create drafts for local usage      '
	@echo '   make publish                        generate using production settings '
	@echo '   make clean                          remove the generated files         '
	@echo '   make regenerate                     regenerate files upon modification '
	@echo '   make apache_rsync                   upload the web site via rsync+ssh  '
	@echo '   make apache_serve                   start/restart local apache server  '
	@echo '   make apache_stop                    stop local apache server           '
	@echo '   make apache_clean                   clears all files on the server     '
	@echo '   make apache_upload                  start server and upload files      '
	@echo '   make ftp                            generate scripts for ftp upload    '
	@echo '   make ftp_upload                     upload files via ftp command       '
	@echo '                                                                          '
	@echo 'Set the DEBUG variable to 1 to enable debugging, e.g. make DEBUG=1 html   '
	@echo 'Set the RELATIVE variable to 1 to enable relative urls                    '
	@echo '                                                                          '

draft:
	$(PELICAN) $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE) $(PELICANOPTS)

publish:
	$(PELICAN) $(INPUTDIR) -o $(OUTPUTDIR) -s $(PUBLISHCONF) $(PELICANOPTS)

clean:
	[ ! -d $(OUTPUTDIR) ] || rm -rf $(OUTPUTDIR)

regenerate:
	$(PELICAN) -r $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE) $(PELICANOPTS)

apache_rsync:
	rsync -vr $(OUTPUTDIR)/* $(APACHE_HTDOCS)

apache_serve:
	$(APACHE_CTRL) -k start

apache_stop:
	$(APACHE_CTRL) -k stop

apache_clean:
	[ ! -d $(APACHE_HTDOCS) ] || rm -rf $(APACHE_HTDOCS)/*

apache_upload:
	$(APACHE_CTRL) -k start
	$(PELICAN) $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE) $(PELICANOPTS)
	rsync -vr $(OUTPUTDIR)/* $(APACHE_HTDOCS)

ftp:
	$(PY3) render_upload.py --user $(USER) --host $(HOST)

ftp_upload:
	$(PELICAN) $(INPUTDIR) -o $(OUTPUTDIR) -s $(PUBLISHCONF) $(PELICANOPTS)
	$(PY3) render_upload.py --user $(USER) --host $(HOST)
	./upload_all.sh

.PHONY: help draft publish clean regenerate apache_rsync apache_serve apache_stop apache_clean ftp ftp_upload
