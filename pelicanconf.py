#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u'Tobias Pleyer'
SITENAME = u'My blog about programming and other stuff'
SITEURL = 'http://blog.tpleyer.de'
MENUITEMS = [
    ('HOME', ''),    
    ('ABOUT', 'pages/about.html'),
    ('CATEGORIES', 'categories.html'),
    ('LINKS', 'pages/links.html'),
    ('ARCHIVE', 'archives.html')
]

DISPLAY_PAGES_ON_MENU = False
DISPLAY_CATEGORIES_ON_MENU = False

THEME = '/home/tobias/website/blog/themes/my'

PATH = 'content'

TIMEZONE = 'Europe/Paris'

DEFAULT_LANG = u'en'
DEFAULT_DATE = 'fs'
DEFAULT_CATEGORY = 'miscellaneous'

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

DEFAULT_PAGINATION = 5

MONTH_ARCHIVE_SAVE_AS = 'posts/{date:%Y}/{date:%b}/index.html'
ARCHIVES_SAVE_AS = 'archives.html'

STATIC_PATHS = ['images']
PLUGIN_PATHS = ['plugins']
PLUGINS = ['code_include']

# Uncomment following line if you want document-relative URLs when developing
RELATIVE_URLS = True
