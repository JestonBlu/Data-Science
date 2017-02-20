#!/bin/sh

Rscript -e "bookdown::render_book(input = 'index.Rmd', output = 'bookdown::gitbook', output_dir = 'docs')"
