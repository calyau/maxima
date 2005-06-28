#!/bin/bash

echo 'A continuacion se esta generando el archivo dvi ....'

texi2dvi -I tex_es/ maxima.es.texi 

echo 'A continuacion se esta generando el archivo html ....'

texi2html -I tex_es/ maxima.es.texi

echo 'A continuacion se esta generando el archivo pdf ....'

rm -rf maxima.es.aux

texi2pdf -I tex_es/ maxima.es.texi

echo 'Hecho. Felicitaciones por usar Maxima !!!'




