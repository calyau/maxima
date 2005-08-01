#!/bin/bash

echo 'Generando manual en formato info ....'

makeinfo --enable-encoding maxima.es.texi

echo 'Generando manual en formato html ....'

texi2html --lang=es --output=maxima.es.html maxima.es.texi

echo 'Generando manual en formato pdf ....'

texi2pdf maxima.es.texi