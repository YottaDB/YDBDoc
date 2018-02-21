#!/usr/local/bin/tcsh -f

echo '# Step 1 : Do the following change'
echo '# In files docs.yottadb.com/*/_static/css/theme.css, make the following replacements:'
echo '#'
set filelist = `ls -1 _build/html/_static/css/theme.css`

echo '# "courier new",monospace -> Inconsolata,"Courier New",monospace'
set from = '"courier new",monospace'
set to   = 'Inconsolata,"Courier New",monospace'
perl -p -i -e "s/$from/$to/g" $filelist

echo '# "Lato","proxima-nova","Helvetica Neue",Arial,sans-serif -> Raleway,Tahoma,sans-serif'
set from = '"Lato","proxima-nova","Helvetica Neue",Arial,sans-serif'
set to   = 'Raleway,Tahoma,sans-serif'
perl -p -i -e "s/$from/$to/g" $filelist

echo '# "Roboto Slab","ff-tisa-web-pro","Georgia",Arial,sans-serif -> Lora,Georgia,serif'
set from = '"Roboto Slab","ff-tisa-web-pro","Georgia",Arial,sans-serif'
set to   = 'Lora,Georgia,serif'
perl -p -i -e "s/$from/$to/g" $filelist

echo '# Consolas,"Andale Mono WT","Andale Mono","Lucida Console","Lucida Sans Typewriter","DejaVu Sans Mono","Bitstream Vera Sans Mono","Liberation Mono","Nimbus Mono L",Monaco,"Courier New",Courier,monospace -> Inconsolata,Consolas,monospace'
set from = 'Consolas,"Andale Mono WT","Andale Mono","Lucida Console","Lucida Sans Typewriter","DejaVu Sans Mono","Bitstream Vera Sans Mono","Liberation Mono","Nimbus Mono L",Monaco,"Courier New",Courier,monospace'
set to   = 'Inconsolata,Consolas,monospace'
perl -p -i -e "s/$from/$to/g" $filelist
echo ""
echo " --> Step 1 complete"
echo ""

echo '# Step 2 : Make the following changes to the html files under docs.yottadb.com:'
echo '#'
echo '# Before the line that contains: <link rel="stylesheet" href="_static/css/theme.css" type="text/css" />'
echo '# Add the lines:'
echo '# <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Raleway" />'
echo '# <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lora" />'
echo '# <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Inconsolata" />'

set filelist = `ls -1 _build/html/*.html`
set from = '<link rel="stylesheet" href="_static\/css\/theme.css" type="text\/css" \/>'
set to1  = '<link rel="stylesheet" href="https:\/\/fonts.googleapis.com\/css?family=Raleway" \/>'
set to2  = '<link rel="stylesheet" href="https:\/\/fonts.googleapis.com\/css?family=Lora" \/>'
set to3  = '<link rel="stylesheet" href="https:\/\/fonts.googleapis.com\/css?family=Inconsolata" \/>'
set to   = "${to1}${to2}${to3}$from"
perl -p -i -e "s/$from/$to/g" $filelist

echo ""
echo " --> Step 2 complete"
echo ""

