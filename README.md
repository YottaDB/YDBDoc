
# README

This directory holds YottaDB documentation that is under development.

Homepage: https://gitlab.com/YottaDB/DB/YDBDoc

## Building the documentation

Install the necessary prerequisite sphinx. On Ubuntu this is:

```sh
apt install sphinx python3-sphinx-rtd-theme python3-sphinx-copybutton graphviz jq
```

Now build the documentation:

```sh
make html man
```

This will create html (and manpage) documentation in folders `*/_build`, and you can view them there with a browser, however links between the files will not work unless you collate the docs into the proper folder structure, which you can do as follows:

```sh
./buildall.sh r2.01  # collate docs into a directory of your choosing, say r2.01
```

Now test the documentation (this will also be done by the CI pipeline if you create a merge request):

```sh
make test
```

Sphinx may also be able to build several other forms of documentation which you can see by typing `make`, though your mileage may vary, since building these formats are not officially maintained by YottaDB.

Note that `make man` produces known warnings about table/cell spanning not supported by Sphinx. This is expected as Sphinx table handling for manpages results in merged table cells being less pretty than in the html. For a clear example of the degeneration caused in both rows and columns, see the table near the end of the StyleGuide manpage: `styleguide.1`.

## Contents

The documentation consists of:

 - Acculturation Guide
 - Administration and Operations Guide
 - M Programmer's Guide
 - Messages and Recovery Procedures Manual
 - Multi-language Programmer's Guide
 - Style Guide
 - YottaDB Plugins
