#! /usr/bin/perl

use YottaDB qw(:all);

y_kill_tree $_ for (qw/^Crab ^Delta ^Horse/);

y_set ("^Crab", 0, 0);
y_set ("^Horse", 0, 0);
