#! /usr/bin/perl

use YottaDB qw(:all);
use Time::HiRes qw(usleep gettimeofday);

while (1) {
    y_trans (sub {
                        my $time = int(1_000_000*gettimeofday());
                        my $rand = int(rand(2**32));
                        y_set "^Delta", $time, $rand;
                        y_set ("^Crab", $time, y_get ("^Crab", y_previous ("^Crab", "")) - $rand);
                        y_set ("^Horse", $time, y_get ("^Horse", y_previous ("^Horse", "")) + $rand);
                        y_ok;
                 }, "BATCH");
    usleep 500_000;
}
