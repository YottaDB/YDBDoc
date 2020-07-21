#! /usr/bin/perl

use YottaDB qw(:all);

my ($tCrab, $tDelta, $tHorse) = (0, 0, 0);

while (1) {
        $tCrab = y_next ("^Crab", $tCrab);
        $tDelta = y_next ("^Delta", $tDelta);
        $tHorse = y_next ("^Horse", $tHorse);

        last unless defined $tCrab && defined $tDelta && defined $tHorse;

        if ($tDelta != $tCrab || $tCrab != $tHorse) {
                print "ACID fail: tDelta=$tDelta tCrab=$tCrab tHorse=$tHorse\n";
                exit 1;
        }
        if (y_get ("^Crab", $tCrab) + y_get ("^Horse", $tHorse)) {
                print "ACID fail: ^Crab($tCrab)=".y_get("^Crab", $tCrab)."; ^Horse($tHorse)=".y_get("^Horse", $tHorse)."\n";
                exit 2;
        }
        if (y_incr ("sDelta", y_get "^Delta", $tDelta) != y_get ("^Horse", $tHorse)) {
                print "ACID fail: Sum ^Delta(0:$tDelta)=".y_get("sDelta")."; ^Horse($tHorse)=".y_get ("^Horse", $tHorse). "\n";
                exit 3;
        }
}

print "ACID test pass\n";
exit 0;
