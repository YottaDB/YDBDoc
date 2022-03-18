# Verify ACID properties of a backed-up database

import yottadb
import sys


if __name__ == "__main__":
    t_crab = "0"
    t_delta = "0"
    t_horse = "0"
    s_delta = 0

    # Iterate over nodes and verify ACID properties
    while True:
        # Find next node; if the last node of all three global variables is reached without any errors,
        # quit loop to signal success
        try:
            t_crab = yottadb.subscript_next("^Crab", (t_crab,))
        except yottadb.YDBNodeEnd:
            t_crab = None
        try:
            t_delta = yottadb.subscript_next("^Delta", (t_delta,))
        except yottadb.YDBNodeEnd:
            t_delta = None
        try:
            t_horse = yottadb.subscript_next("^Horse", (t_horse,))
        except yottadb.YDBNodeEnd:
            t_horse = None
        if t_crab == t_delta == t_horse == None:
            break
        # If the timestamps of the next nodes of each of the global variables don't match,
        # terminate program with exit code 1 to signal timestamp mismatch failure
        if t_delta != t_crab or t_crab != t_horse:
            print(f"ACID fail: tDelta={t_delta} tCrab={t_crab} tHorse={t_horse}\n")
            sys.exit(1)

        v_crab = int(yottadb.get("^Crab", (t_crab,)))
        v_delta = int(yottadb.get("^Delta", (t_delta,)))
        v_horse = int(yottadb.get("^Horse", (t_horse,)))
        # If the values at next node of ^Crab() and ^Horse() don't match,
        # terminate program with exit code 2 to signal value mismatch failure
        if v_crab + v_horse:
            print(f"ACID fail: ^Crab({t_crab})={v_crab}; ^Horse({t_horse})={v_horse}\n")
            sys.exit(2)
        # If the value at the next node of ^Horse() is not the sum of this and all preceding
        # nodes of ^Delta(), terminate program with exit code 3 to signal sum mismatch failure
        s_delta += v_delta
        if s_delta != v_horse:
            print(f"ACID fail: Sum ^Delta(0:{t_delta})={s_delta}; ^Horse({t_horse})={v_horse}\n")
            sys.exit(3)

    print("ACID test pass\n")
    sys.exit(0)
