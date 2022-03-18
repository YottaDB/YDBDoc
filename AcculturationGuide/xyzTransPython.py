# Update a multi-region database using ACID transactions
import yottadb
import time
import random


def xyzTrans():
    crab = yottadb.Key("^Crab")
    delta = yottadb.Key("^Delta")
    horse = yottadb.Key("^Horse")
    curr_time = str(int(time.time() * 1000000))
    random_int = random.randint(0, 2 ** 32)

    # Python set() requires the value parameter to be a bytes-like object, i.e.,
    # bytes or str. Thus the values have been appropriately typecasted.
    delta[curr_time].set(str(random_int))
    crab[curr_time].set(str(int(crab[crab.subscript_previous()].value) - random_int))
    horse[curr_time].set(str(int(horse[horse.subscript_previous()].value) + random_int))

    return yottadb.YDB_OK


if __name__ == "__main__":
    # Infinite loop updating regions in a transaction
    while True:
        yottadb.tp(xyzTrans, transid="BA")
        time.sleep(0.5)
