import yottadb


if __name__ == "__main__":
    # remove existing global variable trees
    yottadb.delete_tree("^Crab")
    yottadb.delete_tree("^Delta")
    yottadb.delete_tree("^Horse")

    # set initial values for ^Crab and ^Horse
    yottadb.set("^Crab", ("0",), "0")
    yottadb.set("^Horse", ("0",), "0")
