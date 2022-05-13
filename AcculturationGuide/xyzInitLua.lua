local ydb = require('yottadb')

-- remove existing global variable trees
ydb.delete_tree('^Crab')
ydb.delete_tree('^Delta')
ydb.delete_tree('^Horse')

-- set initial values for ^Crab and ^Horse
ydb.set('^Crab', {'0'}, '0')
ydb.set('^Horse', {'0'}, '0')