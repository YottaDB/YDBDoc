#include "libyottadb.h"

int main()
{
  ydb_buffer_t Crab, Delta, Horse, sub[1];

  YDB_LITERAL_TO_BUFFER("^Crab", &Crab);
  YDB_LITERAL_TO_BUFFER("^Delta", &Delta);
  YDB_LITERAL_TO_BUFFER("^Horse", &Horse);
  YDB_LITERAL_TO_BUFFER("0", &sub[0]);
  /* remove existing global variable trees */
  ydb_delete_s(&Crab, 0, NULL, YDB_DEL_TREE);
  ydb_delete_s(&Delta, 0, NULL, YDB_DEL_TREE);
  ydb_delete_s(&Horse, 0, NULL, YDB_DEL_TREE);
  /* set initial values for ^Crab and ^Horse */
  ydb_set_s(&Crab, 1, &sub[0], &sub[0]);
  ydb_set_s(&Horse, 1, &sub[0], &sub[0]);
}
