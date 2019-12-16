#include "libyottadb.h"

int main()
{
	ydb_buffer_t	lang[1], value, varname;

	YDB_LITERAL_TO_BUFFER("^hello", &varname);
	YDB_LITERAL_TO_BUFFER("C", &lang[0]);
	YDB_LITERAL_TO_BUFFER("Hello, world!", &value);
	return ydb_set_s(&varname, 1, &lang[0], &value);
}
