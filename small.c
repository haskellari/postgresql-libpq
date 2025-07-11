#include <libpq-fe.h>

int main() {
		    char conninfo[]="dbname = postgres";
		    PGconn     *conn;
		    conn = PQconnectdb(conninfo);
}
