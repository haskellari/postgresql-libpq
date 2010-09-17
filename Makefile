
Database/PQ.o: Database/PQ.hs
	ghc -Wall -c $^

Database/PQ.hs: Database/PQ.hsc
	hsc2hs -I /opt/local/include/postgresql84 $^


clean:
	git clean -f