
Database/PQ.o: Database/PQ.hs
	ghc -Wall -c $^

Database/PQ.hs: Database/PQ.hsc
	hsc2hs -I /opt/local/include/postgresql84 $^

example:
	@runhaskell -lpq -L/opt/local/lib/postgresql83/ -XOverloadedStrings example.hs

clean:
	git clean -f
