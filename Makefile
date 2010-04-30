
PQ.o: PQ.hs
	ghc -c $^

PQ.hs: PQ.hsc
	hsc2hs -I /opt/local/include/postgresql83 $^


clean:
	git clean -f