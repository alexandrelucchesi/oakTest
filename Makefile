Main: Main.hs
	ghc --make Main.hs -o client

clean:
	rm *.hi *.o Main

