# Runs the Haskell program to generate a map in SVG.
FortCusterNavigator.svg: FortCusterNavigator.hs
	runhaskell -W FortCusterNavigator.hs

.PHONY: clean
clean:
	-rm FortCusterNavigator.svg

