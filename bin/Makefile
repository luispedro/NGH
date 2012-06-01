all: trim-qual sam-filter

trim-qual: trim-qual.hs
	ghc -o trim-qual -O2 trim-qual.hs -threaded -rtsopts
sam-filter: sam-filter.hs
	ghc -o sam-filter -O2 sam-filter.hs -threaded -rtsopts

.PHONY: all

