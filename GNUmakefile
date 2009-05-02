# We'll consider any hs file at the top level to be
# something with a main function in it
utilities := $(patsubst %.hs,%,$(wildcard *.hs))

# for fast compiles but slow code
haskell_compiler := ghc --make -prof -auto-all

# for slow compiles but fast code
#haskell_compiler := ghc --make -O2 -fvia-C

all: clean $(utilities)

$(utilities):
	$(haskell_compiler) $@

.PHONY: clean
clean:
	rm -f $(utilities)
	find . -name '*.hi' | xargs rm -f
	find . -name '*.o' | xargs rm -f

# convenience target for moving the execs into the user's bin dir
publish: all
	cp $(utilities) ~/bin
