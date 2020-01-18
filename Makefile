HASKELL = $(wildcard src/**/*.hs)

rio: rts/rts.o .stack-work
	stack install --local-bin-path . riolml:exe:rio

.stack-work: $(HASKELL)
	stack build
	touch --date "@0" $@

rts/rts.o:
	$(MAKE) -C rts/

%.hs: %.hs
