OCB_FLAGS = -use-ocamlfind -I lista3
OCB = ocamlbuild $(OCB_FLAGS)

OCB_FLAGS_WITH_CORE = -tag-line 'true:package(core,ppx_jane,core_bench)' -tags thread,debug $(OCB_FLAGS)
OCB_CORE = ocamlbuild $(OCB_FLAGS_WITH_CORE)


clean: 
	$(OCB) -clean
	rm -f .ocamlinit_tmp

benchmarks_msorts:
	$(OCB_CORE) benchmarks_msorts.native
	./benchmarks_msorts.native

test:
	$(OCB) test.native
	./test.native

%.native:
	$(OCB_CORE) $@

%.byte:
	$(OCB_CORE) $@

%.cmo:
	$(OCB_CORE) $@

%.top: %.cmo
	echo '#use "topfind";;' > .ocamlinit_tmp
	echo '#require "core.top";;' >> .ocamlinit_tmp
	echo '#require "core.syntax";;' >> .ocamlinit_tmp
	echo '#require "core_bench";;' >> .ocamlinit_tmp
	echo '#require "qcheck";;' >> .ocamlinit_tmp
	echo '#camlp4o;;' >> .ocamlinit_tmp
	echo '#thread;;' >> .ocamlinit_tmp
	echo '#directory "_build/lista3";;' >> .ocamlinit_tmp
	echo '#load_rec "$<";;' >> .ocamlinit_tmp
	clear
	utop -init .ocamlinit_tmp


# %.top: %.byte
# 	find _build -regex .*cmo | sed 's/_build\///; s/.cmo//' > $(TARGET).mltop
# 	$(OCB_CORE) $(TARGET).top

# $(TARGET).top: $(TARGET).byte
# 	find _build -regex .*cmo | sed 's/_build\///; s/.cmo//' > $(TARGET).mltop
# 	$(OCB_CORE) $(TARGET).top


# .PHONY: all clean byte native profile debug test

# OCB_FLAGS = -use-ocamlfind -I src -I lib

# all: native byte  # profile debug
# 
# native: 
# 	$(OCB) main.native
# byte: 
# 	$(OCB) main.byte
# profile: 
# 	$(OCB) -tag profile main.native
# debug: 
# 	$(OCB) -tag debug main.byte
