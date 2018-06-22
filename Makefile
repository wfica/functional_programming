OCAMLINIT_PATH = "/home/fica/.ocamlinit"

OCB_FLAGS = -use-ocamlfind -no-hygiene
OCB = ocamlbuild $(OCB_FLAGS)


clean: 
	$(OCB) -clean
	rm -f .ocamlinit_tmp

benchmarks_msorts:
	$(OCB) benchmarks_msorts.native
	./benchmarks_msorts.native

test:
	$(OCB) test.native
	./test.native

%.native:
	$(OCB) $@

%.byte:
	$(OCB) $@

%.cmo:
	$(OCB) $@

%.top: %.cmo
	cat $(OCAMLINIT_PATH) > .ocamlinit_tmp
	for file in $(shell find ./_build -type d); do \
		echo "#directory \"$$file\";;" >> .ocamlinit_tmp;\
	done
	echo '#load_rec "$<";;' >> .ocamlinit_tmp
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
