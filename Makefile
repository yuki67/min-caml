# Sumii's Makefile for Min-Caml (for GNU Make)
# 
# ack.mlなどのテストプログラムをtest/に用意してmake do_testを実行すると、
# min-camlとocamlでコンパイル・実行した結果を自動で比較します。

RESULT = min-caml
SRC   = $(shell ls *.ml *.mli)
TESTS = $(shell ls test/*.ml | grep -v toomanyargs)
JBUILD_BUILD_PATH=./_build/default

default: $(RESULT)

# jbuilderを使ったビルド
$(RESULT): $(JBUILD_BUILD_PATH)/main.exe $(SRC)
	cp $(JBUILD_BUILD_PATH)/main.exe $(RESULT)

$(JBUILD_BUILD_PATH)/main.exe: $(SRC)
	jbuilder build main.exe

# モジュールをすべてロードしたutopを起動する
utop:
	jbuilder utop

do_test: $(TESTS:%.ml=%.cmp)

.PRECIOUS: test/%.s test/% test/%.res test/%.ans test/%.cmp
TRASH = $(TESTS:%.ml=%.s) $(TESTS:%.ml=%) $(TESTS:%.ml=%.res) $(TESTS:%.ml=%.ans) $(TESTS:%.ml=%.cmp)

test/%.s: $(RESULT) test/%.ml
	./$(RESULT) test/$*
test/%: test/%.s libmincaml.S stub.c
	$(CC) $(CFLAGS) -m32 $^ -lm -o $@
test/%.res: test/%
	$< > $@
test/%.ans: test/%.ml
	ocaml $< > $@
test/%.cmp: test/%.res test/%.ans
	diff $^ > $@

clean:
	jbuilder clean
	rm $(RESULT) -f
	rm $(TRASH) -f

# 多分動かない
min-caml.html: main.mli main.ml id.ml m.ml s.ml \
		syntax.ml type.ml parser.mly lexer.mll typing.mli typing.ml kNormal.mli kNormal.ml \
		alpha.mli alpha.ml beta.mli beta.ml assoc.mli assoc.ml \
		inline.mli inline.ml constFold.mli constFold.ml elim.mli elim.ml \
		closure.mli closure.ml asm.mli asm.ml virtual.mli virtual.ml \
		simm.mli simm.ml regAlloc.mli regAlloc.ml emit.mli emit.ml
	./to_sparc
	caml2html -o min-caml.html $^
	sed 's/.*<\/title>/MinCaml Source Code<\/title>/g' < min-caml.html > min-caml.tmp.html
	mv min-caml.tmp.html min-caml.html
	sed 's/charset=iso-8859-1/charset=euc-jp/g' < min-caml.html > min-caml.tmp.html
	mv min-caml.tmp.html min-caml.html
	ocaml str.cma anchor.ml < min-caml.html > min-caml.tmp.html
	mv min-caml.tmp.html min-caml.html

# 多分動かない
release: min-caml.html
	rm -fr tmp ; mkdir tmp ; cd tmp ; cvs -d:ext:sumii@min-caml.cvs.sf.net://cvsroot/min-caml export -Dtomorrow min-caml ; tar cvzf ../min-caml.tar.gz min-caml ; cd .. ; rm -fr tmp
	cp Makefile stub.c SPARC/libmincaml.S min-caml.html min-caml.tar.gz ../htdocs/
