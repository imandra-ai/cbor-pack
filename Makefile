
all: build

build:
	dune build @install

clean:
	dune clean

doc:
	dune build @doc

format:
	dune build @fmt

WATCH?=@check

watch:
	dune build $(WATCH) -w
