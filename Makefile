
all: build

build:
	dune build @install

clean:
	dune clean

doc:
	dune build @doc

format:
	dune build @fmt

test:
	dune runtest

WATCH?=@src/check @runtest

watch:
	dune build $(WATCH) -w
