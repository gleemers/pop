EXAMPLES_DIR = examples

.PHONY: all clean test install compile_examples help

all: build

build:
	mix deps.get
	mix escript.build

$(EXAMPLES_DIR):
	mkdir -p $(EXAMPLES_DIR)

clean:
	mix clean
	rm -f pop
	rm -f *.beam
	rm -f src/pop_lexer.erl
	rm -f src/pop_parser.erl
	rm -rf ebin/*
	rm -rf _build

test: build $(EXAMPLES_DIR)
	./pop examples/hello.pop
	./pop examples/factorial.pop
	./pop examples/fibonacci_sequence.pop
	./pop examples/tail_recursive_fibonacci.pop
	./pop examples/print_example.pop
	./pop examples/comment_example.pop

compile_examples: build $(EXAMPLES_DIR)
	./pop compile examples/hello.pop
	./pop compile examples/factorial.pop
	./pop compile examples/fibonacci_sequence.pop
	./pop compile examples/tail_recursive_fibonacci.pop
	./pop compile examples/print_example.pop
	./pop compile examples/comment_example.pop

install: build
	cp pop /usr/local/bin/

help:
	@echo "Pop Language Makefile"
	@echo "---------------------"
	@echo "make              - Build the Pop compiler"
	@echo "make test         - Run test examples"
	@echo "make compile_examples - Compile example files"
	@echo "make clean        - Clean build artifacts"
	@echo "make install      - Install Pop compiler globally"
	@echo "make help         - Show this help message"
