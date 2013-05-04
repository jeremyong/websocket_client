.PHONY: all clean test

all:
	rebar compile

clean:
	rm -rf ebin

test:
	rebar skip_deps=true ct verbose=1
