.PHONY: all clean

all:
	mkdir -p ebin
	erlc -o ebin -pa ebin src/*.erl
	erlc -o ebin -pa examples src/*.erl

clean:
	rm -rf ebin
