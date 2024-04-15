
default: build
	./squarepants Main build/unstable
	@chmod +x build/unstable


with-unstable: build
	./build/unstable Main --corelib=./corelib build/made-with-unstable
	@chmod +x build/made-with-unstable


build:
	mkdir -p build


# https://nodejs.org/en/docs/guides/simple-profiling/
profile:
	rm -f isolate-0x*-v8.log
	node --prof --stack-size=65500 ./squarepants Main build/unstable
	node --prof-process isolate-0x*-v8.log > profile.log

