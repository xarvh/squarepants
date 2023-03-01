
default:
	reset
	/usr/bin/time --format=%E ./squarepants src/Main.sp build/unstable || (xcowsay "spsp T_T"; exit 1)
	xcowsay "spsp Done"

with-unstable:
	reset
	/usr/bin/time --format=%E ./build/unstable src/Main.sp build/made-with-unstable || (xcowsay "spsp T_T"; exit 1)
	xcowsay "spsp Done"

# https://nodejs.org/en/docs/guides/simple-profiling/
profile:
	rm -f isolate-0x*-v8.log
	node --prof --stack-size=65500 ./squarepants src/Main.sp build/unstable
	node --prof-process isolate-0x*-v8.log > profile.log

