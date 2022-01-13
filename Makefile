default:
	reset
	/usr/bin/time --format=%E node --stack-size=65500 ./spcc-stable Main main build/unstable || (xcowsay "spsp T_T"; exit 1)
	xcowsay "spsp Done"

with-unstable:
	reset
	/usr/bin/time --format=%E node --stack-size=65500  ./build/unstable Main main build/made-with-unstable || (xcowsay "spsp T_T"; exit 1)
	xcowsay "spsp Done"

# https://nodejs.org/en/docs/guides/simple-profiling/
profile:
	rm isolate-0x*-v8.log
	node --prof --stack-size=65500 spcc-stable Main main build/unstable
	node --prof-process isolate-0x*-v8.log > profile.log

