default:
	reset
	cd src; /usr/bin/time --format=%E node --stack-size=65500 ../spcc-stable Main main ../build/unstable || (xcowsay "spsp T_T"; exit 1)
	xcowsay "spsp Done"

# https://nodejs.org/en/docs/guides/simple-profiling/
profile:
	cd src; node --prof --stack-size=65500 ../spcc-stable Main main ../build/unstable
	node --prof-process src/isolate-0x*-v8.log > processed.txt

