default:
	reset
	cd src; /usr/bin/time --format=%E node --stack-size=65500 ../spcc-stable Main main ../build/unstable || (xcowsay "spsp T_T"; exit 1)
	xcowsay "spsp Done"

