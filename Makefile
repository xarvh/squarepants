
default:
	reset
	NODE_OPTIONS="--max-old-space-size=16384" /usr/bin/time -f %E ./spcc sp/Main.sp build/out.js || (xcowsay T_T; exit 1)
	xcowsay Done

