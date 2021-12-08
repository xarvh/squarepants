default:
	reset
	NODE_OPTIONS="--max-old-space-size=16384" ./spcc sp/Main.sp build/out.js; xcowsay Done

