default:
	elm make src/Main.elm

spspcc:
	reset
	./spcc sp/Main.sp build/out.js
	node build/out.js

