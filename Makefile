default:
	elm make src/Main.elm

spspcc:
	reset
	./spcc build/out.js
	node build/out.js

