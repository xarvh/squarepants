default:
	../squarepants App --platform=browser app.js
#	../squarepants CompileText.sp --platform=rawjs

with-unstable:
	reset
	../build/unstable App.sp --platform=browser app.js

deploy:
	git branch -D gh-pages
	git checkout -b gh-pages
	make
	git add -f app.js
	git commit -m 'publish'
	git push -f

