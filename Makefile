all: site/main.js


site/main.js: $(wildcard source/*.elm)
	cd source && elm-make Main.elm --output ../site/main.js

