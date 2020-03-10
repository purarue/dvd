.DEFAULT_GOAL := build

develop: build
	reload-browser
build:
	elm make src/Main.elm --output=main.js
