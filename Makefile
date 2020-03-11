.DEFAULT_GOAL := build

# reload browser script of mine that reloads my browser
# xdotool search --class "Firefox" key --window %@ "ctrl+r
# development is done with entr; every time the file is saved
# its recompiled and reloads the browser:
# find src | entr -cdp make develop

develop: build
	reload-browser
build:
	elm make src/Main.elm --output=main.js
