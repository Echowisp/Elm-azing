all:
	elm make --output ./index.html src/Elmazing.elm

clean:
	rm index.html

