all: index.js

index.js: src/*.purs
	pulp browserify --no-check-main > $@
