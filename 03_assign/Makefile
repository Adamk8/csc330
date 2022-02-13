default: patterns-image.x86-linux

patterns-image.x86-linux: src/main.sml build.cm src/patterns.sml src/testing.sml
	 ml-build build.cm Main.main patterns-image

test: patterns-image.x86-linux
	sml @SMLload  patterns-image.x86-linux 

clean:
	rm -f pattern-image.x86-linux
