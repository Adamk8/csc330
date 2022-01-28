default: set-image.x86-linux

set-image.x86-linux: src/main.sml build.cm src/csc330.sml src/set.sml src/testing.sml
	 ml-build build.cm Main.main set-image

run: set-image.x86-linux
	sml @SMLload  set-image.x86-linux 

clean:
	rm -f set-image.x86-linux
