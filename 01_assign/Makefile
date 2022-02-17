default: babies-image.x86-linux

babies-image.x86-linux: src/babies.sml build.cm src/csc330.sml
	 ml-build build.cm Main.main babies-image

run: babies-image.x86-linux
	sml @SMLload  babies-image.x86-linux data/babies.txt 1920

test-02: babies-image.x86-linux
	sml @SMLload  babies-image.x86-linux data/babies.txt 1920 < tests/test-02.in 

test-03: babies-image.x86-linux
	sml @SMLload  babies-image.x86-linux data/babies.txt 1920 < tests/test-03.in 

test-04: babies-image.x86-linux
	sml @SMLload  babies-image.x86-linux data/babies.txt 1920 < tests/test-04.in 

clean:
	rm -f babies-image.x86-linux
