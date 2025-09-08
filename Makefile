
build:
	cmake -DCMAKE_BUILD_TYPE=Debug -S ./src -B ./build && cmake --build ./build
.PHONY: build

clean:
	rm -rf ./build