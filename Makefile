
build:
	cmake -DCMAKE_BUILD_TYPE=Release -S ./src -B ./build/Release && cmake --build ./build/Release
.PHONY: build

clean:
	rm -rf ./build