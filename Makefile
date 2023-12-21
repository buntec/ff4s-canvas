setup:
	scala-cli setup-ide ./canvas
	scala-cli setup-ide ./examples

format:
	scala-cli fmt .

compile:
	scala-cli --power compile ./canvas

watch-compile:
	scala-cli --power compile ./canvas --watch

compile-examples:
	scala-cli --power compile ./examples

watch-compile-examples:
	scala-cli --power compile ./examples --watch

watch-package-examples:
	scala-cli --power package --js ./examples -o ./examples/main.js --force --js-mode dev --js-module-kind none --watch

publish-local:
	scala-cli --power publish local ./canvas --workspace .

clean:
	git clean -xdf
