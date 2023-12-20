setup:
	scala-cli setup-ide ./canvas
	scala-cli setup-ide ./examples

format:
	scala-cli fmt .

compile:
	scala-cli --power compile ./canvas

watch-compile:
	scala-cli --power compile ./canvas --watch

clean:
	git clean -xdf
