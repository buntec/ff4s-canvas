# Serve the examples at http://127.0.0.1:8080/ (run `just install` and `just watch-examples` first)
serve-examples:
    npm exec vite -- examples --host 127.0.0.1 --port 8080

# Recompile the examples on every source change
watch-examples:
    sbt '~examples/fastLinkJS'

# Build the static examples site (as published to GitHub Pages) into examples/target/site
site:
    sbt examples/site

# Install the JavaScript dependencies (Vite) from package-lock.json
install:
    npm ci

# Run the test suite
test:
    sbt test
