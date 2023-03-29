# my-nn

# notes on clojure

## Install Java steps

`sudo apt install default-jre`

`java --version`

## leiningen

`sudo apt install leiningen`

### new proj

`lein new app clojure-noob`

`lein run`

### dependencies

in project.clj `:dependencies [... [net.mikera/core.matrix "0.63.0"]]`

then fetch: `lein deps`

### Build

`lein uberjar`

`java -jar target/uberjar/clojure-noob-0.1.0-SNAPSHOT-standalone.jar`

## REPL

`lein repl`

## Emacs

`M-x package-list-packages`

install
* `clojure-mode`
* `cider`
* (optional) `paredit`

### cider

`M-x cider-jack-in`

Execute statement: `C-x C-e`

Print the result after point: `C-u C-x C-e`

Compile your current file `C-c C-k`

Documentation: `C-c C-d C-d`

##### Summary:

`C-c M-n M-n` Switch to namespace of current buffer.
`C-x C-e` Evaluate expression immediately preceding point.
`C-c C-k` Compile current buffer.
`C-c C-d C-d` Display documentation for symbol under point.
`M-. and M-,` Navigate to source code for symbol under point and return to your original buffer.
`C-c C-d C-a` Apropros search; find arbitrary text across function names and documentation.

## vscode

install `calva`

### calva shortcuts

`C-k C-i` docs

`C-Enter`, `M-Enter` Evalute forms

`Ctrl+Alt+C Enter` loading a file

`Ctrl+Alt+Enter` after each form in `->`

`Ctrl+Alt+C Ctrl+Alt+D` Interupt command execution

### paredit shortcuts

`Shift+Alt+RightArrow` select outside

`Shift+Alt+LeftArrow` select inside (Shrink selection)
