# jedi

Clojure debugger

pre-Alpha

## Usage
lein repl
(debug-ext port) ; where port is for an external JVM process started with -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=port

## License

Copyright (C) 2012 Mike Messinides

Distributed under the Eclipse Public License, the same as Clojure.
