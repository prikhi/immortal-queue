# Immortal Queue

A Haskell library for building a pool of queue-processing worker threads.


## Developing

You can build the project with stack:

```
stack build
```

For development, you can enable fast builds with file-watching,
documentation-building, & test-running:
```
stack test --haddock --fast --file-watch --pedantic
````

To build & open the documentation, run:

```
stack haddock --open immortal-queue
````


## LICENSE

BSD-3
