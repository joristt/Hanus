# Janus DSL

To setup: `stack setup`

To build: `stack build`

To execute: `stack exec -- janus <args>`

To check available args: `stack exec -- janus -h`

```bash
  Usage: janus (-i|--input STRING) [--dummy-int INT]
    Janus DSL

  Available options:
    -h,--help                Show this help text
    -i,--input STRING        Janus source file
    --dummy-int INT          Dummy integer argument (default: 1)
```

To run the provided test suite: `stack test`
