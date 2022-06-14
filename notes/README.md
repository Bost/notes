#### How to write a section

Begin every `.scrbl` file with the language:

```
#lang notes
```

This language imports every library you need to start writing a Scribble
 document using our flavor of `scribble/manual`.

If you want more, edit `main.rkt` to provide it.


#### How to build

- For the first time : `make all`
- After the first time, for a faster build: `make`


#### Dependencies

- racket (6.8+)
- scribble (6.8+)
