#lang notes

@block{@block-name{Make}
  Makefiles: 95% of what you need to know
  https://youtu.be/DtGrdB8wQ_8

  recompile (only) what is needed.


  @block{@block-name{Makefile format}
    target: dependencies

    Presence of the targets 'all', 'clean' is convention.

    Percent char '%' is a wild card.
    at @"@" - the target side (left side of the ':')
    carret ^ - dependency side (right side of the ':')
    https://youtu.be/DtGrdB8wQ_8?t=963
  }
}
