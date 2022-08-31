#lang notes

A set of objects/object-types and a set of morphism/actions between the objects/object-types

@block{@block-name{Object-types}
  @block{@block-name{Examples:}
    - a set of game-actors. E.g.: animals, persons, etc.
    - a set of environment-objects. E.g.: buildings, roads, furniture, etc.
    - a set of items. E.g.: that particular computer I'm writing this email on
    - a set of game-states
  }
}

@block{@block-name{Morphisms/Actions}
  @block{@block-name{Examples:}
    upgrade-item, go-left, drop-item, sun-rise, sun-set, rainfall
  }

  @block{@block-name{morphism/action acts on one or multiple objects}
    object1, objec2, ..., objectN -> object
  }

  @block{@block-name{pure actions vs. actions with side effects}
    Examples:
    - do-step-forward does not change the world
    - do-eat-apple changes the state-of-world (environment-context). One apple
      disappeared from the world.
  }
}

@block{@block-name{game-world timespace characteristics}
  @block{@block-name{time-flow:}
    turn based games vs. real-time games, with events happening simultaneously.
  }

  @block{@block-name{space-topology, -boundaries, -dimensions, -tilling, -???}
    Examples:
    - just a (set of) few places with a (set of) few transitions between them
    - planar-world with edges and boundaries (e.g. chess board)
    - torus, sphere, circle (e.g. the (Monopoly circle)
    - number of dimensions: 1D, 2D, 3D
    - with boundaries. i.e. edges, corners, walls, etc. E.g. chess boards
    - no boundaries: "going full circle around the world"
    - world-tilling: hexagons, squares, triangles, etc.
    - the "funny" stuff: Möbius stripe
   }
}

@block{@block-name{game-theme}
  @block{@block-name{Examples:}
    middle-ages, winter
   }
}

@block{@block-name{Order/Hierarchy relations among the elements of an object-type:}
  @block{@block-name{Examples:}
  *** game-states
      - a subset of end-states of the game: i.e. the state of the game where the
        game-over message is displayed.
      - a subset of intermediary game-states
      - a subset start-states of the game
  *** Any porcelain-cup is at least as good as any wooden-cup:
      wooden-cups <= porcelain-cups
  }
}

@block{@block-name{rendering-engine, game-physics, ???}
  gravitation, object visibility, etc.
}

@block{@block-name{Invariants/invariant-conditions}
  "the world must make sense": always valid facts, no matter what morphism/action happens.
  @block{@block-name{Examples:}
    - nr. of available vaccination-shots before and after the morphis/action of
      vaccination must differ by exactly one.
    - start-state can be an end-state at the same time, however an end-state
      cannot be reached before reaching a start-state:
        `start-state <= intermediary-state <= end-state`
  }
}

@block{@block-name{Production rules and product-types}
  - denoted by the cross 'x'
  - "bringing together" i.e. "pairing" a set of steel-needles with a set of
    empty-plastic-barrels give rise to the set of empty-syringes:
     `steel-needles x empty-plastic-barrels -> empty-syringes`
}

@block{@block-name{Identity morphism/action}
  - for every object-type
  - the do-nothing action: take something and do-nothing with it.
    (i.e. the multiplication by 1)
}

@block{@block-name{Morphism/Action composition}
  the two actions: one-step-forward immediately followed by another
  one-step-forward can be composed to a single action: two-steps-forward
}

@block{@block-name{Environment context = game-states}
  @block{@block-name{Example}
    "Jim dots, Jack 100" makes no sense w/o context
    - automatons: Mealy / Moore
  }
}
