#lang notes

#+title: CPS

@block{@block-name{Continuation Passing Style}
  [[https://youtu.be/c-7AW4yFXNs][YouTube: Tiago Cogumbreiro - Continuation passing style intro]]
  [[https://cogumbreiro.github.io/teaching/cs450/s21/lecture32.html][Tiago Cogumbreiro - Lecture 32: Monadic continuations]]
  CPS is a form of inversion of control.
  CPS: "Control the Control-Flow"
  CPS: computation abstracted with function

  Returning a value is a function call
  try-catch is dual to bind
  (+ 1 2 (call/cc f) 4 5)
  ;; becomes:
  (f (lambda (x) (+ 1 2 x 4 5)))

  [[https://github.com/lix4/CSSE304-Programming-Language-Concept-Code/blob/master/more-callcc-examples.ss][More call/cc examples]]

  (let ([f 0] [i 0])
    (call/cc (lambda (k) (set! f k)))
    (printf "~a~n" i)
    (set! i (+ i 1))
    (if (< i 10) (f "ignore") 'else))
}

@block{@block-name{Teaching Optics through Conspiracy Theories by Bartosz Milewski #FnConf 2022}
https://youtu.be/GjTP-nozuns?t=540
Continuation is a functor
Continuation function is a handler

  -- in the source code use: {-# LANGUAGE RankNTypes #-}
  :set -XRankNTypes
  p1 :: Int -> Int; p1 x = 1 + x
  -- continuation type definition
  type Cont a = forall r. (a -> r) -> r
  -- multiple line input begin :{
  :{
  -- make continuation
  mkCont :: a -> Cont a
  mkCont a = \k -> k a
  :}

  :{
  -- run continuation
  runCont :: Cont a -> a
  runCont k = k id
  -- show type of runCont `:t runCont`
  :}
  -- multiple line input end :}
  (mkCont 3) p1
  runCont (mkCont 5)

  :set -XRankNTypes
  -- in the source code use: {-# LANGUAGE RankNTypes #-}
  -- multiple line input begin :{
  p1 :: Int -> Int; p1 x = 1 + x
  p2 :: Int -> Int; p2 x = 2 + x
  -- Yoneda type definition
  type Yo f a = forall x. (a -> x) -> f x
  :{
  runYo :: Functor f => Yo f a -> f a
  runYo g = g id
  :}

  :{
  mkYo :: Functor f => f a -> Yo f a
  mkYo fa = \g -> fmap g fa
  :}
  (mkYo [1]) p1
  runYo (mkYo [1])
  -- multiple line input end :}

  #+RESULTS:
}
