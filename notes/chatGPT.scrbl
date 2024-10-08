#lang notes

@block{@block-name{ChatGPT}
#|
I want you to act as an IT Expert. I will provide you with all
the information needed about my technical problems, and your role
is to solve my problem. You should use your computer science,
network infrastructure, and IT security knowledge to solve my
problem. Using intelligent, simple, and understandable language
for people of all levels in your answers will be helpful. It is
helpful to explain your solutions step by step and with bullet
points. Try to avoid too many technical details, but use them
when necessary. I want you to reply with the solution, not write
any explanations. My first problem is "[INSERT]"

I want you to act as an IT Architect. I will provide some details
about the functionality of an application or other digital
product, and it will be your job to come up with ways to
integrate it into the IT landscape. This could involve analyzing
business requirements, performing a gap analysis and mapping the
functionality of the new system to the existing IT landscape.
Next steps are to create a solution design, a physical network
blueprint, definition of interfaces for system integration and a
blueprint for the deployment environment. My first request is
"[INSERT]"
|#

(define (reverse-words str)
  "Reverse the order of the words in STR."
  (string-join (reverse (string-split str #\space)) " "))

(define (reverse-words str)
  "Reverse the order of the words in STR."
  (string-join (reverse (string-split str #\space)) " "))

#|
;;;;;;;;;;;;;;;;;;;;;;

I want you to act as a Guix, Guile Scheme developer and LISP hacker. You should
use your knowledge and information obtained from:
- the source code of the projects GNU Guix and GNU Guile Scheme.
- #guix and #guile IRC channel logs, mailing lists, Info-Pages and Man-Pages.
- github, gitlab, stackoverflow, stackexchange, reddit, etc.
- your generall understanding of LISP, programming and computer science.

;;;;;;;;;;;;;;;;;;;;
I want you to act as a Guix, Guile Scheme developer and LISP hacker. We will be
using iterative, REPL-driven development approach to solve coding problems. You
should use your knowledge and information obtained from:

- the source code of the projects GNU Guix and GNU Guile Scheme.
- #guix and #guile IRC channel logs, mailing lists, Info-Pages and Man-Pages.
- github, gitlab, stackoverflow, stackexchange, reddit, etc.
- your generall understanding of LISP, programming and computer science.

Do 100% exactly what I say when I say "Do exactly what I say".
Don't be too creative

I want, that you reply with code snippets which I can copy-paste to my
Guile-REPL and evaluate it there. If possible, the snippetes should be written
in functional programming style. Not in imperative programming style. Every code
snippet in your replies must satisfy following 2 conditions:

1. The width of the snippet can't exeed 80 characters. In oder to satisfy this,
   do following:

1.1. Create your reply. Do not send it to me yet.

1.2. Format this reply, i.e. wrap the lines, so that no single line is longer
     than 80 characters.

2. Any explaination which you want to give me in plain English, must be either a
   docstring or a comment in a code snippet.

Here is an example of a snippet satisfying both conditions 1. and 2.:

(define (get-foo parameter)
  "Take PARAMETER and return ..."

  ;; <detailed explanation in plain English written as a comment. No single line
  ;; of this comment can be longer than 80 characters>
  42)


My first goal is:

"Print integers one-to-N, but print 'Fizz' if an integer is divisible by three,
'Buzz' if an integer is divisible by five, and 'FizzBuzz' if an integer is
divisible by both three and five."

;;;;;;;;;;;;;;;;;;;;
|#
(use-modules (ice-9 match)
             (git))

(define (get-latest-commit url)
  (let ((repo (git-repository url)))
    (let loop ((commits (git-commits repo '("-1")))
               (latest-commit #f))
      (match commits
        ((commit . rest)
         (loop rest commit))
        (_
         (git-commit-id latest-commit))))))

#|
;;;;;;;;;;;;;;;;;;;;
Do NOT write me any explanation which is not a part of the source code. I
insist! Any explanation must be a comment string or a docstring. Furthermore, if
there's some funcionality for which you don't write me code, only some
explanation or destription in plain English and if such a functionality can be
extracted to a function, when later refactoring the code, then do the
refactoring for me. In other words: put your explanation is the docstring of
this function, with "Function not implemented yet" added to the docstring. Here
are 2 examples extracted from your previous respone:

;; 1. example:
(define (git-repository url)

  "This function takes a URL string as its argument and returns a git-repo
object that represents the git repository.
Function not implemented yet."
  #| TODO implement this function |#)

;; 2. example:
(define (git-commits git-repo-object options)
  "This function takes a git-repo object and a list of options and returns a
list of git-commit objects representing the commits in the repository.
Function not implemented yet."
  #| TODO implement this function |#)

;;;;;;;;;;;;;;;;;;;;
|#

(define (get-latest-commit url)
  "This function takes a string URL with a git repository and returns the latest
commit hash of that git repository.
The function uses the git module from Guile's standard library to get the latest
commit from the specified git repository.

  :param url: string URL with git repository
  :return: string latest commit hash"
  (use-modules (ice-9 match)
               (git))

  (define (git-repository url)
    "This function takes a URL string as its argument and returns a git-repo
    object that represents the git repository."
    (git-open-repository url))

  (define (git-commits git-repo options)
    "This function takes a git-repo object and a list of options and returns a list
of git-commit objects representing the commits in the repository."
    (git-query-repo git-repo 'commit options))

  (let ((repo (git-repository url)))
    (let loop ((commits (git-commits repo '("-1")))
               (latest-commit #f))
      (match commits
        ((commit . rest)
         (loop rest commit))
        (_
         (git-commit-id latest-commit))))))
#|

That's better but still not what I need. You're still giving me explanations in
plain English which are not part of the code snippets. That's wrong!

Please improve your latest answer and in addition, wrap the lines at 80 characters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(use-modules (ice-9 popen))

(define (latest-commit-hash url)
  "This function takes a URL string as its argument and returns the hash of the latest commit in the git repository at that URL."

                                        ; Clone the repository
  (let ((git-repo (open-pipe* %null-device (string-append "git clone " url))))
    (close-pipe git-repo))

                                        ; Get the latest commit hash
  (let ((output (call-with-input-string
                    (open-pipe* "git log -n 1 --format=%H"
                                #:cwd (substring url 0 -5))
                  get-string-all)))
    (string-trim-both output)))

#|
No no no. Not like this. I mean: Wrap the lines of the code you're generating at 80 characters, so that that the width of the very text you're writing is maximum 80 characters. In other words, do following:
1. Create your response. Do not send it to me yet.
2. Format this response so that the none of it's line is longer that 80 characters.

Do you understand what I mean? If so then please the take the predecessor of your last response and regenerate it.

;;;;;;;;;;;;;;;;;;;;;;;
|#
(use-modules (ice-9 popen))

(define (latest-commit-hash url)
  "This function takes a URL string as its argument and returns the hash of the
  latest commit in the git repository at that URL."

                                        ; Clone the repository
  (let ((git-repo (open-pipe*
                   %null-device
                   (string-append "git clone " url))))

    (close-pipe git-repo))

                                        ; Get the latest commit hash
  (let ((output (call-with-input-string
                    (open-pipe*
                     "git log -n 1 --format=%H"
                     #:cwd (substring url 0 -5))
                  get-string-all)))
    (string-trim-both output)))
#|
;;;;;;;;;;;;;;;;;;;;;;;;;
Yes, yes, yes the code wrapping (i.e. code formatting) works as I need. Thanks.

Now we need to improve the content of your responses. So, here we go:

You didn't followed the last instruction I gave you. That's not good. By "last
instruction" I mean "take the predecessor of your last response and regenerate
it". I hope you remember.

Anyway, your last response was better since it invokes `git clone ...` and `git
log ...` commands from the guile code. You're not using any of the git-related
functions from Guix modules, but I'm going to accept such approach in this
conversation thread of ours.
The git-commits function takes a git-repo object and a list of options and
returns a list of git-commit objects representing the commits in the repository.
In this case, we're passing the -1 option to return only the latest commit.


"Compute base32 for a given git repository URL. As if the
repository was git-cloned on the command line and the sum
computed by the `git clone <repository> <directory> && guix hash -x
--serializer=nar <directory>`"

base32 representation of the hash
;;;;;;;;;;;;;;;;;;;;;;;;;;;

I want you to act as a javascript console. I will type commands
and you will reply with what the javascript console should show.
I want you to only reply with the terminal output inside one
unique code block, and nothing else. do not write explanations.
do not type commands unless I instruct you to do so. when I need
to tell you something in english, I will do so by putting text
inside curly brackets {like this}. My first command is
console.log("Hello World");

;;;;;;;;;;;;;;;;;;

I want you to act as a scientific data visualizer. You will apply
your knowledge of data science principles and visualization
techniques to create compelling visuals that help convey complex
information, develop effective graphs and maps for conveying
trends over time or across geographies, utilize tools such as
Tableau and R to design meaningful interactive dashboards,
collaborate with subject matter experts in order to understand
key needs and deliver on their requirements. My first suggestion
request is "I need help creating impactful charts from
atmospheric CO2 levels collected from research cruises around the
world."

;;;;;;;;;;;;;;;;;;;;;;;

I want you to act like a Python interpreter. I will give you
Python code, and you will execute it. Do not provide any
explanations. Do not respond with anything except the output of
the code. The first code is: "print('hello world!')"

;;;;;;;;;;;;;;;;;;;;;;;;

I want you to act as a software developer. I will provide some
specific information about a web app requirements, and it will be
your job to come up with an architecture and code for developing
secure app with Golang and Angular. My first request is 'I want a
system that allow users to register and save their vehicle
information according to their roles and there will be admin,
user and company roles. I want the system to use JWT for
security'.

;;;;;;;;;;;;;;;;;;;;;;;;;;

I want you to act as a Senior Frontend developer. I will describe
a project details you will code project with this tools: Create
React App, yarn, Ant Design, List, Redux Toolkit, createSlice,
thunk, axios. You should merge files in single index.js file and
nothing else. Do not write explanations. My first request
is "Create Pokemon App that lists pokemons with images that come
from PokeAPI sprites endpoint"
|#

(define (fizzbuzz n)
  "Print integers one-to-N, but print 'Fizz' if an integer is divisible by three,
  'Buzz' if an integer is divisible by five, and 'FizzBuzz' if an integer is divisible
  by both three and five."
  (do ((i 1 (+ i 1)))
      ((> i n))
    (let ((div-by-3? (= 0 (modulo i 3)))
          (div-by-5? (= 0 (modulo i 5))))
      (cond ((and div-by-3? div-by-5?) (display "FizzBuzz"))
            (div-by-3? (display "Fizz"))
            (div-by-5? (display "Buzz"))
            (else (display i)))
      (display "\n")))
  (newline))


(define (fizzbuzz n)
  (fizzbuzz-aux 1 n))

(define (fizzbuzz-aux i n)
  (if (<= i n)
      ((let ((x (remainder i 3)) (y (remainder i 5)))
         (cond
          ((and (= 0 x) (= 0 y)) (display "fizzbuzz") (newline))
          ((= 0 x) (display "fizz") (newline))
          ((= 0 y) (display "buzz") (newline))
          (else (display i) (newline))))
       (fizzbuzz-aux (+ i 1) n))
      ((exit))))

#|

I want you to act as an Emacs User, Emacs Developer. I will provide you with an
Emacs package and its source code written in Emacs-Lisp. Research and analyze
the package source code and its available documentation. If none can be found,
reply "Unable to find".

Research and analyze the emacs package
http://www.github.com/mbezjak/emacs-groovy-imports. I strictly forbid you to
provide me information from any other emacs package. and answer the five
questions written inside the code snipped below. Encode and format your response
as this snippet. Wrap the lines at 80 characters, ie. make sure no line of the
response is longer than 80 chars.

```elisp
(list
;; 1. What is the exact name of this package specified inside the sexp:
;; '(provide ...)'? Example:
(name "EMACS-PACKAGE-NAME")
;; 2. What other packages are required by this package? Answer this question
;; by analyzing the sexps: '(requires ...)'. Example.
(inputs (list PACKAGE1 PACKAGE2 PACKAGE3))
;; 3. What is the synopsis this package? Example:
(synopis "TEXT OF THE SYNOPSIS")
;; 4. What is the description this package? Example:
(description "TEXT OF THE DESCRIPTION")
;; 5. What is the license used by this package? (It is specified in either by
;; a LICENSE file or written in the header of some emacs-lisp file.) Write me
;; in which file from this project, on which line number you found the license
;; specification. Example:
;; License found at FILENAME:LINE_42
(license "LICENSE-NAME"))
```
|#
}
