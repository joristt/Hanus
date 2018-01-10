---
author: Joris ten Tusscher, Joris Burgers, Ivo Gabe de Wolff, Cas van der Rest, Orestis Melkonian
title: Concepts of programming languages
subtitle: Janus
theme: uucs
monofontoptions: Scale=0.75
monofont: Fira Mono
mainfont: Fira Sans Light
sansfont: Fira Sans Light


---


# Calculating (n+1)-th and (n+2)-th Fibonacci number


```haskell
procedure fib
	if n = 0 then
		x1 += 1    ; -- 1st Fib nr is 1.
		x2 += 1    ; -- 2nd Fib nr is 1.
	else
		n -= 1
		call fib
		x1 += x2
		x1 <=> x2
	fi x1 = x2     ; -- Why do we need this?
```


---


# Calculating (n+1)-th and (n+2)-th Fibonacci number


```haskell
procedure fib
	if n = 0 then
		x1 += 1    ; -- 1st Fib nr is 1.
		x2 += 1    ; -- 2nd Fib nr is 1.
	else
		n -= 1
		call fib
		x1 += x2
		x1 <=> x2
	fi x1 = x2     ; -- Why do we need this?
```

* Q: How do we calculate the inverse?


---


# Calculating (n+1)-th and (n+2)-th Fibonacci number


```haskell
procedure fib
	if n = 0 then
		x1 += 1    ; -- 1st Fib nr is 1.
		x2 += 1    ; -- 2nd Fib nr is 1.
	else
		n -= 1
		call fib
		x1 += x2
		x1 <=> x2
	fi x1 = x2     ; -- To be able to invert the procedure.
```

* Q: How do we calculate the inverse?
* A: Using the statement inverter.
![The statement inverter for Janus if statements.](img/if-inverse.png "The statement inverter for Janus if statements."){ width=100% }


---


# Calculating (n+1)-th and (n+2)-th Fibonacci number
## The inverse of fib calculates n given the (n+1)-th and (n+2)-th Fibonacci numbers


```haskell
procedure fib'       ; -- The inverse of fib.
	if x1 = x2 then
		x2 -= 1      ; -- 2nd Fib nr is 1.
		x1 -= 1      ; -- 1st Fib nr is 1.
	else
		x1 <=> x2
		x1 -= x2
		call fib'
		n += 1
	fi n = 0
```


---


# Example 2: Probability Density Function of Exp distribution.

* CDF of exp: $\frac{1}{b} e^ {\frac{−-x}{b}}$
* But Janus doesn't allow negative integers, so:
* PDF of exp: $\frac{1}{b} {\frac{1}{e^{\frac{x}{b}}}}$
* Let's also introduce a parameter $\lambda = \frac{1}{b}$:
* PDF of exp: $\lambda {\frac{1}{e^{x\lambda}}}$
* PDF of exp: ${\frac{\lambda}{e^{x\lambda}}}$
* This is still most probably going to be a real number...
* Solution: multiply everything times a very large constant


---


# Example 2: Probability Density Function of Exp distribution.

* PDF of exp: ${\frac{\lambda}{e^{x\lambda}}}$

```haskell



# Relational Programming
\small
\renewcommand{\arraystretch}{1.5}
\begin{tabular}{c|c}
  \textbf{Injective Programming} & \textbf{Relational Programming} \\
  $r$-Turing Complete & Turing Complete \\
  backwards deterministic & backwards non-deterministic \\
  restricted language constructs & search procedure (aka \textit{resolution}) \\
\end{tabular}
\normalsize

# Prolog basics
A logic programs consists of *facts* and *rules*.
```prolog
parent(alice, joe).
parent(bob, joe).
parent(joe, mary).
parent(gloria, mary).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

descendant(X, Y) :- ancestor(Y, X).
```
---

The user can then *query* the runtime system, as such:
```prolog
?- parent(X, joe).
X = alice;
X = bob.

?- ancestor(X, mary).
X = joe;
X = gloria;
X = alice;
X = bob.

?- ancestor(X, mary), descendant(X, alice).
X = joe.
```

# Demonstration - Type Inference
Assume a `type` predicate, relating expressions with types:
```prolog
type(expr, t) :- ... .
```
You would normally use it to perform *type-checking*:
```prolog
?- type(1 + 1, int).
true.
?- type(1 + 1, string).
false.
```
---

But you can also performing *type-inference*:
```prolog
?- type(1 + 1, Type).
Type = int.
?- type("hello world", Type).
Type = string.
?- type(\x:int -> x, Type).
Type = int -> int.
?- type(\x -> x, Type).
Type = ?42 -> ?42.
?- type(\x -> x, int -> Type).
Type = int.
```
---

Going in the reverse direction, you can query the expression:
```prolog
?- type(Expr, int).
Expr = 1;
Expr = 2;
...
Expr = 1 + 1;
Expr = 1 + 2;
...
Expr = if true then 1 else 1;
...
```
Of course, this does not make much sense without a sufficiently expressive type system.

# Demonstration - Program Synthesis

Assume you have implemented a *relational interpreter*:
```prolog
eval(program, result) :- ... .

?- eval(map (+ 1) [1 2 3], Result).
Result = [2 3 4].
```

But you can also perform *program synthesis* by-example:
```prolog
?- eval(F 1, 2),...,eval(map F [1 2 3], [2 3 4]).
...
F = \x -> x + 1;
...
F = \x -> x - 10 + 10 + 1;
...
```
---

Quine generation is pretty straightforward:
```prolog
?- eval(Quine, Quine).
...
Quine = (\a -> a ++ show a) "(\\a -> a ++ show a) ";
...
```

# Logic Programming IRL
In practice, bi-directionality breaks with the usage of *extra-logical* features:

* **Variable projection**: inspecting values at runtime
* **Cut (!)**: disables backtracking in certain places
* **Assert/Retract**: Dynamically insert/remove facts

**MiniKanren** is a more recent logic programming language, which avoids extra-logical
features (as much as possible).

# Higher abstraction
* Relational programming, as well as functional programming, both belong to the
*declarative* paradigm.
* They both raise the level of abstraction, by enabling the programmer to express
*what* needs to be done, instead of *how*.

\centering
\textbf{Question}

\textit{How can we combine them, to get the best of both worlds?}

# Hanus: Janus embedded in Haskell
In our research project, we use *TemplateHaskell* and *QuasiQuotation* to embed
Janus in Haskell:
```haskell
[hanus|
  procedure encode(im :: Image, ret :: [Byte]) {
    -- Janus commands containing Haskell code
    -- e.g. janus_variable += <Haskell code>
  }
|]
encode :: Image -> [Byte]
encode = call encode
decode :: [Byte] -> Image
decode = uncall encode
```

Come and check out our poster in de Vagant!

# Thanks!

Feel free to ask any questions

<!-- Local Variables:  -->
<!-- pandoc/write: beamer -->
<!-- pandoc/latex-engine: "xelatex" -->
<!-- pandoc/template: "beamer-template.tex" -->
<!-- End:  -->
