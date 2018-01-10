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

---

# Janus

A reversible programming language.

Not turing complete!

---

# Reversibility

Every statement can be reverted.
No history is stored.

```haskell
x += y * 3
```

. . .

```haskell
x -= y * 3
```

---

# Injective functions
Reversible languages can only compute injective functions.

\begin{equation}
\forall x, y: f(x) = f(y) \implies x = y
\end{equation}

Every output has only a single input.

. . .

\begin{equation}
h(x) = (x, g(x))
\end{equation}

---

# Turing completeness
Turing machines can compute non-injective functions.

Reversible languages are not turing complete.

Reversible Turing complete.

---

# Turing machines

Infinite tape of memory

Finite set of states

Transition function

- Current state
- Current symbol on tape
- Write symbol
- Move tape pointer
- Next state

---

# Turing machines

\emph{Forward deterministic}: given any state and tape, there is at most one transition \emph{from} that state.

\emph{Backward deterministic}: given any state and tape, there is at most one transition \emph{to} that state.

$P$ is the class of forward deterministic turing machines, $NP$ of non-deterministic turing machines.

Reversible Turing complete: a language that can simulate forward and backward deterministic turing machines.

---

# What do reversible languages compute

Given a forward deterministic turing machine that computes $f(x)$,

There exists a reversible turing machine that computes $x \rightarrow (x, f(x))$.

More memory.

# Example

```fib```: calculates (n+1)-th and (n+2)-th Fibonacci number.

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
	fi x1 = x2
```

---

# Example

```fib```: calculates (n+1)-th and (n+2)-th Fibonacci number.

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

# Example

```fib```: calculates (n+1)-th and (n+2)-th Fibonacci number.

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

# Example

```fib```: calculates (n+1)-th and (n+2)-th Fibonacci number.

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
	fi x1 = x2     ; -- Used for inverting the if-statement.
```

Q: How do we calculate the inverse?
![The statement inverter for Janus if statements.](img/if-inverse.png "The statement inverter for Janus if statements."){ width=100% }

---

# Example

```fib```: calculates (n+1)-th and (n+2)-th Fibonacci number.

```haskell
procedure fibInverse
	if x1 = x2 then
		x2 -= 1          ; -- 2nd Fib nr is 1.
		x1 -= 1          ; -- 1st Fib nr is 1.
	else
		x1 <=> x2
		x1 -= x2
		call fibInverse
		n += 1
	fi n = 0
```

---

# Example

```fib```: calculates (n+1)-th and (n+2)-th Fibonacci number.

* Q: What does the inverse of fib do?

```haskell
procedure fibInverse
	if x1 = x2 then
		x2 -= 1          ; -- 2nd Fib nr is 1.
		x1 -= 1          ; -- 1st Fib nr is 1.
	else
		x1 <=> x2
		x1 -= x2
		call fibInverse
		n += 1
	fi n = 0
```

---

# References

Axelsen, Holger Bock, and Robert Glück. "What do reversible programs compute?." FoSSaCS. 2011.

---

# Questions

\tiny{From CodeComics.com, modified. }
![The life of a Janus programmer.](img/meme.png "The life of a Janus programmer."){ width=94% }


<!-- Local Variables:  -->
<!-- pandoc/write: beamer -->
<!-- pandoc/latex-engine: "xelatex" -->
<!-- pandoc/template: "beamer-template.tex" -->
<!-- End:  -->
