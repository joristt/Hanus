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

---

# Variables

- All global variables
- Default value
- Modification operators
- Only support for ```+=```, ```-=``` and ```^=```

---

# Limitations

- There is no ```*=``` and ```/=```
- A variable that occurs on the left can not occur on the right in the same statement
	- ```x-=x``` is forbidden

```haskell
a b c

procedure main
    a += 3
    b -= a + 4
    c += a - b
```

# Procedures

- No parameters
- There exists version with parameters
- Pass by reference

```haskell
a
procedure main
    call f
    uncall g

procedure f
    a += 3

procedure g
    a -= 5
    a += 1
```
---

# Loop

```haskell
    from e1 do
        s1
    loop
        s2
    until e2
```
- ```e1``` is true only the first iteration, false every other iteration
- ```s1``` is executed after ```e1``` on every iteration
- ```e2``` is false until the last run
- ```s2``` is executed if ```e2``` is true, continiue to ```e1```

---
# Loop

```haskell
a
b

procedure main
    from a = 0 do
        a += 1
    loop
        b += a
    until a = 10
```
Result: a = 10, b = 45

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

<!-- Local Variables:  -->
<!-- pandoc/write: beamer -->
<!-- pandoc/latex-engine: "xelatex" -->
<!-- pandoc/template: "beamer-template.tex" -->
<!-- End:  -->
