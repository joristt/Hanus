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
