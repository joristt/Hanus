---
author: Author name here
title: Concepts of programming languages
subtitle: My title here
theme: uucs
monofontoptions: Scale=0.75
monofont: Fira Mono
mainfont: Fira Sans Light
sansfont: Fira Sans Light
---

Please use Markdown to write your slides. 

This makes sure that slides will be consistent -- and easy for me to
edit in the future.

---

Start a new slide with by beginning a new line three dashes `---`.

For example:

```
---

My contents

---
```

---

# Titles

You can use the hash symbol \# to make the title of a slide.

## Subtitle

You can use more than one hash symbol \#\# to have subtitles on your
slide.

---

* Bullet lists
* are pretty easy
* too!

---

# Emphasis

You can include a word in asterisks to add *emphasis* or two asterisks
to make it **bold**.

That is:

```
*emphasis* vs **bold**
```

---

# Images

Please include any images in the `img` subdirectory.

You can refer to images using the usual markdown syntax:

![My caption](img/uueduc.jpg "Alt caption"){ width=30% }

---

# Staged builds

This is quite easy

. . .

Just insert `. . .` on a new line when you want the slide to appear
incrementally.

---

# Code

You can use backticks to include inline code such as `x` or `y`.

Use three backticks to introduce a code block:

```
main = print "Hello world!"
```

---

# Syntax highlighting

There are syntax highlighting options for the most widely used
languages.

```haskell
foo y = let x = 4 in x + z
  where
  z = 12
```

---

# Making slides

I've included a Makefile to build slides.

You will need to have the Haskell tool `pandoc` installed:

```
> cabal install pandoc
> make
```
---

# Working with markdown

You may want to install the markdown mode for emacs (or some other
editor of choice).

I've included some file local variables at the bottom of this file --
you may find them useful.

---

# Inline latex

You can always use \emph{inline} \LaTeX commands if you want.

But try to avoid this if you can.

Most Markdown commands should suffice.

\LaTeX is useful for formula's

\begin{equation}
\tau + x = \sigma
\end{equation}

Or inline formulas, enclosed in dollar symbols like so $\tau + x$.

---

# Questions

If you can't get things to work, don't hesitate to get in touch!

---

# Janus

A reversible programming language

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

<!-- Local Variables:  -->
<!-- pandoc/write: beamer -->
<!-- pandoc/latex-engine: "xelatex" -->
<!-- pandoc/template: "beamer-template.tex" -->
<!-- End:  -->
