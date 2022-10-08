# OOPSLA'22 Paper Artifact: First Class Namer for Effect Handlers

Docker image: daanx/oopsla22-namedh:1.0

# Getting Started

We provide a docker image to run the examples of the paper.

```
> docker pull daanx/oopsla22-namedh:1.0
> docker run -it daanx/oopsla22-namedh:1.0
```

We now see the koka interpreter prompt as:

```
 _         _
| |       | |
| | _ ___ | | _ __ _
| |/ / _ \| |/ / _' |  welcome to the koka interactive compiler
|   ( (_) |   ( (_| |  version 2.4.1, Jul 11 2022, libc x64 (gcc)
|_|\_\___/|_|\_\__,_|  type :? for help, and :q to quit

loading: std/core
loading: std/core/types
loading: std/core/hnd

>
```

We can then see the provided examples by typing `:l samples/named-handlers/` `<tab>`:

```
> :l samples/named-handlers/
 1 net/             4 file.kk          7 file-scoped.kk
 2 ask.kk           5 heap.kk
 3 net.kk           6 unify.kk
```

When loading `ask.kk` for example we see:

```
> :l samples/named-handlers/ask.kk
compile: samples/named-handlers/ask.kk
loading: std/core
loading: std/core/types
loading: std/core/hnd
check  : samples/named-handlers/ask
modules:
  samples/named-handlers/ask

>
```

and we can run each sample by running `main()`:

```
> main()
check  : interactive
check  : interactive
add default effect for std/core/exn
linking: interactive
created: .koka/v2.4.1/gcc-debug/interactive

hello world
```

We can see the highlighted source of each sample using the `:s` command.

We can see the inferred type of an expression by using the `:t <expr>` command.

Exit the interpreter (and container) by using the `:q` command.


# Step-by-Step Guide

The included examples are:

1. `ask.kk`: A simple reader effect discussed in Section 2.2 of the paper
2. `file.kk`: A non-scoped file effect discussed in Section 2.3.1 (and Figure 1)
3. `file-scoped.kk`: A scoped file effect. This is not directly discussed in the paper
                     but very similar to the scoped reader of Figure 4. 
4. `heap.kk`: A first-class heap discussed in Section 3.2.4 (and Figure 5)
5. `unify.kk`: A unification effect discussed in Section 7.2
6. `net.kk`: A neural network discussed in Section 7.1


## Ask

This is the reader effect discussed in Section 2.2.
The output should look like:

```
> :l samples/named-handlers/ask.kk
...
> main()
...

hello world
```

## File

A non-scoped file effect discussed in Section 2.3.1 and Figure 1.
Reads the first lines of `artifact1.txt` and `artifact2.txt`.
The output should look like:

```
> :l samples/named-handlers/file.kk
...
> main()
...

Hello 
world!
```

## File-Scoped

A scoped file effect. This is not directly discussed in the paper
but very similar to the scoped reader of Figure 4. 
The output should look like:

```
> :l samples/named-handlers/file-scoped.kk
...
> main()
...

artifact1.txt: line 1
artifact2.txt: line 1
```

It is interesting to uncomment the "wrong" functions at the end 
of the source to see the static compile time errors that result when
a file escapes its scope (see the later section on how to modify sources and run them).


## Heap

A first-class heap with scoped references discussed in Section 3.2.4. 
This demonstrates we can model first-class polymorphic heaps with just (named) handlers.

```
> :l samples/named-handlers/heap.kk
...
> main()
...


42
```

## Unify

The unification example discussed in Section 7.2.

```
> :l samples/named-handlers/unify.kk
...
> main()
...

unified type: list int -> list int
```

## Net

The neural network example discussed in Section 7.1.

```
> :l samples/named-handlers/net.kk
...
> main()
...

epochs 1: (1, 1): Var(data: [0.65791339619326994], grad: [1] )
epochs 100: (1, 1): Var(data: [0.51198566799805911], grad: [1] )
...
epochs 4900: (1, 1): Var(data: [0.067684855309453493], grad: [1] )
epochs 5000: (1, 1): Var(data: [0.066819144610864939], grad: [1] )
```

The sampling is randomized so the actual numbers will be different from
run to run. The sample also generates the files `./netplot/plot.py` and `./netplot/data.json`
which can be used to see the "learned" sine function. To see the plot graphically,
you should exit the interpreter (with `:q`), get the docker container name, and 
use that to copy the generated files out of the container onto the local file system:

```
> docker ps -a 
CONTAINER ID   IMAGE                        COMMAND   CREATED        STATUS                     PORTS     NAMES
8fa5d65107b7   daanx/oopsla22-namedh:1.0    "koka"    24 hours ago   Exited (0) 24 hours ago              gifted_saha
...

> docker cp gifted_saha:/build/koka/netplot/plot.py   plot.py
> docker cp gifted_saha:/build/koka/netplot/data.json data.json
```
(here the name `gifted_saha` will be something else that was generated on your system).

Now one can run Python locally to render a plot:

```
> python3 plot.py
```


# Changing The Examples

You can start the docker container with a bash prompt as:

```
> docker run -it daanx/oopsla22-namedh:1.0  bash
```

and use `vim` to edit the samples in the `samples/named-handlers` directory.
You can use the `koka` command to start the interpreter, or directly
run the examples as:

```
> koka -e samples/named-handlers/ask.kk
```

for example.


# Docker 

The provide container is just a build of Koka of the `artifact-namedh` branch.
One can just checkout that branch as:

```
> git clone --recursive https://github.com/koka-lang/koka -b artifact-namedh
> cd koka
> stack build --fast
> stack exec koka
```

and run all the provided examples as described here.
