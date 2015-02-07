-----------------
Slovenske Ljudske
-----------------

A search engine of a subset of Slovene Folk Songs.

[webpage] (http://gto76.github.io/sl-ljudske/index.html)

* Short list of features:
Inverted index,
lemmatization,
stopword removal,
cosine distance,
visualization of the matches.

* Libraries used:
d3js - Javascript library used for the visualization

* Interesting findings:
Most frequent word is mother (mati)

* How to generate webpage:
```
$ cd scala
$ ./run
```
>Script called `run` starts a script called `indexSongs.scala` with predefined parameters, namely the names of the files in folder `input-data`. It generates three js files and stores them in `js` folder. Details are described in at the start of the `indexSongs.scala` script.
