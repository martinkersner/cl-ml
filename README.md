# Common Lisp Machine Learning Library

Martin Kersner, <m.kersner@gmail.com>

## Algorithms
* k-Nearest Neighbors
* Linear Regression
* Logistic Regression
* Decision Trees (ID3)
* Neural Networks (in progress)

## TODO
* Implement missing matrix and vector operations
* Plot graphs
* Verify correctness of algorithms
* Optimize computational speed
* Examples
* Datasets

## Installation steps

### Clone lisp-ml
```bash
git clone --recursive https://github.com/martinkersner/lisp-ml
```

### Install ASDF
```bash
cd lisp-ml/asdf
make
```

## Load lisp-ml
```common-lisp
(load "asdf/build/asdf")
(push (namestring (ext:cd)) asdf:*central-registry*)
(asdf:load-system :lispml)
```
or

```common-lisp
(load "init")
```
