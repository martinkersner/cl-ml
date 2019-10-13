# cl-ml

Machine Learning library for Common Lisp

Martin Kersner, <m.kersner@gmail.com>

`cl-ml` currently supports following machine learning algorithms:
* k-Nearest Neighbors
* Linear Regression
* Logistic Regression
* Decision Trees (ID3)
* Support Vector Machines
* [Multinomial Naive Bayes Classifier](https://github.com/martinkersner/cl-ml#naive-bayes-classifier)
* [Artificial Neural Networks](https://github.com/martinkersner/cl-ml#artificial-neural-networks)

### Requirements
`cl-ml` requires [CLISP](https://www.gnu.org/software/clisp/) and [ASDF](https://gitlab.common-lisp.net/asdf/asdf.git) >= 3.1. [`cl-math`](https://github.com/martinkersner/cl-math.git) and [`cl-plot`](https://github.com/martinkersner/cl-plot.git) are included as submodules to `cl-ml` project.

### Get the latest version
```bash
git clone --recursive https://github.com/martinkersner/cl-ml.git
```

### Start using `cl-ml`
```common-lisp
(load "init")
```

## Algorithms

### Naive Bayes Classifier
Currently, only Multinomial Naive Bayes Classifier is supported. 

1. Create instance of Naive Bayes Classifier
```common-lisp
(defparameter *nbc*
  (make-instance 'naive-bayes-classifier))
```

2. Pass training data. Training data will be transformed to feature vectors within training procedure.
```common-lisp
; X-train represent training data in list of lists format
; y-train represent training labels in cl-math matrix format
(fit *nbc* X-train y-train)
```

3. Predict on new data
```common-lisp
; X-test represent training data in list of lists format
(predict *nbc* X-test)
```

### Artificial Neural Networks
Currently, only [fully connected layers](http://cs231n.github.io/convolutional-networks/#fc) with [sigmoid activation function](https://en.wikipedia.org/wiki/Sigmoid_function) are supported. Optimization is performed using [Stochastic Gradient Descent](https://en.wikipedia.org/wiki/Stochastic_gradient_descent) (SGD).

1. Create network with 2 neurons in input layer, 3 neurons in hidden layer and 1 neuron the output layer.
```common-lisp
(defparameter *nn*
  (make-instance 'neural-network :nn-dims '(2 3 1)))
```

2. Define training parameters for SGD.
```common-lisp
(defparameter *params*
  (generate-params '((num-epoch       50)
                     (lr              0.3)
                     (mini-batch-size 20))))
```

3. Start training.
```common-lisp
; X-train represent training data in cl-math matrix format
; y-train represent training labels in cl-math matrix format
(fit *nn* X-train y-train *params*)
```

4. Predict on new data.
```common-lisp
; X-test represent testing data in cl-math matrix format
(predict *nn* X-test)
```

## Project Roadmap

1. Add new algorithms
   * k-Means
   * Adaboost
   * Autoencoders
   * CNN
1. Add new default data sets
1. Methods for data manipulation
   * Generating new data
1. Methods enabling easier training
   * Cross validation
   * Grid search
