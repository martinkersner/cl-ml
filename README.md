# cl-ml

Machine Learning library for Common Lisp

Martin Kersner, <m.kersner@gmail.com>

`cl-ml` currently supports following machine learning algorithms:
* k-Nearest Neighbors
* Linear Regression
* Logistic Regression
* Decision Trees (ID3)
* Support Vector Machines
* [Artificial Neural Networks](https://github.com/martinkersner/cl-ml#artificial-neural-networks)

### Requirements
`cl-ml` requires [asdf](https://gitlab.common-lisp.net/asdf/asdf.git) to be installed. [`cl-math`](https://github.com/martinkersner/cl-math.git) and [`cl-plot`](https://github.com/martinkersner/cl-plot.git) are included as submodules to `cl-ml` project.

### Get the latest version
```bash
git clone --recursive https://github.com/martinkersner/cl-ml.git
```

### Start using it
```common-lisp
(load "init")
```

## Algorithms

### Artificial Neural Networks
Currently only [fully connected layers](http://cs231n.github.io/convolutional-networks/#fc) with [sigmoid activation function](https://en.wikipedia.org/wiki/Sigmoid_function) are supported. Optimization is performed using [Stochastic Gradient Descent](https://en.wikipedia.org/wiki/Stochastic_gradient_descent) (SGD).

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
