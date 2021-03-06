# A study of varied orientations of rectified linear activation functions in artificial neural networks

## Summary of work

Artificial neural networks have been a dominant learning algorithm since its inception in the late 1950s. Overtime, it has evolved from being a mere implementation of perceptron learning rule to being an elegant implementation of deep belief networks. This transformation has made neural networks a state of the art technique particularly because of its ability to generate its own simpler version of features alongside the ones that are provided from observations. Consequently, neural networks are now being applied on problems as simple as regression analysis to problems as complex as driving cars using pattern recognition. Much of this progression can be attributed to the inherently simple and flexible implementation principles of neural networks. Historically, this flexibility has been demonstrated through the mere incorporation of an untried approach to neural networks. In light of this philosophy, our research addresses issues including role, gradient and properties of an inherent feature of neural networks, “activation functions” by investigating functions that are not implemented in the literature. Concretely, experiments on open source platform (`neuralnet` package of R) and datasets (`Titanic` and MNIST) is performed in order to demonstrate the impact of variations of the popular rectified linear activation function in neural networks.

## Organisation

- in the `activation_functions` directory there's
	-	a plot of all the activation functions that were compared	
	-	an .R code that generated that plot
- source code of the how the activation functions are compared in
	- 	the titanic dataset
	-	the MNIST dataset
- in the `outputs` directory, there's:
	 output of comparisons I had generated in the past

## Implementation

**NOTE:** 
The `neuralnet` directory is the exact sourcecode of the `neuralnet` package in R with the following exceptions:
- `neuralnet/R/neuralnet.R` is changed to `neuralnet/R/neuralnetwork1.R`
- in this file:
	-  the name of function `neuralnet` is changed to `neuralnetwork1`
	-  the block meant to represent `tanh` (lines 216 - 241), is changed to implement `new leaky function`  
	-  the block meant to represent `sigmoid` (lines 242 - 258), is changed to implement `linear function`
	-  these blocks can be changed to implement more variations
	-  the reason, the names `tanh` and `sigmoid` are kept is to make debugging simpler
	-  this way, it is also easy to implement even more function, say in a new file named `neuralnetwork2`
	
In the experiments, wherever the `neuralnet` function is invoked, the original `tanh` and `sigmoid` functions of the `neuralnet` package are implemented.

In contrast, wherever the `neuralnetwork1` function is invoked, the `new leaky function` and `linear function` that exists in this directory: `neuralnet/R/neuralnetwork1.R` are implemented.

