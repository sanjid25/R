- in the `activation_functions` directory there's
	-	a plot of all the activation functions that were compared	
	-	an .R code that generated that plot
- source code of the how the activation functions are compared in
	- 	the titanic dataset
	-	the MNIST dataset
- in the `outputs` directory, there's:
	 output of comparisons I had generated in the past

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

