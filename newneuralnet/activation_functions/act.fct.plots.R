# Four types of activation functions:
# 1. sigmoid 
# 2. tangent hyperbolic
# 3. true linear
# 4. new leaky linear

par(mfrow = c(2,2),
    mar = c(0, 0, 1, 0),
    oma = c(3.2,3,3,3))

draw <- function(x, y, location){
  
    plot.new()
    plot.window(xlim = c(-2, 2), ylim = c(-1, 1), xaxs = "i", yaxs = "i")
    lines(x, y)
    abline(h = 0, v = 0, lty = "1313")
    box()
}

x = seq(-5, 5, length = 1000)
y = 1 / (1 + exp(-x))
draw(x = x, y = y, location = 1)
axis(2)
axis(3)
mtext("Sigmoid", side = 3, line = 2, oma = TRUE)

y = (1 - exp(-2 * x)) / (1 + exp(-2 * x))
draw(x = x, y = y, location = 1)
mtext("Hyperbolic tangent", side = 3, line = 2, oma = TRUE)

y = x
draw(x = x, y = y, location = 2)
mtext("True linear", side = 1, line = 2, oma = TRUE)

y[y < 0] = y[y < 0] * 0.1
draw(x = x, y = y, location = 2)
axis(1)
axis(4)
mtext("New leaky function", side = 1, line = 2, oma = TRUE)