
############
# THE DATA #
############

# LOADING THE DATA
angiosperm =
    data.frame(
        x = c(-10.1756, -5.9206, -0.0924, -0.0499,
              -0.01000,  0.0112,  0.0300,  0.0500,
               0.08030,  0.1292,  4.7520,  6.4299),
        y = c( 1.5002,  1.5765,  1.5001,  1.5084,
               1.4245, -0.9539, -0.9274, -1.0617,
              -0.5808,  0.3372,  0.4925,  0.5535))

iridium =
    data.frame(
        x = c(-15.6662, -14.7554, -14.3673, -10.4248, 
              -10.3796, -10.1748,  -7.9293,  -3.0748,
               -2.6648,  -2.4139,  -0.1451,  -0.1289,
               -0.1110,  -0.0900,  -0.0702,  -0.0501,
               -0.0301,  -0.0101,   0.0101,   0.0301,
                0.0501,   0.0702,   0.0901,   0.1292,
                0.1499,   0.1752,   1.8501,   3.8422,
                3.8515,   3.9370,   5.8029,   5.8037,
                6.0117,  10.1825,   11.178),
        y = c(1.1407, 0.8319, 1.0537, 0.8954,
              0.7381, 0.6489, 0.8002, 0.7263,
              0.6575, 0.7119, 1.2493, 1.4299,
              1.4806, 1.6918, 1.9783, 1.2836,
              1.2556, 1.7541, 3.1038, 3.5373,
              3.7520, 2.1787, 1.8019, 1.3630,
              1.2529, 1.4332, 0.7576, 1.3415,
              1.4338, 1.4064, 1.1562, 0.8212,
              0.9578, 1.0077, 1.1373))

# DATA PREPROCESSING
# demarcation where the data are split
dline = 0.4 # must remain unchanged unless layout adjusted accordingly  				 				

# subsets of data assigned to corresponding relevant variables
left_angio <- angiosperm[angiosperm$x < -dline,]
middle_angio <- angiosperm[abs(angiosperm$x) <= dline,]
right_angio <- angiosperm[angiosperm$x > dline,]	
					
left_iridium <- iridium[iridium$x < -dline,]			
middle_iridium <- iridium[abs(iridium$x) <= dline,]
right_iridium <- iridium[iridium$x > dline,]			

##############
# THE CANVAS #
##############

# defining the function for lines of text
llines <- function(x){
	lcm(x * par("csi") * 2.54)
}

opar = par(	
  # leave a bit of room to the right of each plot 
  mar = c(0,0,0,0.5),
  # keep space for main texts and breathing space to the right
	oma = c(4,4,0,1),
	cex = 1,
  cex.main = 1,
  cex.axis = 1, # increase for bigger tick labels
  font.axis = 1,  # replace with 2 for bold tick labels
  pch = 16  # replace for other point characters
  )		

layout(	rbind(	
    c(0, 7, 8, 9),			# 7, 8, 9 are the rectangles demonstrating the scale
		c(0, 1, 2, 3),			# 1, 2, 3 are meant to be angiosperm plots
		c(0, 4, 5, 6),			# 4, 5, 6 are meant to be iridium plots
    c(0, 0, 0, 0)
	),
	heights = c(llines(4), 1, 1, llines(2)),
	widths = c(llines(2), 2, 1, 2) 
)

# SETTING THE WINDOW PARAMETERS
# vertical limits for top plots
angio_ylim <- c(-1, 1.5)
# vertical limits for bottom plots
iridium_ylim <- c(0.5, 4)						

# horizontal limits for right plots
right_xlim <-  c(0, 16)
# horizontal limits for left plots
left_xlim <- -rev(right_xlim)					
# horizontal limits for middle plots
middle_xlim <- c(-0.2, 0.2)						

# SETTING THE TICK MARK PARAMETERS
# HORIZONTAL AXES
# tick marks for left plots
left_tick <- seq(left_xlim[1], left_xlim[2], length = 9)		
# labelled tick marks for left plots
l_left_tick <- head(left_tick[rep(c(TRUE, FALSE), 1)], -1)      # discount values in between
                                                                # ...and the last entry
# tick marks for right plots
right_tick <- -rev(left_tick)						                    
# labelled tick marks for right plots
l_right_tick <- right_tick[rep(c(TRUE, FALSE), 1)][-1]          # discount values in between
                                                                # ...and the first entry
# tick marks for middle plots
middle_tick <- seq(middle_xlim[1], middle_xlim[2], length = 5)  
# labelled tick marks for middle plots
l_middle_tick <- middle_tick[rep(c(TRUE, FALSE), 1)]            # discount values in between

# VERTICAL AXES
# tick marks for top plots
top_tick <- seq(angio_ylim[1], angio_ylim[2], length = 6)
# labelled tick marks for top plots
l_top_tick <- top_tick[rep(c(TRUE, FALSE), 1)]                  # discount values in between

# tick marks for bottom plots
bottom_tick <- seq(iridium_ylim[1], iridium_ylim[2], length = 8)
# labelled tick marks for bottom plots
l_bottom_tick <- bottom_tick[rep(c(FALSE, TRUE), 1)]            # discount values in between

# height of labelled tick marks
lt.height <- -0.5
# height of unlabelled tick marks
ut.height <- -0.25

###############
# THE DRAWING #
###############

# DATA PLOTTING FUNCTION
my.plot <- function(data, xlim, ylim, count){
	plot.new()
	plot.window(xlim = xlim, ylim = ylim, xaxs = "i")
	points(data)
	lines(data)
	box()
}

# RANGE INDICATOR PLOTTING FUNCTION
draw_indicator <- function(xlim){
  plot.new(); 
  plot.window(xlim = xlim, ylim = c(-1,1), xaxs = "i");   
  # find center of space
  center_x <- mean(xlim); center_y <- 0
  # draw rectangle of width 0.4 units around the center
  rect(center_x - 0.2, center_y - 0.2, center_x + 0.2, center_y + 0.2)
}

# The drawing of the data takes place as sets of commands in 6 steps.
# Since `box` function draws a border,
# at each of the 6 steps, the axis line is removed and tick marks retained
# With respect to the parameters defined already,
# the labelled and the unlabelled axes are drawn 
# Furthermore, dotted axes and vertical labels are drawn
# in/around plots where they are meant to be.

# TOP LEFT
my.plot(left_angio, xlim = left_xlim, ylim = angio_ylim)
axis(3, lwd=0, lwd.ticks=1, label = FALSE, at = left_tick, tcl = ut.height)
axis(3, lwd=0, lwd.ticks=1, at = l_left_tick, tcl = lt.height)
axis(2, lwd=0, lwd.ticks=1, label = FALSE, at = top_tick, tcl = ut.height)
axis(2, lwd=0, lwd.ticks=1, at = l_top_tick, tcl = lt.height)
mtext("Log 10 Angiosperm\nPollen/Fern Spores", line = 3, side = 2)

# TOP MIDDLE
my.plot(middle_angio, xlim = middle_xlim, ylim = angio_ylim)
# vertical dotted line denoting the axis
abline(v = 0, lty = "1313")
axis(3, lwd=0, lwd.ticks=1, label = FALSE, at = middle_tick, tcl = ut.height)
axis(3, lwd=0, lwd.ticks=1, at = l_middle_tick, tcl = lt.height)

# TOP RIGHT
my.plot(right_angio, xlim = right_xlim, ylim = angio_ylim)
axis(3, lwd=0, lwd.ticks=1, label = FALSE, at = right_tick, tcl = ut.height)
axis(3, lwd=0, lwd.ticks=1, at = l_right_tick, tcl = lt.height)
axis(4, lwd=0, lwd.ticks=1, label = FALSE, at = top_tick, tcl = ut.height)
axis(4, lwd=0, lwd.ticks=1, label = FALSE, at = l_top_tick, tcl = lt.height)

# BOTTOM LEFT
my.plot(left_iridium, xlim = left_xlim, ylim = iridium_ylim, count = 4)		# bottom left corner		
axis(1, lwd=0, lwd.ticks=1, label = FALSE, at = left_tick, tcl = ut.height)
axis(1, lwd=0, lwd.ticks=1, at = l_left_tick, tcl = lt.height)
axis(2, lwd=0, lwd.ticks=1, label = FALSE, at = bottom_tick, tcl = ut.height)
axis(2, lwd=0, lwd.ticks=1, at = l_bottom_tick, tcl = lt.height)
mtext("Log Iridium\n(Log PPT)", line = 3, side = 2) 

# BOTTOM MIDDLE
my.plot(middle_iridium, xlim = middle_xlim, ylim = iridium_ylim)
# vertical dotted line denoting the axis
abline(v = 0, lty = "1313")
axis(1, lwd=0, lwd.ticks=1, label = FALSE, at = middle_tick, tcl = ut.height)
axis(1, lwd=0, lwd.ticks=1, at = l_middle_tick, tcl = lt.height)


# BOTTOM RIGHT
my.plot(right_iridium, xlim = right_xlim, ylim = iridium_ylim)
axis(1, lwd=0, lwd.ticks=1, label = FALSE, at = right_tick, tcl = ut.height)
axis(1, lwd=0, lwd.ticks=1, at = l_right_tick, tcl = lt.height)
axis(4, lwd=0, lwd.ticks=1, label = FALSE, at = bottom_tick, tcl = ut.height)
axis(4, lwd=0, lwd.ticks=1, label = FALSE, at = l_bottom_tick, tcl = lt.height)

# The three rectanges that indicate the range of the horizontal axes are drawn
draw_indicator(xlim = left_xlim)
draw_indicator(xlim = middle_xlim)
draw_indicator(xlim = right_xlim)

# The label for the horizontal axis
mtext(text = "Distance from K-T Boundary (meters)", outer = TRUE, line = 2, side = 1)

# retain par parameters
par(opar) 
