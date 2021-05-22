\name{plots with legend or colorkeys}
\alias{legendplot}
\alias{legendmatplot}
\alias{legendhist}
\alias{legendpairs}
\alias{legend.plt}
\alias{legend.oma}
\alias{colorkeyplot}
\alias{colorkeymatplot}
\alias{colorkeyhist}
\alias{colorkeypairs}
\alias{colorkey.plt}
\alias{colorkey.oma}
\alias{createKey}
\title{
   Plots with legend or colorkeys outside of the plotting region
}
\description{
  \code{legendplot}, \code{legendmatplot}, \code{legendhist} and \code{legendpairs} create 
    plots with a legend adjacent to it, using R's default plotting functions plot, matplot, hist and pairs. 
  
  \code{colorkeyplot}, \code{colorkeymatplot}, \code{colorkeyhist} and \code{colorkeypairs} 
  create a plot with a colorkey adjacent to it. 
  
  \code{createKey} creates suitable colors for the color variables.

  \code{legend.plt} and \code{colorkey.plt} are general functions that might 
  also work with other plotting methods, and that add a legend or color key by changing the \code{plt} parameter.
  
  \code{legend.oma} and \code{colorkey.oma} are general functions that might 
  also work with other plotting methods, and that add a legend or color key by changing the \code{oma} parameter.
}
\usage{
legendplot    (..., legend = list(), legend.side = 4, legend.cex = 1, legend.pars = NULL)  
legendmatplot (..., legend = list(), legend.side = 4, legend.cex = 1, legend.pars = NULL)  
legendhist    (..., legend = list(), legend.side = 4, legend.cex = 1, legend.pars = NULL)  
legendpairs   (..., legend = list(), legend.side = 4, legend.cex = 1, legend.pars = NULL)  

legend.plt (method = "plot", ..., legend = list(), legend.side = 4, legend.cex = 1, 
            legend.pars = NULL)  
legend.oma (method = "pairs", ..., legend = list(), legend.side = 4, legend.cex = 1, 
            legend.pars = NULL)  

colorkeyplot    (..., colorkey = list(), colorkey.side = 4)
colorkeymatplot (..., colorkey = list(), colorkey.side = 4)
colorkeyhist    (..., colorkey = list(), colorkey.side = 4)
colorkeypairs   (..., colorkey = list(), colorkey.side = 4)

colorkey.plt (method = "plot",..., colorkey = list(), colorkey.side = 4)
colorkey.oma (method = "pairs",..., colorkey = list(), colorkey.side = 4)

createKey (x, clim = NULL, col = NULL, NAcol = "black")
}

\arguments{
  \item{method }{A plotting method to which to add the legend or colorkey,
    such as \link{plot}, \code{matplot}, \code{boxplot}, ... Note that not all
    of R's plotting functions can be used.
    }
  \item{... }{Any argument passed to \link{plot}, \link{matplot}, 
    \link{hist} or any other \code{method}.
    }
  \item{colorkey.side, legend.side }{On which side of the plot 
    (1=bottom, 2=left, 3=top, 4=right) to put the legend or color key.
    } 
  \item{legend.cex }{The expansion factor of the space around the legend.
    } 
  \item{legend.pars }{A list that determines the size of the legend 
    and of the main plotting region, as returned by any of the legend plotting 
    functions. 
    It should contain two vectors, one that sets the size of the plotting region
    called \code{plt.main} and one that sets the size of the legend,
    called \code{plt.legend}. The format of these vectors is as the \link{par}ameter \code{"plt"}.
    See last example. 
    } 
  \item{colorkey }{A list with arguments passed to function \link{colkey}.
    } 
  \item{legend }{A list with arguments passed to function \link{legend}. 
    } 
  \item{x }{The variable for which the color key has to be created.
    } 
  \item{col }{Colors to be used for the color key.
    If \code{col} is \code{NULL}, 
    then a red-yellow-blue colorscheme (\code{jet.col}) will be used.
    }
  \item{clim }{The range of the color values, used in the color key. 
    } 
  \item{NAcol }{Color to be used for \code{NA} values.
    }
}

\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\value{
 The legend plotting functions return as \code{invisible}, a \code{list} that contains the plotting parameters for the regions of the legend and of the main plotting region, elements called \code{plt.legend} and \code{plt.main}. For the \code{pairs} method, the list returned contains the size of the outer margin  instead, i.e. the \code{oma} parameter. 
}

\note{The method that changes the oma parameter (based on legend.oma or colorkey.oma) is not optimal, as plot.new is called several times in this function. This means you will need to "hit return to see next plot" several times before you see the actual figure.  
}
\examples{

# save plotting parameters
pm <- par(mfrow = c(2, 2))
pmar <- par(mar = c(5.1, 4.1, 4.1, 2.1))


# ============================================================================
#  Colorkey and legend added to simple plot
# ============================================================================

par(mfrow = c(2,1))
x <- seq(0, 2*pi, length.out = 30)
y <- sin(x)

# Note: this forgets the names of the x and y-variables.
colorkeyplot(x = x, y = y, col = createKey(y), pch = 18,
              main = "colorkeyplot with 'plot'", 
              colorkey = list(clim = range(y)))
abline (v = 4)
abline (h = 0.4)

legendplot(x = x, y = y, col = c("red", "blue")[(y > 0)+1], 
          main = "legendplot with 'plot'", pch = 18, 
          xlab = "x", ylab = "y", 
          legend = list(col = c("red","blue"), pch = 18, 
                          legend = c(">0", "<0")))
abline (v = pi)
abline (h = 0)

par(mfrow = c(1,1))
legendplot(x = x, y = y, col = c("red", "blue")[(y > 0)+1], 
          main = "legendplot with 'plot'", pch = 18,
          legend.side = 1, las = 1,
          legend = list(col = c("red","blue"), pch = 18, 
                      horiz = TRUE, legend = c(">0", "<0")))
abline (v = pi)
abline (h = 0)

# We do not label the y-axis, so the legend can be a 
# closer to the axis (legend.cex)
par(mfrow = c(1,1), mar = c(4,2,4,2))
legendplot(x = x, y = y, col = c("red", "blue")[(y > 0)+1], 
            main = "legendplot with 'plot'", pch = 18,
            legend.side = 2, legend.cex = 0.5, ylab = "",
            legend = list(col = c("red","blue"), pch = 18, 
                          horiz = FALSE, legend = c(">0", "<0")))

# Here we have a title with two lines, so the legend is put further away
# Also the legend is put near the bottom here.
legendplot(x = x, y = y, col = c("red", "blue")[(y > 0)+1], 
            main = "legendplot with 'plot'", pch = 18,
            legend.side = 2, legend.cex = 2, ylab = c("axis","on two lines"),
            legend = list(col = c("red","blue"), pch = 18, x = "bottomleft",
                          horiz = FALSE, legend = c(">0", "<0")))

# This works as ordinary legend function (except for the labeling of the axes)
par(mfrow = c(1,1), mar = c(4,4,2,2))
legendplot(x = x, y = y, col = c("red", "blue")[(y > 0)+1], 
            main = "legendplot with 'plot'", pch = 18,
            legend.side = 0,
            legend = list(col = c("red","blue"), pch = 18, x = "right",
                          horiz = TRUE, legend = c(">0", "<0")))

## =============================================================================
## ... added to a more complex plot
## =============================================================================

legend.plt(method = "points2D", x = x, y = y, colvar = y, 
           pch = c(18, 20)[(y > 0)+1], cex = 2, 
           colkey = list(side = 1, dist = -0.25, length = 0.4, shift = -0.15), 
           main = "legendplot with 'points2D'",
           legend = list(pch = c(18, 20), pt.cex = 2,
                         horiz = FALSE, legend = c(">0", "<0")))

# to use the image function with a color key - easier to do with image2D...
colorkey.plt(method = "image", x = 1:nrow(volcano), y = 1:ncol(volcano), 
             z = volcano, col = jet.col(100), 
             main = "colorkeyplot with 'image'",
             colorkey = list(col = jet.col(100), clim = range(volcano), clab = "m"))

## =============================================================================
## with matplot
## =============================================================================

# this is not a very instructive figure!  

lon <- Hypsometry$x           # Longitude
iy  <- seq(10, 180, by = 10)  # Index to latitudes where we want to see data
lat <- Hypsometry$y[iy]       # corresponding latitudes

Col <- createKey(iy)
colorkeymatplot(main = "matplot with color key",
                 xlab = "longitude", ylab = "heigh, m",
                 x = lon, y = Hypsometry$z[,iy], col = Col, type = "l", 
                 colorkey = list(clim = range(lat), clab = "latitude"))  

n <- 100
colorkey.plt(method = "pie", x = rep(1, n), labels = "", 
             col = rainbow(n), border = NA,
             main = "colorkeyplot with 'pie'", 
             colorkey = list(col = rainbow(n), clim = c(1,n)))

## =============================================================================
## A complex figure, consisting of overlays (based on example(boxplot))
## =============================================================================

plotit <- function(){
  boxplot(len ~ dose, data = ToothGrowth,
          boxwex = 0.25, at = 1:3 - 0.2,
          subset = supp == "VC", col = "yellow",
          main = "Guinea Pigs' Tooth Growth",
          xlab = "Vitamin C dose mg", ylab = "tooth length",
          xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")
  
  boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
          boxwex = 0.25, at = 1:3 + 0.2,
          subset = supp == "OJ", col = "orange")
}
legend.plt(method = "plotit", 
           legend = list(legend = c("Ascorbic acid", "Orange juice"),
                         fill = c("yellow", "orange")))

# All in one - putting legend on other side..
pm <- par(mar = c(4,3,4,2))
legend.plt(formula = len ~ dose:supp, data = ToothGrowth,
           boxwex = 0.5, col = c("orange", "yellow"),
           main = "Guinea Pigs' Tooth Growth",
           xlab = "Vitamin C dose mg", ylab = "tooth length",
           sep = ":", lex.order = TRUE, ylim = c(0, 35), yaxs = "i", 
           method = "boxplot", legend.side = 2, 
           legend = list(legend = c("Ascorbic acid", "Orange juice"),
                         fill = c("yellow", "orange")))
par(mar = pm)

## =============================================================================
## Nesting..
## =============================================================================

Fun1 <- function()
  legend.plt(x = 0, method = "plot", type = "n", xlab = "", ylab = "", axes = FALSE,
             frame.plot = TRUE, 
             legend = list(legend = 
            c("this can", "also be used", "to write text", "next to a plot")))

X <- legend.plt(method = "Fun1", legend.side = 1, 
              legend = list(legend = 
                     c("but also to put text", "below a plot"), 
                     horiz = TRUE, x = "left", box.col = "grey"))

print(X)
P <- par(plt = X$plt.legend, new = TRUE)  
plot.new()
legend("right", legend = "second legend")
par (plt = X$plt.main, new = TRUE)
plot.new()
legend("left", legend = "another legend")

## =============================================================================
## Pairs - note: this is not optimal 
## =============================================================================

legendpairs(iris, legend = list(legend = levels(iris$Species), cex = 0.5, col = 1:3, pch = 1), 
    legend.side = 4, col = (1:3)[iris$Species])

legendpairs( iris[1:4], main = "Anderson's Iris Data -- 3 species",
            pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)],
            legend.side = 1,
            legend = list(levels(iris$Species), pt.bg = c("red", "green3", "blue"), 
                         pch = 21, title = "Species", horiz = TRUE))

# reset plotting parameters
par(mfrow = pm)
par(mar = pmar)

## Pairs with a color key

colorkeypairs(swiss[,c(1,4,5)], pch = 18, cex = 2, 
    col = createKey(swiss[,2]),
    colorkey=list(clim = range(swiss[,2]), clab = "Agriculture"))

## =============================================================================
## Aligning plots
## =============================================================================

par(mfrow = c(2,1))
AA <- legendplot(1:10, runif(10), xlab = "x", ylab = "y", pch= 18, 
      cex = 2, col = 1:10, 
      legend = list(col = 1:10, legend = 11111:11120, pch = 18, pt.cex = 2))

  legendplot(1:10, runif(10), xlab = "x", ylab = "y", pch= 18, 
      cex = 2, col = 1:10, legend.pars = AA,   # use par settings of previous plot
      legend = list(plot=FALSE))


}
\keyword{ hplot }

