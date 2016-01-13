\name{Color key legend}
\alias{colkey}
\title{
   Plots a color legend
}
\description{
  \code{colkey} plots a color legend, either to an existing plot or starting
  a new plot. 
}
\usage{
colkey (col = NULL, clim, clab = NULL, clog = FALSE, add = FALSE, 
        cex.clab = NULL, col.clab = NULL, side.clab = NULL, 
        line.clab = NULL, adj.clab = NULL, font.clab = NULL,
        side = 4, length = 1, width = 1, dist = 0, shift = 0,
        addlines = FALSE, breaks = NULL, at = NULL, labels = TRUE, tick = TRUE,
        line = NA, pos = NA, outer = FALSE, font = NA, lty = 1, lwd = 1, 
        lwd.ticks = 1, col.axis = NULL, col.ticks = NULL, col.box = NULL,
        hadj = NA, padj = NA, cex.axis = par("cex.axis"),
        mgp = NULL, tck = NULL, tcl = NULL, las = NULL) 
}

\arguments{
  \item{col }{Colors to be used for the color key.
    If \code{col} is \code{NULL}, 
    then a red-yellow-blue colorscheme (\code{jet.col}) will be used.
    }
  \item{clim }{The range of the color values, used in the color key. 
    } 
  \item{clab }{The label to be written on top of the color key. 
    The label will be written at the same level as the main title.
    To lower it, either \code{clab} can be made a vector, with the first values empty 
    strings. Alternatively, it can be lowered by argument \code{line.clab}.
    } 
  \item{clog }{If \code{TRUE}, then values of the color key will be log transformed.
    } 
  \item{add }{If \code{TRUE}, the color key will be added to the current plot and
    positioned in the margin. If \code{FALSE} a new plot will be started and the color
    key will be positioned in the centre.
    } 
  \item{cex.clab }{The size of the label 
    written on top of the color key; default = same as axis labels. 
    } 
  \item{col.clab }{The color of the label 
    written on top of the color key; default = same as main title. 
    } 
  \item{side.clab }{The side of the label 
    written on top of the color key; default = same as main title, i.e. side = 3. 
    Values of 1, 2, 4 will put the colorkey label at bottom, left and right of the key respectively.     
    } 
  \item{line.clab }{The numer of lines in the margin where the colorkey title is to be drawn.
    If unspecified, it is at line.clab =1.75.
    } 
  \item{adj.clab }{The adjustment of the colorkey title, 
    a number inbetween 0 (left) to 1 (right). The default is to put it central. 
    } 
  \item{font.clab }{The font of the colorkey title, 
    a number inbetween 0 (left) to 1 (right). The default is to put it central. 
    } 
  \item{side }{Where to put the color key. 1 = bottom, 2 = left, 3 = top, 4 = right.
    } 
  \item{length }{Relative length of the color key; 1 = same length as the axis.
    } 
  \item{width }{Relative width of the color key.
    } 
  \item{dist }{Distance of the color key to the margin. 
    Positive values are further into the margin, negative values cause the color key
    to be positioned closer to or within the main plot. Reasonable range is [-0.5, 0.05].
    } 
  \item{shift }{Shift relative to the centre. 
    Positive values are upward when side = 2 or 4, and to the right for side = 1 or 3.
    It does not make sense to use this argument if \code{length = 1}. 
    Reasonable range is [-0.2, 0.2].
    } 
  \item{addlines }{If \code{TRUE}, will draw lines inbetween the colors.
    } 
  \item{breaks }{a set of finite numeric breakpoints for the colors;
    must have one more breakpoint than color and be in increasing order.
    Unsorted vectors will be sorted, with a warning.
    }
  \item{at, labels, tick, line, pos, outer, font, lty, lwd }{Additional parameters
    as from the \link{axis} command.
    } 
  \item{lwd.ticks, hadj, padj, cex.axis, mgp, tck, tcl, las }{Additional parameters 
    as from the \link{axis} command.
    } 
  \item{col.box, col.axis, col.ticks }{Additional parameters 
    to set the color of the color legend framing \code{box}, 
    the axis label and the axis ticks.
    } 
}

\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\examples{
# save plotting parameters
 pm <- par(mfrow = c(2, 2))
 pmar <- par(mar = c(5.1, 4.1, 4.1, 2.1))

## =======================================================================
##  colorkey as argument of a plot3D function
## =======================================================================
# default, colkey = NULL: adds colkey because multiple colors
 image2D(z = volcano)  

# default, colkey = NULL: no colkey because only one color
 image2D(z = volcano, col = "grey", shade = 0.2, contour = TRUE)  

# colkey = FALSE: no color key, no extra space foreseen
 image2D(z = volcano, colkey = FALSE)
 
# colkey = list(plot = FALSE): no color key, extra space foreseen
 image2D(z = volcano, colkey = list(plot = FALSE, side = 3))
 colkey (side = 3, add = TRUE, clim = range(volcano))
 
 
## =======================================================================
##  colorkey in new plot
## =======================================================================
 
 colkey(side = 1, clim = c(0, 1), add = FALSE, clab = "z", 
   col.clab = "red", adj.clab = 0)
 colkey(side = 2, clim = c(0, 1), clab = "z", length = 0.5, width = 0.5)
 colkey(side = 3, clim = c(0, 1), lwd = 3, clab = c("a","b","c","d"), 
   line.clab = 5)
 colkey(side = 4, clim = c(1e-6, 1), clog = TRUE, 
   clab = "a very long title in bold and close to the key", 
   line.clab = 1, side.clab = 2, font.clab = 2)

## =======================================================================
##  colorkey added to existing plot
## =======================================================================

 par(mfrow = c(1, 1))

 image2D(volcano, xlab = "", clab = "m", 
       colkey = list(side = 1, length = 0.5, width = 0.5, 
         line.clab = 1))
 colkey(side = 3, clim = range(volcano), add = TRUE)

# 'dist' to put colkey within the image 
# 'shift' to position colkey to the right or upward
 par(mfrow = c(1, 1))
 image2D(volcano, colkey = FALSE)

 colkey(clim = range(volcano), dist = -0.15, shift = 0.2,
        side = 3, add = TRUE, clab = "key 1", col.clab = "white",
        length = 0.5, width = 0.5, col.axis = "white", 
        col.ticks = "white", cex.axis = 0.8)

 colkey(clim = range(volcano), dist = -0.1, shift = -0.2,
        side = 4, add = TRUE, clab = "key 2", col.clab = "white",
        length = 0.3, width = 0.5, col.axis = "white", 
        col.ticks = "white", col.box = "red", cex.axis = 0.8)

 colkey(clim = range(volcano), dist = -0.3, 
        side = 1, add = TRUE, clab = "key 3", col.clab = "white",
        length = 0.3, width = 0.5, col.axis = "white", 
        col.ticks = "white", at  = c(100, 140, 180), 
        labels = c("a", "b", "c"), font = 2)

 colkey(clim = range(volcano), dist = -0.3, shift = -0.2,
        side = 2, add = TRUE, clab = "key 4", col.clab = "white",
        length = 0.3, width = 0.5, col.axis = "white", 
        col.ticks = "white", col.box = "red", cex.axis = 0.8,
        las = 3)

## =======================================================================
##  colorkey in other plots
## =======================================================================

 par(mfrow = c(1, 1))
 par(mar = par("mar") + c(0, 0, -2, 0))
 image2D(volcano, clab = "height, m", 
       colkey = list(dist = -0.15, shift = 0.2,
       side = 3, length = 0.5, width = 0.5, line.clab = 2.5,
       cex.clab = 2, col.clab = "white", col.axis = "white", 
       col.ticks = "white", cex.axis = 0.8))

## =======================================================================
## Several color keys in composite plot
## =======================================================================

 persp3D(z = volcano, zlim = c(-60, 200), phi = 20, bty = "b",   
    colkey = list(length = 0.2, width = 0.4, shift = 0.15,
      cex.axis = 0.8, cex.clab = 0.85), lighting = TRUE, lphi = 90,
    clab = c("height","m"), plot = FALSE)

# create gradient in x-direction
 Vx <- volcano[-1, ] - volcano[-nrow(volcano), ]

# add as image with own color key, at bottom 
 image3D(z = -60, colvar = Vx/10, add = TRUE, 
    colkey = list(length = 0.2, width = 0.4, shift = -0.15,
      cex.axis = 0.8, cex.clab = 0.85),
   clab = c("gradient","m/m"), plot = TRUE)

## =======================================================================
## categorical colors; use addlines = TRUE to separate colors
## =======================================================================

 with(iris, scatter3D(x = Sepal.Length, y = Sepal.Width, 
   z = Petal.Length, colvar = as.integer(Species), 
   col = c("orange", "green", "lightblue"), pch = 16, cex = 2,  
   clim = c(1, 3), ticktype = "detailed", phi = 20,
   xlab = "Sepal Length", ylab = "Sepal Width", 
   zlab = "Petal Length",  main = "iris",
   colkey = list(at = c(1.33, 2, 2.66), side = 1, 
   addlines = TRUE, length = 0.5, width = 0.5,
   labels = c("setosa", "versicolor", "virginica") )))

# reset plotting parameters
 par(mfrow = pm)
 par(mar = pmar)
}
\keyword{ hplot }

