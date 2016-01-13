\name{3-D arrows, segments, polygons, boxes, rectangles}
\alias{arrows3D}
\alias{segments3D}
\alias{box3D}
\alias{border3D}
\alias{rect3D}
\alias{polygon3D}
\alias{arrows2D}
\alias{segments2D}
\alias{rect2D}
\alias{polygon2D}
\title{
  Plots arrows, segments, points, lines, polygons, rectangles and boxes in a 3D perspective plot or in 2D.
}
\description{
  Functions \code{arrows3D} and \code{segments3D} draw arrows and line segments 
  between pairs of points.
  
  Functions \code{box3D} and \code{border3D} draw boxes between pairs of points. 
  
  \code{polygon3D} draws polygons; \code{rect3D} draws rectangles.
  
  The 2D functions \code{arrows2D}, \code{segments2D}, \code{rect2D} and \code{polygon2D} are included
  for their side effect of having a color key.
}
\usage{
arrows3D (x0, y0, z0, x1 = x0, y1 = y0, z1 = z0, ...,  
         colvar = NULL, phi = 40, theta = 40,
         col = NULL, NAcol = "white", breaks = NULL,
         colkey = NULL, panel.first = NULL,
         clim = NULL, clab = NULL, bty = "b", type = "triangle", 
         add = FALSE, plot = TRUE)

segments3D (x0, y0, z0, x1 = x0, y1 = y0, z1 = z0, ..., 
         colvar = NULL, phi = 40, theta = 40,
         col = NULL, NAcol = "white", breaks = NULL,
         colkey = NULL, panel.first = NULL,
         clim = NULL, clab = NULL, bty = "b", 
         add = FALSE, plot = TRUE)

box3D (x0, y0, z0, x1, y1, z1, ..., 
         colvar = NULL, phi = 40, theta = 40,
         col = NULL, NAcol = "white", breaks = NULL,
         border = NA, facets = TRUE, colkey = NULL,
         panel.first = NULL, clim = NULL, clab = NULL, bty = "b",
         add = FALSE, plot = TRUE)

border3D(x0, y0, z0, x1, y1, z1, ..., 
         colvar = NULL, phi = 40, theta = 40,
         col = NULL, NAcol = "white", breaks = NULL,
         colkey = NULL, panel.first = NULL,
         clim = NULL, clab = NULL, bty = "b", 
         add = FALSE, plot = TRUE)  

rect3D (x0, y0, z0, x1 = NULL, y1 = NULL, z1 = NULL, ..., 
         colvar = NULL, phi = 40, theta = 40,
         col = NULL, NAcol = "white", breaks = NULL,
         border = NA, facets = TRUE, colkey = NULL,
         panel.first = NULL, clim = NULL, clab = NULL, bty = "b",
         add = FALSE, plot = TRUE)

polygon3D (x, y, z, ..., 
         colvar = NULL, phi = 40, theta = 40,
         col = NULL, NAcol = "white", breaks = NULL,
         border = NA, facets = TRUE, colkey = NULL,
         panel.first = NULL, clim = NULL, clab = NULL, bty = "b",
         add = FALSE, plot = TRUE)  
         
arrows2D (x0, y0, x1 = x0, y1 = y0, ..., colvar = NULL,
         col = NULL, NAcol = "white", breaks = NULL,
         colkey = NULL, clim = NULL, clab = NULL, 
         type = "triangle", add = FALSE, plot = TRUE)  

segments2D (x0, y0, x1 = x0, y1 = y0, ..., colvar = NULL,
         col = NULL, NAcol = "white", breaks = NULL,
         colkey = NULL, clim = NULL, clab = NULL, 
         add = FALSE, plot = TRUE) 

rect2D (x0, y0, x1 = x0, y1 = y0, ..., colvar = NULL,
         col = NULL, NAcol = "white", breaks = NULL,
         colkey = NULL, clim = NULL, clab = NULL, 
         add = FALSE, plot = TRUE) 
         
polygon2D (x, y, ..., colvar = NULL, 
         col = NULL, NAcol = "white", breaks = NULL,
         border = NA, facets = TRUE,
         colkey = NULL, clim = NULL, clab = NULL, 
         add = FALSE, plot = TRUE)
}

\arguments{
  \item{x0, y0, z0 }{coordinates of points \emph{from} which to draw.  
    }
  \item{x1, y1, z1 }{coordinates of points \emph{to} which to draw. 
    For \code{arrows3D} and \code{segments3D}, at least one must be supplied. 
    For \code{rect3D} exactly one must be \code{NULL}.
    }
  \item{x, y, z }{coordinates of the vertices of the polygon. 
    The polygon will be closed by joining the last point to the first point. 
    The coordinates can contain missing values (\code{NA}). 
    These \code{NA} values break the polygon into several complete polygons.
    }
  \item{colvar }{The variable used for coloring. It need
     not be present, but if specified, it should be a vector of 
     dimension equal to the coordinates or to the number of polygons.
     Values of \code{NULL}, \code{NA}, or \code{FALSE}
     will toggle off coloration according to \code{colvar}. 
    }
  \item{theta, phi }{the angles defining the viewing direction. 
    \code{theta} gives the azimuthal direction and \code{phi} the colatitude. 
    See \link{persp}.
    }
  \item{col }{Color palette to be used for coloring the arrows or segments 
    as specified by the \code{colvar} variable. 
    If \code{col} is \code{NULL} and \code{colvar} is specified, 
    then a red-yellow-blue colorscheme (\link{jet.col}) will be used.
    If \code{col} is \code{NULL} and \code{colvar} is not specified, then 
    \code{col} will be "black". 
    }
  \item{NAcol }{Colors to be used for \code{colvar} values that are \code{NA}.
    }
  \item{breaks }{a set of finite numeric breakpoints for the colors;
    must have one more breakpoint than color and be in increasing order.
    Unsorted vectors will be sorted, with a warning.
    }
  \item{colkey }{A logical, \code{NULL} (default), or a \code{list} with parameters
    for the color key (legend). List parameters should be one of 
    \code{side, plot, length, width, dist, shift, addlines, col.clab, cex.clab,
      side.clab, line.clab, adj.clab, font.clab}
    and the axis parameters \code{at, labels, tick, line, pos, outer, font, lty, lwd,
    lwd.ticks, col.box, col.axis, col.ticks, hadj, padj, cex.axis, mgp, tck, tcl, las}.
    The defaults for the parameters are \code{side = 4, plot = TRUE, length = 1, width = 1, 
      dist = 0, shift = 0, addlines = FALSE, col.clab = NULL, cex.clab = par("cex.lab"), 
      side.clab = NULL, line.clab = NULL, adj.clab = NULL, font.clab = NULL})
    See \link{colkey}.
    
    The default is to draw the color key on side = 4, i.e. in the right margin.
    If \code{colkey} = \code{NULL} then a color key will be added only if \code{col} is a vector.
    Setting \code{colkey = list(plot = FALSE)} will create room for the color key 
    without drawing it.
    if \code{colkey = FALSE}, no color key legend will be added.
    } 
  \item{border }{The color of the lines drawn around the surface facets.
    The default, \code{NA}, will disable the drawing of borders.
    }
  \item{facets }{If \code{TRUE}, then \code{col} denotes the color of the surface facets. 
    If \code{FALSE}, then the surface facets are colored ``white'' and the \code{border} 
    (if \code{NA}) will be colored as specified by \code{col}. 
    If \code{NA} then the facets will be transparent. 
    It is usually faster to draw with \code{facets = FALSE}.
    }
  \item{panel.first }{A \code{function} to be evaluated after the plot axes are 
    set up but before any plotting takes place. 
    This can be useful e.g. for drawing background grids or scatterplot smooths. 
    The function should have as argument the transformation matrix, e.g. it should
    be defined as \code{function(pmat)}. See example of \link{persp3D} and last example of \link{voxel3D}.
    } 
  \item{clab }{Only if \code{colkey} is not \code{NULL} or \code{FALSE}, 
    the label to be written on top of the color key. 
    The label will be written at the same level as the main title.
    To lower it, \code{clab} can be made a vector, with the first values empty 
    strings. 
    } 
  \item{clim }{Only if \code{colvar} is specified, the range of the color variable, used
    for the color key. Values of \code{colvar} that extend the range will be put to \code{NA}.
    } 
  \item{bty }{The type of the perspective box, the default draws only the back panels.
    Only effective if the \link{persp} 
    argument (\code{box}) equals \code{TRUE} (this is the default). See \link{perspbox}.
    }
  \item{type }{The type of the arrow head, one of \code{"simple"} 
    (which uses R-function \link{arrows}), \code{"curved"} or \code{"triangle"} and \code{"cone"}.
    The latter two are the same in \code{plot3D} (but differ in package \code{plot3Drgl}).
    }
  \item{add }{Logical. If \code{TRUE}, then the arrows, segments, ... will be added to the current plot.
    If \code{FALSE} a new plot is started. 
    } 
  \item{plot }{Logical. If \code{TRUE} (default), a plot is created, 
    otherwise the viewing transformation matrix is returned (as invisible). 
    } 
  \item{\dots}{additional arguments passed to the plotting methods. 

    The following \link{persp} arguments can be specified: 
    \code{xlim, ylim, zlim, xlab, ylab, zlab, main, sub, r, d, 
    scale, expand, box, axes, nticks, ticktype}.
    The arguments \code{xlim}, \code{ylim}, \code{zlim} only affect the axes for 3D plots.
    All objects will be plotted, including those that fall out of these ranges.
    To select objects only within the axis limits, use \link{plotdev}.
    
    \code{shade} and \code{lighting} arguments will have no effect.
    
    \code{alpha} can be given a value inbetween 0 and 1 to make colors transparent.
         
    In addition, the \link{perspbox} arguments
    \code{col.axis, col.panel, lwd.panel, col.grid, lwd.grid} can 
    also be given a value.

    For \code{arrows3D}, the following \link{arrows} arguments can be specified:
    \code{length, code, angle}.
    
    For \code{polygon3D}, the following \link{polygon} arguments can be specified:
    \code{border}.
    
    For all the functions, arguments \code{lty, lwd} can be specified.
     
    The arguments after \dots must be matched exactly.
    }
}

\value{
  Returns the viewing transformation matrix.

  See \link{trans3D}.
}
\seealso{
  \link{arrows} for the 2-D arrows function on which \code{arrows3D} is based.

  \link{segments} for the 2-D arrows function on which \code{segments3D} is based.

}

\examples{
# save plotting parameters
  pm <- par("mfrow")

## ========================================================================
## arrows, points, segments, box
## ========================================================================

# Create a grid of x, y, and z values
 xx <- yy <- seq(-0.8, 0.8, by = 0.2)
 zz <- seq(-0.8, 0.8, by = 0.8)

 M <- mesh(xx, yy, zz)
 x0 <- M$x; y0 <- M$y; z0 <- M$z
 x1 <- x0 + 0.1

 Col <- c("red", "blue", "green") 
 arrows3D(x0, y0, z0, x1 = x1, colvar = z0, lwd = 2, 
          d = 2, clab = "z-value", col = Col, length = 0.1,
          xlim = c(-0.8, 0.8), ylim = c(-0.8, 0.8),
          main = "arrows3D, points3D, segments3D, border3D")

# add starting point of arrows
 points3D(x0, y0, z0, add = TRUE, colvar = z0, 
           colkey = FALSE, pch = ".", cex = 3, col = Col)

# use segments to add section
 x0 <- c(-0.8, 0.8,  0.8, -0.8)
 x1 <- c( 0.8, 0.8, -0.8, -0.8)
         
 y0 <- c(-0.8, -0.8, 0.8, -0.8)
 y1 <- c(-0.8,  0.8, 0.8, 0.8)

 z0 <- c(0., 0., 0., 0.)
 segments3D(x0, y0, z0, x1, y1, z1 = z0,
     add = TRUE, col = "black", lwd = 2)

# add a box 
 border3D(-0.8, -0.8, -0.8, 0.8, 0.8, 0.8,
       col = "orange", add = TRUE, lwd = 3)

## ========================================================================
## boxes, cubes
## ========================================================================

# borders are boxes without facets  
 border3D(x0 = seq(-0.8, -0.1, by = 0.1), 
       y0 = seq(-0.8, -0.1, by = 0.1),
       z0 = seq(-0.8, -0.1, by = 0.1),
       x1 = seq(0.8, 0.1, by = -0.1),
       y1 = seq(0.8, 0.1, by = -0.1),
       z1 = seq(0.8, 0.1, by = -0.1),
       col = gg.col(8), lty = 2, 
       lwd = c(1, 4), phi = 20, main = "border3D")

 box3D(x0 = -0.8, y0 = -0.8, z0 = -0.8, 
       x1 = 0.8, y1 = 0.8, z1 = 0.8, 
       border = "black", lwd = 2, 
       col = gg.col(1, alpha = 0.8), 
       main = "box3D")

 box3D(x0 = seq(-0.8, -0.1, by = 0.1), 
       y0 = seq(-0.8, -0.1, by = 0.1),
       z0 = seq(-0.8, -0.1, by = 0.1),
       x1 = seq(0.8, 0.1, by = -0.1),
       y1 = seq(0.8, 0.1, by = -0.1),
       z1 = seq(0.8, 0.1, by = -0.1),
       col = rainbow(n = 8, alpha = 0.1), 
       border = "black", lwd = 2, phi = 20)

# here the perspective does not always work 
# use alpha.col to set the transparency of a vector of colors
 box3D(x0 = runif(3), y0 = runif(3), z0 = runif(3),
       x1 = runif(3), y1 = runif(3), z1 = runif(3),
       col = c("red", "lightblue", "orange"), alpha = 0.5,
       border = "black", lwd = 2)

## ========================================================================
## rectangles
## ========================================================================
# at constant 'z'
 rect3D(x0 = seq(-0.8, -0.1, by = 0.1),
        y0 = seq(-0.8, -0.1, by = 0.1),
        z0 = seq(-0.8, -0.1, by = 0.1),
        x1 = seq(0.8, 0.1, by = -0.1),
        y1 = seq(0.8, 0.1, by = -0.1),
        col = gg.col(8), border = "black",
        bty = "g", lwd = 2, phi = 20, main = "rect3D")

# constant y and with transparent facets
 rect3D(x0 = 0, y0 = 0, z0 = 0, x1 = 1, z1 = 5,
        ylim = c(0, 1), facets = NA, border = "red",
        bty = "g", lwd = 2, phi = 20)

# add rect at constant z, with colored facet
 rect3D(x0 = 0, y0 = 0, z0 = 0, x1 = 1, y1 = 1,
        border = "red", add = TRUE)

## ========================================================================
## arrows added to a persp plot 
## ========================================================================

 x <- y <- seq(-10, 10, length = 30)
 z <- outer(x, y, FUN = function(x,y) x^2 + y^2)

 persp3D(x, y, z, theta = 30, phi = 30, 
         col = "lightblue", ltheta = 120, shade = 0.75, 
         ticktype = "detailed", xlab = "X", 
         ylab = "Y", zlab = "x^2+y^2" )  

# Points where to put the arrows
 x <- y <- seq(-10, 10, len = 6)
 X0 <- outer(x, y, FUN = function (x,y) x)
 Y0 <- outer(x, y, FUN = function (x,y) y)
 Z0 <- outer(x, y, FUN = function (x,y) x^2 + y^2)

 X1 <- X0 + 1
 Y1 <- Y0 + 1
 Z1 <- Z0 + 10

 arrows3D(X0, Y0, Z0, X1, Y1, Z1, lwd = 2, 
         add = TRUE, type = "curved", col = "red")

 segments3D(X0, Y0, Z0, X0, Y0, rep(0, length(X0)), lwd = 2, 
         add = TRUE, col = "green")

## ========================================================================
## polygon3D 
## ========================================================================

 x <- runif(10)
 y <- runif(10)
 z <- runif(10)
 
 polygon3D(x, y, z)

# several polygons, separated by NAs
 x <- runif(39) 
 y <- runif(39)
 z <- runif(39)
 ii <- seq(4, 36, by  = 4)
 x[ii] <- y[ii] <- z[ii] <- NA 

# transparent colors (alpha)
 polygon3D(x, y, z, border = "black", lwd = 3,
   col = gg.col(length(ii) + 1, alpha = 0.8), 
   main = "polygon3D")

## ========================================================================
## 2D examples, with color key
## ========================================================================

arrows2D(x0 = runif(10), y0 = runif(10),
         x1 = runif(10), y1 = runif(10), colvar = 1:10, 
         code = 3, main = "arrows2D, segments2D")

segments2D(x0 = runif(10), y0 = runif(10),
         x1 = runif(10), y1 = runif(10), colvar = 1:10, 
         lwd = 2, add = TRUE, colkey = FALSE) 

# transparency
rect2D(x0 = runif(10), y0 = runif(10),
       x1 = runif(10), y1 = runif(10), colvar = 1:10, 
       alpha = 0.4, lwd = 2, main = "rect2D") 

## ========================================================================
## polygon2D 
## ========================================================================

 x <- runif(10)
 y <- runif(10)
 
 polygon2D(x, y)    # same as polygon

# several polygons, separated by NAs
 x <- runif(59) 
 y <- runif(59)

 ii <- seq(5, 55, by  = 5)
 x[ii] <- y[ii] <- NA 

# transparent colors (alpha)
 polygon2D(x, y, border = "black", lwd = 3,
   colvar = 1:(length(ii) + 1), 
   col = gg.col(), alpha = 0.2,
   main = "polygon2D")

# restore plotting parameters
 par(mfrow = pm)

}
\keyword{ hplot } 

