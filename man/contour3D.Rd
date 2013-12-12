\name{3-D contours}
\alias{contour3D}
\title{
 Contours in 3-D plots.
}
\description{
  \code{contour3D} adds a \link{contour} in a 3-D plot. 
}
\usage{
contour3D (x = NULL, y = NULL, z = NULL, 
       ..., colvar = NULL, phi = 40, theta = 40,
       col = NULL, colkey = NULL, 
       panel.first = NULL, clim = NULL, clab = NULL, bty = "b", 
       dDepth = 1e-1, addbox = TRUE, add = FALSE, plot = TRUE)
}

\arguments{
  \item{x, y, z }{Matrix (2-D), vector, or one value containing the values 
    where the image is to be plotted. 
    At least one of them should be one number, as this will determine where the 
    image is plotted, parallel to the (y-z) plane (x one number), 
    to the (x-z) plane (y one number) or to the (z-y) plane (z one number).

    If two are vectors, the first vector should be of length equal to 
    \code{nrow(colvar)} and the second should be of length equal to 
    \code{ncol(colvar)}. 
    }
  \item{colvar }{The variable used for coloring. 
    Values of \code{colvar} equal to \code{NULL}, \code{NA}, or \code{FALSE}
    will toggle off coloration according to \code{colvar}. This gives good results
    only if \code{border} is given a color, or when \code{shade} is >0 (see \link{persp}).
    }
  \item{col }{Color palette to be used for the \code{colvar} variable.
    If \code{col} is \code{NULL} and \code{colvar} is specified, 
    then a red-yellow-blue colorscheme (\link{jet.col}) will be used.
    If \code{col} is \code{NULL} and \code{colvar} is not specified, then 
    \code{col} will be "black". 
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
  \item{clab }{Only if \code{colkey = TRUE}, the label to be written on top of the
    color key. The label will be written at the same level as the main title.
    to lower it, \code{clab} can be made a vector, with the first values empty 
    strings. 
    } 
  \item{clim }{Only if \code{colvar} is specified, the range of the color variable, used
    for the color key. Values of \code{colvar} that extend the range will be put to \code{NA}.
    } 
  \item{theta, phi }{The angles defining the viewing direction. 
    \code{theta} gives the azimuthal direction and \code{phi} the colatitude. 
    see \link{persp}.
    }
  \item{panel.first }{A \code{function} to be evaluated after the plot axes are 
    set up but before any plotting takes place. 
    This can be useful for drawing background grids or scatterplot smooths. 
    The function should have as argument the transformation matrix, e.g. it should
    be defined as \code{function(pmat)}. See example of \link{persp3D} and last example of \link{voxel3D}.
    } 
  \item{bty }{The type of the box, the default only drawing background panels.
    Only effective if the \link{persp} 
    argument (\code{box}) equals \code{TRUE} (this is the default). See \link{perspbox}.
    }
  \item{dDepth }{When a contour is added on an image, the image polygons may
    hide some contour segments. To avoid that, the viewing depth of the segments
    can be artificially decreased with the factor \code{dDepth} times the 
    \link{persp} argument \code{expand} (usually = 1), to make them 
    appear in front of the polygons. Too large values of \code{dDepth} 
    may create visible artifacts.
    }   
  \item{addbox }{If \code{TRUE} will draw a box around the
    plot.
    }   
  \item{add }{Logical. If \code{TRUE}, then the contours will be added to the current plot.
    If \code{FALSE} a new plot is started. 
    } 
  \item{plot }{Logical. If \code{TRUE} (default), a plot is created, 
    otherwise the viewing transformation matrix is returned (as invisible). 
    } 
  \item{\dots}{additional arguments passed to the plotting methods. 

    The following \link{persp} arguments can be specified: 
    \code{xlim, ylim, zlim, xlab, ylab, zlab, main, sub, r, d, 
    scale, expand, box, axes, nticks, ticktype}.      
    The arguments \code{xlim}, \code{ylim}, \code{zlim} only affect the axes.
    All objects will be plotted, including those that fall out of these ranges.
    To select objects only within the axis limits, use \link{plotdev}.

    In addition, the \link{perspbox} arguments
    \code{col.axis, col.panel, lwd.panel, col.grid, lwd.grid} can 
    also be given a value.
    
    The arguments \code{lty, lwd} can also be specified.
    
    \code{shade} and \code{lighting} arguments will have no effect.
    
    \code{alpha} can be given a value inbetween 0 and 1 to make colors transparent.
         
    The arguments after \dots must be matched exactly.
    }
}
\value{
  Returns the viewing transformation matrix.
  See \link{trans3D}.
}
\seealso{
  \link{contour} for R's 2-D contour function.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>
}
\examples{
# save plotting parameters
 pm <- par("mfrow")

## =======================================================================
## Contours
## =======================================================================
 par (mfrow = c(2, 2))

 r <- 1:nrow(volcano)
 c <- 1:ncol(volcano)
 contour3D(x = r, y = c, z = 100, colvar = volcano, zlim = c(0, 150),
   clab = c("height", "m"))

 contour3D(x = 100, y = r, z = c, colvar = volcano, clab = c("height", "m"))

 contour3D(z = volcano, colvar = volcano, lwd = 2, 
   nlevels = 20, clab = c("height", "m"), colkey = FALSE)

 contour3D(y = volcano, colvar = volcano, lwd = 2, 
   nlevels = 10, clab = c("height", "m"))

## =======================================================================
## Composite images and contours in 3D
## =======================================================================
 persp3D(z = volcano, zlim = c(90, 300), col = "white", 
         shade = 0.1, d = 2, plot = FALSE)
 contour3D(z = volcano, colvar = volcano, lwd = 2, add = TRUE, 
         nlevels = 20, clab = c("height", "m"), plot = FALSE,
         colkey = list(at = seq(90, 190, length.out = 5)))
 contour3D(z = 300, colvar = volcano, lwd = 2, col = "grey",
         add = TRUE, nlevels = 5)
          
## =======================================================================
## the viewing depth of contours (dDepth)
## =======================================================================

# too low
 persp3D(z = volcano, col = "white", shade = 0.1, plot = FALSE)
 contour3D(z = volcano, colvar = volcano, lwd = 2, 
         add = TRUE, dDepth = 0, col = "black")

# default
 persp3D(z = volcano, col = "white", shade = 0.1, plot = FALSE)
 contour3D(z = volcano, colvar = volcano, lwd = 2, 
         add = TRUE, dDepth = 0.1, col = "black")

# too high
 persp3D(z = volcano, col = "white", shade = 0.1, plot = FALSE)
 contour3D(z = volcano, colvar = volcano, lwd = 1, 
         add = TRUE, dDepth = 0.5, col = "black")

# reset plotting parameters
 par(mfrow = pm)
}
\keyword{ hplot }

                                                   