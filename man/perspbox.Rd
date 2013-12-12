\name{Perspective box}
\alias{perspbox}
\title{
  Creates an empty perspective box, ready for adding objects
}
\description{
  \code{perspbox} draws a box and labels, and makes space for a colorkey (if any).
}
\usage{
perspbox (x = seq(0, 1, length.out = nrow(z)), 
          y = seq(0, 1, length.out = ncol(z)), z, 
          bty = c("b", "b2", "f", "g", "bl", "bl2", "u", "n"),  ..., 
          col.axis = "black", col.panel = NULL, lwd.panel = 1,                     
          col.grid = NULL, lwd.grid = 1, 
          phi = 40, theta = 40, col = NULL,
          colkey = NULL, plot = TRUE)
}

\arguments{
  \item{x, y }{Vectors with x and y values. It is sufficient to pass the 
    ranges of the x- and y-values, as they will not be drawn. 
    If \code{z} is a matrix, it is required that \code{length(x) = nrow(z)} and 
    \code{length(y) = ncol(z)}.
    }
  \item{z }{Matrix or vector with z-values. If \code{z} is a matrix, 
    it is sufficient to pass a diagonal matrix with the range of the z-values, 
    as they will not be drawn. 
    }
  \item{bty }{The type of the box; only effective if the \link{persp} 
    argument \code{box} equals \code{TRUE} (the default).
    Unless \code{bty} is equal to \code{"u"} then the arguments 
    \code{col.axis, col.panel, lwd.panel, col.grid, lwd.grid} will be ignored.
    \code{"f"} is the full box, the default as from \link{persp}, 
    \code{"b"} has only the back panels visible, when \code{"b2"}
    has back panels and grid lines, \code{"g"} has grey background with white 
    gridlines, \code{"bl"} has a black background, 
    \code{"bl2"} has a black background with grey lines.
    \code{"u"} means that the user will specify the arguments 
    \code{col.axis, col.panel, lwd.panel, col.grid, lwd.grid} manually.
    \code{"n"} means that no box will be drawn. This is the same as setting
    \code{box = FALSE}.
    }
  \item{col.axis, col.panel, col.grid }{The color of the axis line, of the 
    axis panel or of the grid lines. Only used if \code{bty = "u"}.
    }
  \item{lwd.panel, lwd.grid }{The width of the panel border or of the 
    grid lines. Only used if \code{bty = "u"}.
    }
  \item{theta, phi }{The angles defining the viewing direction. 
    \code{theta} gives the azimuthal direction and \code{phi} the colatitude. 
    see \link{persp}.
    }
  \item{col }{Colors to be used for coloring the \code{colvar} variable. 
    Here only used for assessing if a color key should be drawn.
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
  \item{plot }{Logical. If \code{TRUE} (default), a plot is created, 
    otherwise the viewing transformation matrix is returned (as invisible). 
    } 
  \item{\dots}{additional arguments passed to \link{persp}. 

    The following \link{persp} arguments can be specified: 
    \code{xlim, ylim, zlim, xlab, ylab, zlab, main, sub, r, d, 
    scale, expand, box, axes, nticks, ticktype}.      

    Arguments \code{scale} and \code{expand} affect the size of the axes.
    
    The arguments after \dots must be matched exactly.
    }
}

\value{
  Function \code{perspbox} returns the viewing transformation matrix.
  See \link{trans3D}.
}

\seealso{
  \link{persp3D}, \link{scatter2D}, \link{surf3D} for examples where box types 
   different than the default are used.
   
  \link{Hypsometry} for an example where colored axis-panels are added to a 
  figure started with \code{perspbox}.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\details{
  The arguments \code{xlim}, \code{ylim}, \code{zlim} only affect the axes.
  All objects will be plotted, including those that fall out of these ranges.
  To select objects only within the axis limits, use \link{plotdev}.

  The predefined box types \code{bty} are defined as follows:

  ``f'': all panels are shown and transparent, also the \link{persp} default.

  ``b'': only backward panels shown.
  
  ``b2'': as ``b'' with \code{col.grid = "grey"}.

  ``g'': only backward panels shown; \code{col.panel = grey(0.95)}, 
    \code{col.axis = "grey"},
     \code{lwd.grid = 2} and \code{col.grid = "white"}.

  ``bl'': only backward panels shown; \code{col.panel = "black"}, 
  \code{col.axis = "grey"},
     \code{lwd.grid = 2} and \code{col.grid = "white"}.

  ``n'': no box is drawn.
}

\examples{
# save plotting parameters                            
 pm   <- par("mfrow")
 pmar <- par("mar")

## ========================================================================
## The 4 predefined box types
## ========================================================================

 par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
 
# box type with only backward panels
 perspbox(z = volcano, bty = "b", ticktype = "detailed", d = 2, 
          main  = "bty = 'b'")
# box as in 'persp'
 perspbox(z = volcano, bty = "f", ticktype = "detailed", 
          d = 2, main  = "bty = 'f'")

# back panels with gridlines, detailed axes
 perspbox(z = volcano, bty = "b2", ticktype = "detailed", 
          d = 2, main  = "bty = 'b2'")

# ggplot-type, simple axes 
 perspbox(z = volcano, bty = "g", 
          d = 2, main  = "bty = 'g'")

## ========================================================================
## A user-defined box
## ========================================================================

 par(mfrow = c(1, 1))

 perspbox(z = diag(2), bty = "u", ticktype = "detailed", 
          col.panel = "gold", col.axis = "white",  
          scale = FALSE, expand = 0.4, 
          col.grid = "grey", main = "user-defined")

# restore plotting parameters
 par(mfrow = pm)
 par(mar = pmar)
}
\keyword{ hplot }

