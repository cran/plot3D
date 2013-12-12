\name{3-D perspectives}
\alias{persp3D}
\alias{ribbon3D}
\alias{hist3D}
\title{
 Perspective plots, 3-D ribbons and 3-D histograms.
}
\description{
  \code{persp3D} extends R's \link{persp} function. 
  
  \code{ribbon3D} is similar to \code{persp3D} but has ribbon-like colored surfaces.

  \code{hist3D} generates 3-D histograms.
}
\usage{
persp3D (x = seq(0, 1, length.out = nrow(z)), 
       y = seq(0, 1, length.out = ncol(z)), z, ..., 
       colvar = z, phi = 40, theta = 40,
       col = NULL,  NAcol = "white", border = NA, facets = TRUE,
       colkey = NULL, resfac = 1,
       image = FALSE, contour = FALSE, panel.first = NULL,
       clim = NULL, clab = NULL, bty = "b", 
       lighting = FALSE, shade = NA, ltheta = -135, lphi = 0,
       inttype = 1, curtain = FALSE, add = FALSE, plot = TRUE)

ribbon3D (x = seq(0, 1, length.out = nrow(z)), 
      y = seq(0, 1, length.out = ncol(z)), z, ..., 
      colvar = z, phi = 40, theta = 40, 
      col = NULL,  NAcol = "white", border = NA, facets = TRUE,
      colkey = NULL, resfac = 1, 
      image = FALSE, contour = FALSE, panel.first = NULL,
      clim = NULL, clab = NULL, bty = "b",
      lighting = FALSE, shade = NA, ltheta = -135, lphi = 0,
      space = 0.4, along = "x", 
      curtain = FALSE, add = FALSE, plot = TRUE)

hist3D (x = seq(0, 1, length.out = nrow(z)),
      y = seq(0, 1, length.out = ncol(z)), z, ..., 
      colvar = z, phi = 40, theta = 40, 
      col = NULL, NAcol = "white", border = NA, facets = TRUE,
      colkey = NULL, image = FALSE, contour = FALSE, 
      panel.first = NULL, clim = NULL, clab = NULL, bty = "b", 
      lighting = FALSE, shade = NA, ltheta = -135, lphi = 0,
      space = 0, add = FALSE, plot = TRUE)  
}

\arguments{
  \item{z }{Matrix (2-D) containing the values to be plotted as a \link{persp} plot. 
    }
  \item{x, y }{Vectors or matrices with x and y values. 
    If a vector, \code{x} should be of length equal to \code{nrow(z)} and 
    \code{y} should be equal to \code{ncol(z)}. If a matrix (only for \code{persp3D}), 
    \code{x} and \code{y} should have the same dimension as \code{z}.
    }
  \item{colvar }{The variable used for coloring. If present, it should have the 
    same dimension as \code{z}. Values of \code{NULL}, \code{NA}, or \code{FALSE}
    will toggle off coloration according to \code{colvar}. This gives good results
    only if \code{border} is given a color, or when \code{shade} is > 0 or 
    \code{lighting} is \code{TRUE}).
    }
  \item{col }{Color palette to be used for the \code{colvar} variable.
    If \code{col} is \code{NULL} and \code{colvar} is specified, 
    then a red-yellow-blue colorscheme (\link{jet.col}) will be used.
    If \code{col} is \code{NULL} and \code{colvar} is not specified, then 
    \code{col} will be \code{grey}. 
    
    Finally, to mimic the behavior of \link{persp}, set \code{colvar} = \code{NULL}
    and make \code{col} a matrix of colors with (nrow(z)-1) rows and 
    (ncol(z)-1) columns.
    }
  \item{NAcol }{Color to be used for \code{NA} values of \code{colvar}; default is ``white''. 
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
  \item{resfac }{Resolution factor, one value or a vector of two numbers, for 
    the x and y- values respectively. A value > 1 will increase the 
    resolution. For instance, if \code{resfac} equals \code{3} then for each 
    adjacent pair of x- and y-values, z will be interpolated to two intermediary points. 
    This uses simple linear interpolation. If \code{resfac} is one number then
    the resolution will be increased similarly in x and y-direction.  
    }
  \item{theta, phi }{The angles defining the viewing direction. 
    \code{theta} gives the azimuthal direction and \code{phi} the colatitude. 
    see \link{persp}.
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
  \item{image }{If \code{TRUE}, an image will be plotted at the bottom. 
    Also allowed is to pass a \code{list} with arguments for the \link{image2D} function.
    An optional parameter to this \code{list} is the \code{side} where the image 
    should be plotted. Allowed values for \code{side} are a z-value, 
    or \code{side = "zmin", "zmax"}, for positioning at 
    bottom or top respectively.  The default is to put the image at the bottom.
    }
  \item{contour }{If \code{TRUE}, a \link{contour} will be plotted at the bottom. 
    Also allowed is to pass a \code{list} with arguments for the \link{contour} function.
    An optional parameter to this \code{list} is the \code{side} where the image 
    should be plotted. Allowed values for \code{side} are a z-value, 
    or \code{side = "zmin", "zmax"}, for positioning at 
    bottom or top respectively.  The default is to put the image at the bottom.
    }
  \item{panel.first }{A \code{function} to be evaluated after the plot axes are 
    set up (and if applicable, images or contours drawn) but before any plotting takes place. 
    This can be useful for drawing background grids or scatterplot smooths. 
    The function should have as argument the transformation matrix (pmat), e.g. it should
    be defined as \code{function(pmat)}. See example.
    } 
  \item{along }{The direction along which the ribbons are drawn, one of "x", "y" or "xy",
    for ribbons parallel to the x- y- or both axes. In the latter case, the 
    figure looks like a net.
    }
  \item{curtain }{If \code{TRUE}, the ribbon or persp edges will be draped till the bottom. 
    }
  \item{space }{The amount of space (as a fraction of the average bar/ribbon width) 
    left between bars/ribbons. A value inbetween [0, 0.9] (\code{hist3D}) 
    or [0.1, 0.9] (\code{ribbon3D}). Either one number, or a two-valued vector, 
    for the x- and y- direction.
    }
  \item{bty }{The type of the box, the default only drawing background panels.
    Only effective if the \link{persp} 
    argument (\code{box}) equals \code{TRUE} (this is the default). See \link{perspbox}.
    }
  \item{lighting }{If not \code{FALSE} the facets will be illuminated, and colors may
    appear more bright. To switch on lighting, the argument \code{lighting} 
    should be either set to \code{TRUE} (using default settings) or it can be a 
    list with specifications of one of the following: 
    \code{ambient, diffuse, specular, exponent, sr} and \code{alpha}. 

    Will overrule \code{shade} not equal to \code{NA}.
    
    See examples in \link{jet.col}.
    }  
  \item{shade }{the degree of shading of the surface facets. 
    Values of shade close to one yield shading similar to a point light 
    source model and values close to zero produce no shading. 
    Values in the range 0.5 to 0.75 provide an approximation to daylight illumination.
    See \link{persp}.
    } 
  \item{ltheta, lphi }{if finite values are specified for \code{ltheta} and 
    \code{lphi}, the surface is shaded as though it was being illuminated from 
    the direction specified by azimuth \code{ltheta} and colatitude \code{lphi}.
    See \link{persp}.
    }
  \item{inttype }{The interpolation type to create the polygons, either 
    averaging the \code{colvar} (\code{inttype = 1, 3} or extending 
    the \code{x, y, z} values (\code{inttype = 2}) - see details.
    } 
  \item{add }{Logical. If \code{TRUE}, then the surfaces will be added to the current plot.
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
    
    \code{alpha} can be given a value inbetween 0 and 1 to make colors transparent.

    For all functions, the arguments \code{lty, lwd} can be specified; this is only
    effective is \code{border} is not \code{NA}.
    
    The arguments after \dots must be matched exactly.
    }
}
\details{
  \code{persp3D} is an extension to the default \link{persp} plot that has
  the possibility to add a color key, to increase the 
  resolution in order to make smoother images, 
  to toggle on or off facet coloration, ... 
  
  The perspective plots are drawn as filled polygons. Each polygon is defined by 
  4 corners and a color, defined in its centre. 
  When facets are colored, there are three interpolation schemes as set by \code{inttype}. 

  The default (\code{inttype = 1}) is similar to R's function \code{persp}, 
  and assumes that the \code{z}-values define the points on the corners of 
  each polygon. In case a \code{colvar} is defined, its values are to be recalculated 
  to the middle of each polygon, i.e. the color values need to be of size 
  (nx-1)(ny-1), and averages are taken from the original data 
  (nx and ny are number of x and y points). 
  This will make the colors (and/or shading) smoother.
  When \code{inttype = 1} then \code{NA} values in \code{colvar} will be used as 
  such during the averaging. This will tend to make the \code{NA} region larger.
  
  An alternative is to set \code{inttype = 3}, which is similar to \code{inttype = 1}
  except for the \code{NA} values, which will be removed during the averaging.
  This will tend to make the \code{NA} region smaller.

  By setting \code{inttype = 2}, a second interpolation scheme 
  is selected. This is mainly of use in case a \code{colvar} is defined, and it 
  is not desirable that the colors are smoothened. 
  In this scheme, it is assumed that the \code{z} values and \code{colvar} 
  values are both defined in the centre of the polygons.
  To color the facets the x, y, z grid is extended (to a (nx+1)(ny+1) grid), 
  while \code{colvar} is used as such. 
  This will make the z-values (topography) smoother than the original data.
  This type of interpolation may be preferable for color variables that have \code{NA} values,
  as taking averages tends to increase the \code{NA} region.
}
\note{
  To make a \code{contour} to appear on top of an image, 
  i.e. when \code{side = "z"}, the viewing depth
  of the contour segments is artificially decreased. In some cases this 
  may produce slight artifacts. The viewing depth can be adjusted with argument \code{dDepth},
  e.g. \code{persp3D(z = volcano, contour = list(side = "z", dDepth = 0.))}


  Parts of this help page come from the help pages of the R-core function 
   \link{persp}.
}
\value{
  Returns, as invisible, the viewing transformation matrix.

  See \link{trans3D}.
}
\seealso{
  \link{persp} for the function on which this is based.

  \link{Hypsometry} for an example where axis-panels are colored.
    
  \link{scatter3D} for a combination of a persp surface and data points.  
  
  \link{text3D} for annotating axes (hist3D).
  
  \link{plotdev} for zooming, rescaling, rotating a plot.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>
}
\references{
The \link{persp} function on which this implementation is based:

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) 
The New S Language. Wadsworth & Brooks/Cole. 
}
\examples{
# save plotting parameters
 pm <- par("mfrow")

## =======================================================================
## Ribbon, persp, color keys, facets
## =======================================================================

 par(mfrow = c(2, 2))
# simple, no scaling,
 persp3D(z = volcano, main = "volcano", clab = c("height", "m"))
  
# keep ratios between x- and y (scale = FALSE) 
# change ratio between x- and z (expand)
 persp3D(z = volcano, x = 1: nrow(volcano), y = 1:ncol(volcano),
       expand = 0.3, main = "volcano", facets = FALSE, scale = FALSE,
       clab = "height, m", colkey = list(side = 1, length = 0.5))

# ribbon, in x--direction
 V <- volcano[, seq(1, ncol(volcano), by = 3)]  # lower resolution
 ribbon3D(z = V, colkey = list(width = 0.5, length = 0.5, 
          cex.axis = 0.8, side = 2), clab = "m")

# ribbon, in y-direction
 Vy <- volcano[seq(1, nrow(volcano), by = 3), ]
 ribbon3D(z = Vy, expand = 0.3, space = 0.3, along = "y", 
          colkey = list(width = 0.5, length = 0.5, cex.axis = 0.8))
  
## =======================================================================
## Several ways to visualise 3-D data
## =======================================================================

 x <- seq(-pi, pi, by = 0.2)
 y <- seq(-pi, pi, by = 0.3)
 grid <- mesh(x, y)

 z    <- with(grid, cos(x) * sin(y))

 par(mfrow = c(2,2))

 persp3D(z = z, x = x, y = y)

 persp3D(z = z, x = x, y = y, facets = FALSE, curtain = TRUE)

# ribbons in two directions and larger spaces
 ribbon3D(z = z, x = x, y = y, along = "xy", space = 0.3)

 hist3D(z = z, x = x, y = y, border = "black")  
  
## =======================================================================
## Contours and images added
## =======================================================================

 par(mfrow = c(2, 2))
 x <- seq(1, nrow(volcano), by = 3)
 y <- seq(1, ncol(volcano), by = 3) 

 Volcano <- volcano [x, y]
 ribbon3D(z = Volcano, contour = TRUE, zlim= c(-100, 200), 
          image = TRUE)

 persp3D(z = Volcano, contour = TRUE, zlim= c(-200, 200), image = FALSE)

 persp3D(z = Volcano, x = x, y = y, scale = FALSE, 
       contour = list(nlevels = 20, col = "red"), 
       zlim = c(-200, 200), expand = 0.2,
       image = list(col = grey (seq(0, 1, length.out = 100))))

 persp3D(z = Volcano, contour = list(side = c("zmin", "z", "350")), 
       zlim = c(-100, 400), phi = 20, image = list(side = 350))

## =======================================================================
## Use of inttype
## =======================================================================

 par(mfrow = c(2, 2))
 persp3D(z = Volcano, shade = 0.5, colkey = FALSE)
 persp3D(z = Volcano, inttype = 2, shade = 0.5, colkey = FALSE)

 x <- y <- seq(0, 2*pi, length.out = 10)
 z <- with (mesh(x, y), cos(x) *sin(y)) + runif(100)
 cv <- matrix(nrow = 10, 0.5*runif(100))
 persp3D(x, y, z, colvar = cv)              # takes averages of z
 persp3D(x, y, z, colvar = cv, inttype = 2) # takes averages of colvar

## =======================================================================
## Use of inttype with NAs
## =======================================================================

 par(mfrow = c(2, 2))
 VV <- V2 <- volcano[10:15, 10:15]
 V2[3:4, 3:4] <- NA
 V2[4, 5] <- NA

 image2D(V2, border = "black")  # shows true NA region

# averages of V2, including NAs, NA region larger
 persp3D(z = VV, colvar = V2, inttype = 1, theta = 0, 
       phi = 20, border = "black", main = "inttype = 1")

# extension of VV; NAs unaffected
 persp3D(z = VV, colvar = V2, inttype = 2, theta = 0, 
       phi = 20, border = "black", main = "inttype = 2")

# average of V2, ignoring NA; NA region smaller
 persp3D(z = VV, colvar = V2, inttype = 3, theta = 0, 
       phi = 20, border = "black", main = "inttype = 3")

## =======================================================================
## Use of panel.first
## =======================================================================

 par(mfrow = c(1, 1))

# A function that is called after the axes were drawn
 panelfirst <- function(trans) {
    zticks <- seq(100, 180, by = 20)
    len <- length(zticks)
    XY0 <- trans3D(x = rep(1, len), y = rep(1, len), z = zticks,
                   pmat = trans)
    XY1 <- trans3D(x = rep(1, len), y = rep(61, len), z = zticks,
                   pmat = trans)
    segments(XY0$x, XY0$y, XY1$x, XY1$y, lty = 2) 
    
    rm <- rowMeans(volcano)             
    XY <- trans3D(x = 1:87, y = rep(ncol(volcano), 87), 
                  z = rm, pmat = trans)
    lines(XY, col = "blue", lwd = 2)
 }
 persp3D(z = volcano, x = 1:87, y = 1: 61, scale = FALSE, theta = 10,
       expand = 0.2, panel.first = panelfirst, colkey = FALSE)
                                                       
## =======================================================================
## with / without colvar / facets 
## =======================================================================

 par(mfrow = c(2, 2))
 persp3D(z = volcano, shade = 0.3, col = gg.col(100))

# shiny colors - set lphi for more brightness
 persp3D(z = volcano, lighting = TRUE, lphi = 90)

 persp3D(z = volcano, col = "lightblue", colvar = NULL, 
   shade = 0.3, bty = "b2")

# this also works:
#  persp3D(z = volcano, col = "grey", shade = 0.3)

# tilted x- and y-coordinates of 'volcano'
 volcx <- matrix(nrow = 87, ncol = 61, data = 1:87)
 volcx <- volcx + matrix(nrow = 87, ncol = 61, 
        byrow = TRUE, data = seq(0., 15, length.out = 61))

 volcy <- matrix(ncol = 87, nrow = 61, data = 1:61)
 volcy <- t(volcy + matrix(ncol = 87, nrow = 61, 
        byrow = TRUE, data = seq(0., 15, length.out = 87)))

 persp3D(volcano, x = volcx, y = volcy, phi = 80)
    
## =======================================================================
## Several persps on one plot
## =======================================================================

 par(mfrow = c(1, 1))
 clim <- range(volcano)
 persp3D(z = volcano, zlim = c(100, 600), clim = clim, 
   box = FALSE, plot = FALSE)
  
 persp3D(z = volcano + 200, clim = clim, colvar = volcano, 
       add = TRUE, colkey = FALSE, plot = FALSE)

 persp3D(z = volcano + 400, clim = clim, colvar = volcano, 
       add = TRUE, colkey = FALSE)    # plot = TRUE by default

## =======================================================================
## hist3D
## =======================================================================

 par(mfrow = c(2, 2))
 VV   <- volcano[seq(1, 87, 15), seq(1, 61, 15)]
 hist3D(z = VV, scale = FALSE, expand = 0.01)

# transparent colors
 hist3D(z = VV, scale = FALSE, expand = 0.01, 
   col = jet.col(100, alpha = 0.3), border = "black")

 hist3D(z = VV, scale = FALSE, expand = 0.01, facets = FALSE, lwd = 2)

 hist3D(z = VV, scale = FALSE, expand = 0.01, facets = NA)

## =======================================================================
## hist3D and ribbon3D with greyish background, rotated, rescaled,...
## =======================================================================

 par(mfrow = c(2, 2))
 hist3D(z = VV, scale = FALSE, expand = 0.01, bty = "g", phi = 20,
        col = "#0072B2", border = "black", shade = 0.2, ltheta = 90,
        space = 0.3, ticktype = "detailed", d = 2)

# extending the ranges
 plotdev(xlim = c(-0.2, 1.2), ylim = c(-0.2, 1.2), theta = 45)

 ribbon3D(z = VV, scale = FALSE, expand = 0.01, bty = "g", phi = 20,
        col = "lightblue", border = "black", shade = 0.2, ltheta = 90,
        space = 0.3, ticktype = "detailed", d = 2, curtain = TRUE)

 ribbon3D(z = VV, scale = FALSE, expand = 0.01, bty = "g", phi = 20, zlim = c(95,183),
        col = "lightblue", lighting = TRUE, ltheta = 50, along = "y",
        space = 0.7, ticktype = "detailed", d = 2, curtain = TRUE)

## =======================================================================
## hist3D for a 1-D data set
## =======================================================================

 par(mfrow = c(2, 1))
 x <- rchisq(1000, df = 4)
 hs <- hist(x, breaks = 15)
 
 hist3D(x = hs$mids, y = 1, z = matrix(ncol = 1, data = hs$density), 
  bty = "g", ylim = c(0., 2.0), scale = FALSE, expand = 20, 
  border = "black", col = "white", shade = 0.3, space = 0.1, 
  theta = 20, phi = 20, main = "3-D perspective")

          
# reset plotting parameters
 par(mfrow = pm)
}
\keyword{ hplot }

                                                   