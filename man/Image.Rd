\name{2D image and contour plots}
\alias{image2D}
\alias{contour2D}
\alias{image2D.matrix}
\alias{image2D.array}
\alias{image2D.list}
\title{
 Extended image and contour plots for 2-D (and 3-D) data.
}
\description{
  \code{image2D} extends R's \link{image} function. Input can be a matrix (2-D) or an array (3-D) or a list.
  
  \code{contour2D} extends R's \link{contour} function.    
}
\usage{
image2D (z, ...)
contour2D (z, x = seq(0, 1, length.out = nrow(z)),
         y = seq(0, 1, length.out = ncol(z)), ...,
         col = NULL, NAcol = NULL, 
         colkey = NULL, resfac = 1,
         clab = NULL, add = FALSE, plot = TRUE)

\method{image2D}{matrix}(z, x = seq(0, 1, length.out = nrow(z)), 
               y = seq(0, 1, length.out = ncol(z)), colvar = z, ...,
               col = NULL, NAcol = "white", breaks = NULL,
               border = NA, facets = TRUE, contour = FALSE, 
               colkey = NULL, resfac = 1, clab = NULL, 
               lighting = FALSE, shade = NA, ltheta = -135, lphi = 0,
               theta = 0, rasterImage = FALSE,
               add = FALSE, plot = TRUE) 

\method{image2D}{array}(z, margin = c(1, 2), subset, ask = NULL, ...) 
\method{image2D}{list}(z, ...) 
}

\arguments{
  \item{z }{Matrix (2-D) or array (3-D) or a list with matrices or arrays,
    with z-values. By default \code{colvar} is equal to \code{z}, hence 
    \code{z} also defines the variable used to color the \link{image}. 
    Only when \code{shade} or \code{lighting} is toggled on does it make 
    sense to use \code{z} different from \code{colvar}.
    }
  \item{x, y }{Vectors or matrix with x and y values. 
     If a vector \code{x} should be of length equal to \code{nrow(z)} and 
     \code{y} should be of length equal to \code{ncol(z)}. 
     If a matrix (only for \code{image2D}), they should have
     the same dimension as \code{z} or be of dimension = \code{dim(z)+1}.
    }
  \item{colvar }{Only used when \code{shade} or \code{lighting} is toggled on.
    The variable used to color the image. 
    }
  \item{col }{Color palette to be used for the \link{image} function or for the contours.
    See details.
    }
  \item{NAcol }{Color to be used for \code{NA} values of \code{z}; for \code{image2D}, the default is ``white'',
    for \code{contour2D}, the default is to do nothing. 
    }
  \item{breaks }{a set of finite numeric breakpoints for the colors;
    must have one more breakpoint than color and be in increasing order.
    Unsorted vectors will be sorted, with a warning.
    }
  \item{contour }{If \code{TRUE}, then a \link{contour} plot will be
    added to the image plot, unless \code{x, y} are a matrix. 
    Also allowed is to pass a \code{list} with arguments for the 
    \link{contour} function. 
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
  \item{clab }{Only if \code{colkey} is not \code{NULL} or \code{FALSE}, 
    the label to be written on top of the color key. 
    The label will be written at the same level as the main title.
    To lower it, \code{clab} can be made a vector, with the first values empty 
    strings. 
    } 
  \item{resfac }{Resolution factor, one value or a vector of two numbers, for 
    the x and y- values respectively. A value > 1 will increase the 
    resolution. For instance, if \code{resfac} equals \code{3} then for each 
    adjacent pair of x- and y-values, z will be interpolated to two intermediary points. 
    This uses simple linear interpolation. If \code{resfac} is one number then
    the resolution will be increased similarly in x and y-direction.  
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
  \item{theta }{The angle defining the azimuthal direction. 
    Implemented for consistency with the other functions based on \link{persp}.
    }
  \item{border }{The color of the lines drawn around the surface facets.
    The default, \code{NA}, will disable the drawing of borders.
    }
  \item{facets }{If \code{TRUE}, then \code{col} denotes the color of the surface facets. 
    If \code{FALSE}, then the surface facets are colored ``white'' and the \code{border}
    will be colored as specified by \code{col}. If \code{NA} then the facets will be
    transparent. 
    It is usually faster to draw with \code{facets = FALSE}.
    }
  \item{rasterImage }{If \code{TRUE}, the function \link{rasterImage} will be
    used for plotting rather than \link{image} or \link{polygon}. This requires
    the \code{x} and \code{y} to be a vector with equally spaced elements.
    Note that by default, \link{rasterImage} linearly interpolates the image,
    so it will appear smoother.
    }
  \item{add }{Logical. If \code{TRUE}, then the points will be added to the current plot.
    If \code{FALSE} a new plot is started. 
    } 
  \item{plot }{Logical. If \code{TRUE} (default), a plot is created, 
    otherwise (for 3D plots) the viewing transformation matrix is returned (as invisible).
    }   
  \item{margin }{A vector giving the subscripts which the \code{image} 
    function will be applied over. 
    The image function will loop over the index that is not in \code{margin}.
    For instance, \code{c(1, 2)}, indicates to plot rows(x) and columns(y) and to 
     loop over index \code{3};  
    \code{c(2, 1)} will do the same but the image will be transposed.
    \code{margin} should be a vector with two numbers inbetween \code{1}, and \code{3}.
    }
  \item{ask }{A logical; if \code{TRUE}, the user is asked before each plot, 
    if \code{NULL} the user is only asked if more than one page of plots is 
    necessary and the current graphics device is set interactive, 
    see \link{par}(ask) and \link{dev.interactive}.
    }
  \item{subset }{Either a logical expression indicating over which elements to loop,
    or a vector or integers denoting the indices of the elements over which to loop.
    Missing values are taken as \code{FALSE}.
    }
  \item{\dots}{additional arguments passed to the plotting methods \link{image},
    \link{rasterImage}, \link{polygon} and  \link{contour}. 

    \code{alpha} can be given a value inbetween 0 and 1 to make colors transparent.
    
    The arguments after \dots must be matched exactly.
    }
}
\details{
  \code{image2D} is an extension to the default \link{image} plot that has
  the possibility to add a color key and contourlines, and to increase the 
  resolution in order to make smoother images. It also uses a different color
  scheme, it can deal with decreasing x- and y- values and x and y can be
  a matrix. In the latter case, the image will be drawn as a set of \link{polygon}s;
  if \code{x} and \code{y} are a vector, either R-function \link{image} or 
  \link{rasterImage} will be used.

  \code{image2D.array} and \code{image2D.list} are versions that
  accept a 3 dimensional array respectively a list with z-matrices 
  as their first argument to produce multiple plots.
  
  For argument \code{col} of the \code{image2D} function, 
  both \code{NA} and \code{NULL} are allowed, 
  in which case the color will be white, and no color key will be drawn.
  
  To set the ranges of the z-variable, both arguments \code{zlim} (as in \link{image})
  and \code{clim} (as in the other \code{plot3D} functions) are accepted. 

  Upon returning from the \code{image2D} and \code{contour2D} functions, the figure coordinates are defined
  by the main figure (excluding the color key). Thus, one can safely add other 
  plotting elements.  
}
\note{
  The first argument, \code{z} generally determines the color variable. 
  For consistency with the other functions, another variable, \code{colvar}
  is also defined and set by default equal to \code{z}. \code{colvar} will
  only be used if \code{shade} or \code{lighting} are toggled on. In this case,
  \code{z} will be used to define the shading (orientation of each facet), while 
  \code{colvar} will define the color.

  When \code{x} and \code{y} is a vector, the function uses R-function \link{image}.
  This means that the x- and y- axis will extend the x- and y- values with half
  a grid cell. 
  
  In contrast, when \code{x} and \code{y} are a matrix, 
  the axis will not extend the x- or y- values.  See first example.
  
}


\value{
  Returns nothing.
}

\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\seealso{
  \link{jet.col},  \link{ImageOcean}, 
  \link{Oxsat}, \link{persp3D}, \link{scatter2D} 
  for other examples where \code{image2D} is used.

  \link{image} and \link{contour} for the original R functions.

  \code{plot.image} from the fields package.
}

\examples{
# save plotting parameters
 pm <- par("mfrow")

## =======================================================================
## Difference between x or y a vector/matrix and rasterImage
## =======================================================================

 par(mfrow = c(2, 2))
 x <- y <- 1:3
 z <- matrix (nrow = 3, ncol = 3, data = 1:9)
 image2D(z, x, y, border = "black")
 image2D(z, x, y, rasterImage = TRUE, border = "black")
 image2D(z, x = matrix(nrow = 3, ncol = 3, data = x), 
       y, border = "black")
 image2D(z, x, y, border = "black", theta = 45)

## =======================================================================
## shading, light, adding contours, points and lines
## =======================================================================

 par(mfrow = c(2, 2))
 nr <- nrow(volcano)
 nc <- ncol(volcano)

 image2D(volcano, x = 1:nr, y = 1:nc, lighting = TRUE, 
       main = "volcano", clab = "height, m")

 abline(v = seq(10, 80, by = 10))
 abline(h = seq(10, 60, by = 10))
 points(50, 30, pch = 3, cex = 5, lwd = 3, col = "white")

 image2D(z = volcano, x = 1:nr, y = 1:nc, lwd = 2, shade = 0.2,
       main = "volcano", clab = "height, m")

 image2D(volcano, x = 1:nr, y = 1:nc, contour = TRUE, shade = 0.5, lphi = 0,
       col = "lightblue", main = "volcano")

 breaks <- seq(90, 200, by = 10)
 image2D(volcano, x = 1:nr, y = 1:nc, col = jet.col(length(breaks)-1),
       main = "volcano", clab = "height, m", breaks = breaks)

## =======================================================================
## Contour plots
## =======================================================================

 par(mfrow = c(2, 2))
 V <- volcano - 150
 
# default, no color key
 contour2D(z = V, colkey = FALSE, lwd = 2)

# imposed levels
 contour2D(z = V, lwd = 2, levels = seq(-40, 40, by = 20))

# negative levels dashed
 contour2D(z = V, col = "black", lwd = 2, 
           levels = seq(0, 40, by = 20))
 contour2D(z = V, col = "black", lwd = 2, lty = 2, 
           levels = seq(-40, -20, by = 20), add = TRUE)

# no labels, imposed number of levels, colorkey
 contour2D(z = V, lwd = 2, nlevels = 20, drawlabels = FALSE, 
           colkey = list(at = seq(-40, 40, by = 20)))

## =======================================================================
## A large data set, input is an array
## =======================================================================

 par(mfrow = c(1, 1))
 image2D(z = Oxsat$val[, , 1], x = Oxsat$lon, y = Oxsat$lat,
       main = "surface oxygen saturation data 2005", NAcol = "black", 
       clab = c("","","\%"))

# images at first 9 depths - use subset to select them
 image2D(z = Oxsat$val, subset = 1:9, 
       x = Oxsat$lon, y = Oxsat$lat,
       margin = c(1, 2), NAcol = "black", 
       xlab = "longitude", ylab = "latitude", 
       zlim = c(0, 115),
       main = paste("depth ", Oxsat$depth[1:9], " m"),
       mfrow = c(3, 3))

# images at latitude - depth section - increase resolution
 z <- Oxsat$val[,  Oxsat$lat > - 5 & Oxsat$lat < 5, ]
 image2D(z = z, x = Oxsat$lon, y = Oxsat$depth,
       margin = c(1, 3), NAcol = "black", 
       resfac = 3, ylim = c(5000, 0))
  
# show position of transects 
 image2D(z = Oxsat$val[ , ,1], 
       x = Oxsat$lon, y = Oxsat$lat,
       NAcol = "black")
 abline(h = Oxsat$lat[Oxsat$lat > - 5 & Oxsat$lat < 5])      

## =======================================================================
## Image of a list of matrices
## =======================================================================

 listvolcano <- list(volcano = volcano, logvolcano = log(volcano)) 
 image2D(listvolcano, x = 1:nr, y = 1:nc, contour = TRUE,
       main = c("volcano", "log(volcano)"), 
       clab = list("height, m", "log(m)"),
       zlim = list(c(80, 200), c(4.4, 5.5)))

## =======================================================================
## Image of a list of arrays
## =======================================================================

\dontrun{
# crude conversion from oxsat to oxygen 
 listoxygen <- list(Oxsat$val, Oxsat$val/100 * 360)
  
 image2D(z = listoxygen, 
       x = Oxsat$lon, y = Oxsat$lat,
       margin = c(1, 2), NAcol = "black", 
       main = c("Oxygen saturation ", " Oxygen concentration"),
       mtext = paste("depth ", Oxsat$depth, " m")
       )
}

## =======================================================================
## 'x', 'y' and 'z' are matrices
## =======================================================================

 par(mfrow = c(2, 1))

# tilted x- and y-coordinates of 'volcano'
 volcx <- matrix(nrow = 87, ncol = 61, data = 1:87)
 volcx <- volcx + matrix(nrow = 87, ncol = 61, 
        byrow = TRUE, data = seq(0., 15, length.out = 61))

 volcy <- matrix(ncol = 87, nrow = 61, data = 1:61)
 volcy <- t(volcy + matrix(ncol = 87, nrow = 61, 
        byrow = TRUE, data = seq(0., 25, length.out = 87)))

 image2D(volcano, x = volcx, y = volcy)

# x and y can also be of dimension dim(z)+1:
\dontrun{
# tilted x- and y-coordinates of 'volcano'
 volcx <- matrix(nrow = 88, ncol = 62, data = 1:88)
 volcx <- volcx + matrix(nrow = 88, ncol = 62,
        byrow = TRUE, data = seq(0., 15, length.out = 62))

 volcy <- matrix(ncol = 88, nrow = 62, data = 1:62)
 volcy <- t(volcy + matrix(ncol = 88, nrow = 62,
        byrow = TRUE, data = seq(0., 25, length.out = 88)))

 image2D(volcano, x = volcx, y = volcy)
}

# use of panel function
 image2D(volcano, x = volcx, y = volcy, NAcol = "black", 
       panel.first = substitute(box(col = "lightgrey", lwd = 30)))
                                    
## =======================================================================
## Image with NAs and logs
## =======================================================================

 par(mfrow = c(2, 2))
# normal volcano
 image2D(volcano, clab = c("height", "m"))

# logarithmic z-axis
 image2D(volcano, log = "z", clab = c("height", "m"),
     main = "log='z'")

# Including NAs
 VOLC <- volcano - 110
 VOLC [VOLC <= 0] <- NA
 image2D(VOLC, main = "including NAs and rescaled")

# both
 image2D(VOLC, NAcol = "black", log = "z", zlim = c(1, 100),
     main = "NAs and log = 'z'")

## =======================================================================
## Image with contour specification (alpha sets the transparency)
## =======================================================================

 par(mfrow = c(1, 1))
 image2D(volcano, shade = 0.2, rasterImage = TRUE,
   contour = list(col = "white", labcex = 0.8, lwd = 3, alpha = 0.5))
# same:
\dontrun{
 image2D(z = volcano, shade = 0.2, rasterImage = TRUE)
 contour2D(z = volcano, col = "white", labcex = 0.8, 
   lwd = 3, alpha = 0.5, add = TRUE)
}
# reset plotting parameters
 par(mfrow = pm)
}
\keyword{ hplot }

