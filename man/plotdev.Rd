\name{Composite plots}
\alias{plot.plist}
\alias{plotdev}
\alias{setplist}
\alias{getplist}
\alias{selectplist}

\title{
  Handling and plotting plotting lists.
}
\description{
  S3 method \code{plot.plist} and function \code{plotdev} plot the plotting list 
  to the current device.
  Changes can be made to the perspective view, to the lighting and shading, 
  or to make colors transparent.

  \code{getplist} and \code{setplist} retrieve and store information in the plotting list.
  
  \code{selectplist} selects parts from the plotting list, based on a user-defined function.
}
\usage{
getplist()
setplist(plist)
plotdev(...)
\method{plot}{plist} (x, ...) 
selectplist(plist, SS)
}

\arguments{
  \item{x, plist }{The plotting \code{list} as generated (invisibly) 
    by any of the 3D plotting functions.
    }
  \item{SS }{Function which tests points for inclusion in the plotting list.
    It should take as argument three vectors (x, y, z) and return a vector of 
    equal length that is either \code{TRUE} or \code{FALSE}, denoting whether
    the point should be selected or not.
    }
  \item{... }{Additional arguments to change the view or coloration. 
    Supported arguments to change the view are :
    \code{theta, phi, xlim, ylim, zlim, d, r, scale, expand}. See \link{perspbox},
    \link{persp}.
    
    Supported arguments to change the lighting, or coloration are :
    \code{ltheta, lphi, shade, lighting}. See \link{jet.col}.
    }
}

\details{
  All 3-D functions from package \code{plot3D} produce or update a plotting list
  that is local to the package. One can access this plotting list via \code{getplist}
  and \code{setplist}. The list is used to plot when, in a 3-D function, the argument
  \code{plot} is \code{TRUE} or via function \code{plotdev}.
  
  The plotting \code{list} can contain the following items:

 \itemize{
   \item \code{mat}, the viewing transformation matrix,
    a 4 x 4 matrix suitable for projecting 3D coordinates 
    (x, y, z) into the 2D plane using homogeneous 4D coordinates (x,y,z,v). 
  
    It can be used to superimpose additional graphical elements on the 3D plot, 
    by any function that is defined on \link{persp}.
  
    It can also be used to add \code{lines}, \code{arrows} or 
    \code{points}, using the function \link{trans3D}. 
  
   \item \code{plt}, with original \code{plt} parameters and the \code{plt} 
    parameters used for the main frame.

   \item \code{persp}, with settings for the perspective box.

   \item \code{xlim, ylim, zlim}, with ranges.
   
   \item \code{scalefac}, the scaling factors in x, y and z direction,
    used e.g. for shading.

   \item \code{dot} other plotting parameters passed to \code{persp}.
   
   \item \code{colkey}, \code{numkey}, with settings for the color key(s).
  
   \item \code{poly, segm, pt, CIpt, labels, arr} the information for drawing 
    polygons, segments, points, points with confidence intervals, labels and arrows,
    that are part of the plot.  
  }

  For the item \code{poly} the elements are:
 \itemize{
  \item \code{x, y, z} : A matrix with typically 4 or 5 rows, the first rows defining the x-, y- or z- 
    values of each polygon, the last row contains \code{NA} (and which therefore terminates a polygon).
  
  \item \code{col}: a vector with the colors for the facets of each polygon.
  
  \item \code{lwd, lty, border}: a vector with the line widths, line type and colors for the border of each polygon.
    (note in R-function \link{polygon}, passing a vector of line widths is not implemented;
    therefore, only the first value of \code{lwd} will be used for all polygons).
  
    When \code{plot.plist} is called, the projection depth is calculated and used to sort the
    facets and function \code{polygon} used to draw them.
  }
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\value{
  Returns the updated plotting list.
}

\note{
 Once a 3D plot has been generated, a new device can be opened and 
 \code{plotdev} used to plot also on this device.
 
 \code{plotdev} and \code{plot(getplist())} are the same.
 
 In an extension package, \code{plot3Drgl}, a similar function, \code{plotrgl},
 plots the graphs to the device opened with \code{rgl}. This allows interactive
 zooming, rotating, etc...
}

\examples{
# save plotting parameters                            
 pm   <- par("mfrow")
 pmar <- par("mar")

## ========================================================================
## The volcano
## ========================================================================

 par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))

# The volcano at lower resolution
 x <- seq(1, nrow(volcano), by = 2)
 y <- seq(1, ncol(volcano), by = 2)
 V <- volcano[x,y]

 persp3D(z = V)

# rotate
 plotdev(theta = 0)

# light and transparence
 plotdev(lighting  = TRUE, lphi = 90, alpha = 0.6)  

# zoom
 plotdev(xlim = c(0.2, 0.6), ylim = c(0.2, 0.6), phi = 60) 
 
## ========================================================================
## Two spheres 
## ========================================================================

 par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))

# create a sphere
 M  <- mesh(seq(0, 2*pi, length.out = 30),
            seq(0,   pi, length.out = 30))
 u  <- M$x ; v  <- M$y

 x <- cos(u)*sin(v)
 y <- sin(u)*sin(v)
 z <- cos(v)

 surf3D(x = 2*x, y = 2*y, z = 2*z, 
        colvar = NULL, lighting = TRUE, #plot = FALSE,
        facets = NA, col = "blue", lwd = 5)
 
 surf3D(x, y, z, colvar = NULL, lighting = TRUE, 
        col = "red", add = TRUE)

 names(getplist())

# plot with different view:
 plotdev(phi = 0)  
\dontrun{  # will plot same 3-D graph to pdf
 pdf(file = "save.pdf")
 plotdev()
 dev.off()
}
             
## ========================================================================
## Two spheres and two planes 
## ========================================================================

 par(mar = c(2, 2, 2, 2))

# equation of a sphere
 M  <- mesh(seq(0, 2*pi, length.out = 100),                                     -
            seq(0,   pi, length.out = 100))
 u  <- M$x ; v  <- M$y

 x <- cos(u)*sin(v)
 y <- sin(u)*sin(v)
 z <- cos(v)

 surf3D(x, y, z, colvar = z, 
        theta = 45, phi = 20, bty = "b",
        xlim = c(-1.5, 1.5), ylim = c(-1, 2), 
        zlim = c(-1.5, 1.5), plot = FALSE)

# add a second sphere, shifted 1 unit to the right on y-axis; 
# no facets drawn for this sphere 
 surf3D (x, y+1, z, colvar = z, add = TRUE, 
         facets = FALSE, plot = FALSE)

# define a plane at z = 0
 Nx <- 100
 Ny <- 100
  
 x <- seq(-1.5, 1.5, length.out = Nx)
 y <- seq(-1, 2, length.out = Ny)

 image3D (x = x, y = y, z = 0, add = TRUE, colvar = NULL, 
          col = "blue", facets = TRUE, plot = FALSE)

# another, small plane at y = 0 - here x and y have to be matrices!
 x <- seq(-1., 1., length.out = 50)
 z <- seq(-1., 1., length.out = 50)
 
 image3D (x = x, y = 0, z = z, colvar = NULL, 
         add = TRUE, col = NA, border = "blue", 
         facets = TRUE, plot = TRUE)       

\dontrun{  # rotate 
 for (angle in seq(0, 360, by = 10)) 
   plotdev(theta = angle)
}

## ========================================================================
## Zooming, rescaling, lighting,...
## ========================================================================

 par(mfrow = c(2, 2)) 

# The volcano
 x <- seq(1, nrow(volcano), by = 2)
 y <- seq(1, ncol(volcano), by = 2)
 V <- volcano[x,y]
# plot the volcano
 persp3D (x, y, z = V, colvar = V, theta = 10, phi = 20, 
          box = FALSE, scale = FALSE, expand = 0.3, 
          clim = range(V), plot = FALSE)

# add a plane (image) at z = 170; jetcolored, transparant: only border
 image3D(x, y, z = 170, add = TRUE, clim = range(V), 
         colvar = V, facets = NA, plot = FALSE, colkey = FALSE)

# add a contour (image) at z = 170; jetcolored, 
 contour3D(x, y, z = 170, add = TRUE, clim = range(V),
           colvar = V, plot = FALSE, colkey = FALSE)

# plot it  - 
 plot(getplist())   #  same as plotdev()

# plot but with different expansion
 plotdev(expand = 1)

# other perspective, and shading
 plotdev(d = 2, r = 10, shade = 0.3)
    
# zoom and rotate
 plotdev(xlim = c(10, 30), ylim = c(20, 30), phi = 50)

## ========================================================================
## Using setplist
## ========================================================================

 polygon3D(runif(3), runif(3), runif(3))
# retrieve plotting list
 plist <- getplist()
 names(plist)
 plist$poly
# change copy of plotting list
 plist$poly$col <- "red"
# update internal plotting list
 setplist(plist)
# plot updated list
 plotdev()
 

## ========================================================================
## Using selectplist
## ========================================================================

 polygon3D(runif(10), runif(10), runif(10), col = "red", 
   alpha = 0.2, plot = FALSE, ticktype = "detailed", 
   xlim = c(0,1), ylim = c(0, 1), zlim = c(0, 1))
 polygon3D(runif(10)*0.5, runif(10), runif(10), col = "yellow", 
   alpha = 0.2, plot = FALSE, add = TRUE)
 polygon3D(runif(10)*0.5+0.5, runif(10), runif(10), col = "green", 
   alpha = 0.2, plot = FALSE, add = TRUE)
 points3D(runif(10), runif(10), runif(10), col = "blue", 
   add = TRUE, plot = FALSE)
 segments3D(x0 = runif(10), y0 = runif(10), z0 = runif(10), 
   x1 = runif(10), y1 = runif(10), z1 = runif(10), 
   colvar = 1:10, add = TRUE, lwd = 3)

# retrieve plotting list
 plist <- getplist()

# selection function 
 SS <- function (x, y, z)  {
   sel <- rep(TRUE, length.out = length(x))
   sel[x < 0.5] <- FALSE
   return(sel)
 } 
# The whole polygon will be removed or kept.  
 plot(x = selectplist(plist, SS), 
   xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1))

# restore plotting parameters
 par(mfrow = pm)
 par(mar = pmar)
}
\keyword{ hplot }

