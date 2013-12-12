\name{2-D data set}
\docType{data}
\alias{ImageOcean}
\alias{Hypsometry}

\title{The earths hypsometry (land elevation) and the ocean's bathymetry}

\description{
\code{Hypsometry} is a relatively crude data set of the earths land elevation
(positive) and ocean depth (negative), at 1 dg intervals.

\code{ImageOcean} plots the ocean's bathymetry.
}
\usage{
ImageOcean (...)
Hypsometry
}
\arguments{
  \item{\dots}{ arguments passed to function \link{image2D}. }
}  
\format{  
  A list with the bathymetry (depth) and hypsometry (altitude) of the
  world.  It contains:
  \describe{
    \item{x}{the latitude,}
    \item{y}{the longitude,}
    \item{z}{the height (m).}
  }
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\examples{
# save plotting parameters
 pm <- par("mfrow")
 mar <- par("mar")

## =======================================================================
## Images of the hypsometry
## =======================================================================

 par(mfrow = c(2, 2))
 image2D(Hypsometry, asp = TRUE, xlab = expression(degree*E), 
   ylab =  expression(degree*N), contour = TRUE)

# remove ocean
 zz         <- Hypsometry$z
 zz[zz < 0] <- NA
 image2D(zz, x = Hypsometry$x, y = Hypsometry$y, NAcol = "black")

## =======================================================================
## A short version for plotting the Ocean's bathymetry
## =======================================================================

 ImageOcean(asp = TRUE, contour = TRUE)
 ImageOcean(col = "white", 
   contour = list(levels = seq(-6000, 0, by = 2000)))

## =======================================================================
## A complex image of part of the ocean 
## =======================================================================

# elaborate version
 par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
 ii <- which(Hypsometry$x > -50 & Hypsometry$x < -20)
 jj <- which(Hypsometry$y >  10 & Hypsometry$y <  40)

# Draw empty persp box
 zlim <- c(-10000, 0)
 pmat <- perspbox(z = Hypsometry$z[ii, jj], 
                  xlab = "longitude", ylab = "latitude", zlab = "depth", 
                  expand = 0.5, d = 2, zlim = zlim, phi = 20, theta = 30,
                  colkey = list(side = 1))

# A function that makes a black panel with grey edge:
 panelfunc <- function(x, y, z) {
    XY <- trans3D(x, y, z, pmat = pmat)
    polygon(XY$x, XY$y, col = "black", border = "grey")
 }
 
# left panel
 panelfunc(x = c(0, 0, 0, 0), y = c(0, 0, 1, 1), 
           z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
 
# back panel
 panelfunc(x = c(0, 0, 1, 1), y = c(1, 1, 1, 1),
           z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
 
# bottom panel
 panelfunc(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0),
           z = c(zlim[1], zlim[1], zlim[1], zlim[1]))
 
# Actual bathymetry, 2 times increased resolution, with contours
 persp3D(z = Hypsometry$z[ii,jj], add = TRUE, resfac = 2,  
       contour = list(col = "grey", side = c("zmin", "z")), 
       zlim = zlim, clab = "depth, m", 
       colkey = list(side = 1, length = 0.5, dist = -0.1))

# shorter alternative for same plot, higher resolution
\dontrun{
 persp3D(z = Hypsometry$z[ii,jj], resfac = 4,  
       contour = list(col = "grey", side = c("zmin", "z")), 
       zlim = zlim, clab = "depth, m", bty = "bl2",
       xlab = "longitude", ylab = "latitude", zlab = "depth", 
       expand = 0.5, d = 2, phi = 20, theta = 30,
       colkey = list(side = 1, length = 0.5, dist = -0.1))
}

# reset plotting parameters
 par(mfrow = pm)
 par(mar = mar)
}
\seealso{
  \link{image2D}, for the image function that visualises the bathymetry
}
\details{
Hypsometry is based on dataset \code{Bathymetry} from the R-package \code{marelac}.
}
\keyword{hplot}

