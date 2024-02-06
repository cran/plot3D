\name{Colors}
\alias{jet.col}
\alias{jet2.col}
\alias{gg.col}
\alias{gg2.col}
\alias{ramp.col}
\alias{alpha.col}
\title{
   Colors, shading, lighting.
}
\description{
  \code{jet.col} generates the matlab-type colors.

  \code{jet2.col} is similar but lacks the deep blue colors

  \code{gg.col} and \code{gg2.col} generate gg-plot-like colors.

  \code{ramp.col} creates color schemes by interpolation. 

  \code{alpha.col} creates transparent colors. 
}
\usage{
jet.col (n = 100, alpha = 1)

jet2.col (n = 100, alpha = 1)

gg.col (n = 100, alpha = 1)

gg2.col (n = 100, alpha = 1)

ramp.col (col = c("grey", "black"), n = 100, alpha = 1)

alpha.col (col = "grey", alpha = 0.5)
}

\arguments{
  \item{n }{Number of colors to generate.
    }
  \item{alpha }{Value in the range [0, 1] for alpha 
    transparency channel (0 means transparent and 1 means opaque).
    Transparency defined in the color palette is overruled when 
    \code{lighting} or \code{shading} is switched on. 
    To combine transparency with lighting or shading, pass argument \code{alpha} 
    to the plotting functions directly.
    }
  \item{col }{Colors to interpolate, change.
    }
}

\value{
  A list with colors.
}
\seealso{
  \link{colorRamp} and \link{colorRampPalette} for comparable (and more elaborate) 
  R-functions.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}
\references{
The gg-plot type of colors \code{gg.plot} is a color-blind friendly palette from 
\code{http://wiki.stdout.org/rcookbook/Graphs}.
}
\details{
In addition to the color functions described here, colors 
can also be adapted by shading and lighting, or made transparent. 
Shading will be overruled if lighting is not \code{FALSE}.

To make colors transparent, use argument \code{alpha}, with a value inbetween 0 and 1.

To switch on shading, the argument \code{shade} should be given a value inbetween 0 and 1.

To switch on lighting, the argument \code{lighting} should be either set to \code{TRUE}
(in which case default settings will be used) or should be a list with specifications of 
one of the following: \code{ambient, diffuse, specular, exponent, sr} and \code{alpha}.

The defaults are:
\code{ambient = 0.3, diffuse = 0.6, specular = 1., exponent = 20, sr = 0, alpha = 1}

Lighting is defined as the sum of ambient, diffuse and specular light.
If \code{N} is the normal vector on the facets (3-values, x-, y-, z direction) 
and \code{I} is the light vector, then 
\code{col = (ambient + Id + sr * Is) * col + (1 - sr) * Is}, where
\code{Is = specular * abs(Light) ^ exponent}, \code{Id = diffuse * Light} and 
\code{Light = sum(N*I)}.

The lighting algorithm is very simple, i.e. it is flat shading, no interpolation.

Toggling on lighting or shading also requires the input of the angles of the 
light source, as \code{ltheta} and \code{lphi}, whose defaults are: 
\code{ltheta = -135, lphi = 0}. This usually works well for shading, but may
not be optimal for lighting.
}
\examples{
# save plotting parameters
 pm <- par("mfrow")
 pmar <- par("mar")

## =======================================================================
## Transparency and various color schemes
## =======================================================================

 par(mfrow = c(3, 3))
 for (alph in c(0.25, 0.75))
   image2D(volcano, alpha = alph, 
         main = paste("jet.col, alpha = ", alph))  
 image2D(volcano, main = "jet.col")
 image2D(volcano, col = jet2.col(100), main = "jet2.col")
 image2D(volcano, col = gg.col(100), main = "gg.col")
 image2D(volcano, col = gg2.col(100), main = "gg2.col")
 image2D(volcano, col = rainbow(100), main = "rainbow")
 image2D(volcano, col = terrain.colors(100), main = "terrain.colors")
 image2D(volcano, col = ramp.col(c("blue", "yellow", "green", "red")),
       main = "ramp.col")  

## =======================================================================
## Shading, lighting -  one color
## =======================================================================

# create grid matrices
 X      <- seq(0, pi, length.out = 50)
 Y      <- seq(0, 2*pi, length.out = 50)
 M      <- mesh(X, Y)
 phi    <- M$x
 theta  <- M$y

# x, y and z grids
 x <- sin(phi) * cos(theta)
 y <- cos(phi)
 z <- sin(phi) * sin(theta)

# these are the defaults
 p <- list(ambient = 0.3, diffuse = 0.6, specular = 1.,
           exponent = 20, sr = 0, alpha = 1)

 par(mfrow = c(3, 3), mar = c(0, 0, 0, 0))
 Col <- "red"

 surf3D(x, y, z, box = FALSE, col = Col, shade = 0.9)
 surf3D(x, y, z, box = FALSE, col = Col, lighting = TRUE)  
 surf3D(x, y, z, box = FALSE, col = Col, lighting = list(ambient = 0))
 surf3D(x, y, z, box = FALSE, col = Col, lighting = list(diffuse = 0))
 surf3D(x, y, z, box = FALSE, col = Col, lighting = list(diffuse = 1))
 surf3D(x, y, z, box = FALSE, col = Col, lighting = list(specular = 0))
 surf3D(x, y, z, box = FALSE, col = Col, lighting = list(exponent = 5))
 surf3D(x, y, z, box = FALSE, col = Col, lighting = list(exponent = 50))
 surf3D(x, y, z, box = FALSE, col = Col, lighting = list(sr = 1)) 

## =======================================================================
## Shading, lighting with default colors
## =======================================================================

 x <- seq(-pi, pi, len = 100)
 y <- seq(-pi, pi, len = 100)
 grid <- mesh(x, y)

 z    <- with(grid, cos(x) * sin(y))
 cv   <- with(grid, -cos(y) * sin(x))

# lphi = 180, ltheta = -130  - good for shade
# lphi = 90, ltheta = 0  - good for lighting

 par(mfrow = c(2, 2))
 persp3D(z = z, x = x, y = y, colvar = cv, zlim = c(-3, 3), colkey = FALSE)
 persp3D(z = z, x = x, y = y, colvar = cv, zlim = c(-3, 3), 
       lighting = TRUE, colkey = FALSE)
 persp3D(z = z, x = x, y = y, colvar = cv, zlim = c(-3, 3), 
       shade = 0.25, colkey = FALSE)
 persp3D(z = z, x = x, y = y, colvar = cv, zlim = c(-3, 3), 
       lighting = TRUE, lphi = 90, ltheta = 0, colkey = FALSE)

## =======================================================================
## transparency of a vector of colors
## =======================================================================

 par(mfrow = c(1, 1))
 x <- runif(19)
 y <- runif(19)
 z <- runif(19)
# split into 5 sections (polygons) 
 ii <- seq(4, 19, by = 4)
 x[ii] <- y[ii] <- z[ii] <- NA
  
 polygon3D(x, y, z, border = "black", lwd = 2, 
   col = alpha.col(c("red", "lightblue", "yellow", "green", "black"), 
                  alpha = 0.4))

# the same, now passing alpha as an argument to polygon3D:
\dontrun{
 polygon3D(x, y, z, border = "black", lwd = 2, 
   col = c("red", "lightblue", "yellow", "green", "black"), 
                  alpha = 0.4)
}
# reset plotting parameters
 par(mfrow = pm)
 par(mar = pmar)
}
\keyword{ hplot }

