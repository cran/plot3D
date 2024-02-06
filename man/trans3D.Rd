\name{trans3D}
\alias{trans3D}
\title{
Transformation of 3D elements
}
\description{
  \code{trans3D} is the \code{plot3D} equivalent of \link{trans3d}, that projects
  3-D elements to 2 dimensions.
}
\usage{
trans3D (x, y, z, pmat) 
}

\arguments{
  \item{x, y, z }{Vectors, matrices, arrays, with x, y and z-values. 
    }
  \item{pmat }{A 4 x 4 viewing transformation matrix, suitable for projecting 
    the 3D coordinates (x,y,z) into the 2D plane using homogeneous 
    4D coordinates (x,y,z,t); such matrices are returned by any of the 3-D
    plotting functions from package \code{plot3D} and by \link{persp}().
    }
}

\value{
  A list with two components:
  
    \item{x, y }{the projected 2-D coordinates of the 3-D input \code{x, y, z}}

  In contrast to \link{trans3d}, \code{trans3D} the returned values x and y 
  will be of the same class and dimensions as the input x and y.
  If inputted \code{x, y, z} are matrices or arrays, so will the projected
   coordinates be.
}

\seealso{
  \link{scatter3D}, \link{slice3D}, \link{surf3D}.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\examples{
## ========================================================================
## 3-D mesh
## ========================================================================

 x <- y <- z <- c(-1 , 0, 1)

# plot a 3-D mesh
 (M <- mesh(x, y, z))

# plot result
 pmat <- scatter3D(M$x, M$y, M$z, pch = "+", cex = 3, colkey = FALSE)

# add line
 XY <- trans3D(x = c(-1, 1), y = c(-1, 1), z = c(-1, 1), pmat = pmat) 
 lines(XY, lwd = 2, col = "blue")

## ========================================================================
## Example 2
## ========================================================================

 pmat <- perspbox (z = diag(2))
 XY <- trans3D(x = runif(30), y = runif(30), z = runif(30), pmat = pmat) 
 polygon(XY, col = "darkblue")
}
\keyword{ hplot }

