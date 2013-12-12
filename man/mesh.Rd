\name{Mesh generation}
\alias{mesh}
\title{
  Rectangular grids. 
}
\description{
  \code{mesh} creates a rectangular full 2-D or 3-D grid.
}
\usage{
mesh (x, y, z = NULL) 
}

\arguments{
  \item{x, y, z }{Vectors with x, y and z-values. 
    They can be of arbitrary length.
    }
}

\value{
  Function \code{mesh} returns a \code{list} with the expanded x- y- and z 
  arrays (in case \code{z} is not \code{NULL}) or matrices (in case \code{z = NULL}).
  The dimensions of these list elements are the same and equal to 
  \code{c(length(x), length(y), length(z))}.
}

\seealso{
  \link{persp3D}, \link{arrows3D}, \link{slice3D}, \link{surf3D}
   for other examples that use \code{mesh}.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\examples{

## ========================================================================
## 2-D mesh
## ========================================================================

 x <- c(-1 , 0, 1)
 y <- 1 : 4

# 2-D mesh
 (M <- mesh(x, y))

# calculate with this mesh
 V <- with (M, x/2 * sin(y))

# same as:
 V2 <- outer(x, y, FUN = function(x, y) x/2*sin(y))

## ========================================================================
## 3-D mesh
## ========================================================================

 x <- y <- z <- c(-1 , 0, 1)

# 3-D mesh
 (M <- mesh(x, y, z))

# calculate with 3-D mesh
 V <- with (M, x/2 * sin(y) *sqrt(z+2))
  
# plot result
 scatter3D(M$x, M$y, M$z, V, pch = "+", cex = 3, colkey = FALSE)
}
\keyword{ hplot }

