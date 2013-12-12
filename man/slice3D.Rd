\name{3-D volume visualisation}
\alias{slice3D}
\alias{slicecont3D}
\alias{isosurf3D}
\alias{triangle3D}
\alias{voxel3D}
\alias{createisosurf}
\alias{createvoxel}
\title{
 Functions for plotting 3-D volumetric data.
}
\description{
  
  \code{slice3D} plots a 3-D dataset with a color variable as slices or on surfaces.

  \code{slicecont3D} plots a 3-D dataset with a color variable as contours on slices.
  
  \code{isosurf3D} plots isosurfaces from a 3-D dataset.

  \code{voxel3D} plots isosurfaces as scatterpoints.

  \code{createisosurf} create the isosurfaces (triangulations)
  from volumetric data. Its output can be plotted with \code{triangle3D}.

  \code{createvoxel} creates voxels (x, y, z) points from volumetric data. 
  Its output can be plotted with \link{scatter3D}.
}
\usage{
slice3D (x, y, z, colvar, ..., phi = 40, theta = 40,
         xs = min(x), ys = max(y), zs = min(z),
         col = jet.col(100), NAcol = "white", 
         border = NA, facets = TRUE, colkey = NULL, 
         panel.first = NULL, clim = NULL, 
         clab = NULL, bty = "b", 
         lighting = FALSE, shade = NA, ltheta = -135, lphi = 0,  
         add = FALSE, plot = TRUE) 

slicecont3D (x, y, z, colvar, ..., phi = 40, theta = 40,
         xs = NULL, ys = NULL, zs = NULL, level = NULL,
         col = jet.col(100), NAcol = "white", 
         border = NA, facets = TRUE, 
         colkey = NULL, panel.first = NULL,
         clim = NULL, clab = NULL, bty = "b", 
         dDepth = 0, add = FALSE, plot = TRUE) 

isosurf3D (x, y, z, colvar, ..., phi = 40, theta = 40, 
         level = mean(colvar, na.rm = TRUE), isofunc = createisosurf,
         col = NULL, border = NA, facets = TRUE, 
         colkey = NULL, panel.first = NULL, 
         clab = NULL, bty = "b", 
         lighting = FALSE, shade = 0.5, ltheta = -135, lphi = 0, 
         add = FALSE, plot = TRUE) 

voxel3D (x, y, z, colvar, ..., phi = 40, theta = 40, 
         level = mean(colvar, na.rm = TRUE), eps = 0.01, operator = "=", 
         col = jet.col(100), panel.first = NULL, 
         bty = "b", add = FALSE, plot = TRUE) 

triangle3D (tri, colvar = NULL, ..., phi = 40, theta = 40,
           col = NULL, NAcol = "white", 
           border = NA, facets = TRUE,
           colkey = NULL, panel.first = NULL,
           lighting = FALSE, shade = 0.5, ltheta = -135, lphi = 0, 
           clim = NULL, clab = NULL,
           bty = "b", add = FALSE, plot = TRUE)  

createisosurf (x, y, z, colvar, level = mean(colvar, na.rm = TRUE))

createvoxel (x, y, z, colvar, level = mean(colvar, na.rm = TRUE), eps = 0.01,
             operator = "=")
}

\arguments{
  \item{x, y, z }{Vectors with x, y and z-values. 
    They should be of length equal to the first, second and 
    third dimension of \code{colvar} respectively.
    }
  \item{colvar }{The variable used for coloring. 
    It should be an array of dimension equal to 
    \code{c(length(x), length(y), length(z))}.
    For \code{triangle3D}, \code{colvar} should be of length = nrow(\code{tri}) / 3.
    It must be present.
    }
  \item{tri }{A three-columned matrix (x, y, z) with triangle coordinates. 
    A triangle is defined by three consecutive rows.
    }  
  \item{isofunc }{A function defined as \code{function(x, y, z, colvar, level)},
    and that returns the three-columned matrix with triangle coordinates.
    The default, \code{createisosurf} uses function \link{computeContour3d} 
    from package \code{misc3d}. 
    }
  \item{theta, phi }{the angles defining the viewing direction. 
    \code{theta} gives the azimuthal direction and \code{phi} the colatitude. see \link{persp}.
    }
  \item{col }{Colors to be used for coloring the \code{colvar} variable. 
    If \code{col} is \code{NULL} 
    then a red-yellow-blue colorscheme (\link{jet.col}) will be used.
    }
  \item{NAcol }{Colors to be used for \code{colvar} values that are \code{NA}.
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
  \item{panel.first }{A \code{function} to be evaluated after the plot axes are 
    set up but before any plotting takes place. 
    This can be useful for drawing background grids or scatterplot smooths. 
    The function should have as argument the transformation matrix, e.g. it should
    be defined as \code{function(pmat)}. See last example and example of \link{persp3D}.
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
  \item{xs, ys, zs }{Vectors or matrices.
    Vectors specify the positions in x, y or z where the slices (planes) are to be drawn.
    The values of \code{colvar} will be projected on these slices.
    Matrices specify a surface on which the \code{colvar} will be projected.
    } 
  \item{level }{The level(s) at which the \code{contour} will be generated or the
    isosurfaces generated. 
    
    There can be more than one level, but for \code{slicecont3D} too many 
    will give a crowded view, and one is often best.
    For \code{isosurf3D}, the use of multiple values may need transparent colors to visualise.
    For \code{voxel3D}, \code{level} should either be one number (if \code{operator}
    equals \code{'=', '<', '>'}) or two numbers (for \code{operator = '<>'}).
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
  \item{bty }{The type of the box, the default only draws background panels.
    Only effective if the \link{persp} 
    argument (\code{box}) equals \code{TRUE} (this is the default). See \link{perspbox}.
    }
  \item{eps }{The voxel precision, only used when \code{operator = "="}.
    A point is selected if it closer than \code{eps*diff(range(colvar))} 
    to the required level.
    }
  \item{operator }{One of '=', '<', '>', '<>' for selection of points `equal' (within
    precision), larger or smaller than the required level or to be within an 
    interval.
    }
  \item{dDepth }{When a contour is added on an image, the image polygons may
    hide some contour segments. To avoid that, the viewing depth of the segments
    can be artificially decreased with the factor \code{dDepth} times the 
    \link{persp} argument \code{expand} (usually = 1), to make them 
    appear in front of the polygons. 
    Too large values of \code{dDepth} may create visible artifacts.
    See \link{contour3D}.
    }   
  \item{add }{Logical. If \code{TRUE}, then the slices, voxels or surfaces will be added to the current plot.
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

    For all functions, the arguments \code{lty, lwd} can be specified.
    
    The arguments after \dots must be matched exactly.
    }
}

\note{
  The \code{isosurf3D} function uses function  \code{computeContour3d}, 
  from package \code{misc3d}, which is based on the marching cubes algorithm.
  Please cite the package \code{misc3d} (Feng & Tierney, 2008) when using \code{isosurf3D}.
  
  For \code{voxel3D}, coloring is always according to the z-variable. A more flexible
  coloration can be achieved by using \code{createvoxel}, followed by \link{scatter3D}.
  See examples.
}
                         
\value{
  The plotting functions return the viewing transformation matrix,
  See \link{trans3D}.
  
  Function \code{createisosurf} returns a three-columned matrix (x, y, z) with 
  triangle coordinates. One triangle is defined by three consecutive rows.
  It can be plotted with \code{triangle3D}.
  
  Function \code{createvoxel} returns a list with the elements \code{x, y, z} 
  defining the points that are at a distance of less than 
  \code{eps*diff(range(colvar))} from the required \code{level}.
  Its output can be plotted with \link{scatter3D}.
}
\seealso{
  \link{Oxsat} for another example of \code{slice3D}.

  \link{plotdev} for zooming, rescaling, rotating a plot.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\examples{
# save plotting parameters
 pm <- par("mfrow")
 pmar <- par("mar")

## =======================================================================
## Simple slice3D examples
## =======================================================================

 par(mfrow = c(2, 2))
 x <- y <- z <- seq(-1, 1, by = 0.1)
 grid   <- mesh(x, y, z)
 colvar <- with(grid, x*exp(-x^2 - y^2 - z^2))

# default is just the panels
 slice3D  (x, y, z, colvar = colvar, theta = 60)

# contour slices
 slicecont3D (x, y, z, ys = seq(-1, 1, by = 0.5), colvar = colvar, 
           theta = 60, border = "black")
          
 slice3D  (x, y, z, xs = c(-1, -0.5, 0.5), ys = c(-1, 0, 1), 
           zs = c(-1, 0), colvar = colvar, 
           theta = 60, phi = 40)

## =======================================================================
## coloring on a surface
## =======================================================================

 XY <- mesh(x, y)
 ZZ <- XY$x*XY$y
 slice3D  (x, y, z, xs = XY$x, ys = XY$y, zs = ZZ, colvar = colvar, 
           lighting =  TRUE, lphi = 90, ltheta = 0)

## =======================================================================
## Specifying transparent colors
## =======================================================================

 par(mfrow = c(1, 1))
 x <- y <- z <- seq(-4, 4, by = 0.2)
 M <- mesh(x, y, z)

 R <- with (M, sqrt(x^2 + y^2 + z^2))
 p <- sin(2*R) /(R+1e-3)

\dontrun{
# This is very slow - alpha = 0.5 makes it transparent

 slice3D(x, y, z, colvar = p, col = jet.col(alpha = 0.5), 
         xs = 0, ys = c(-4, 0, 4), zs = NULL, d = 2) 
}

 slice3D(x, y, z, colvar = p, d = 2, theta = 60, border = "black",
         xs = c(-4, 0), ys = c(-4, 0, 4), zs = c(-4, 0))

## =======================================================================
## A section along a transect
## =======================================================================

 data(Oxsat)
 Ox <- Oxsat$val[,  Oxsat$lat > - 5 & Oxsat$lat < 5, ]
 slice3D(x = Oxsat$lon, z = -Oxsat$depth, y = 1:5, colvar = Ox, 
         ys = 1:5, zs = NULL, NAcol = "black", 
         expand = 0.4, theta = 45, phi = 45)

## =======================================================================
## isosurf3D example - rather slow
## =======================================================================

 par(mfrow = c(2, 2), mar  = c(2, 2, 2, 2))
 x <- y <- z <- seq(-2, 2, length.out = 15)
 xyz <- mesh(x, y, z)
 F <- with(xyz, log(x^2 + y^2 + z^2 + 
                10*(x^2 + y^2) * (y^2 + z^2) ^2))

# use shading for level = 1 - show triangulation with border
 isosurf3D(x, y, z, F, level = 1, shade = 0.9, 
           col = "yellow", border = "orange")

# lighting for level - 2
 isosurf3D(x, y, z, F, level = 2, lighting = TRUE,
           lphi = 0, ltheta = 0, col = "blue", shade = NA)  
 
# three levels, transparency added
 isosurf3D(x, y, z, F, level = seq(0, 4, by = 2), 
   col = c("red", "blue", "yellow"), 
   clab = "F", alpha = 0.2, theta = 0, lighting = TRUE)  

# transparency can also be added afterwards with plotdev()
\dontrun{
 isosurf3D(x, y, z, F, level = seq(0, 4, by = 2), 
   col = c("red", "blue", "yellow"), 
   shade = NA, plot = FALSE, clab = "F")  
 plotdev(lighting = TRUE, alpha = 0.2, theta = 0)
}
# use of creatisosurf
 iso <- createisosurf(x, y, z, F, level = 2)
 head(iso)
 triangle3D(iso, col = "green", shade = 0.3)

\dontrun{
 # higher resolution
  x <- y <- z <- seq(-2, 2, length.out = 50)
  xyz <- mesh(x, y, z)
  F <- with(xyz, log(x^2 + y^2 + z^2 + 
                10*(x^2 + y^2) * (y^2 + z^2) ^2))

# three levels
  isosurf3D(x, y, z, F, level = seq(0, 4, by = 2), 
    col = c("red", "blue", "yellow"), 
    shade = NA, plot = FALSE, clab = "F")  
  plotdev(lighting = TRUE, alpha = 0.2, theta = 0)
}

## =======================================================================
## voxel3D example
## =======================================================================

 par(mfrow = c(2, 2), mar  = c(2, 2, 2, 2))

# fast but needs high resolution grid
 x <- y <- z <- seq(-2, 2, length.out = 70)
 xyz <- mesh(x, y, z)
 F <- with(xyz, log(x^2 + y^2 + z^2 + 
                10*(x^2 + y^2) * (y^2 + z^2) ^2))

 voxel3D(x, y, z, F, level = 4, pch = ".", cex = 5)

## =======================================================================
## rotation 
## =======================================================================

 plotdev(theta = 45, phi = 0)
 plotdev(theta = 90, phi = 10)

# same using createvoxel -  more flexible for coloring
 vox <- createvoxel(x, y, z, F, level = 4)
 scatter3D(vox$x, vox$y, vox$z, colvar = vox$y, 
   bty = "g", colkey = FALSE)


## =======================================================================
## voxel3D to show hypox sites
## =======================================================================

 par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
 Hypox <- createvoxel(Oxsat$lon, Oxsat$lat, Oxsat$depth[1:19], 
                      Oxsat$val[,,1:19], level = 40, operator = "<")

 panel <- function(pmat) {  # an image at the bottom
   Nx <- length(Oxsat$lon)
   Ny <- length(Oxsat$lat)
   M <- mesh(Oxsat$lon, Oxsat$lat) 
   xy <- trans3D(pmat = pmat, x = as.vector(M$x), y = as.vector(M$y), 
        z = rep(-1000, length.out = Nx*Ny)) 
   x <- matrix(nrow = Nx, ncol = Ny, data = xy$x)
   y <- matrix(nrow = Nx, ncol = Ny, data = xy$y)
   Bat <- Oxsat$val[,,1]; Bat[!is.na(Bat)] <- 1
   image2D(x = x, y = y, z = Bat, NAcol = "black", col = "grey",
         add = TRUE, colkey = FALSE)
 }
   
 scatter3D(Hypox$x, Hypox$y, -Hypox$z, colvar = Hypox$cv, 
           panel.first = panel, pch = ".", bty = "b", 
           theta = 30, phi = 20, ticktype = "detailed",
           zlim = c(-1000,0), xlim = range(Oxsat$lon), 
           ylim = range(Oxsat$lat) )
           
# restore plotting parameters
 par(mfrow = pm)
 par(mar = pmar)
}
\references{
Lorensen, W.E. and Cline, H.E.,
Marching Cubes: a high resolution 3D surface reconstruction algorithm,
Computer Graphics, Vol. 21, No. 4, pp 163-169 (Proc. of SIGGRAPH), 1987.

Dai Feng, Luke Tierney, Computing and Displaying Isosurfaces in R, 
Journal of Statistical Software 28(1), 2008.
URL \url{http://www.jstatsoft.org/v28/i01/}.
}

\keyword{ hplot }

