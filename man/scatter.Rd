\name{Scatter plots}
\alias{scatter3D}
\alias{text3D}
\alias{points3D}
\alias{lines3D}
\alias{scatter2D}
\alias{points2D}
\alias{lines2D}
\alias{text2D}
\title{
 Colored scatter plots and text in 2-D and 3-D
}
\description{
  
  \code{scatter2D and scatter3D} plot a (2- or 3 dimensional) dataset with a color variable as points or lines.

  \code{text3D} plot a 3-D dataset with a color variable as text labels.

  \code{points3D} is shorthand for \code{\link{scatter3D}(..., type = "p")}

  \code{lines3D} is shorthand for \code{\link{scatter3D}(..., type = "l")}

  \code{points2D} is shorthand for \code{\link{scatter2D}(..., type = "p")}

  \code{lines2D} is shorthand for \code{\link{scatter2D}(..., type = "l")}

  The 2D functions \code{scatter2D} and \code{text2D} are included
  for their side effect of having a color key.

}
\usage{
scatter3D (x, y, z, ..., colvar = z, phi = 40, theta = 40,
           col = NULL, NAcol = "white",  
           colkey = NULL, panel.first = NULL, 
           clim = NULL, clab = NULL, 
           bty = "b", CI = NULL, surf = NULL, 
           add = FALSE, plot = TRUE)
          
text3D (x, y, z, labels, ..., 
        colvar = NULL, phi = 40, theta = 40, col = NULL, NAcol = "white",  
        colkey = NULL, panel.first = NULL, 
        clim = NULL, clab = NULL, 
        bty = "b", add = FALSE, plot = TRUE)

points3D (x, y, z, ...)

lines3D (x, y, z, ...)

scatter2D (x, y, ..., colvar = NULL, col = NULL, NAcol = "white", 
         colkey = NULL, clim = NULL, clab = NULL, 
         CI = NULL, add = FALSE, plot = TRUE)

text2D (x, y, labels, ..., colvar = NULL, 
         col = NULL, NAcol = "white", colkey = NULL, 
         clim = NULL, clab = NULL, add = FALSE, plot = TRUE) 
}

\arguments{
  \item{x, y, z }{Vectors with x, y and z-values of the points to be plotted. 
    They should be of equal length, and the same length as \code{colvar} (if present).
    }
  \item{colvar }{The variable used for coloring. For \code{scatter3D}, it need
    not be present, but if specified, it should be a vector of equal length as 
    (\code{x, y, z}).
    }
  \item{theta, phi }{the angles defining the viewing direction. 
    \code{theta} gives the azimuthal direction and \code{phi} the colatitude. see \link{persp}.
    }
  \item{col }{Color palette to be used for coloring the \code{colvar} variable. 
    If \code{col} is \code{NULL} and \code{colvar} is specified, 
    then a red-yellow-blue colorscheme (\link{jet.col}) will be used.
    If \code{col} is \code{NULL} and \code{colvar} is not specified, then 
    \code{col} will be "black".    
    }
  \item{NAcol }{Colors to be used for \code{colvar} values that are \code{NA}.
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
  \item{CI }{A \code{list} with parameters and values for the confidence 
    intervals or \code{NULL}.
    If a list it should contain at least the item \code{x}, \code{y} or \code{z} 
    (latter for \code{scatter3D}). These should be 2-columned matrices, defining the left/right intervals. 
    Other parameters should be one of (with defaults):
    \code{alen = 0.01, lty = par("lty"), lwd = par("lwd"), col = NULL},
    to set the length of the arrow head, the line type and width, and the color.
    If \code{col} is \code{NULL}, then the colors as specified by \code{colvar} are used.
    See examples.
    } 
  \item{panel.first }{A \code{function} to be evaluated after the plot axes are 
    set up but before any plotting takes place. 
    This can be useful for drawing background grids or scatterplot smooths. 
    The function should have as argument the transformation matrix, e.g. it should
    be defined as \code{function(pmat)}. See example of \link{persp3D} and last example of \link{voxel3D}.
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
  \item{bty }{The type of the box, the default draws only the back panels.
    Only effective if the \link{persp} 
    argument (\code{box}) equals \code{TRUE} (this is the default). See \link{perspbox}.
    Note: the \code{bty = "g", "b2", "bl"} can also be specified 
    for \code{scatter2D} (if \code{add = FALSE}).
    }
  \item{labels }{The text to be written. A vector of length equal to length of
    x, y, z.
    }
  \item{surf }{If not \code{NULL}, a list specifying a (fitted) surface to be added on 
    the scatterplot. 
    The list should include at least \code{x}, \code{y}, \code{z}, defining the surface, 
    and optional: \code{colvar, col, NAcol, border, facets, 
    lwd, resfac, clim, ltheta, lphi, shade, lighting, fit}. Note that the default is 
    that \code{colvar} is not specified which will set \code{colvar = z}.
    The argument \code{fit} should give the fitted z-values, in the same order as the 
    z-values of the scatter points, for instance produced by \code{predict}. 
    When present, this will produce droplines from points to the fitted surface.
    }
  \item{add }{Logical. If \code{TRUE}, then the points will be added to the current plot.
    If \code{FALSE} a new plot is started. 
    } 
  \item{plot }{Logical. If \code{TRUE} (default), a plot is created, 
    otherwise (for 3D plots) the viewing transformation matrix is returned (as invisible). 
    } 
  \item{\dots}{additional arguments passed to the plotting methods. 

    The following \link{persp} arguments can be specified: 
    \code{xlim, ylim, zlim, xlab, ylab, zlab, main, sub, r, d, 
    scale, expand, box, axes, nticks, ticktype}.      
    The arguments \code{xlim}, \code{ylim}, \code{zlim} only affect the axes for 3D plots.
    All objects will be plotted, including those that fall out of these ranges.
    To select objects only within the axis limits, use \link{plotdev}.

    In addition, the \link{perspbox} arguments
    \code{col.axis, col.panel, lwd.panel, col.grid, lwd.grid} can 
    also be given a value.

    \code{shade} and \code{lighting} arguments will have no effect.
    
    \code{alpha} can be given a value inbetween 0 and 1 to make colors transparent.
         
    For all functions, the arguments \code{lty, lwd} can be specified; \code{type}
    can be specified for all except \code{text3D}.
    
    In case \code{type = "p"} or \code{"b"}, then \code{pch, cex, bg} can also be specified.
    
    The arguments after \dots must be matched exactly.
    }
}

\value{
  Function \code{scatter3D} returns the viewing transformation matrix.
  See \link{trans3D}.
}
\note{
  For \code{scatter2D} and \code{scatter3D} the plottypes that are supported 
  are: \code{type = "p"},  \code{type = "l"}, \code{type = "h"},
   \code{type = "o"}. For \code{type = "b"},  \code{type = "o"} is used instead.
}
\seealso{
  \link{persp} for the function on which this implementation is based.

  \link{mesh}, \link{trans3D}, \code{slice3D}, for other examples of 
  \code{scatter2D} or \code{scatter3D}.

  \link{plotdev} for zooming, rescaling, rotating a plot.

  package \code{scatterplot3D} for an implementation of scatterplots that is 
  not based on \code{persp}.
}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}


\examples{
# save plotting parameters
 pm <- par("mfrow")

 
## =======================================================================
## A sphere 
## =======================================================================

 par(mfrow = c(1, 1))
 M  <- mesh(seq(0, 2*pi, length.out = 100), 
            seq(0,   pi, length.out = 100))
 u  <- M$x ; v  <- M$y

 x <- cos(u)*sin(v)
 y <- sin(u)*sin(v)
 z <- cos(v)

# full  panels of box are drawn (bty = "f")
 scatter3D(x, y, z, pch = ".", col = "red", 
           bty = "f", cex = 2, colkey = FALSE)

## =======================================================================
## Different types
## =======================================================================

 par (mfrow = c(2, 2))
 z <- seq(0, 10, 0.2)
 x <- cos(z)
 y <- sin(z)*z

# greyish background for the boxtype (bty = "g") 
 scatter3D(x, y, z, phi = 0, bty = "g",
           pch = 20, cex = 2, ticktype = "detailed")
# add another point
 scatter3D(x = 0, y = 0, z = 0, add = TRUE, colkey = FALSE, 
           pch = 18, cex = 3, col = "black")

# add text
 text3D(x = cos(1:10), y = (sin(1:10)*(1:10) - 1), 
        z = 1:10, colkey = FALSE, add = TRUE, 
        labels = LETTERS[1:10], col = c("black", "red"))

# line plot
 scatter3D(x, y, z, phi = 0, bty = "g", type = "l", 
           ticktype = "detailed", lwd = 4)

# points and lines
 scatter3D(x, y, z, phi = 0, bty = "g", type = "b", 
           ticktype = "detailed", pch = 20, 
           cex = c(0.5, 1, 1.5))

# vertical lines
 scatter3D(x, y, z, phi = 0, bty = "g",  type = "h", 
           ticktype = "detailed")

## =======================================================================
## With confidence interval
## =======================================================================

 x <- runif(20)
 y <- runif(20)
 z <- runif(20)

 par(mfrow = c(1, 1))
 CI <- list(z = matrix(nrow = length(x),
                       data = rep(0.05, 2*length(x))))

# greyish background for the boxtype (bty = "g")
 scatter3D(x, y, z, phi = 0, bty = "g", CI = CI,
   col = gg.col(100), pch = 18, cex = 2, ticktype = "detailed",
   xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1))

# add new set of points
 x <- runif(20)
 y <- runif(20)
 z <- runif(20)

 CI2 <- list(x = matrix(nrow = length(x),
                       data = rep(0.05, 2*length(x))),
             z = matrix(nrow = length(x),
                       data = rep(0.05, 2*length(x))))

 scatter3D(x, y, z, CI = CI2, add = TRUE, col = "red", pch = 16)

## =======================================================================
## With a surface
## =======================================================================

 par(mfrow = c(1, 1))
 
# surface = volcano
 M <- mesh(1:nrow(volcano), 1:ncol(volcano))

# 100 points above volcano 
 N  <- 100
 xs <- runif(N) * 87
 ys <- runif(N) * 61
 zs <- runif(N)*50 + 154
  
# scatter + surface
 scatter3D(xs, ys, zs, ticktype = "detailed", pch = 16, 
   bty = "f", xlim = c(1, 87), ylim = c(1,61), zlim = c(94, 215), 
   surf = list(x = M$x, y = M$y, z = volcano,  
               NAcol = "grey", shade = 0.1))
 
## =======================================================================
## A surface and CI
## =======================================================================

 par(mfrow = c(1, 1))
 M <- mesh(seq(0, 2*pi, length = 30), (1:30)/100)
 z <- with (M, sin(x) + y)
 
# points 'sampled'
 N <- 30
 xs <- runif(N) * 2*pi
 ys <- runif(N) * 0.3

 zs <- sin(xs) + ys + rnorm(N)*0.3
 
 CI <- list(z = matrix(nrow = length(xs), 
                       data = rep(0.3, 2*length(xs))),
            lwd = 3)
  
# facets = NA makes a transparent surface; borders are black
 scatter3D(xs, ys, zs, ticktype = "detailed", pch = 16, 
   xlim = c(0, 2*pi), ylim = c(0, 0.3), zlim = c(-1.5, 1.5), 
   CI = CI, theta = 20, phi = 30, cex = 2,
   surf = list(x = M$x, y = M$y, z = z, border = "black", facets = NA)
   )

## =======================================================================
## droplines till the fitted surface
## =======================================================================

 with (mtcars, {

  # linear regression
   fit <- lm(mpg ~ wt + disp)

  # predict values on regular xy grid
   wt.pred <- seq(1.5, 5.5, length.out = 30)
   disp.pred <- seq(71, 472, length.out = 30)
   xy <- expand.grid(wt = wt.pred, 
                     disp = disp.pred)

   mpg.pred <- matrix (nrow = 30, ncol = 30, 
      data = predict(fit, newdata = data.frame(xy), 
      interval = "prediction"))

# fitted points for droplines to surface
   fitpoints <- predict(fit) 

   scatter3D(z = mpg, x = wt, y = disp, pch = 18, cex = 2, 
      theta = 20, phi = 20, ticktype = "detailed",
      xlab = "wt", ylab = "disp", zlab = "mpg",  
      surf = list(x = wt.pred, y = disp.pred, z = mpg.pred,  
                  facets = NA, fit = fitpoints),
      main = "mtcars")
  
 })

## =======================================================================
## Two ways to make a scatter 3D of quakes data set
## =======================================================================

 par(mfrow = c(1, 1)) 
# first way, use vertical spikes (type = "h")
 with(quakes, scatter3D(x = long, y = lat, z = -depth, colvar = mag, 
      pch = 16, cex = 1.5, xlab = "longitude", ylab = "latitude", 
      zlab = "depth, km", clab = c("Richter","Magnitude"),
      main = "Earthquakes off Fiji", ticktype = "detailed", 
      type = "h", theta = 10, d = 2, 
      colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
      )

# second way: add dots on bottom and left panel
# before the scatters are drawn, 
# add small dots on basal plane and on the depth plane
 panelfirst <- function(pmat) {
    zmin <- min(-quakes$depth)
    XY <- trans3D(quakes$long, quakes$lat, 
                  z = rep(zmin, nrow(quakes)), pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = quakes$mag, pch = ".", 
            cex = 2, add = TRUE, colkey = FALSE)
 
    xmin <- min(quakes$long)
    XY <- trans3D(x = rep(xmin, nrow(quakes)), y = quakes$lat, 
                  z = -quakes$depth, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = quakes$mag, pch = ".", 
            cex = 2, add = TRUE, colkey = FALSE)
 }

 with(quakes, scatter3D(x = long, y = lat, z = -depth, colvar = mag, 
      pch = 16, cex = 1.5, xlab = "longitude", ylab = "latitude", 
      zlab = "depth, km", clab = c("Richter","Magnitude"),
      main = "Earthquakes off Fiji", ticktype = "detailed", 
      panel.first = panelfirst, theta = 10, d = 2, 
      colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
      )

## =======================================================================
## text3D and scatter3D
## =======================================================================

 with(USArrests, text3D(Murder, Assault, Rape, 
    colvar = UrbanPop, col = gg.col(100), theta = 60, phi = 20,
    xlab = "Murder", ylab = "Assault", zlab = "Rape", 
    main = "USA arrests", 
    labels = rownames(USArrests), cex = 0.6, 
    bty = "g", ticktype = "detailed", d = 2,
    clab = c("Urban","Pop"), adj = 0.5, font = 2))

 with(USArrests, scatter3D(Murder, Assault, Rape - 1, 
    colvar = UrbanPop, col = gg.col(100), 
    type = "h", pch = ".", add = TRUE))

## =======================================================================
## zoom near origin
## =======================================================================

# display axis ranges
 getplist()[c("xlim","ylim","zlim")] 

# choose suitable ranges
 plotdev(xlim = c(0, 10), ylim = c(40, 150), 
         zlim = c(7, 25))

## =======================================================================
## text3D to label x- and y axis
## =======================================================================

 par(mfrow = c(1, 1))
 hist3D (x = 1:5, y = 1:4, z = VADeaths,
        bty = "g", phi = 20,  theta = -60,
        xlab = "", ylab = "", zlab = "", main = "VADeaths",
        col = "#0072B2", border = "black", shade = 0.8,
        ticktype = "detailed", space = 0.15, d = 2, cex.axis = 1e-9)

 text3D(x = 1:5, y = rep(0.5, 5), z = rep(3, 5),
       labels = rownames(VADeaths),
       add = TRUE, adj = 0)
 text3D(x = rep(1, 4),   y = 1:4, z = rep(0, 4),
       labels  = colnames(VADeaths),
       add = TRUE, adj = 1)

## =======================================================================
## Scatter2D; bty can also be set = to one of the perspbox alernatives
## =======================================================================

 par(mfrow = c(2, 2))
 x <- seq(0, 2*pi, length.out = 30)

 scatter2D(x, sin(x), colvar = cos(x), pch = 16, 
         ylab = "sin", clab = "cos", cex = 1.5)
# other box types:
 scatter2D(x, sin(x), colvar = cos(x), type = "l", lwd = 4, bty = "g")
 scatter2D(x, sin(x), colvar = cos(x), type = "b", lwd = 2, bty = "b2")
# transparent colors and spikes
 scatter2D(x, sin(x), colvar = cos(x), type = "h", lwd = 4, alpha = 0.5)
  
## =======================================================================
## mesh examples and scatter2D
## =======================================================================

 par(mfrow = c(1, 2))
 x <- seq(-1, 1, by = 0.1)
 y <- seq(-2, 2, by = 0.2)

 grid <- mesh(x, y)
 z    <- with(grid, cos(x) * sin(y))
 image2D(z, x = x, y = y)
 points(grid)
 scatter2D(grid$x, grid$y, colvar = z, pch = 20, cex = 2)

## =======================================================================
## scatter plot with confidence intervals
## =======================================================================

 par(mfrow = c(2, 2))
 x  <- sort(rnorm(10)) 
 y  <- runif(10)
 cv <- sqrt(x^2 + y^2)

 CI <- list(lwd = 2)
 CI$x <- matrix (nrow = length(x), data = c(rep(0.25, 2*length(x))))
 scatter2D(x, y, colvar = cv, pch = 16, cex = 2, CI = CI)
 scatter2D(x, y, colvar = cv, pch = 16, cex = 2, CI = CI, type = "b")

 CI$y <- matrix (nrow = length(x), data = c(rep(0.05, 2*length(x))))
 CI$col <- "black"
 scatter2D(x, y, colvar = cv, pch = 16, cex = 2, CI = CI)

 CI$y[c(2,4,8,10), ] <- NA  # Some points have no CI
 CI$x[c(2,4,8,10), ] <- NA  # Some points have no CI
 CI$alen <- 0.02            # increase arrow head
 scatter2D(x, y, colvar = cv, pch = 16, cex = 2, CI = CI)

## =======================================================================
## Scatter on an image
## =======================================================================
 
 par(mfrow = c(1, 1))
# image of oxygen saturation
 oxlim <- range(Oxsat$val[,,1], na.rm  = TRUE) 
 image2D(z = Oxsat$val[,,1], x = Oxsat$lon, y = Oxsat$lat,
       contour = TRUE, 
       xlab = "longitude", ylab = "latitude", 
       main = "Oxygen saturation", zlim = oxlim, clab = "\%")

# (imaginary) measurements at 5 sites
 lon   <- c( 11.2,   6.0, 0.9,  -4, -8.8)
 lat   <- c(-19.7,-14.45,-9.1,-3.8, -1.5)
 O2sat <- c(   90,    95,  92,  85,  100)
               
# add to image; use same zrange; avoid adding  a color key
 scatter2D(colvar = O2sat, x = lon, y = lat, clim = oxlim, pch = 16,
         add = TRUE, cex = 2, colkey = FALSE)

## =======================================================================
## Scatter on a contourplot
## =======================================================================

 par(mfrow = c(1, 1))

# room for colorkey by setting colkey = list(plot = FALSE)

# contour plot of the ocean's bathymetry
 Depth <- Hypsometry$z
 Depth[Depth > 0] <- NA
 contour2D(z = Depth, x = Hypsometry$x, y = Hypsometry$y, 
       xlab = "longitude", ylab = "latitude", 
       col = "black", NAcol = "grey", levels = seq(-6000, 0, by = 2000),
       main = "Oxygen saturation along ship track", 
       colkey = list(plot = FALSE))

# add data to image; with  a color key
 scatter2D(colvar = O2sat, x = lon, y = lat, pch = 16,
         add = TRUE, cex = 2, clab = "\%")

## =======================================================================
## scatter2D for time-series plots
## =======================================================================

# Plotting sunspot 'anomalies'
sunspot <- data.frame(year = time(sunspot.month), 
  anom = sunspot.month - mean(sunspot.month))

# long-term moving average of anomaly
ff <- 100
sunspot$ma <- filter(sunspot$anom, rep(1/ff, ff), sides = 2)

with (sunspot, lines2D(year, anom, 
  colvar = anom > 0, 
  col = c("pink", "lightblue"),
  main = "sunspot anomaly", type = "h", 
  colkey = FALSE, las = 1, xlab = "year", ylab = ""))
lines2D(sunspot$year, sunspot$ma, add = TRUE)  

# The same
#with (sunspot, plot(year, anom, 
#  col = c("pink", "lightblue")[(anom > 0) + 1],
#  main = "sunspot", type = "h", las = 1))

# but this does not work due to NAs...  
# lines(sunspot$year, sunspot$ma)  

## =======================================================================
## text2D
## =======================================================================

 with(USArrests, text2D(x = Murder, y = Assault + 5, colvar = Rape, 
     xlab = "Murder", ylab = "Assault", clab = "Rape", 
     main = "USA arrests", labels = rownames(USArrests), cex = 0.6, 
     adj = 0.5, font = 2))

 with(USArrests, scatter2D(x = Murder, y = Assault, colvar = Rape, 
     pch = 16, add = TRUE, colkey = FALSE))

# reset plotting parameters
 par(mfrow = pm)
}
\keyword{ hplot }

