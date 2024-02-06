\name{3-D data set}
\docType{data}
\alias{Oxsat}
\title{
  Yearly averaged oxygen saturation from the NODC World Ocean
  Atlas 2005.
}

\description{
  Percentage Oxygen Saturation from the NODC World Ocean Atlas 2005 (WOA05). 
  
  The values are gridded in 2dg * 2dg longitude - latitude sets, 
  and there are 33 depth intervals. 
}

\usage{
data(Oxsat)
}

\format{
 list with 
  \itemize{
   \item \code{lon}, the longitude (dg E), at 2 dg resolution, 180 values. 
   \item \code{lat}, the latitude (dg N), at 2 dg resolution, 90 values.
   \item \code{depth}, the water depth (m), 33 values.
   \item \code{val}, the saturation value (\%).   
    \code{val} is an array of dimension (180, 90, 33), (lon, lat, depth).
   \item \code{name}, the long name of the variable.
   \item \code{units}, the units of measurement.
  }
}
\details{
  The ``objectively analyzed climatology'' has been used to extract these data.

  The original data were averaged over the 4 seasons, and converted to half 
  the resolution for latitude and longitude.
  The longitude was converted to the European view, i.e. the original data 
  from (0, 360) was changed to (-180, 180).
}
\author{
  Karline Soetaert <karline.soetaert@nioz.nl>
}

\examples{
# save plotting parameters
 pm <- par("mfrow")

## ========================================================================
## plot all surface data
## ========================================================================

 par(mfrow = c(1, 1))
 image2D(z = Oxsat$val[ , , 1], x = Oxsat$lon, y = Oxsat$lat,
       main = "surface oxygen saturation (\%) for 2005")

## ========================================================================
## plot a selection of latitude-depth profiles; input is an array
## ========================================================================

 lon <- Oxsat$lon
 image2D (z = Oxsat$val, margin = c(2, 3), x = Oxsat$lat, 
        y = Oxsat$depth, subset = (lon > 18 & lon < 23),
        ylim = c(5500, 0), NAcol = "black", zlim = c(0, 110),
        xlab = "latitude", ylab = "depth, m")

 ImageOcean()
 abline ( v = lon[lon > 18 & lon < 23])

## ========================================================================
## plot with slices
## ========================================================================

 par(mfrow = c(1, 1))
 ii <- which (Oxsat$lon > -90 & Oxsat$lon < 90)
 jj <- which (Oxsat$lat > 0 & Oxsat$lat < 90)

 xs <- Oxsat$lon[ii[length(ii)]]  # E boundary
 ys <- Oxsat$lat[jj[1]]           # S boundary

 slice3D(colvar = Oxsat$val[ii,jj,], x = Oxsat$lon[ii],  
        y = Oxsat$lat[jj], z = -Oxsat$depth,
        NAcol = "black", xs = xs, ys = ys, zs = 0, 
        theta = 35, phi = 50, colkey = list(length = 0.5),
        expand = 0.5, ticktype = "detailed",
        clab = "\%", main = "Oxygen saturation", 
        xlab = "longitude", ylab = "latitude", zlab = "depth")

# restore plotting parameters
 par(mfrow = pm)
}

\references{
  \url{https://www.nodc.noaa.gov/OC5/WOA05/woa05nc.html} 
   
  Originally made available by CSIRO:
  
  Mark A. Collier and Paul J. Durack, 2005. CSIRO netCDF version of the
   NODC World Ocean Atlas 2005. CSIRO Marine and Atmospheric Research Paper 015.
   December 2006
}

\seealso{
  \link{image2D} for plotting.
}

\keyword{datasets}

