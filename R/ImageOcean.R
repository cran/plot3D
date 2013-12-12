## =============================================================================
## Plot Ocean bathymetry
## =============================================================================

ImageOcean <- function(...) {
  dots <- list(...)
  if (is.null(dots$xlab)) 
    dots$xlab  <- "longitude"
  if (is.null(dots$ylab)) 
    dots$ylab  <- "latitude"
  if (is.null(dots$clab)) 
    dots$clab  <- "depth, m"
  if (is.null(dots$NAcol)) 
    dots$NAcol  <- "black"
  zz       <- Hypsometry$z
  zz[zz>0] <- NA
  
  do.call("image2D", c(alist(zz, x = Hypsometry$x, y = Hypsometry$y), dots))
}

