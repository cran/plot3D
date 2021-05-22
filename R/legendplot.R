## =============================================================================
## Create a color key 
## =============================================================================

createKey <- function(x, clim = NULL, col = NULL, NAcol = "black")  {
  if (is.null(col))
    col <- jet.col(100)
  if (is.null(clim))
    clim <- range(x)
  else
    x[x > max(clim) | x < min(clim)] <- NA 
  cmin   <- clim[1]
  crange <- diff(clim)
  N      <- length(col) -1
  Col <- col[1 + trunc((x - cmin)/crange*1.00000000001*N)]
  Col[is.na(x)] <- NAcol
  Col
}

## =============================================================================
## wrapper around plotting functions that includes  a legend 
## =============================================================================

drawlegend <- function (legend, New = TRUE, pm = NULL) {

  if (!legend$plot) return()

  if (New) 
    par(new = TRUE)
  usr <- par("usr")
  Plt <- par(plt = legend$parleg)      # legend parameters
  
  legendargs <- formalArgs("legend")
  colkeypar <- legend[names(legend)%in%legendargs]

  if (is.null(colkeypar$legend))
    colkeypar$legend <- ""
  if (is.null(colkeypar$x))
    colkeypar$x <- "center"
    
  if (! is.null(pm)) 
    pmar <- par(mar = pm)
  plot(0, type = "n", xlim = c(0,1), ylim = c(0,1), 
       axes = FALSE, frame.plot = FALSE, 
       yaxs = "i", xaxs = "i", xlab = "", ylab = "")
  do.call("legend", colkeypar)

  par(plt = Plt)
  par(usr = usr)

  if (New) 
    par(new = FALSE)  

  if (! is.null(pm)) 
     par(mar = pmar)

}

## =============================================================================
## Outer margins set with oma, e.g. for pairs plots
## =============================================================================

omapairs <- function(arglist, legend, legend.side = 4, oma = NULL, X = NULL, FAC = 50){

  legend$plot <- FALSE
  if (is.null(X))
     X <- do.call("legend", legend)$rect
  if (is.null(oma))
    oma <- c(4, 4, if (!is.null(arglist$main)) 6 else 4, 4)
  
  pin <- par("pin")
  dh <- X$h*FAC*pin[2]/4.535
  dw <- X$w*FAC*pin[1]/5.135
  if (legend.side == 1) {
    oma <- oma + c(dh, 0, 0, 0)
  } else if (legend.side == 2) {
    oma <- oma + c(0, dw, 0, 0)
  } else if (legend.side == 3) {
    oma <- oma + c(0, 0, dh, 0)
  } else if (legend.side == 4) {
    oma <- oma + c(0, 0, 0, dw)
  }
  oma
}

## =============================================================================
## Size of the legend and the main plot
## =============================================================================

legendsize <- function(legend, legend.side = 4, parplt = par("plt"), cex = 1) { 
    legendplot <- legend$plot
    legend$plot <- FALSE
    X      <- do.call("legend", legend)$rect
    cexplt <- cex
    dw     <- X$w
    dh     <- X$h
    dd     <- 0.02
    dd2    <- 0.01 #0.12
    parleg <- parplt

    if (legend.side == 1) {
      parplt <- parplt + c(0, 0, dh + 0.1 * cexplt[1], 0)
      parleg[4] <- parplt[3] - dd2*cexplt[2]
    } else if (legend.side == 2) {
      parplt <- parplt + c(dw  + 0.08*cexplt[1], 0, 0, 0)
      parleg[2] <- parplt[1] - dd2*cexplt[2]
    } else if (legend.side == 3) {
      parplt <- parplt - c(0, 0, 0, dh + 0.05*cexplt[1])
      parleg[3] <- parplt[4] + dd2*cexplt[2]
    } else if (legend.side == 4) {
      parplt <- parplt - c(0, dw + 0.02*cexplt[1], 0, 0)
      parleg[1] <- parplt[2] + dd2*cexplt[2]
    } 
    legend$plot   <- ifelse(is.null(legendplot), TRUE, legendplot)
    legend$parleg <- check.plt(parleg)
    legend$parplt <- check.plt(parplt) 
    return(legend)      
}  

## =============================================================================
## main functions for plotting with a legend
## =============================================================================

legendplot <- function(..., legend = list(), legend.side = 4, 
                      legend.cex = 1, legend.pars = NULL)
  legend.plt( ..., method = "plot", legend = legend, legend.side = legend.side, 
             legend.cex = legend.cex, legend.pars = legend.pars)
  
legendmatplot <- function(..., legend = list(), legend.side = 4, 
                         legend.cex = 1, legend.pars = NULL)
  legend.plt(..., method = "matplot", legend = legend, legend.side = legend.side, 
                legend.cex = legend.cex, legend.pars = legend.pars)

legendhist <- function(..., legend = list(), legend.side = 4, 
                         legend.cex = 1, legend.pars = NULL)
  legend.plt(..., method = "hist", legend = legend, legend.side = legend.side, 
                legend.cex = legend.cex, legend.pars = legend.pars)

legendpairs <- function(..., legend = list(), legend.side = 4, 
                        legend.cex = 1, legend.pars = NULL)
  legend.oma(..., method = "pairs", legend = legend, legend.side = legend.side, 
             legend.cex = legend.cex, legend.pars = legend.pars)

legend.plt <- function(method = "plot", ..., #add = FALSE, 
   legend = list(), legend.side = 4, legend.cex = 1, legend.pars = NULL)  {

   if (method == "pairs"){
    P <- legend.oma(..., method = "pairs", legend = legend, legend.side = legend.side, 
                legend.cex = legend.cex, legend.pars = legend.pars)
    return(invisible(P))
   }
  
   if (! is.list(legend)) 
    legend <- list(legend = legend)
   arglist    <- as.list(match.call(expand.dots = TRUE))
   arglist[1] <- NULL
   arglist[formalArgs(legend.plt)] <- NULL
   if (length(inot <- which (names(arglist) == ""))) {
     formalnames <- names(formals(method))
     names(arglist)[inot] <- formalnames[inot]
   }
   
   plot.new()    
   par(new = TRUE)
   pltori <- par("plt")  
   
   # if legend is only element, and this is unnamed, then it is the legend labels
   if (length(inot <- which (names(legend) == ""))) {
     if (length(inot) > 1)
       stop(" 'legend' cannot have more than one unnamed argument") 
     names(legend)[inot] <- "legend"
   }
   if (is.null(legend$legend))
    legend$legend <- ""
   if (is.null(legend$x)) {
    if      (legend.side == 1) legend$x <- "bottom"
    else if (legend.side == 2) legend$x <- "left"
    else if (legend.side == 3) legend$x <- "top"
    else if (legend.side == 4) legend$x <- "right"
    else legend$x <- "center"
   }

    # normal position of legend, as in the default method
   if (legend.side == 0) {
     do.call(method, arglist) 
     do.call("legend", legend)
     return(invisible(list(plt.legend = pltori, plt.main = pltori)))
   }

  # plotting parameters for the legend and main plot
   cexplt <- rep(legend.cex, length.out = 2)
   parplt <- pltori
   parleg <- pltori

 # determine size of the legend
  if (is.null(legend.pars)) {
    legend <- legendsize(legend, legend.side = legend.side, parplt = parplt, cex = cexplt)
  } else {
    legend$parleg <- check.plt(legend.pars$plt.legend)
    legend$parplt <- check.plt(legend.pars$plt.main)
    if(is.null(legend$plot)) legend$plot <- TRUE
  }
   
  drawlegend(legend, New = FALSE)#, pm = c(2,2,2,2))  

  par(plt = legend$parplt, new = TRUE)  
  do.call(method, arglist) 

  par(mar = par("mar"))
  invisible(list(plt.legend = legend$parleg, plt.main = legend$parplt))
}

# a legend while setting the outer margins

legend.oma <- function(method = "pairs", ..., #add = FALSE, 
   legend = list(), legend.side = 4, legend.cex = 1, legend.pars = NULL)  {

   if (! is.list(legend)) 
     legend <- list(legend = legend)
   
   arglist    <- as.list(match.call(expand.dots = TRUE))
   arglist[1] <- NULL
   arglist[formalArgs(legend.oma)] <- NULL
   if (length(inot <- which (names(arglist) == ""))) {
     formalnames <- names(formals(method))
     names(arglist)[inot] <- formalnames[inot]
   }
   
   cexplt <- rep(legend.cex, length.out = 2)
   
   # if legend is only one element, and this is unnamed, then it is the legend labels
   if (length(inot <- which (names(legend) == ""))) {
     if (length(inot) > 1)
       stop(" 'legend' cannot have more than one unnamed argument") 
     names(legend)[inot] <- "legend"
   }
   if (is.null(legend$legend))
    legend$legend <- ""
   if (is.null(legend$x)){
    if (legend.side == 1) legend$x <- "bottom"
    if (legend.side == 2) legend$x <- "left"
    if (legend.side == 3) legend$x <- "top"
    if (legend.side == 4) legend$x <- "right"
   }
   plot.new()

  # determine size of the legend
  oma <- par("oma") 

  if (! is.null(legend.pars$oma))
    arglist$oma <- legend.pars$oma
  else if (is.null(arglist$oma)) 
    arglist$oma <- omapairs(arglist, legend, legend.side)

  par (oma = arglist$oma) 

# Note: for unknown reason I need to call plot.new twice...
  plot.new()
  do.call(method, arglist)

#  par(opar)  
  par(new = TRUE, oma = oma, mfrow = c(1,1))         
  plot.new() 
  if (is.null(legend$plot)) legend$plot <- TRUE  
  drawlegend(legend, New = FALSE, pm = c(2,2,2,2))  
  par(oma = oma)
  return(invisible(list(oma = arglist$oma)))
  }

## =============================================================================
## wrapper around plotting functions that includes  a legend 
## =============================================================================
colorkeyplot <- function(..., colorkey = list(), colorkey.side = 4)
  colorkey.plt (method = "plot", ..., 
                colorkey = colorkey, colorkey.side = colorkey.side)  

colorkeymatplot <- function(..., colorkey = list(), colorkey.side = 4)
  colorkey.plt (method = "matplot", ..., # add = FALSE, 
                colorkey = colorkey, colorkey.side = colorkey.side)  

colorkeyhist <- function(..., colorkey = list(), colorkey.side = 4)
  colorkey.plt (method = "hist", ..., 
                colorkey = colorkey, colorkey.side = colorkey.side)  

colorkeypairs <- function(..., colorkey = list(), colorkey.side = 4)
  colorkey.oma (method = "pairs", ..., 
                colorkey = colorkey, colorkey.side = colorkey.side)  

colorkey.plt <- function(method = "plot", ..., # add = FALSE, 
   colorkey = list(), colorkey.side = 4)  {

  arglist <- as.list(match.call(expand.dots=TRUE))
   
  arglist[formalArgs(colorkey.plt)] <- NULL
  arglist[1] <- NULL

  if (length(inot <- which (names(arglist) == ""))) {
     formalnames <- names(formals(method))
     names(arglist)[inot] <- formalnames[inot]
  }
  plot.new()        
  par(new = TRUE)
  pltori <- par("plt")  
  
  ck <- colorkey
  ck$clim <- ck$clab <- ck$col <- ck$breaks <- ck$clog <- NULL
  colkey <- check.colkey(ck)
  colkey$plot <- TRUE
  colkey$side <- colorkey.side
  
  col    <- colorkey$col
  breaks <- colorkey$breaks
  if (is.null(col))
    if (is.null(breaks))
      col <- jet.col(100)
    else
      col <- jet.col(length(breaks) - 1)
 # breaks <- check.breaks(breaks, col)
  if (! is.null(breaks))
    clim <- range(breaks)
  else
    clim <- colorkey$clim
  clog <- colorkey$clog
  if (is.null(clog)) clog <- FALSE  
 # The   
  drawcolkey(colkey, col = col, clim = clim, clab = colorkey$clab, 
             clog = clog, New = FALSE)  

  par(plt = colkey$parplt, new = TRUE)  
  do.call(method, arglist) 

  par(mar = par("mar"))
  invisible(list(plt.legend = colkey$parleg, plt.main = colkey$parplt))
}


# a color key while adjusting the outer margin

colorkey.oma <- function(method = "pairs", ..., # add = FALSE, 
                         colorkey = list(), colorkey.side = 4)  {
  
  arglist <- as.list(match.call(expand.dots=TRUE))
  
  arglist[formalArgs(colorkey.oma)] <- NULL
  arglist[1] <- NULL
  
  if (length(inot <- which (names(arglist) == ""))) {
    formalnames <- names(formals(method))
    names(arglist)[inot] <- formalnames[inot]
  }
  plot.new() 
  oma <- par("oma")

  colorkey$side <- colorkey.side
  ck <- colorkey
  ck$clim <- ck$clab <- ck$col <- ck$breaks <- ck$clog <- NULL
  
  colkey <- check.colkey(ck)  # settings of the color key
  colkey$plot <- TRUE
  colkey$side <- colorkey.side
  
  col    <- colorkey$col
  breaks <- colorkey$breaks
  if (is.null(col))
    if (is.null(breaks))
      col <- jet.col(100)
    else
      col <- jet.col(length(breaks) - 1)
  
  # breaks <- check.breaks(breaks, col)
  if (! is.null(breaks))
    clim <- range(breaks)
  else
    clim <- colorkey$clim
  clog <- colorkey$clog
  if (is.null(clog)) clog <- FALSE  

#  par(new = TRUE)
  CP <- colkey$parleg #  pars of the colorkey in plt units

  if (is.null(arglist$oma)) 
    arglist$oma <- omapairs(arglist, colkey, colorkey.side, 
      X = list(h = diff(CP[3:4]), w = diff(CP[1:2])), FAC = 150)

  par (oma = arglist$oma) 
  plot.new()
  
  do.call(method, arglist) 
  
  par(new = TRUE, mfrow = c(1,1), oma = oma, mar = c(2,1,2,2))
  plot.new()
  drawcolkey(colkey, col = col, clim = clim, clab = colorkey$clab, 
             clog = clog, New = FALSE)  
  par(mar = par("mar"), oma = oma)
  invisible(list(oma = arglist$oma))
}

