\name{plot3D-package}
\alias{plot3D-package}
\alias{plot3D}
\docType{package}
\title{
  Plotting multi-dimensional data.
}
\description{
  Functions for visualising 2-D and 3-D data.
  
  Many of the functions are extensions of R's \link{persp} or \link{image} function.
  
  Other packages that provide visualisation of 3-D data (and which might be better suited) are:
  \code{rgl, scatterplot3D, misc3D}.
}

\author{
  Karline Soetaert
}
\note{
This package is dedicated to Carlo.
}
\references{
  \url{https://www.rforscience.com/oceanview.html}

  \url{https://www.rforscience.com/plot3d.html}
}
\seealso{
Functions that are based on the \link{persp} function:
 \itemize{
  \item \link{persp3D}: an extended version of persp.
  \item \link{ribbon3D}: a perspective plot as ribbons.
  \item \link{hist3D}: 3-D histograms.
  \item \link{scatter3D}, \link{points3D}, \link{lines3D}: colored points, lines, ... in 3-D. 
  \item \link{slice3D}, \link{slicecont3D}: slices from a full 3-D data set.
  \item \link{isosurf3D}: isosurfaces from a full 3-D data set as triangles.
  \item \link{voxel3D}: isosurfaces from a full 3-D data set as points.
  \item \link{surf3D}, \link{spheresurf3D}: 3-D shapes or surfaces.
  \item \link{arrows3D}: arrows in 3-D.
  \item \link{segments3D}: line segments in 3-D.
  \item \link{polygon3D}: 3-D polygons.
  \item \link{box3D}, \link{border3D}, \link{rect3D}: boxes and rectangles in 3-D.
  \item \link{text3D}: labels in 3-D.
 }
Functions defined on the \link{image} function:
 \itemize{
  \item \link{image2D}, for an image function to visualise 2-D or 3-D data.
  \item \link{ImageOcean}: an image of the ocean's bathymetry.
 }
Other plotting functions:
 \itemize{
  \item \link{contour2D}, for a contour function to visualise 2-D data and that have a color key.
  \item \link{scatter2D}: colored points, lines, ... in 2-D. 
  \item \link{text2D}, \link{arrows2D}, \link{segments2D}, \link{rect2D}, 
    \link{polygon2D} for other 2D functions that have a color key.
 }
Colors and colorkey:
 \itemize{
  \item \link{colkey}: adds a color legend.
  \item \link{jet.col}, \link{ramp.col}, \link{gg.col}, \code{alpha.col}: suitable colors, shade and lighting.
 }
Utility functions:
 \itemize{
  \item \link{mesh}: to generate rectangular (x, y) or (x, y, z) meshes.
 }
Data sets:
 \itemize{
  \item \link{Oxsat}: 3-D data set with the ocean's oxygen saturation values. 
  \item \link{Hypsometry}: 2-D data set with the worlds elevation 
     and ocean's bathymetry.
 }
}
\note{
Some of the functions based on \code{persp} will not work properly for all
values of \code{phi} (which turns the plots upside-down). 
This is because an assumption is made as to how the perspective plots are viewed.
}
\examples{
# run all examples
\dontrun{
 example(persp3D)  
 example(surf3D)  
 example(slice3D)
 example(scatter3D)
 example(segments3D)
 example(image2D)
 example(image3D)
 example(contour3D)
 example(colkey) 
 example(jet.col) 
 example(perspbox)
 example(mesh)
 example(trans3D)
 example(plot.plist)
 example(ImageOcean)
 example(Oxsat)
}
}
\keyword{ package }