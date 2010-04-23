\name{legend.gradient}
\Rdversion{1.1}
\alias{legend.gradient}

\title{ Legend Gradient }

\description{
\code{legend.gradient} creates and displays a gradient legend on a plot or image file. The 
place and size of the legend is defined by coordinates, previously identified.
}

\usage{
legend.gradient(pnts,cols=heat.colors(100),limits=c(0,1), title='Legend', ...)
}
\arguments{
  \item{pnts}{x and y coordinates of the gradient location in the plot}
  \item{cols}{a set of 2 or more colors used in the image, to create the gradient}
  \item{limits}{to specify the min and max values of the gradient in the legend}
  \item{title}{to specify the title of the legend}
  \item{...}{other graphical parameters defined by image() or plot()}
}

\details{

}

\value{
nothing is returned, a gradient legend is added to a plot or a image.
}

\author{Lorena Falconi \email{lorefalconi@gmail.com}}

\examples{

tasc = read.asc.gz("D:/Lorena/R Package (fragmentation)/rf.6kybp.asc.gz")

# Create a color ramp
colormap=c("grey","yellow","yellow2","yellowgreen","goldenrod3","firebrick")

#create an image
image(tasc,col=colormap,zlim=c(0,1), axes=F, xlab="", ylab="", ann=FALSE)

#put in the gradient scale
pnts = cbind(x =c(146.458, 146.688, 146.688, 146.458), y =c(-16.333, -16.333, -16.752,-16.752))

#create the scale legend
legend.gradient(pnts,colormap,c("Low","High"))

#create the Scalebar
Scalebar(x= 145.101, y=-19.535, distance=1)

}
