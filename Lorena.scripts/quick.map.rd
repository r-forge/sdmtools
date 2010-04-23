\name{quick.map}
\Rdversion{1.1}
\alias{quick.map}

\title{ Quick Map }

\description{
\code{quick.map} creates and displays a gradient legend on a plot or image file. The 
place and size of the legend is defined by coordinates, previously identified.
}

\usage{
quick.map(sdm.asc, threshold, bkgd.col = 'grey',cols=heat.colors(100))
}
\arguments{
  \item{sdm.asc}{an object of class "asc" as defined in the adehabitat package}
  \item{threshold}{to set the threshold of the asc object, it will be }
  \item{bkgd.col}{to specify the background color}
  \item{cols}{a set of 2 or more colors to be used in the image and in the gradiente legend}
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

#put in the gradient scale
pnts = cbind(x =c(146.458, 146.688, 146.688, 146.458), y =c(-16.333, -16.333, -16.752,-16.752))

quick.map(tasc,0.08,bkgd.col = 'darkgrey')

Scalebar(x= 145.101, y=-19535, distance=20)

}
