# Created by Easton R. White
# Last edited 24-Apr-2020
# Build figure 2 map for monitoring commentary piece (this code takes awhile)


library(KernSmooth)
library(fields)
library(PBSmapping)
library(RColorBrewer)
#library(INLA)
#library(sp)


load("figure2/wcann_processed.RData")
#head(dat)

#dat50 <- dat[sample(1:nrow(dat),size = round(0.5*nrow(dat))),]
#dat10 <- dat[sample(1:nrow(dat),size = round(0.1*nrow(dat))),]

minX = min(dat$LON)
maxX = max(dat$LON)
minY = min(dat$LAT)
maxY = max(dat$LAT)

# fit 2d kernel density estimate (from KernSmooth package)
#   takes ~1 minute
fit <- bkde2D(x=cbind(dat$LON,dat$LAT), 
              bandwidth=c(0.1,0.1),
              gridsize=c(2000,12000),
              range.x=list(c(minX,maxX),c(minY,maxY)), truncate=TRUE)

# load coastline from PBSmapping package
data(nepacLL) 
attr(nepacLL,"zone")="10" # tell it we're in zone 10

# define legend colors
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
spec200 <- rf(200)

# define 2-panel dimensions
dev.new(width=7.25, height=7)
layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(3.75,3.5), heights=c(7))

# make map of U.S. West Coast
plotMap(nepacLL, xlim=c(minX,maxX),ylim=c(minY,maxY),
        col='grey',main="",plt = c(0.03, 0.97, 0.08, 0.95),
        cex.axis=1.5, cex.lab=1.5)
#title("WCANN Effort",line=1)

# add 2d kernel density surface
image(fit$x1,fit$x2,fit$fhat, col = spec200, add=T)

# add back map of U.S. West Coast (on top)
lev = levels(as.factor(nepacLL$PID))
for(i in 1:length(lev)) {
  indx = which(nepacLL$PID == lev[i])
  polygon(nepacLL$X[indx], nepacLL$Y[indx], col = "grey")
}

# add legend color scale
minP <- min(fit$fhat, na.rm=T)
maxP <- max(fit$fhat, na.rm=T)
minP <- 0
maxP <- 1
image.plot(smallplot=c(.85,.88,0.08,0.95), col=spec200,
           zlim=c(round(minP,1),round(maxP,1)), legend.only=TRUE, 
           legend.shrink=0.3, lab.break=round(seq(minP,maxP,length.out=4),1))
