library(jpeg)
library(raster)
library(rgeos)

setwd('~/Dropbox/Research/grants/macrosystems/figs')

## function to read in image and make polygon output
arthOutline <- function(f, maxCol = 100) {
    a <- readJPEG(f)
    
    r <- as.matrix(as.raster(a))
    cols <- unique(as.vector(r))
    
    r <- raster(matrix((r %in% cols[colSums(col2rgb(cols)) < maxCol]) * 1, nrow = nrow(r)))
    
    l <- rasterToContour(r)
    x <- l@lines[[1]]@Lines[[1]]@coords
    
    return(x)
}

## funciton to add arthropod to a plot by name
addArth <- function(arth, x, y, width, col) {
    a <- switch(arth,
                'fly' = arthOutline('fly.jpg'),
                'moth' = arthOutline('moth.jpg', 400),
                'beetle' = arthOutline('beetle.jpg'))
    yasp <- diff(range(par('usr')[3:4])) / diff(range(par('usr')[1:2]))
    a[, 2] <- a[, 2] * yasp
    
    asp <- width / diff(range(a[, 1]))
    a <- a * asp
    
    a[, 1] <- a[, 1] - mean(a[, 1]) + x
    a[, 2] <- a[, 2] - mean(a[, 2]) + y
    
    polygon(a, col = col, border = NA)
}

## function to add DNA image to plot
addDNA <- function(x, y, width, col) {
    xx <- seq(1-0.2, 4, length = 100) * pi
    y1 <- sin(xx) * 1.5
    y2 <- sin(xx - pi/1.25) * 1.5
    
    x0 <- seq(1, 3.8, by = 0.2) * pi
    ybar0 <- sin(x0) * 1.5
    ybar1 <- sin(x0 - pi/1.25) * 1.5
    
    asp <- width / diff(range(xx))
    xx <- xx * asp
    x0 <- x0 * asp
    
    yasp <- asp * diff(range(par('usr')[3:4])) / diff(range(par('usr')[1:2]))
    y1 <- y1 * yasp
    y2 <- y2 * yasp
    ybar0 <- ybar0 * yasp
    ybar1 <- ybar1 * yasp
    
    x0 <- x0 - mean(xx) + x
    xx <- xx - mean(xx) + x
    
    
    lines(xx, y1 + y, col = col, lwd = 3)
    lines(xx, y2 + y, col = col, lwd = 3)
    segments(x0 = x0, y0 = ybar0 + y, y1 = ybar1 + y, col = col)
}

