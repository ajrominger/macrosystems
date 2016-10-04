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
