library(jpeg)
library(raster)
library(rgeos)
library(ape)

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
addArth <- function(arth, x, y, width, col, alpha = 1) {
    a <- switch(arth,
                'fly' = arthOutline('fly.jpg'),
                'moth' = arthOutline('moth.jpg', 400),
                'beetle' = arthOutline('beetle.jpg'))
    
    yasp <- diff(range(par('usr')[3:4])) / diff(range(par('usr')[1:2]))
    if(arth %in% c('fly', 'beetle')) yasp <- yasp * 1.5
    
    a[, 2] <- a[, 2] * yasp
    
    asp <- width / diff(range(a[, 1]))
    a <- a * asp
    
    ai <- a
    for(i in 1:length(x)) {
        ai[, 1] <- a[, 1] - mean(a[, 1]) + x[i]
        ai[, 2] <- a[, 2] - mean(a[, 2]) + y[i]
        
        polygon(ai, col = rgb(t(col2rgb(col)), maxColorValue = 255, alpha = alpha*255), 
                border = ifelse(alpha == 1, NA, col))
    }
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
    
    
    lines(xx, y1 + y, col = col, lwd = 2)
    lines(xx, y2 + y, col = col, lwd = 2)
    segments(x0 = x0, y0 = ybar0 + y, y1 = ybar1 + y, col = col)
}


cylinder <- function(x, y, width, col) {
    elip <- function(x, y, r, a, b) {
        cbind(cos(r)*a*b/sqrt((b*cos(r))^2+(a*sin(r))^2) + x, 
              sin(r)*a*b/sqrt((b*cos(r))^2+(a*sin(r))^2) + y)
    }
    
    width <- width
    
    rt <- seq(0, 2*pi, length = 100)
    rb <- seq(pi, 2*pi, length = 100)
    a <- width/2
    b <- a/2
    
    height <- width * 1.5
    
    xy <- rbind(cbind(x-a, y+height/2), elip(x, y-height/2, rb, a, b), cbind(x+a, y+height/2))
    xytop <- elip(x, y + height/2, rt, a, b)
    
    yasp <- diff(range(par('usr')[3:4])) / diff(range(par('usr')[1:2]))
    xy[, 2] <- xy[, 2] * yasp
    xytop[, 2] <- xytop[, 2] * yasp
    
    polygon(xy, col = col)
    polygon(xytop, col = col)
}


## metabarcoding top panel

pdf('subfig_metabTop.pdf', width = 8, height = 5)
layout(matrix(1:2, nrow = 1), widths = 5, 1)

xmax <- 15
ymax <- 5

par(mar = rep(0.1, 4))
plot(1, xlim = c(0, xmax), ylim = c(1, ymax), type = 'n', axes = FALSE)

## figure of arthropods going in

r <- -c(1/2, seq(3/8, 5/8, length = 2), seq(3/8, 5/8, length = 3), seq(3/8, 5/8, length = 4)) * pi
a <- c(0, rep(0.05*ymax, 2), rep(0.05*ymax*2, 3), rep(0.05*ymax*3, 4))
xy <- cbind(a*cos(r) + xmax/10, a*sin(r) + 0.9*ymax)
set.seed(123)
taxaXY <- sample(c(rep('beetle', 2), rep('fly', 3), rep('moth', 4)))

cylinder(6, 8, 3, 'gray')
addArth('fly', xy[taxaXY == 'fly', 1], xy[taxaXY == 'fly', 2], 2, col = hsv(1), alpha = 0.3)
addArth('moth', xy[taxaXY == 'moth', 1][1:2], xy[taxaXY == 'moth', 2][1:2], 3.5, col = hsv(0.85), alpha = 0.3)
addArth('moth', xy[taxaXY == 'moth', 1][3:4], xy[taxaXY == 'moth', 2][3:4], 3.5, col = hsv(0.77), alpha = 0.3)
addArth('beetle', xy[taxaXY == 'beetle', 1], xy[taxaXY == 'beetle', 2], 2.5, col = hsv(0.475), alpha = 0.3)



## figure of DNA
dnaY <- seq(0.25, 0.9, length = 7)*ymax

for(i in 1:7) {
    if(i < 3) {
        col <- 'gray85'
    } else if(i < 4) {
        col <- 'gray35'
    } else if(i < 5) {
        col <- 'gray20'
    } else {
        col <- 'gray60'
    }
    
    addDNA(0.8*xmax, dnaY[i], 3, col = col)
}

segments(x0 = xmax*0.91, x1 = par('usr')[2], y0 = rev(dnaY), y1 = c(5, 5, 5, 4, 3.6, 2, 2), lwd = 2)


## phylogenetic tree

tre <- read.tree(text = '(A:4,(beetle:3,(C:2,(fly:1,bfly:1):1):1):1);')
par(mar = c(0.1, 0, 0.1, 0.1))
plot(tre, direction = 'leftwards', show.tip.label = FALSE, edge.width = 3)
treXY <- do.call(cbind, get('last_plot.phylo', envir = .PlotPhyloEnv)[c('xx', 'yy')])[1:5, ]
segments(x0 = 0, y0 = 5:1, x1 = 0.5, col = hsv(seq(1, 0.3, length = 5)), lwd = 4)
segments(x0 = 0, x1 = 0.9, y0 = 3.6, lwd = 3, col = 'gray80')
segments(x0 = 0.9, y0 = 3.6, y1 = 4, lwd = 3, col = 'gray80')
segments(x0 = 0, x1 = 0.5, y0 = 3.6, lwd = 4, col = hsv(0.77))

dev.off()



## metabarcoding bottom panel

pdf('subfig_metabBottom.pdf', width = 5, height = 5)
par(cex = 1.2)
barplot(c(4, 3, 3, 1), ylab = 'Abundance')

par(xpd = NA)
addArth('fly', 0.7, -0.75, 0.75, col = hsv(1))
addArth('beetle', 0.7+1.2, -0.75, 0.75, col = hsv(0.475))
addArth('moth', 0.7+1.2*2, -0.7, 1.2, col = hsv(0.85))
addArth('moth', 0.7+1.2*3, -0.7, 1.2, col = hsv(0.77))
par(xpd = FALSE)
dev.off()


## metabarcoding hierrarchical small

pdf('subfig_metabMod.pdf')

layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE))

par(mar = c(3, 0, 0, 1) + 0.1, mgp = c(0, 0, 0), cex = 1.5)
curve(dnorm(x), from = -2, to = 2, xlab = expression(theta[1]), ylab = '', axes = FALSE)
polygon(seq(-2, 2, length = 100), dnorm(seq(-2, 2, length = 100)), col = 'gray80')
axis(1, at = c(-100, 100))

par(mar = c(3, 1, 0, 0) + 0.1, mgp = c(0, 0, 0), cex = 1.5)
curve(dgamma(x, 2, 4), from = 0, to = 2, xlab = expression(theta[2]), ylab = '', axes = FALSE)
polygon(seq(0, 2, length = 100), dgamma(seq(0, 2, length = 100), 2, 4), col = 'gray80')
axis(1, at = c(-100, 100))

par(mar = c(1, 4, 0, 4), mgp = c(0, 0, 0), cex = 1.5)
curve(dexp(x), from = 0, to = 4, xlab = 'n', ylab = '', axes = FALSE)
polygon(c(0, seq(0, 4, length = 100)), dexp(c(4, seq(0, 4, length = 100))), col = 'gray40')
box()

dev.off()