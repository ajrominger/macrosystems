library(plotrix)
library(RColorBrewer)
library(socorro)

setwd('~/Dropbox/Research/grants/macrosystems/figs')

## ===========
## site layout
## ===========

plotsx <- c(0, 20, 60)
plotsy <- c(20, 60, 40)
quadsx <- c(2, 1, 3)
quadsy <- c(3, 1, 0)
grid <- 20
gridMax <- 100
plotMax <- 5
microMax <- 5

pdf('fig_site.pdf', width = 6, height = 4)

par(mar = rep(0.1, 4))
plot(1:gridMax, 1:gridMax, asp = 1, type = 'n', xlim = c(0, 1.8*gridMax), axes = FALSE)

## site

segments(x0 = seq(0, gridMax, by = grid), y0 = 0, y1 = gridMax)
segments(x0 = 0, x1 = gridMax, y0 = seq(0, gridMax, by = grid))
rect(plotsx, plotsy, plotsx + grid, plotsy + grid, col = hsv(0.43, 0.6, 0.7))

arrows(x0 = 0, x1 = gridMax, y0 = 1.05*gridMax, angle = 90, length = 0.05, code = 3)
boxed.labels(gridMax/2, 1.05*gridMax, labels = sprintf('%s m', gridMax),
             bg = 'white', border = FALSE)

polygon(c(plotsx[3]+grid, plotsx[3]+grid, 1.3*gridMax, 1.3*gridMax), 
        c(plotsy[3], plotsy[3]+grid, gridMax, gridMax/2),
        col = hsv(0.43, 0.6, 0.7, alpha = 0.5), border = NA)

## plot

rect(gridMax*1.3, gridMax/2, gridMax*1.8, gridMax, col = hsv(0.43, 0.6, 0.7, 0.8))

segments(x0 = gridMax*1.3 + (0:plotMax)*gridMax/(2*plotMax), y0 = gridMax/2, y1 = gridMax)
segments(x0 = gridMax*1.3, x1 = 1.8*gridMax, 
         y0 = gridMax/2 + (0:plotMax)*gridMax/(2*plotMax))
rect(quadsx*gridMax/(2*plotMax) + gridMax*1.3, quadsy*gridMax/(2*plotMax) + gridMax/2, 
     (quadsx + 1)*gridMax/(2*plotMax) + gridMax*1.3, (quadsy + 1)*gridMax/(2*plotMax) + gridMax/2,
     col = hsv(0, 0.3, 0.3))


arrows(x0 = gridMax*1.3, x1 = gridMax*1.8, y0 = 1.05*gridMax, angle = 90, length = 0.05, code = 3)
boxed.labels(gridMax*1.3 + (plotMax/2)*gridMax/(2*plotMax), 1.05*gridMax, labels = sprintf('%s m', grid),
             bg = 'white', border = FALSE)


polygon(c(quadsx[3]*gridMax/(2*plotMax) + gridMax*1.1, (quadsx[3]+1)*gridMax/(2*plotMax) + gridMax*1.1,
          gridMax*1.4, gridMax*1.1),
        c(quadsy[3]*gridMax/(2*plotMax) + gridMax/2, quadsy[3]*gridMax/(2*plotMax) + gridMax/2,
          gridMax*0.3, gridMax*0.3),
        col = hsv(0, 0.3, 0.3, 0.5), border = NA)

## env sampling sites
points((quadsx+0.5)*gridMax/(2*plotMax) + gridMax*1.3, (quadsy+0.5)*gridMax/(2*plotMax) + gridMax/2, 
       pch = 21, bg = hsv(0.11, 0.7), cex = 1.2)



## litter/soil plot

rect(gridMax*1.1, 0, gridMax*1.4, 0.3*gridMax, col = hsv(0, 0.3, 0.3, 0.8))

segments(x0 = gridMax*1.1, x1 = gridMax*1.4, y0 = (0:4)*0.3*gridMax/4)
segments(x0 = gridMax*1.1 + (0:4)*0.3*gridMax/4, y0 = 0, y1 = 0.3*gridMax)

arrows(x0 = gridMax*1.1, x1 = gridMax*1.4, y0 = -0.05*gridMax, angle = 90, length = 0.05, code = 3)
boxed.labels(gridMax*1.1 + (4/2)*0.3*gridMax/4, -0.05*gridMax, labels = '4 m',
             bg = 'white', border = FALSE)

polygon(x = c(gridMax*1.1 + 3.5*0.3*gridMax/4, gridMax*1.6, gridMax*1.6), 
        y = c(1.5*0.3*gridMax/4, 0.05*gridMax + c(0, microMax)*0.2*gridMax/microMax),
        col = hsv(0.6, 0.5, 0.9, alpha = 0.5), border = NA)

## mibrobe plot

rect(gridMax*1.6, 0.05*gridMax, gridMax*1.8, 0.25*gridMax, col = hsv(0.6, 0.5, 0.9, 0.8))

segments(x0 = gridMax*1.6, x1 = gridMax*1.8, y0 = 0.05*gridMax + (0:microMax)*0.2*gridMax/microMax)
segments(x0 = gridMax*1.6 + (0:microMax)*0.2*gridMax/microMax, y0 = 0.05*gridMax, y1 = 0.25*gridMax)

arrows(x0 = gridMax*1.6, x1 = gridMax*1.8, y0 = 0*gridMax, angle = 90, length = 0.05, code = 3)
boxed.labels(gridMax*1.6 + (microMax/2)*0.2*gridMax/microMax, 0*gridMax, labels = '10cm',
             bg = 'white', border = FALSE)

dev.off()
