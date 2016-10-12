library(plotrix)
library(RColorBrewer)
library(raster)
library(socorro)

## =========================
## chrono, elev, precip maps
## =========================

## set-up colors
rain.colors <- colorRampPalette(brewer.pal(9, 'YlGnBu'))
chrono.colors <- colorRampPalette(hsv(c(0.1, 0, 0.05), c(0.7, 0.9, 0.3), c(1, 0.8, 0.3)))
elev.colors <- colorRampPalette(gray(c(0, 0.1, 10^seq(-0.75, 0, length = 10))))


## get chrono and dimensions sites loaded
eval(parse(text=c('{', readLines('~/Dropbox/hawaiiDimensions/geoData/maps/sites_map.R', n=35), '}')))

age.unit <- rep('mya', length(hi.geo.poly$AGE_RANGE))
age.unit[grep('A.D.', hi.geo.poly$AGE_RANGE)] <- 'yr'
age.unit[grep('yr', hi.geo.poly$AGE_RANGE)] <- 'ya'

age.val <- as.character(hi.geo.poly$AGE_RANGE)
age.val <- gsub(' to ', '-', age.val)
age.val <- gsub('A.D.|[[:alpha:]]| |,|/|>', '', age.val)
age.val[age.val == ''] <- NA
age.val <- sapply(strsplit(age.val, '-'), function(x) mean(as.numeric(x)))
age.val[age.unit == 'ya'] <- age.val[age.unit == 'ya'] / 10^6
age.val[age.unit == 'yr'] <- (2015 - age.val[age.unit == 'yr']) / 10^6

## get precip
setwd('~/Dropbox/hawaiiDimensions/geodata/site_selection/StateRFGrids_mm2/staterf_mmann')
precip <- raster('w001001.adf')


## get elevation
setwd('~/Dropbox/hawaiiDimensions/geoData/site_selection/elrange_n83')
elev <- readOGR('.', 'elrange_n83')
elev <- spTransform(elev, CRS(proj4string(islands)))


## =========================
## plot maps and site layout
## =========================

setwd('~/Dropbox/Research/grants/macrosystems/figs')

jpeg(filename='fig_map.jpg', width=10, height=12, units='in', res=40)
close.screen(all.screens = TRUE)

split.screen(figs=matrix(c(0, 1, 0.6, 1,
                           0, 1, 0.3, 0.7,
                           0, 1, 0, 0.4), nrow = 3, byrow = TRUE), erase=FALSE)


## chrono age

par(mar=rep(0, 4), bg = 'transparent')
plot(islands, col = 'gray30')
# plot(hi.geo.poly[hi.geo.poly$age_cluster > 0, ],
#      col = chrono.colors(9)[hi.geo.poly$age_cluster[hi.geo.poly$age_cluster > 0]],
#      border = chrono.colors(9)[hi.geo.poly$age_cluster[hi.geo.poly$age_cluster > 0]],
#      add = TRUE)
# plot(islands, add = TRUE)
# 
# plot(sites, add=TRUE, pch=21, col='white', bg='black')

split.screen(figs=matrix(c(0, 0.5, 0.5, 0.8), nrow=1), erase=FALSE)
par(mar=c(2, 1, 0, 1) + 1, mgp=c(2,0.5,0), xpd=NA, bg = 'transparent')
plot(1, xlim=c(min(age.val, na.rm = TRUE), 6.4), ylim=c(0, 1.5), log='x',type='n',
     axes = FALSE, xlab='Substrate age (My)', ylab='', xaxs='i', yaxs='i')

a <- seq(par('usr')[1], par('usr')[2], length = 21)
rect(xleft = 10^a[-1], xright = 10^a[-21], ybottom = 0, ytop = 0.6, col = chrono.colors(20), border = NA)
rect(10^min(a), 0, 10^max(a), 0.6)

logAxis(1, labels=FALSE)
axis(side=1, at=c(10^(-4:0), 6))

close.screen(4)


## elevation

screen(2, new = FALSE)
par(mar=rep(0, 4), bg = 'transparent')
plot(islands, col = 'black')
col.elev <- elev.colors(length(unique(rowSums(elev@data[, c('LOWELEV', 'HIGHELEV')]))))
plot(elev, col = col.elev[as.numeric(as.factor(rowSums(elev@data[, c('LOWELEV', 'HIGHELEV')]) / 2))],
     border = col.elev[as.numeric(as.factor(rowSums(elev@data[, c('LOWELEV', 'HIGHELEV')]) / 2))],
     add = TRUE)
plot(islands)

plot(sites, add=TRUE, pch=21, col='white', bg='black')

split.screen(figs=matrix(c(0, 0.5, 0.5, 0.8), nrow=1), erase=FALSE)
par(mar=c(2, 1, 0, 1) + 1, mgp=c(2,0.5,0), xpd=NA, bg = 'transparent')
plot(1, xlim=c(0, max(elev$HIGHELEV) * 0.3048), ylim=c(0, 1.5),type='n',
     axes = FALSE, xlab='Elevation (m)', ylab='', xaxs='i', yaxs='i')

a <- seq(par('usr')[1], par('usr')[2], length = 21)
rect(xleft = a[-1], xright = a[-21], ybottom = 0, ytop = 0.6, col = elev.colors(20), border = NA)
rect(min(a), 0, max(a), 0.6)
axis(1)

close.screen(4)


## precip

screen(3, new = FALSE)
par(mar=rep(0, 4), bg = 'transparent')
plot(spTransform(islands, CRS(proj4string(precip))))
rasterImage(precip, add = TRUE, legend = FALSE)

split.screen(figs=matrix(c(0, 1, 0.6, 1,
                           0, 1, 0.3, 0.7,
                           0, 1, 0, 0.4), nrow = 3, byrow = TRUE), erase=FALSE)
screen(3, new = FALSE)
par(mar = rep(0, 4), bg = 'transparent')
plot(spTransform(islands, CRS(proj4string(precip))))

plot(spTransform(sites, CRS(proj4string(precip))), add=TRUE, pch=21, col='white', bg='black')

split.screen(figs=matrix(c(0, 0.5, 0.5, 0.8), nrow=1), erase=FALSE)
par(mar=c(2, 1, 0, 1) + 1, mgp=c(2,0.5,0), xpd=NA, bg = 'transparent')
plot(1, xlim=range(values(precip), na.rm = TRUE), ylim=c(0, 1.5),type='n',
     axes = FALSE, xlab='Substrate age (My)', ylab='', xaxs='i', yaxs='i')

a <- seq(par('usr')[1], par('usr')[2], length = 21)
rect(xleft = a[-1], xright = a[-21], ybottom = 0, ytop = 0.6, col = rain.colors(20), border = NA)
rect(min(a), 0, max(a), 0.6)
axis(1)

close.screen(4)

dev.off()
close.screen(all.screens = TRUE)




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

pdf('fig_map.pdf', width = 6, height = 4)

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

segments(x0 = gridMax*1.3 + (0:plotMax)*gridMax/(2*plotMax), y0 = gridMax/2, y1 = gridMax)
segments(x0 = gridMax*1.3, x1 = 1.8*gridMax, 
         y0 = gridMax/2 + (0:plotMax)*gridMax/(2*plotMax))
rect(quadsx*gridMax/(2*plotMax) + gridMax*1.3, quadsy*gridMax/(2*plotMax) + gridMax/2, 
     (quadsx + 1)*gridMax/(2*plotMax) + gridMax*1.3, (quadsy + 1)*gridMax/(2*plotMax) + gridMax/2,
     col = hsv(0.6, 0.5, 0.9))


arrows(x0 = gridMax*1.3, x1 = gridMax*1.8, y0 = 1.05*gridMax, angle = 90, length = 0.05, code = 3)
boxed.labels(gridMax*1.3 + (plotMax/2)*gridMax/(2*plotMax), 1.05*gridMax, labels = sprintf('%s m', grid),
             bg = 'white', border = FALSE)


polygon(c(quadsx[3]*gridMax/(2*plotMax) + gridMax*1.3, (quadsx[3]+1)*gridMax/(2*plotMax) + gridMax*1.3,
          gridMax*1.7, gridMax*1.4),
        c(quadsy[3]*gridMax/(2*plotMax) + gridMax/2, quadsy[3]*gridMax/(2*plotMax) + gridMax/2,
          gridMax*0.3, gridMax*0.3),
        col = hsv(0.6, 0.5, 0.9, alpha = 0.5), border = NA)

## env sampling sites
points((quadsx+0.5)*gridMax/(2*plotMax) + gridMax*1.3, (quadsy+0.5)*gridMax/(2*plotMax) + gridMax/2, 
       pch = 21, bg = hsv(0.11, 0.7), cex = 1.2)


## mibrobe plot

segments(x0 = gridMax*1.4, x1 = gridMax*1.7, y0 = (0:microMax)*0.3*gridMax/microMax)
segments(x0 = gridMax*1.4 + (0:microMax)*0.3*gridMax/microMax, y0 = 0, y1 = 0.3*gridMax)

arrows(x0 = gridMax*1.4, x1 = gridMax*1.7, y0 = -0.05*gridMax, angle = 90, length = 0.05, code = 3)
boxed.labels(gridMax*1.4 + (microMax/2)*0.3*gridMax/microMax, -0.05*gridMax, labels = '10 cm',
             bg = 'white', border = FALSE)

dev.off()
