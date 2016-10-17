library(meteR)

setwd('~/Dropbox/Research/grants/macrosystems/figs')

ipd <- ipd(meteESF(S0 = 100, N0 = 10000, E0 = 50000))
sad <- sad(meteESF(S0 = 100, N0 = 10000, E0 = 50000))

pdf('subfig_ipd.pdf', width = 2, height = 2)
par(mar = c(2, 2, 0, 0), mgp = c(0.5, 0, 0))
plot(ipd$d(seq(1, 5, length = 50)), type = 'l', xlab = 'Metabolic rate', ylab = 'Probability',
     axes = FALSE, frame.plot = TRUE, lwd = 4, col = hsv(0.42, 0.6, 0.8))
dev.off()

pdf('subfig_sad.pdf', width = 2, height = 2)
par(mar = c(2, 2, 0, 0), mgp = c(0.5, 0, 0))
plot(sad$d(1:50), log = 'y', type = 'l', xlab = 'Abundance', ylab = 'Probability',
     axes = FALSE, frame.plot = TRUE, lwd = 4, col = hsv(0.6, 0.3, 0.8))
dev.off()

pdf('subfig_sar.pdf', width = 2, height = 2)
par(mar = c(2, 2, 0, 0), mgp = c(0.5, 0, 0))
curve(log(x), xlab = 'Area', ylab = 'Species',
     axes = FALSE, frame.plot = TRUE, lwd = 4, col = hsv(0.05, 0.7, 0.8))
dev.off()

