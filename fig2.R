library(extrafont)
loadfonts(device = "win")

ys <- 1:5

deaths <- c(8.2, 7, 9.5, 6.5, 8)

hstart <- list(pt5 = c(1.2, 2.5, 5.05, 6.1, 8),
               pt4 = 6.6,
               pt3 = c(0.7, 7),
               pt2 = c(0.5, 4),
               pt1 = c(3.7, 4, 6.6))

hend <- list(pt5 = c(1.3, 2.65, 5.2, 6.2, 8.1),
             pt4 = 7,
             pt3 = c(1.2, 7.2),
             pt2 = c(1.9, 4.2),
             pt1 = c(3.9, 4.6, 7.6))

controls <- list(pt5 = c(4.8, 5.1, 5.65, 6.25, 7.4),
                 pt4 = c(3.7, 3.9, 5, 5.15, 6.4),
                 pt3 = c(6.2, 7.05, 7.35, 8, 8.15),
                 pt2 = c(3.1, 4.45, 4.9, 5.1, 5.5),
                 pt1 = c(4.8, 5.1, 5.35, 6, 7.1))

controls_deaths <- mapply(append, x = controls, values = deaths, SIMPLIFY = F)

status <- list(pt5 = c('E', 'A', 'E', 'C', 'E', 'D'),
               pt4 = c('E', 'E', 'E', 'E', 'E', 'B'),
               pt3 = c('E', 'A', 'D', 'E', 'E', 'E'),
               pt2 = c('E', 'E', 'E', 'E', 'E', 'E'),
               pt1 = c('D', 'E', 'E', 'E', 'B', 'E'))


#png('Fig2.png', height = 6, width = 8, units = 'in', res = 300)
tiff('Fig2.tiff', height = 6, width = 8, units = 'in', res = 600, bg = 'white', family = 'Corbel')

# main plot
par(xpd = NA, cex = 0.9, mar = rep(0, 4))
plot(1, type = 'n', xlim = c(-3, 10), ylim = c(-5, 6), axes = F, xlab = NA, ylab = NA)
mapply(rect, xleft = hstart, ybottom = ys - 0.15, xright = hend, ytop = ys + 0.15, col = 'grey80')
segments(0, ys, 10, ys, lty = 3)
points(deaths, ys, pch = 18, cex = 1.5)
segments(deaths - 3.5, ys, deaths - 0.5)
mapply(segments, x0 = controls, y0 = as.list(ys), y1 = as.list(ys + 0.25))
mapply(text, x = controls_deaths, y = as.list(ys + 0.4), labels = status)
text(-1, ys, paste0('Participant ', 5:1), adj = 1)
arrows(1, 0.3, x1 = 3, length = 0.07)
text(3.5, 0.3, 'Time', adj = 0)

# key
rect(-3, -5, 10, -0.5, col = 'grey96')
ky <- seq(-1, -4.5, length.out = 7)
kx <- c(-2, -1, 2.8, 3.3)
points(kx[1], ky[2], pch = 18, cex = 1.5)
segments(x0 = kx[1] - 0.5, y0 = ky[3:5], x1 = kx[1] + 0.5, lty = c(3, 1, 1))
segments(x0 = kx[1], y0 = ky[5], y1 = ky[5] + 0.3)
rect(kx[1] - 0.5, ky[6] - 0.2, kx[1] + 0.5, ky[6] + 0.2, col = 'grey80')
text(kx[1], ky[7], 'A-E')
text(kx[2], ky[2:7], c('Opioid-related death', 'Not eligible for control days', 'Days 730-28 before death', 'Control day', 'Hospital admission', 'Exposure status'), adj = 0)
text(kx[3], ky[2:6], LETTERS[1:5], font = 2, adj = 0)
text(kx[4], ky[2:6], c('Days 1-14 of hospital admission', 'Days 15+ of hospital admission', 'Days 1-2 after hospital discharge', 'Days 3-14 after hospital discharge', 'Not hospitalised and not recently discharged (reference)'), adj = 0)
text(c(kx[1] - 0.5, kx[3]), ky[1], c('Key', 'Exposure status'), adj = 0, font = 2)

dev.off()

