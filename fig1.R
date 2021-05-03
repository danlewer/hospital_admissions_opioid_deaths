library(extrafont)
loadfonts(device = 'win')

nboxes <- 5
ratio <- 2.5

ymax <- nboxes + (nboxes - 1)/ratio

z <- seq_len(nboxes)-1

yb <- rev(z + z/ratio)
yt <- yb + 1
  
mp <- rowMeans(cbind(yt, yb))

xp <- c(0, 2, 2.5, 4.5)
xm1 <- mean(xp[1:2])
xm2 <- mean(xp[3:4])

#png('Fig1.png', height = 6, width = 6, units = 'in', res = 300)
tiff('Fig1.tiff', height = 6, width = 6, res = 300, units = 'in', family = 'Corbel')

par(mar = rep(0, 4), cex = 0.8)
plot(1, type = 'n', xlim = c(0, max(xp)), ylim = c(0, ymax), axes = F, xlab = NA, ylab = NA)

rect(xp[1], yb, xp[2], yt)
rect(xp[3], yb[-length(yb)], xp[4], yt[-length(yt)])

arrows(xm1, yb[-length(yb)], xm1, yt[-1], length = 0.1)
arrows(xp[2], mp[-length(yb)], xp[3], length = 0.1)

text(xm1, mp, c('All opioid-related deaths in England\n1 Jan 2010-31 Dec 2019 (published data)\n16,586',
              'Individuals with a linked record in\nhospital databases\n14,629',
              'Age 18-64\n13,647',
              'No conflict recorded during linkage\n13,639',
              'No conflict between dates of death\nrecorded in hospital and mortality data\n13,611'))

text(xm2, mp[-length(yb)], c('Deaths among individuals without a\nlinked record in hospital databases\n(estimate)\n1,957',
                           'Aged <18 or 65+ at death\n982',
                           'Possible error in date of death\nand/or final hospital admission\nrecorded during linkage process\n8',
                           'Admission with discharge date >3 days\nafter death date\n28'))

dev.off()
