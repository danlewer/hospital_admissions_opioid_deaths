
library(data.table)
library(RColorBrewer)
library(devEMF)

d <- read.csv("https://raw.githubusercontent.com/danlewer/hospital_admissions_opioid_deaths/main/conditional_ORs_by_sex_and_year.csv", col.names = c('s', 'v', 'p', 'e', 'l', 'u'))
setDT(d)
d[, c('e', 'l', 'u') := lapply(.SD, log), .SDcols = c('e', 'l', 'u')]
d[, p := factor(p, paste0('exp.Basic', letters[1:4]))]
d <- d[order(p, s, v)]

interval <- 2
group_interval <- 5

d[, b := interval]
d[, a := b]
d[, left_edge := s != shift(s, 1)]
d[, right_edge := s != shift(s, -1)]
d$left_edge[is.na(d$left_edge)] <- T
d$right_edge[is.na(d$right_edge)] <- T
d$b[d$left_edge] <- group_interval
d$a[d$right_edge] <- group_interval
d[, o := cumsum(b) + cumsum(a) - a]
d[, o := max(o) - o]
d[, v := factor(v, c(1:2, 2010:2019), c('Male', 'Female', 2010:2019))]

ylims <- c(min(d$o) - group_interval, max(d$o) + group_interval)
group_seps <- rowMeans(with(d, cbind(o[left_edge][-1], o[right_edge][-sum(left_edge)])))
ylabs <- c(ylims[2], group_seps[c(2, 4, 6)], ylims[1])
ylabs <- ylabs[-length(ylabs)] + diff(ylabs)/2
xlabs <- c(0.05, 0.25, 0.5, 1, 2, 5, 10, 20)
xlims <- range(log(xlabs))

ar <- d[v %in% c(2011, 2018)]
ar <- split(ar, f = ar$p)

cl <- brewer.pal(3, 'Set1')[2]

#png('sex_age.png', height = 10, width = 8, units = 'in', res = 300)
emf('sex_age.emf', height = 10, width = 8, family = 'Corbel')

par(xpd = NA, mar = c(7, 7, 0, 7))
plot(1, type = 'n', xlim = xlims, ylim = ylims, axes = F, xlab = NA, ylab = NA)
#rect(xlims[1], ylims[1], xlims[2], ylims[2])
segments(xlims[1] * 1.3, group_seps[c(2, 4, 6)], x1 = xlims[2])
segments(xlims[1] * 1.3, ylims, x1 = xlims[2])
segments(xlims[1], group_seps[c(1, 3, 5, 7)], x1 = xlims[2], lty = 3)
segments(0, ylims[1], y1 = ylims[2])
with(d, {
  points(e, o, pch = 19, col = cl)
  arrows(l, o, u, o, angle = 90, code = 3, length = 0.04, col = cl)
})
text(xlims[1] * 1.1, ylabs, c('A: Days 1-14\nof admission',
                              'B: Days 15+\nof admission',
                              'C: Days 1-2\nafter discharge',
                              'D: Days 3-14\nafter discharge'), adj = 1)
axis(1, at = log(xlabs), labels = xlabs, pos = ylims[1])
with(d[!(v %in% 2011:2018)], text(xlims[2] * 1.3, o, v, adj = 0.5))
lapply(ar, function(x) with(x, arrows(xlims[2] * 1.3, y0 = o[1], y1 = o[2], length = 0.08)))
title(xlab = 'Conditional odds ratio\nof opioid-related death')

dev.off()
