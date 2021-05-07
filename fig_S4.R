library(data.table)
library(RColorBrewer)
library(devEMF)

d <- read.csv("https://raw.githubusercontent.com/danlewer/hospital_admissions_opioid_deaths/main/conditional_odds_ratios_for_chart.csv", col.names = c('m', 'g', 'e', 'l', 'u'))
setDT(d)
d[, c('e', 'l', 'u') := lapply(.SD, log), .SDcols = c('e', 'l', 'u')]
d <- d[m != 'ahm']
model_order <- c('standard', 'short', 'long', 'endDeath', 'end2019', paste0('farr', 1:10), 'farr_ma')
d[, m := factor(m, model_order)]
d <- d[order(g, m)]

interval <- 1
farr_interval <- 0.25
farr_ma_interval <- 0.7
group_interval <- 3

d[, b := interval]
d$b[grepl('farr', d$m)] <- farr_interval
d$b[d$m == 'farr_ma'] <- farr_ma_interval
d[, a := b]
d[, left_edge := g != shift(g, 1)]
d[, right_edge := g != shift(g, -1)]
d$left_edge[is.na(d$left_edge)] <- T
d$right_edge[is.na(d$right_edge)] <- T
d$b[d$left_edge] <- group_interval
d$a[d$right_edge] <- group_interval
d[, o := cumsum(b) + cumsum(a) - a]
d[, o := max(o) - o]

d[, cols := as.character(m)]
d$cols[grepl('farr', d$cols)] <- 'farr'
col6 <- brewer.pal(7, 'Set2')[c(1:5, 7)]
d[, cols := factor(cols, c('standard', 'short', 'long', 'endDeath', 'end2019', 'farr'), col6)]
d[, cols := as.character(cols)]
d$cols[d$m == 'farr_ma'] <- NA

d[, size := fifelse(grepl('farr', m), 0.5, 1)]

ylims <- c(min(d$o) - group_interval, max(d$o) + group_interval)
group_seps <- rowMeans(with(d, cbind(o[left_edge][-1], o[right_edge][-sum(left_edge)])))
ylabs <- c(ylims[2], group_seps, ylims[1])
ylabs <- ylabs[-length(ylabs)] + diff(ylabs)/2
xlabs <- c(0.2, 0.5, 1, 2, 5, 10, 15)
xlims <- range(log(xlabs))

#png('fig_S4.png', height = 8, width = 10, units = 'in', res = 300)
emf('fig_S4.emf', height = 8, width = 10, family = 'Corbel')

par(xpd = NA, mar = c(7, 7, 0, 20))
plot(1, type = 'n', xlim = xlims, ylim = ylims, axes = F, xlab = NA, ylab = NA)
#rect(xlims[1], ylims[1], xlims[2], ylims[2])
segments(xlims[1] * 1.3, group_seps, xlims[2], group_seps)
segments(xlims[1] * 1.3, ylims, x1 = xlims[2])
segments(0, ylims[1], y1 = ylims[2])
with(d, {
  points(e, o, col = cols, pch = 19, cex = size)
  arrows(l, o, u, o, angle = 90, code = 3, length = 0.04, col = cols)
})
text(xlims[1] * 1.1, ylabs, c('A: Days 1-14\nof admission',
                              'B: Days 15+\nof admission',
                              'C: Days 1-2\nafter discharge',
                              'D: Days 3-14\nafter discharge'), adj = 1)
axis(1, at = log(xlabs), labels = xlabs, pos = ylims[1])
for(i in 1:2) {
  with(d[m == 'farr_ma'][i], polygon(x = c(l, e, u, e), y = c(o, o-farr_ma_interval, o, o+farr_ma_interval), border = col6[6], col = col6[6]))
}
text(mean(xlims), ylims[1] - 12, 'Conditional odds ratio\nof opioid-related death\n(reference = time in the community)')

# legend

ys <- seq(55, 15, length.out = 6)
arrows(3, ys[-6], 3.4, ys[-6], code = 3, angle = 90, length = 0.07, col = col6[-6])
points(rep(3.2, 5), ys[-6], col = col6[-6], pch = 19)

text(3.7, ys[-6], c('Control days sampled from\n730-28 days before death\n(main analysis)',
                    'Control days sampled from\n365-28 days before death',
                    'Control days sampled from\n1095-28 days before death',
                    'Standard self-controlled case series;\nend follow-up at death',
                    'Standard self-controlled case series;\nend follow-up at 31 December 2019'),
     adj = 0)

ys <- seq(7, 12, length.out = 7)
xs <- seq(3.1, 3.3, length.out = 5)
points(xs, ys[-(1:2)], pch = 19, cex = 0.5, col = col6[6])
arrows(xs - 0.2, ys[-(1:2)], xs + 0.2, ys[-(1:2)], length = 0.03, angle = 90, code = 3, col = col6[6])
text(3.7, mean(ys), 'Farrington model for event-\ndependent exposures, on 10\nsamples of 500 cases. Diamond\nshows pooled value from fixed-\neffects meta-analysis', adj = 0)
polygon(c(min(xs), mean(xs), max(xs), mean(xs)), c(mean(ys[1:2]), ys[1], mean(ys[1:2]), ys[2]), col = col6[6], border = col6[6])

dev.off()

# -------------
# results table
# =============

results_table <- d[!(m %in% paste0('farr', 1:10))]
vals <- c('e', 'l', 'u')
f <- function(x) format(round(exp(x), 2), nsmall = 2, digits = 2)
results_table[, (vals) := lapply(.SD, f), .SDcols = vals]
results_table[, q := paste0(e, ' (', l, '-', u, ')')]
results_table[, q := gsub(' ', '', q)]
results_table[, q := gsub('\\(', ' (', q)]
results_table <- dcast(results_table, m ~ g, value.var = 'q')

fwrite(results_table, 'alternative_models_results_table.csv')
