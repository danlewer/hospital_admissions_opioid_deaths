library(data.table)
library(extrafont)


fp <- read.csv("https://raw.githubusercontent.com/danlewer/hospital_admissions_opioid_deaths/main/fig3_data.csv", col.names = c('m', 'e', 'l', 'u', 'p'))
all_res <- c(1, 10, 11, 2, 12, 13, 3, 14, 15, 20, 21, 26, 27, 28, 4, 16, 17, 22, 23, 29, 30, 31)
fp <- fp[all_res, ]
main_exp <- c(1, 4, 7, 15)
cols <- rep('grey60', nrow(fp))
cols[main_exp] <- 'black'
ys <- rev(seq_len(nrow(fp)))

vals <- c('e', 'l', 'u')
f <- function(x) format(round(exp(x), 2), nsmall = 2, digits = 2)
setDT(fp)
fp[, paste0(vals, '_f') := lapply(.SD, f), .SDcols = vals]
fp[, OR := paste0(e_f, ' (', l_f, '-', u_f, ')')]
fp[, OR := gsub('\\(', ' (', gsub(' ', '', OR))]

xpos <- c(-9, -4, -2.5)

#png('Fig3.png', height = 8, width = 10, units = 'in', res = 300, family = 'Corbel')
tiff('Fig3.tiff', height = 8, width = 10, units = 'in', res = 600, family = 'Corbel')

postscript("Fig3.eps",
           horizontal = F, onefile = T, paper = "special", height = 3.5, width = 6, family = 'serif')


par(xpd = NA)
plot(1, type = 'n', xlim = c(min(xpos), 2.5), ylim = c(0, 25), axes = F, xlab = NA, ylab = NA)
segments(0, 0.5, 0, 22.5, lty = 3)
horiz <- c(0, ys[main_exp]) + 0.5
segments(min(xpos), horiz, 2.5, horiz)
segments(min(xpos) + 0.3, c(3.5, 5.5, 7.5, 11.5, 13.5, 15.5, 18.5, 21.5), 2.5, col = 'grey70', lty = 3)
points(fp$e, ys, pch = 19, col = cols)
arrows(fp$l, ys, fp$u, ys, angle = 90, code = 3, length = 0.05, col = cols)
xs <- rep(min(xpos) + 0.3, nrow(fp))
xs[main_exp] <- min(xpos)
text(xs, ys, c('A: Day 1-14 of admission',
               '(acute admission)',
               '(psychiatric admission)',
               'B: Day 15+ of admission',
               '(acute admission)',
               '(psychiatric admission)',
               'C: Day 1-2 after discharge',
               '(after acute admission)',
               '(after psychiatric admission)',
               '(after planned discharge)',
               '(against medical advice)',
               '(after admission of length 1 day)',
               '(after admission of length 2-6 days)',
               '(after admission of length 7+ days)',
               'D: Day 3-14 after discharge',
               '(after acute admission)',
               '(after psychiatric admission)',
               '(after planned discharge)',
               '(against medical advice)',
               '(after admission of length 1 day)',
               '(after admission of length 2-6 days)',
               '(after admission of length 7+ days)'),
     adj = 0, col = cols)

text(xpos[2], ys, fp$OR, adj = 0.5, col = cols)
text(xpos[3], ys, fp$p, adj = 0.5, col = cols)

text(xpos[1], 23, 'Exposure status', adj = c(0, 0))
text(xpos[2:3], 23, c('Conditional odds\nratio (95% CI)', 'p-value'), adj = c(0.5, 0))

xx <- c(0.25, 0.5, 1, 2, 5, 10)
axis(1, log(xx), xx, pos = 0.5)
text(0, -2.75, 'Conditional odds ratio of\nopioid-related death')
text(0, 25, 'Reference = time not\nclose to admission')
arrows(0, 24, 0, 23, length = 0.1)

dev.off()
