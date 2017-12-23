
skewed.poor <- template_dat[ even == 'skew.hi' & rich == 100, unique(templates.id)]

i <- skewed.poor[1]

input <- template_dat[ templates.id == i, templates]

betacoeff <- 1

effs <- primer_eff(N = length(input), mmv = 0.9)#mmv = 'custom', mmv.beta = betacoeff*(c(0.95,0.05)))


layout(1); plot(effs, col = hsv(1,1,0,0.1))

output <- round(do_pcr(template_copies = input, template_effs = effs, 
ncycles = 20, inflection = 20, stochastic = FALSE))

out.scale <- output/sum(output)

in.scale <- input/sum(input)

plot(in.scale, out.scale, log = 'xy')

abun.diff <- out.scale - in.scale

layout(1)
plot(abun.diff ~ effs)

layout(mat = matrix(c(1,2), nrow = 1))
lims <- c(-1, 1)
plot(abun.diff, ylim = lims)
abline(h = 0, lwd = 2, col = hsv(1,0.5,1), lty = 2)
hist(abun.diff, xlim = lims, main = "")
layout(1)
