################################################################################
# generate a bunch of 'purified dna' samples
################################################################################

library(data.table)

functions.f <- list.files("functions", full.names = TRUE)
sapply(functions.f, source, echo = FALSE)

# set evenness levels
even.levs <- c('even', 'skew.lin', 'skew.low', 'skew.med', 'skew.hi')
N_even <- length(even.levs)

# set richness levels
rich.levs <- c(100, 500, 1000, 5000, 10000)
N_rich <- length(rich.levs)

# number of samples (DNA extracts from the 'same' environment)
reps.each <- 24
reps.levs <- 1:reps.each

N_comm <- length(even.levs) * length(rich.levs)

# number of copies of template (essentially DNA template concentration)
N_templates <- 1e10

# set up sample data
temp <- expand.grid(reps.levs, even.levs, rich.levs)
colnames(temp) <- c('rep', 'even', 'rich')
sample_dat <- data.table(
  sample = 1:nrow(temp), 
  comm = rep(1:N_comm, each = reps.each), 
  temp)
rm(temp)

# simulate template counts
set.seed(2)
template_dat <- sample_dat[ , 
  list(templates = sim_templates(N_sp = rich, N_out = N_templates, 
    evenness = even, stochastic = TRUE, sort = TRUE)), 
  by = sample]

template_dat[, species := seq_along(templates), by = sample]

template_dat <- merge(x = sample_dat, y = template_dat, by = 'sample')

template_dat
