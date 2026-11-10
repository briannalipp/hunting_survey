# Load packages
library(here)
library(dplyr)
library(jagsUI)

# Load data
data <- readRDS(here("Anna SEM", "AB_data.rds"))

# set dummy columns
dummy_vars <- grep("^(gender|race|region)_", names(data), value = TRUE)

demo <- c("hispanic", "metro", "age_group4", "education", "income", dummy_vars)

# make boxes from diagram
# Drop one reference category per categorical variable
ref_cats <- c("gender_Female", "race_White", "region_West")
demo <- setdiff(demo, ref_cats)
vals <- c("wvo_domination", "wvo_mutualism", "christian_nationalism_score")
ideo <- "political_ideology"                # 0 = conservative, 100 = liberal; mediator between values and lifestyle/exposure/support
life <- c("has_pets", "eats_meat")         # binary
expo <- c("knows_hunter", "is_hunter", "game_meat_freq") # binary, ordinal-as-continuous
outc <- "hunting_support"                   # 1-5

# compile one dataset for the model drop any row with an na
sem_data <- data %>%
  select(all_of(c(demo, vals, ideo, life, expo, outc))) %>%
  na.omit()

# check how many cases made it
cat("Complete cases:", nrow(sem_data), "of", nrow(data), "\n")

# standardize nonbinary values
z <- function(x) as.numeric(scale(x)) # turn a variable into a z score

to_scale <- c("age_group4", "education", "income",
              vals, ideo, "game_meat_freq") # appply z to every nonbinary var
sem_data[to_scale] <- lapply(sem_data[to_scale], z)

# keep hunting support ordinal
sem_data$hunting_support <- as.integer(
  as.character(sem_data$hunting_support)
)

# check that all are 0 or 1, stops with an error instead of running wrong model
bin_vars <- c("hispanic", "metro", "has_pets", "eats_meat", "knows_hunter", "is_hunter")
stopifnot(all(sapply(sem_data[bin_vars], function(x) all(x %in% c(0, 1)))))

# Build jags
X <- as.matrix(sem_data[, demo]); storage.mode(X) <- "numeric"
V <- as.matrix(sem_data[, vals])

# set number of ordinal responses
C <- 5

jags_data <- list(
  N = nrow(X), K = ncol(X),
  X = X,
  V = V,
  ideo  = sem_data[[ideo]],
  pets  = sem_data$has_pets,
  meat   = sem_data$eats_meat,
  knows = sem_data$knows_hunter,
  hunt  = sem_data$is_hunter,
  gm    = sem_data$game_meat_freq,
  y     = sem_data[[outc]],
  C     = C
  R     = diag(3)
)

# THE MODEL
model_txt <- "
model {
 
  for (i in 1:N) {
 
    # STAGE 1: Demographics -> values (residuals allowed to correlate)
    for (v in 1:3) {
      mu_v[i, v] <- a_v[v] + inprod(B_v[v, 1:K], X[i, 1:K])
    }
    V[i, 1:3] ~ dmnorm(mu_v[i, 1:3], Omega)
 
    # STAGE 2: Values (+ demographics) -> ideology
    ideo[i] ~ dnorm(mu_i[i], tau_i)
    mu_i[i] <- a_i + inprod(g[1:3], V[i, 1:3]) + inprod(B_i[1:K], X[i, 1:K])
 
    # STAGE 3: Values + ideology (+ demographics) -> lifestyle & exposure
    pets[i] ~ dbern(p_pets[i])
    logit(p_pets[i]) <- a_pets + inprod(d_pets[1:3], V[i, 1:3]) + e_pets * ideo[i] + inprod(B_pets[1:K], X[i, 1:K])
 
    meat[i] ~ dbern(p_meat[i])
    logit(p_meat[i]) <- a_meat + inprod(d_meat[1:3], V[i, 1:3]) + e_meat * ideo[i] + inprod(B_meat[1:K], X[i, 1:K])
 
    knows[i] ~ dbern(p_knows[i])
    logit(p_knows[i]) <- a_knows + inprod(d_knows[1:3], V[i, 1:3]) + e_knows * ideo[i] + inprod(B_knows[1:K], X[i, 1:K])
 
    hunt[i] ~ dbern(p_hunt[i])
    logit(p_hunt[i]) <- a_hunt + inprod(d_hunt[1:3], V[i, 1:3]) + e_hunt * ideo[i] + inprod(B_hunt[1:K], X[i, 1:K])
 
    gm[i] ~ dnorm(mu_gm[i], tau_gm)
    mu_gm[i] <- a_gm + inprod(d_gm[1:3], V[i, 1:3]) + e_gm * ideo[i] + inprod(B_gm[1:K], X[i, 1:K])
 
# STAGE 4: Lifestyle + exposure (+ values, ideology, demographics)
# -> ordinal hunting support

y[i] ~ dcat(prob[i, 1:C])

logit(cum_prob[i, 1]) <- cutpoints[1] - eta_y[i]
logit(cum_prob[i, 2]) <- cutpoints[2] - eta_y[i]
logit(cum_prob[i, 3]) <- cutpoints[3] - eta_y[i]
logit(cum_prob[i, 4]) <- cutpoints[4] - eta_y[i]

prob[i, 1] <- cum_prob[i, 1]
prob[i, 2] <- cum_prob[i, 2] - cum_prob[i, 1]
prob[i, 3] <- cum_prob[i, 3] - cum_prob[i, 2]
prob[i, 4] <- cum_prob[i, 4] - cum_prob[i, 3]
prob[i, 5] <- 1 - cum_prob[i, 4]

eta_y[i] <-
  c_pets * pets[i] + c_meat * meat[i] +
  c_knows * knows[i] + c_hunt * hunt[i] +
  c_gm * gm[i] +
  inprod(h[1:3], V[i, 1:3]) +
  c_ideo * ideo[i] +
  inprod(B_y[1:K], X[i, 1:K])
  }
 
  # PRIORS
  Omega ~ dwish(R[1:3, 1:3], 4)
  Sigma <- inverse(Omega)
  rho_dom_mut <- Sigma[1,2] / sqrt(Sigma[1,1] * Sigma[2,2])
  rho_dom_cn  <- Sigma[1,3] / sqrt(Sigma[1,1] * Sigma[3,3])
  rho_mut_cn  <- Sigma[2,3] / sqrt(Sigma[2,2] * Sigma[3,3])
 
  for (v in 1:3) {
    a_v[v] ~ dnorm(0, 0.1)
    g[v]   ~ dnorm(0, 0.1)
    h[v]   ~ dnorm(0, 0.1)
    for (j in 1:K) { B_v[v, j] ~ dnorm(0, 0.1) }
  }
 
  for (j in 1:K) {
    B_i[j] ~ dnorm(0, 0.1);    B_pets[j] ~ dnorm(0, 0.1)
    B_meat[j] ~ dnorm(0, 0.1);  B_knows[j] ~ dnorm(0, 0.1)
    B_gm[j] ~ dnorm(0, 0.1);   B_y[j] ~ dnorm(0, 0.1)
    B_hunt[j] ~ dnorm(0, 0.1)
  }
 
  a_i ~ dnorm(0, 0.1); a_pets ~ dnorm(0, 0.1); a_meat ~ dnorm(0, 0.1)
  a_knows ~ dnorm(0, 0.1); a_gm ~ dnorm(0, 0.1); a_y ~ dnorm(0, 0.1)
  a_hunt ~ dnorm(0, 0.1)
 
  for (v in 1:3) {
    d_pets[v] ~ dnorm(0, 0.1); d_meat[v] ~ dnorm(0, 0.1)
    d_knows[v] ~ dnorm(0, 0.1); d_gm[v] ~ dnorm(0, 0.1)
    d_hunt[v] ~ dnorm(0, 0.1)
    
    # Ordered logistic cutpoints
cutpoints[1] ~ dnorm(0, 1)

for (k in 2:4) {
  cutpoints[k] ~ dnorm(0, 1) T(cutpoints[k-1], )
}
  }
  c_pets ~ dnorm(0, 0.1); c_meat ~ dnorm(0, 0.1)
  c_knows ~ dnorm(0, 0.1); c_gm ~ dnorm(0, 0.1)
  c_ideo ~ dnorm(0, 0.1)
  c_hunt ~ dnorm(0, 0.1)
  e_hunt ~ dnorm(0, 0.1)
  e_pets ~ dnorm(0, 0.1); e_meat ~ dnorm(0, 0.1)
  e_knows ~ dnorm(0, 0.1); e_gm ~ dnorm(0, 0.1)
 
  tau_i ~ dgamma(0.1, 0.1); tau_gm ~ dgamma(0.1, 0.1); tau_y ~ dgamma(0.1, 0.1)
 
  # EFFECTS ON HUNTING SUPPORT
  # (binary mediators are on the logit scale, so treat as relative sizes)
 
  # Ideology's total effect on support: direct + through each lifestyle/exposure variable
  ideo_via_med <- e_pets * c_pets + e_meat * c_meat + e_knows * c_knows + e_hunt * c_hunt + e_gm * c_gm
  ideo_total   <- c_ideo + ideo_via_med
 
  for (v in 1:3) {
    # Route A: value -> mediator -> support (skipping ideology)
    ind_pets[v]  <- d_pets[v]  * c_pets
    ind_meat[v]   <- d_meat[v]   * c_meat
    ind_knows[v] <- d_knows[v] * c_knows
    ind_hunt[v]  <- d_hunt[v]  * c_hunt
    ind_gm[v]    <- d_gm[v]    * c_gm
    ind_direct_med[v] <- ind_pets[v] + ind_meat[v] + ind_knows[v] + ind_hunt[v] + ind_gm[v]
 
    # Route B: value -> ideology -> (mediators ->) support
    ind_via_ideo[v] <- g[v] * ideo_total
 
    ind_total[v]    <- ind_direct_med[v] + ind_via_ideo[v]
    total_effect[v] <- ind_total[v] + h[v]   # indirect + direct
  }
}
"

params <- c(
  "a_v", "B_v", "g", "h", "a_i", "B_i",
  "d_pets", "d_meat", "d_knows", "d_gm",
  "c_pets", "c_meat", "c_knows", "c_gm", "c_ideo", "e_pets", "e_meat", "e_knows", "e_gm", "e_hunt", "c_hunt", "d_hunt", "B_hunt", "B_y",
  "B_pets", "B_meat", "B_knows", "B_gm",
  "tau_i", "tau_gm", "tau_y",
  "rho_dom_mut", "rho_dom_cn", "rho_mut_cn",
  "ind_pets", "ind_meat", "ind_knows", "ind_gm", "ind_hunt", "ind_direct_med", "ind_via_ideo", "ind_total", "total_effect", "ideo_via_med", "ideo_total"
)

model_file <- here("Anna SEM", "AB_model.jags")
writeLines(model_txt, model_file)

fit <- jags(
  data = jags_data,
  parameters.to.save = params,
  model.file = model_file,
  n.chains = 3,
  n.iter = 10000,
  n.burnin = 5000,
  n.thin = 5,
  parallel = TRUE
)

print(fit)

# Check model convergence
rhats <- unlist(fit$Rhat)
cat("Max Rhat:", max(rhats, na.rm = TRUE), "\n")   # want < 1.05
cat("Min n.eff:", min(unlist(fit$n.eff), na.rm = TRUE), "\n")

# Match N_ columns to predictor names
coef_map <- data.frame(column = seq_along(demo), predictor = demo)
coef_map

saveRDS(fit, here("Anna SEM", "AB_fit.rds"))

fit <- readRDS(here("Anna SEM", "AB_fit.rds"))

#

fit_test$summary[grep("^(e_|c_|ideo_|ind_|total_effect|rho)", rownames(fit_test$summary)),
              +             c("mean", "2.5%", "97.5%")]

fit_test$summary[grep("^(g|h|d_|a_i)", rownames(fit_test$summary)), c("mean","2.5%","97.5%")]

# print demographic results that are creidble 
s <- as.data.frame(fit_test$summary[grep("^B_", rownames(fit_test$summary)),
                               c("mean", "2.5%", "97.5%")])
s$param <- rownames(s)
s$predictor <- demo[as.integer(gsub(".*[,\\[]([0-9]+)\\]$", "\\1", s$param))]
s[s[["2.5%"]] > 0 | s[["97.5%"]] < 0, ]
