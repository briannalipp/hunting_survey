# ============================================================
# BAYESIAN PATH MODEL OF HUNTING SUPPORT (JAGS)
# ============================================================

library(here)
library(dplyr)
library(jagsUI)

data <- readRDS(here("Anna SEM", "AB_data.rds"))

# ------------------------------------------------------------
# Variables
# ------------------------------------------------------------

dummy_vars <- grep("^(gender|race|region|age_group4|education|income)_",
                   names(data), value = TRUE)

demo <- c("hispanic", "metro", dummy_vars)               # X
vals <- c("wvo_domination", "wvo_mutualism",
          "christian_nationalism_score")                 # V
med  <- c("has_pets", "eats_meat", "knows_hunter", "is_hunter")  # M (order matters)

sem_data <- data %>%
  select(all_of(c(demo, vals, "political_ideology", med,
                  "game_meat_freq", "hunting_support"))) %>%
  na.omit()

# Standardize continuous variables
sem_data <- sem_data %>%
  mutate(across(all_of(c(vals, "political_ideology")),
                ~ as.numeric(scale(.x))))

# check: every binary column is 0/1
stopifnot(all(sapply(sem_data[c(demo, med)], function(x) all(x %in% 0:1))))

cat("Complete cases:", nrow(sem_data), "\n")


# ------------------------------------------------------------
# JAGS data
# ------------------------------------------------------------

jags_data <- list(
  N = nrow(sem_data),
  K = length(demo),
  X = as.matrix(sem_data[demo]),
  V = as.matrix(sem_data[vals]),
  ideology = sem_data$political_ideology,
  M = as.matrix(sem_data[med]),
  gm = sem_data$game_meat_freq,
  y = sem_data$hunting_support,
  G = 4,
  C = 5,
  R = diag(3)
)


# ------------------------------------------------------------
# Model
# ------------------------------------------------------------
# Index keys:
#   v (values):    1 = domination, 2 = mutualism, 3 = Christian nationalism
#   j (mediators): 1 = has_pets, 2 = eats_meat, 3 = knows_hunter, 4 = is_hunter
#   k (demos):     see coef_key below

model_string <- "
model {

  for (i in 1:N) {

    # Stage 1: demographics -> wildlife values (correlated residuals)
    for (v in 1:3) {
      mu_v[i,v] <- a_v[v] + inprod(B_v[v,1:K], X[i,1:K])
    }
    V[i,1:3] ~ dmnorm(mu_v[i,1:3], Omega)

    # Stage 2: values + demographics -> ideology
    mu_i[i] <- a_i + inprod(g[1:3], V[i,1:3]) + inprod(B_i[1:K], X[i,1:K])
    ideology[i] ~ dnorm(mu_i[i], tau_i)

    # Stage 3a: binary mediators (logit)
    for (j in 1:4) {
      logit(p_m[i,j]) <- a_m[j] + e_m[j] * ideology[i] +
                         inprod(d_m[j,1:3], V[i,1:3]) +
                         inprod(B_m[j,1:K], X[i,1:K])
      M[i,j] ~ dbern(p_m[i,j])
    }

    # Stage 3b: game meat frequency (cumulative logit)
    eta_gm[i] <- e_gm * ideology[i] + inprod(d_gm[1:3], V[i,1:3]) +
                 inprod(B_gm[1:K], X[i,1:K])
    for (k in 1:(G-1)) {
      Q_gm[i,k] <- ilogit(cut_gm[k] - eta_gm[i])
    }
    p_gm[i,1] <- Q_gm[i,1]
    for (k in 2:(G-1)) {
      p_gm[i,k] <- Q_gm[i,k] - Q_gm[i,k-1]
    }
    p_gm[i,G] <- 1 - Q_gm[i,G-1]
    gm[i] ~ dcat(p_gm[i,1:G])

    # Stage 4: everything -> hunting support (cumulative logit)
    eta_y[i] <- c_ideo * ideology[i] + inprod(h[1:3], V[i,1:3]) +
                inprod(c_m[1:4], M[i,1:4]) + c_gm * gm[i] +
                inprod(B_y[1:K], X[i,1:K])
    for (k in 1:(C-1)) {
      Q_y[i,k] <- ilogit(cut_y[k] - eta_y[i])
    }
    p_y[i,1] <- Q_y[i,1]
    for (k in 2:(C-1)) {
      p_y[i,k] <- Q_y[i,k] - Q_y[i,k-1]
    }
    p_y[i,C] <- 1 - Q_y[i,C-1]
    y[i] ~ dcat(p_y[i,1:C])
  }

  # ---------------- Priors ----------------

  # Stage 1
  for (v in 1:3) {
    a_v[v] ~ dnorm(0, 0.1)
    for (k in 1:K) { B_v[v,k] ~ dnorm(0, 0.1) }
  }
  Omega ~ dwish(R, 4)
  Sigma <- inverse(Omega)     # residual covariance of the three values

  # Stage 2
  a_i ~ dnorm(0, 0.1)
  for (v in 1:3) { g[v] ~ dnorm(0, 0.1) }
  for (k in 1:K) { B_i[k] ~ dnorm(0, 0.1) }
  tau_i ~ dgamma(0.1, 0.1)

  # Stage 3a
  for (j in 1:4) {
    a_m[j] ~ dnorm(0, 0.1)
    e_m[j] ~ dnorm(0, 0.1)
    for (v in 1:3) { d_m[j,v] ~ dnorm(0, 0.1) }
    for (k in 1:K) { B_m[j,k] ~ dnorm(0, 0.1) }
  }

  # Stage 3b
  e_gm ~ dnorm(0, 0.1)
  for (v in 1:3) { d_gm[v] ~ dnorm(0, 0.1) }
  for (k in 1:K) { B_gm[k] ~ dnorm(0, 0.1) }

  # Stage 4
  c_ideo ~ dnorm(0, 0.1)
  c_gm ~ dnorm(0, 0.1)
  for (j in 1:4) { c_m[j] ~ dnorm(0, 0.1) }
  for (v in 1:3) { h[v] ~ dnorm(0, 0.1) }
  for (k in 1:K) { B_y[k] ~ dnorm(0, 0.1) }

  # Ordered cutpoints (first cutpoint absorbs the intercept)
  cut_gm[1] ~ dnorm(0, 0.1)
  for (k in 2:(G-1)) {
    delta_gm[k] ~ dgamma(2, 2)
    cut_gm[k] <- cut_gm[k-1] + delta_gm[k]
  }
  cut_y[1] ~ dnorm(0, 0.1)
  for (k in 2:(C-1)) {
    delta_y[k] ~ dgamma(2, 2)
    cut_y[k] <- cut_y[k-1] + delta_y[k]
  }
}
"

model_file <- tempfile(fileext = ".txt")
writeLines(model_string, model_file)


# ------------------------------------------------------------
# Run
# ------------------------------------------------------------

params <- c(
  "a_v", "B_v", "Sigma",
  "a_i", "g", "B_i", "tau_i",
  "a_m", "e_m", "d_m", "B_m",
  "e_gm", "d_gm", "B_gm", "cut_gm",
  "c_ideo", "c_m", "c_gm", "h", "B_y", "cut_y"
)

set.seed(123)
Sys.time()
fit_test <- jags(
  data = jags_data, inits = NULL,
  parameters.to.save = params, model.file = model_file,
  n.chains = 4, n.adapt = 1000,
  n.burnin = 2000, n.iter = 7000, n.thin = 1,
  parallel = TRUE, DIC = FALSE
)
Sys.time()

# ------------------------------------------------------------
# Save (with a key so B_*[k] can be matched to variable names)
# ------------------------------------------------------------

coef_key <- data.frame(k = seq_along(demo), variable = demo)

saveRDS(list(fit = fit_test, coef_key = coef_key, vals = vals, med = med),
        here("Anna SEM", "AB_SEM_fit_test1110.rds"))

summary(fit_test$summary[, "Rhat"])   # want all below ~1.01 (1.1 at most)
summary(fit_test$summary[, "n.eff"])  # want at least ~400 for every parameter

print(fit)
