# Different Bernoulli PMFs --------------------------------------------------------------------

par(mfrow = c(1,3)) 

theta <- 0.1 # true parameter
x <- c(0, 1)
probs <-  c(1 - theta, theta)
plot(x, probs,
     type="h",
     xlim = c(0,1),
     ylim = c(0,1),
     xlab=expression(italic(x)),
     ylab=expression(italic(f(x))),
     main = bquote(paste("Bernoulli PMF ", italic(theta), " = ", .(theta), sep = "")),
     col = gray(.5),
     lwd = 5,
     xaxt = "n")
axis(1, at = c(0, 1), labels = c(0, 1))


theta <- 0.5 # true parameter
x <- c(0, 1)
probs <-  c(1 - theta, theta)
plot(x, probs,
     type="h",
     xlim = c(0,1),
     ylim = c(0,1),
     xlab=expression(italic(x)),
     ylab=expression(italic(f(x))),
     main = bquote(paste("Bernoulli PMF ", italic(theta), " = ", .(theta), sep = "")),
     col = gray(.5),
     lwd = 5,
     xaxt = "n")
axis(1, at = c(0, 1), labels = c(0, 1))


theta <- 0.9 # true parameter
x <- c(0, 1)
probs <-  c(1 - theta, theta)
plot(x, probs,
     type="h",
     xlim = c(0,1),
     ylim = c(0,1),
     xlab=expression(italic(x)),
     ylab=expression(italic(f(x))),
     main = bquote(paste("Bernoulli PMF ", italic(theta), " = ", .(theta), sep = "")),
     col = gray(.5),
     lwd = 5,
     xaxt = "n")
axis(1, at = c(0, 1), labels = c(0, 1))

# Different Binomial PMFs   -----------------------------------------------------------------------------------

par(mfrow = c(1,3))

n <- 10      # number of trails
theta <- 0.1 # true parameter
x <- 0:n
probs <- dbinom(x, size = n, prob = theta)

plot(x, probs,
     type = "h",
     xlim = c(0, n),
     ylim = c(0, max(probs)),
     xlab = expression(italic(x)),
     ylab = expression(italic(f(x))),
     main = bquote(paste("Binomial PMF ", italic(n), " = ", .(n), " and ", italic(theta), " = ", .(theta))),
     col = gray(0.5),
     lwd = 5,
     xaxt = "n")
axis(1, at = 0:n, labels = 0:n)

theta <- 0.5 # true parameter
x <- 0:n
probs <- dbinom(x, size = n, prob = theta)
plot(x, probs,
     type = "h",
     xlim = c(0, n),
     ylim = c(0, max(probs)),
     xlab = expression(italic(x)),
     ylab = expression(italic(f(x))),
     main = bquote(paste("Binomial PMF ", italic(n), " = ", .(n), " and ", italic(theta), " = ", .(theta))),
     col = gray(0.5),
     lwd = 5,
     xaxt = "n")
axis(1, at = 0:n, labels = 0:n)

theta <- 0.9 # true parameter
x <- 0:n
probs <- dbinom(x, size = n, prob = theta)
plot(x, probs,
     type = "h",
     xlim = c(0, n),
     ylim = c(0, max(probs)),
     xlab = expression(italic(x)),
     ylab = expression(italic(f(x))),
     main = bquote(paste("Binomial PMF ", italic(n), " = ", .(n), " and ", italic(theta), " = ", .(theta))),
     col = gray(0.5),
     lwd = 5,
     xaxt = "n")
axis(1, at = 0:n, labels = 0:n)

# Different Normal PDFs ----------------------------------------------------------------------------------

par(mfrow =c(1,1))

mu <- 0     # true parameter
sigma <- 1  # true parameter

x <- seq(-4, 4, length = 100)
f_x <- dnorm(x, mean = mu, sd = sigma)

plot(x, f_x,
     type = "l",
     xlim = c(-4, 4),
     ylim = c(0, 0.5),
     xlab = expression(italic(x)),
     ylab = expression(italic(f(x))),
     main = "Normal PDFs",
     col = gray(.5),
     lwd = 3)

mu <- 1    # true parameter
sigma <- 1 # true parameter
x <- seq(-4, 4, length = 100)
f_x <- dnorm(x, mean = mu, sd = sigma)
lines(x, f_x,
      type = "l",
      col = gray(.1),
      lwd = 3)
mu <- -1
sigma <- 1
x <- seq(-4, 4, length = 100)
f_x <- dnorm(x, mean = mu, sd = sigma)
lines(x, f_x,
      type = "l",
      col = gray(.9),
      lwd = 3)
legend(1.8, 0.49,
       legend=c(bquote(paste( italic(mu)," = -1, ", italic(sigma), " = 1" )),
                bquote(paste( italic(mu)," =  0, ", italic(sigma), " = 1" )),
                bquote(paste( italic(mu)," =  1, ", italic(sigma), " = 1" ))
       ),
       col=c(gray(.9), gray(.5),gray(.1)),
       lty= rep(1,3),
       lwd = 3,
       cex = 1,
       box.lty=0)


# ML for 1 Bernounlli Trial ---------------------------------------------------------------------------------------------

par(mfrow =c(1,1))
theta <- seq(0, 1, 0.001)
L <- theta
plot(theta, L, type = "l", xlab=  bquote(paste(italic(theta))),
     ylab= bquote(paste("L(",italic(theta),")")))
max(L)

theta <- seq(0, 1, 0.001)
L <- 1-theta
plot(theta, L, type = "l",
     xlab=  bquote(paste(italic(theta))),
     ylab= bquote(paste("L(",italic(theta),")")))
max(L)

# ML for n Bernoulli Trials ---------------------------------------------------------------------------------------------

par(mfrow =c(1,1))

# parameter space
theta = seq(0, 1, 0.001)

# simulate data
set.seed(3479)
n <- 10000
x <- sample(c(0,1), size  = n, replace = T, prob = c(0.5,0.5))

# the likelihood function
L <- theta^(sum(x == 1)) * (1 - theta)^(n-sum(x == 1))


plot(theta,
     L,
     type = "l",
     xlab=  bquote(paste(italic(theta))),
     ylab= bquote(paste("L(",italic(theta),")")),
     col=c(gray(.5)),
     lwd = 3,
     main = paste0("Likelihood Function for ",n, " Bernoulli Trials")
     )

# the L  function maxima
max(L)

d <-  cbind(theta,L)

# ML is the mean
(MLE = d[which.max(d[,2]), 1])
mean(x)

points(MLE, max(L), pch = 19, col = "red")
abline(v = MLE, col = "red")
# As for the log-likelihood:

loglik <-  sum(x == 1) * log(theta) + (n - sum(x == 1)) * log(1 - theta)
d <- cbind(theta,loglik)
MLE <-  d[which.max(d[,2]), 1]
MLE


# Log Likelihood vs. Likelihood ---------------------------------------------------------------------------------------------

n <- 1000
true_p <- 0.7
set.seed(123)
data <- rbinom(n, size = 1, prob = true_p)
p_values <- seq(0.01, 0.99, by = 0.01)
likelihood <- sapply(p_values, function(p) {
  prod(p^data * (1 - p)^(1 - data))
})

log_likelihood <- sapply(p_values, function(p) {
  sum(data * log(p) + (1 - data) * log(1 - p))
})
par(mfrow = c(1, 2))

# Plot the likelihood function
plot(p_values, likelihood, type = "l",  col=c(gray(.5)),
     lwd = 3,
     xlab = expression(italic(theta)), ylab = "Likelihood",
     main = "Likelihood Function")

# Plot the log-likelihood function
plot(p_values, log_likelihood, type = "l",  col=c(gray(.5)),
     lwd = 3,
     xlab = expression(italic(theta)), ylab = "Log-Likelihood",
     main = "Log-Likelihood Function")


# Normalized posterior distributions ----------------------------------------------------------

n <- 100      # Number of trials
k <- 40       # Number of successes
alpha <- 2    # Prior alpha parameter
beta <- 2     # Prior beta parameter


# Define the normalized posterior function (Beta distribution)
normalized_posterior <- function(theta, k, n, alpha, beta) {
   dbeta(theta, k + alpha, n - k + beta)
}

# Parameter space
theta_vals <- seq(0, 1, length.out = 100)

norm_post_vals <- normalized_posterior(theta_vals, k, n, alpha, beta)


par(mfrow = c(1, 1))
plot(theta_vals, norm_post_vals, type = "l", col=c(gray(.5)),
     lwd = 3,
     main = 
        bquote(paste("Posterior for ", italic(theta), " (Beta PDF) ",)),
     xlab = expression(theta), ylab = "Density")

# Un-normalized posterior distributions ----------------------------------------------------------

n <- 100         # Number of trials
k <- 40          # Number of successes
alpha <- 2       # Alpha for the Beta prior
beta <- 2        # Beta for the Beta prior

# Define a grid of theta values (0 to 1)
theta_vals <- seq(0, 1, length.out = 1000)

# Define the normalized posterior function (Beta distribution)
normalized_posterior <- function(theta, k, n, alpha, beta) {
   dbeta(theta, k + alpha, n - k + beta)
}
norm_post_vals <- normalized_posterior(theta_vals, k, n, alpha, beta)

# Define the prior (Beta distribution)
prior <- function(theta, alpha, beta) {
   dbeta(theta, alpha, beta)
}

# Define the likelihood (Binomial distribution)
likelihood <- function(theta, k, n) {
   dbinom(k, size = n, prob = theta)
}

# Compute the prior and likelihood for each theta
prior_vals <- prior(theta_vals, alpha, beta)
likelihood_vals <- likelihood(theta_vals, k, n)

# Compute the unnormalized posterior (prior * likelihood)
unnormalized_posterior <- prior_vals * likelihood_vals

# Find MAP
maxpoint_normalized <- c(theta_grid[which.max(norm_post_vals)],
                         max(norm_post_vals)
)

maxpoint_unnormalized <- c(theta_grid[which.max(unnormalized_posterior)],
                           max(unnormalized_posterior)
)

# The estimations are identical 
maxpoint_normalized[1]
maxpoint_unnormalized[1]


par(mfrow = c(1, 2))
plot(theta_grid, 
     norm_post_vals, 
     type = "l", 
     col=c(gray(.5)),
     lwd = 3,
     main = 
        bquote(paste("Proper Posterior for ", italic(theta), " (Beta PDF) ",)),
     xlab = expression(theta), ylab = "Density")
points(maxpoint_normalized[1],
       maxpoint_normalized[2], pch = 19, col = "red")
abline(v = maxpoint_normalized[1], col = "red")

plot(theta_grid, 
     unnormalized_posterior, 
     type = "l", 
     col=c(gray(.5)),
     lwd = 3,
     main = 
        bquote(paste("Improper Posterior for ", italic(theta))),
     xlab = expression(theta), ylab = "Density")

points(maxpoint[1],
       maxpoint[2], pch = 19, col = "red")
abline(v = maxpoint[1], col = "red")
