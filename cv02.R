# =============================================================================
# Brownuv most -- jedna z moznosti generovani

W0 <- 0
dt <- 0.001
t <- seq (0, 1, by = dt)

W <- generuj.Wp (t, dt, W0)
W1 <- tail (W, 1)
X <- W - t * W1

plot (t, X, type = "l", lty = 1, xlab = "t", ylab = "Brownuv most")
abline (h = W0 , lty = 2)

M <- sapply (1:5 , function (k) {
	W <- generuj.Wp (t, dt, W0)
	W1 <- tail (W, 1)
	X <- W - t * W1
})

matplot (t, M, type = "l", lty = 1, xlab = "t", ylab = "Brownuv most")
abline (h = W0 , lty = 2)

# =============================================================================
# geometricky Brownuv pohyb -- verze bez pouziti cyklu

generuj.gBp <- function (t, dt, X0, r, sigma) {
	dW <- rnorm (length (t) - 1) * sqrt (dt)
	dX <- 1 + r * dt + sigma * dW
	X <- cumprod (c (X0 , dX))
}

# =============================================================================
# geometricky Brownuv pohyb -- verze s for-cyklem

generuj.gBp <- function (t, dt, X0, r, sigma) {
	X <- X0
	X.posledni <- X0
	n <- length (t)
	for (k in 2:n) {
		dW <- rnorm (1)	* sqrt (dt)
		dX <- (r * dt + sigma * dW) * X.posledni
		X.posledni <- X.posledni + dX
		X <- append (X, X.posledni)
	}
	X
}

# =============================================================================

X0 <- 100
dt <- 0.001
r <- 0
sigma <- 0.2
t <- seq (0, 1, by=dt)

X <- generuj.gBp (t, dt, X0, r, sigma)

plot (t, X, type="l", col="red", xlab="t", ylab="geometricky Brownuv pohyb")
abline (h = X0 , lty = 2)

# =============================================================================

X0 <- 100
dt <- 0.001
r <- 0
sigma <- 0.2
t <- seq (0, 1, by=dt)

M <- sapply (1:10 , function (k) {
	generuj.gBp (t, dt, X0, r, sigma)
})

matplot (t, M, type = "l", lty = 1, xlab = "t", ylab = "geometricky Brownuv pohyb")
abline (h = X0 , lty = 2)

# =============================================================================

X0 <- 100
dt <- 0.001
r <- 0
sigma <- 1
t <- seq (0, 1, by=dt)

M <- sapply (1:1000 , function (k) {
	generuj.gBp (t, dt, X0, r, sigma)
})

# matplot (t, M, type = "l", lty = 1, xlab = "t", ylab = "geometricky Brownuv pohyb", col = "grey")
# abline (h = X0 , lty = 2)

L <- log (M)
mean <- apply (L, 1, mean)
sd <- apply (L, 1, sd)

matplot (t, cbind (mean, sd), type = "l", lty = 1, xlab = "t", ylab = "EX, SD")

# =============================================================================
