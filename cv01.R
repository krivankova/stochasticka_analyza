# =============================================================================

W0 <- 0
dt <- 0.001
Wt <- W0
W <- W0
t <- seq (0 , 1 , by = dt )

for ( k in 1:1000) {
dW <- sqrt ( dt ) * rnorm (1)
Wt <- Wt + dW
W <- append (W , Wt )
}

plot (t , W , type = "l" , col = "red" , xlab = "t" , ylab = "W")
abline ( h = W0 , lty = 2)

# =============================================================================

W0 <- 0
dt <- 0.001
W <- W0
t <- seq (0 , 1 , by = dt )

dW <- rnorm ( length ( t ) - 1) * sqrt ( dt )
W <- cumsum ( c ( W0 , dW ))

plot (t , W , type = "l" , col = "red" , xlab = "t" , ylab = "W")
abline ( h = W0 , lty = 2)

# =============================================================================

generuj.Wp <- function (t , dt , W0 ) {
	dW <- rnorm (length (t) - 1) * sqrt (dt)
	W <- cumsum (c (W0, dW))
}

W <- generuj.Wp (t , dt , W0 )

plot (t , W , type = "l" , col = "red" , xlab = "t" , ylab = "W")
abline ( h = W0 , lty = 2)

# =============================================================================

M <- sapply (1:10 , function (k) {
	generuj.Wp (t , dt , W0 )
})

matplot (t, M, type = "l", lty = 1, xlab = "t", ylab = "W", main = "trajektorie W")
abline (h = W0 , lty = 2)

# =============================================================================
# ukol

M <- sapply (1:1000 , function (k) {
  generuj.Wp (t , dt , W0 )
})

matplot (t, M, type = "l", lty = 1, lwd = 0.5, col="grey", xlab = "t", ylab = "W", main = "trajektorie W")
#abline (h = W0 , lty = 2)

# =============================================================================

index <- which (t == 0.6)
vyber <- M[index ,]

hist (vyber, breaks = 50, freq = FALSE)

qqnorm (vyber, pch = "+")
qqline (vyber, col = "blue")

library (Hmisc)
Ecdf ( vyber )

x <- seq (-2 ,2 , by=0.1)
y <- pnorm (x , mean = 0 , sd = sqrt (0.6))
lines (x, y, col="red")

ks.test (vyber, pnorm, mean=0 , sd=sqrt (0.6))
ks.test (vyber, pnorm, mean=1 , sd=sqrt (0.6))
ks.test (vyber, pnorm, mean=0 , sd=1)

# =============================================================================
