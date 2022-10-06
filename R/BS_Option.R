#' Calculate the value of a given European Option with Black & Scholes formulae as well as some Greeks for further information
#' @param Call Boolean; indicate if the option is an European Call or Put
#' @param S Numeric; Current stock value
#' @param T Numeric; Time to maturity
#' @param t Numeric; Current time (has to be between 0 and T)
#' @param K Numeric; Strike Price of the underlying option
#' @param r Numeric; Riskfree rate of interest
#' @param d Numeric; Annual dividend yield
#' @param sigma Numeric; Implicite Volatility of the underlying stock
#' @param ratio Numeric; Ratio of the option
#' @return Value of an European Call or Put Option, calculated by Black & Scholes formulae
#' @export

BS_Option <- function(
    Call = TRUE,
    S = 100,
    T = 1,
    t = 0,
    K = 75,
    r = 1,
    d = 0,
    sigma = 40,
    ratio = 1){

  # convert absolute values (in percentage) to ratio
  r     <- r/100
  sigma <- sigma/100
  d     <- d/100
  tau   <- T - t

  # calculate d1 and d2
  d1 <- (log((S * exp(-d * tau))/(K * exp(-r * tau))) + 1/2 * sigma^2 * tau)/(sigma * sqrt(tau))
  d2 <- d1 - sigma * sqrt(tau)

  # call, time to maturity > 0
  if (Call && tau > 0){

    return(
      list(
        Price = ratio * (S * exp(-d*tau) * pnorm(d1) - K * exp(-r*tau) * pnorm(d2)),
        Greeks = c(
          # d/dS V(S,t)
          Delta = exp(-d * tau) * pnorm(d1),
          # d^2/dS^2 V(S,t)
          Gamma = dnorm(d1) / (S * sigma * sqrt(tau)) * exp(-d * tau),
          # d/dsigma V(s,t)
          Vega = S * exp(-d * tau) * dnorm(d1),
          # d/dt V(S,t)
          Theta = -S * exp(-d * tau) * (dnorm(d1) * sigma)/(2 * sqrt(tau)) - r * K * exp(-r * tau) * pnorm(d2) + d * S * exp(-d * tau) * pnorm(d1),
          # d/dr V(S,t)
          Rho = K * tau * exp(-r * tau) * pnorm(d2)
          )
        )
    )
  }

  # call, time to maturity = 0
  if(Call && T == 0){
    return( Price = pmax( S-K , 0 ) )
  }

  # put, time to maturity > 0
  if(isFALSE(Call) && tau > 0){

    return(
      list(
        Price = ratio * (K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)),
        Greeks = c(
          Delta = -exp(-d * tau) * (1 - pnorm(d1)),
          Gamma = dnorm(d1) / (S * sigma * sqrt(T)) * exp(-d * tau),
          Vega = K * exp(-d * tau) * dnorm(d2) * sqrt(tau),
          Theta = -S * exp(-d * tau) * (dnorm(d1) * sigma)/(2 * sqrt(tau)) + r * K * exp(-r * tau) * pnorm(-d2) - d * S * exp(-d * tau) * pnorm(-d1),
          Rho = - K * tau * exp(-r * tau) * pnorm(-d2)
          )
        )
    )

    }
  # put, time to maturity = 0
  if(isFALSE(Call) && T == 0){

    return( Price = pmax( K-S , 0 ) )

  }
}
