% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulations.R
\name{spot_price_simulate}
\alias{spot_price_simulate}
\title{Simulate spot prices of an N-factor model through Monte Carlo simulation}
\usage{
spot_price_simulate(
  x_0,
  parameters,
  t = 1,
  dt = 1,
  N_simulations = 2,
  antithetic = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{x_0}{\code{vector}. Initial values of the state variables, where the length must correspond to the number of factors specified in the parameters.}

\item{parameters}{\code{vector}. A named vector of parameter values of a specified N-factor model. Function \code{NFCP_parameters} is recommended.}

\item{t}{\code{numeric}. Number of years to simulate.}

\item{dt}{\code{numeric}. Discrete time step, in years, of the Monte Carlo simulation.}

\item{N_simulations}{\code{numeric}. The total number of Monte Carlo simulations.}

\item{antithetic}{\code{logical}. Should antithetic price paths be simulated?}

\item{verbose}{\code{logical}. Should simulated state variables be output?}
}
\value{
\code{spot_price_simulate} returns a list when \code{verbose = T} and a matrix of simulated price paths when \code{verbose = F}. The returned objects in the list are:

\tabular{ll}{

\code{State_Variables} \tab A matrix of simulated state variables for each factor is returned when \code{verbose = T}. The number of factors returned corresponds to the number of factors in the specified N-factor model. \cr

\code{Prices} \tab A matrix of simulated price paths. Each column represents one simulated price path and each row represents one simulated observation. \cr

}
}
\description{
Simulate risk-neutral price paths of an an N-factor commodity pricing model through Monte Carlo Simulation.
}
\details{
\loadmathjax
The \code{spot_price_simulate} function is able to quickly and efficiently simulate a large number of state variables and risk-neutral price paths of a commodity following the N-factor model.
Simulating risk-neutral price paths of a commodity under an N-factor model through Monte Carlo simulations allows for the
valuation of commodity related investments and derivatives, such as American options and real Options through dynamic programming methods.
The \code{spot_price_simulate} function quickly and efficiently simulates an N-factor model over a specified number of years, simulating antithetic price paths as a simple variance reduction technique.
The \code{spot_price_simulate} function uses the \code{mvrnorm} function from the \code{MASS} package to draw from a multivariate normal distribution for the correlated simulation shocks of state variables.

The N-factor model stochastic differential equation is given by:

Brownian Motion processes (ie. factor one when \code{GBM = T}) are simulated using the following solution:

\mjdeqn{x_{1,t+1} = x_{1,t} + \mu^*\Delta t + \sigma_1 \Delta t Z_{t+1}}{x[1,t+1] = x[1,t] + mu^* * Delta t + sigma[1] * Delta t * Z[t+1]}

Where \mjeqn{\Delta t}{Delta t} is the discrete time step, \mjeqn{\mu^*}{mu^*} is the risk-neutral growth rate and \mjeqn{\sigma_1}{sigma[1]} is the instantaneous volatility. \mjeqn{Z_t}{Z[t]} represents the
independent standard normal at time \mjeqn{t}{t}.

Ornstein-Uhlenbeck Processes are simulated using the following solution:

\mjdeqn{x_{i,t} = x_{i,0}e^{-\kappa_it}-\frac{\lambda_i}{\kappa_i}(1-e^{-\kappa_it})+\int_0^t\sigma_ie^{\kappa_is}dW_s}{x[i,t] = x[i,0] * e^(-kappa[i] * t) - lambda[i]/kappa[i] * (1 - e^(-kappa[i] * t)) + int_0^t (sigma[i] *
e^(kappa[i] * s) dW[s])}

Where a numerical solution is obtained by numerically discretising and approximating the integral term using the Euler-Maruyama integration scheme:
\mjdeqn{\int_0^t\sigma_ie^{\kappa_is}dW_s = \sum_{j=0}^t \sigma_ie^{\kappa_ij}dW_s}{int_0^t ( sigma[i] e^(kappa[i] * s) dw[s])}

Finally, deterministic seasonality is considered within the spot prices of simulated price paths.
}
\examples{


# Example 1
## Simulate a geometric Brownian motion (GBM) process:
simulated_spot_prices <- spot_price_simulate(
 x_0 = log(20),
 parameters = c(mu_rn = (0.05 - (1/2) * 0.2^2), sigma_1 = 0.2),
 t = 1,
 dt = 1/12,
 N_simulations = 1e3)

# Example 2
## Simulate the Short-Term/Long-Term model:

### Step 1 - Obtain contemporary state variable estimates through the Kalman Filter:
SS_2F_filtered <- NFCP_Kalman_filter(parameter_values = SS_oil$two_factor,
                                    parameter_names = names(SS_oil$two_factor),
                                    log_futures = log(SS_oil$stitched_futures),
                                    dt = SS_oil$dt,
                                    futures_TTM = SS_oil$stitched_TTM,
                                    verbose = TRUE)

### Step 2 - Use these state variable estimates to simulate futures spot prices:
simulated_spot_prices <- spot_price_simulate(
 x_0 = SS_2F_filtered$x_t,
 parameters = SS_oil$two_factor,
 t = 1,
 dt = 1/12,
 N_simulations = 1e3,
 antithetic = TRUE,
 verbose = TRUE)
}
\references{
Schwartz, E. S., and J. E. Smith, (2000). Short-Term Variations and Long-Term Dynamics in Commodity Prices. \emph{Manage. Sci.}, 46, 893-911.

Cortazar, G., and L. Naranjo, (2006). An N-factor Gaussian model of oil futures prices. \emph{Journal of Futures Markets: Futures, Options, and Other Derivative Products}, 26(3), 243-268.
}
