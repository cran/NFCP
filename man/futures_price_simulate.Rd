% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulations.R
\name{futures_price_simulate}
\alias{futures_price_simulate}
\title{Simulate futures prices of an N-factor model through Monte Carlo simulation}
\usage{
futures_price_simulate(
  x_0,
  parameters,
  dt,
  N_obs,
  futures_TTM,
  ME_TTM = NULL,
  verbose = TRUE
)
}
\arguments{
\item{x_0}{\code{vector}. Initial values of the state variables, where the length must correspond to the number of factors specified in the parameters.}

\item{parameters}{\code{vector}. A named vector of parameter values of a specified N-factor model. Function \code{NFCP_parameters} is recommended.}

\item{dt}{\code{numeric}. Discrete time step, in years, of the Monte Carlo simulation.}

\item{N_obs}{\code{numeric}. Number of discrete observations to simulate.}

\item{futures_TTM}{\code{vector} or \code{matrix}.  The time-to-maturity of observed futures contracts, in years, at a given observation date. This time-to-maturity can either be constant (ie. class 'vector') or variable (ie. class 'matrix') across observations.
The number of rows of object 'futures_TTM' must be either 1 or equal to argument 'N_obs'. NA values are allowed.}

\item{ME_TTM}{\code{vector}. the time-to-maturity groupings to consider for observed futures prices. The length of \code{ME_TTM} must be equal to the number of 'ME' parameters specified in object 'parameter_names'. The maximum of 'ME_TTM' must be greater than the maximum value of 'futures_TTM'.
When the number of 'ME' parameter values is equal to one or the number of columns of object 'log_futures', this argument is ignored.}

\item{verbose}{\code{logical}. Should simulated state variables be output?}
}
\value{
\code{futures_price_simulate} returns a list with three objects when \code{verbose = T} and a matrix of simulated futures prices when \code{verbose = F}. The list objects returned are:

#'\tabular{ll}{

\code{state_vector} \tab  A \code{matrix} of Simulated state variables at each discrete time point. The columns represent each factor of the N-factor model and the rows represent
the simulated values at each discrete simulated time point. \cr

\code{futures_prices} \tab  A \code{matrix} of Simulated futures prices, with each column representing a simulated futures contract. \cr

\code{spot_prices} \tab A vector of simulated spot prices \cr
}
}
\description{
Simulate Futures price data with dynamics that follow the parameters of an N-factor model through Monte Carlo simulation.
}
\details{
\loadmathjax
The \code{futures_price_simulate} function simulates futures price data using the Kalman filter algorithm, drawing from a normal
distribution for the shocks in the transition and measurement equations at each discrete time step. At each discrete time point,
an observation of the state vector is generated through the transition equation, drawing from a normal distribution with a covariance equal to \mjeqn{Q_t}{Q[t]}.
Following this, simulated futures prices are generated through the measurement equation, drawing from a normal distribution with covariance matrix equal to \mjeqn{H}{H}.

Input \code{futures_TTM} can be either a matrix specifying the constant time to maturity of futures contracts to simulate, or it can be a matrix where \code{nrow(futures_TTM) == N_obs} for the time-varying time to maturity of the futures contracts to simulate. This allows for the simulation of both aggregate stitched data and individual futures contracts.
}
\examples{


# Example 1 - Simulate Crude Oil with constant time-to-maturity:

simulated_futures <- futures_price_simulate(x_0 = c(log(SS_oil$spot[1,1]), 0),
                                           parameters = SS_oil$two_factor,
                                           dt = SS_oil$dt,
                                           N_obs = nrow(SS_oil$stitched_futures),
                                           futures_TTM = SS_oil$stitched_TTM)

##Simulate Crude Oil Contracts with a rolling-window of measurement error:

simulated_futures_prices <- futures_price_simulate(x_0 = c(log(SS_oil$spot[1,1]), 0),
                                                  parameters = SS_oil$two_factor,
                                                  dt = SS_oil$dt,
                                                  N_obs = nrow(SS_oil$contracts),
                                                  futures_TTM = SS_oil$contract_maturities,
                                                  ME_TTM = c(1/4, 1/2, 1, 2, 5))
}
\references{
Schwartz, E. S., and J. E. Smith, (2000). Short-Term Variations and Long-Term Dynamics in Commodity Prices. \emph{Manage. Sci.}, 46, 893-911.

Cortazar, G., and L. Naranjo, (2006). An N-factor Gaussian model of oil futures prices. \emph{Journal of Futures Markets: Futures, Options, and Other Derivative Products}, 26(3), 243-268.
}
