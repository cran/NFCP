% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameter_estimation.R
\name{NFCP_MLE}
\alias{NFCP_MLE}
\title{N-factor model parameter estimation through the Kalman filter and maximum likelihood estimation}
\usage{
NFCP_MLE(
  log_futures,
  dt,
  futures_TTM,
  N_factors,
  N_season = 0,
  N_ME = 1,
  ME_TTM = NULL,
  GBM = TRUE,
  estimate_initial_state = FALSE,
  Domains = NULL,
  cluster = FALSE,
  ...
)
}
\arguments{
\item{log_futures}{\code{matrix}. The natural logarithm of observed futures prices. Each row must correspond to quoted futures prices at a particular date and every column must correspond to a unique futures contract.
NA values are allowed.}

\item{dt}{\code{numeric}. Constant, discrete time step of observations, in years.}

\item{futures_TTM}{\code{vector} or \code{matrix}.  The time-to-maturity of observed futures contracts, in years, at a given observation date. This time-to-maturity can either be constant (ie. class 'vector') or variable (ie. class 'matrix') across observations.
The number of columns of 'futures_TTM' must be identical to the number of columns of object 'log_futures'. The number of rows of object 'futures_TTM' must be either 1 or equal to the number of rows of object 'log_futures'.}

\item{N_factors}{\code{numeric}. Number of state variables in the spot price process.}

\item{N_season}{\code{numeric}. The number of deterministic, cyclical seasonal factors to include in the spot price process.}

\item{N_ME}{\code{numeric}. The number of independent measuring errors of observable futures contracts to consider in the Kalman filter.}

\item{ME_TTM}{\code{vector}. the time-to-maturity groupings to consider for observed futures prices. The length of \code{ME_TTM} must be equal to the number of 'ME' parameters specified in object 'parameter_names'. The maximum of 'ME_TTM' must be greater than the maximum value of 'futures_TTM'.
When the number of 'ME' parameter values is equal to one or the number of columns of object 'log_futures', this argument is ignored.}

\item{GBM}{\code{logical}. When \code{TRUE}, factor 1 of the model is assumed to follow a Brownian Motion, inducing a unit-root in the spot price process.}

\item{estimate_initial_state}{\code{logical}. Should the initial state vector be specified as unknown parameters of the commodity pricing model? These are generally estimated with low precision (see \bold{details}).}

\item{Domains}{\code{matrix}. An option matrix of two columns specifying the lower and upper bounds for parameter estimation. The 'NFCP_domains' function is recommended. When not specified, the default parameter bounds returned by the 'NFCP_domains' function are used.}

\item{cluster}{\code{cluster}.    An optional object returned by one of the makeCluster commands in the \code{parallel} package to allow for parameter estimation
to be performed across multiple cluster nodes.}

\item{...}{additional arguments to be passed into the \code{genoud} genetic algorithm numeric optimization. These can highly influence the maximum likelihood estimation procedure. See \code{help(genoud)}}
}
\value{
\code{NFCP_MLE} returns a \code{list} with 10 objects. 9 objects are returned when the user has specified not to calculate the hessian matrix at solution.

\tabular{ll}{

\code{MLE} \tab \code{numeric} The Maximum-Likelihood-Estimate of the solution. \cr

\code{estimated_parameters} \tab \code{vector}. Estimated parameters. \cr

\code{standard_errors} \tab \code{vector}. Standard error of the estimated parameters. Returned only when \code{hessian = T} is specified.  \cr

\code{Information Criteria} \tab \code{vector}. The Akaikie and Bayesian Information Criterion. \cr

\code{x_t} \tab \code{vector}. The final observation of the state vector.

When deterministic seasonality is considered, it also returns the observation point

along the deterministic curve. \cr

\code{X} \tab \code{matrix}. Optimal one-step-ahead state vector.

When deterministic seasonality is considered, it also returns the observation point

along the deterministic curve. \cr

\code{Y} \tab \code{matrix}. Estimated futures prices. \cr

\code{V} \tab \code{matrix}. Estimation error. \cr

\code{Filtered Error} \tab \code{matrix}. positive mean error (high bias), negative mean error (low bias),

mean error (bias) and root mean squared error (RMSE)

of the filtered values to observed futures prices.  \cr

\code{Term Structure Fit} \tab \code{matrix}. The mean error (Bias), mean absolute error, standard deviation of error

 and root mean squared error (RMSE) of each observed futures contract. \cr

\code{Term Structure Volatility Fit} \tab \code{matrix}. Theoretical and empirical volatility of observed futures contract returns \cr

\code{proc_time} \tab \code{list}. The real and CPU time (in seconds) the \code{NFCP_MLE} function has taken. \cr

\code{genoud_value} \tab \code{list}. Outputs of \code{genoud}.

}
}
\description{
\loadmathjax
The \code{NFCP_MLE} function performs parameter estimation of commodity pricing models under the N-factor framework of Cortazar and Naranjo (2006). It uses term structure futures data and estimates unknown parameters through maximum likelihood estimation.
\code{NFCP_MLE} allows for missing observations, a variable number of state variables, deterministic seasonality and a variable number of measurement error terms.
}
\details{
The \code{NFCP_MLE} function facilitates parameter estimation of commodity pricing models under the N-factor framework through the Kalman filter and maximum likelihood estimation. \code{NFCP_MLE}
uses genetic algorithms through the \code{genoud} function of the \code{rgenoud} package to numerically optimize the log-likelihood score returned from the \code{NFCP_Kalman_filter} function.

Parameter estimation of commodity pricing models can involve a large number of observations, state variables and unknown parameters. It also features an objective log-likelihood function that is nonlinear and
discontinuous with respect to model parameters. \code{NFCP_MLE} is designed to perform parameter estimation as efficiently as possible, maximizing the likelihood of attaining a global optimum.


Arguments passed to the \code{genoud} function can greatly influence estimated parameters as well as computation time and must be considered when performing parameter estimation. All arguments of the \code{genoud} function
may be passed through the \code{NFCP_MLE} function.


When \code{grad} is not specified, the \code{grad} function from the \code{numDeriv} package is called to
approximate the gradient within the \code{genoud} optimization algorithm through Richardsons extrapolation.

Richardsons extrapolation is regarded for its ability to improve the approximation of estimation methods,
which may improve the likelihood of obtained a global maxmimum estimate of the log-likelihood.

The population size can highly influence parameter estimates at the expense of increased computation time. For commodity pricing models with a large number of unknown parameters, large population sizes may be necessary to maximize the estimation process.

\code{NFCP_MLE} by default performs boundary constrained optimization of log-likelihood scores and does not allow does not allow for out-of-bounds evaluations within
the \code{genoud} optimization process, preventing candidates from straying beyond the bounds provided by argument \code{Domains}.

When \code{Domains} is not specified, the default bounds specified by the \code{NFCP_domains} function are used. The size of the search domains of unknown parameters can highly
influence the computation time of the \code{NFCP_MLE} function, however setting domains that are too restrictive may result in estimated parameters returned at the upper or lower bounds. Custom search domains can be used
through the \code{NFCP_domains} function and subsequently the \code{Domains} argument of this function.

Finally, the maximum likelihood estimation process of parameters provides no in-built guarantee that the estimated parameters of commodity models are financially sensible results. When the commodity model has been over-parameterized
(i.e., the number of factors N specified is too high) or the optimization algorithm has failed to attain a global maximum likelihood estimate, estimated parameters may be irrational.

Evidence of irrational parameter estimates include correlation coefficients that are extremely large (e.g., > 0.95 or < -0.95), risk-premiums or drift terms that are unrealistic, filtered state variables that are unrealistic and extremely large/small mean-reverting terms with associated large standard errors.

Irrational parameter estimates may indicate that the number of stochastic factors (i.e., \code{N_factors}) of the model or number of seasonal factors (i.e., \code{N_season}) are too high.

\bold{The N-factor model}
The N-factor framework was first presented in the work of Cortazar and Naranjo (2006, equations 1-3).
It is a risk-premium class of commodity pricing model, in which futures prices are given by discounted expected future spot prices,
where these spot prices are discounted at a given level of risk-premium, known as the cost-of-carry.

The N-factor framework describes the spot price process of a commodity as the correlated sum of \mjeqn{N}{N} state variables \mjeqn{x_t}{x[t]}. The 'NFCP' package also allows for a deterministic,
cyclical seasonal function \mjeqn{season(t)}{season(t)} to be considered.

When \code{GBM = TRUE}:
\mjdeqn{log(S_{t}) = season(t) + \sum_{i=1}^N x_{i,t}}{log(S[t]) = season(t) + sum_{i=1}^n x[i,t]}
When \code{GBM = FALSE}:
\mjdeqn{log(S_{t}) = E + season(t) + \sum_{i=1}^N x_{i,t}}{log(S[t]) = E + season(t) + sum_{i=1}^n x[i,t]}

Where \code{GBM} determines whether the first factor follows a Brownian Motion or Ornstein-Uhlenbeck process to induce a unit root in the spot price process.

When \code{GBM = TRUE}, the first factor corresponds to the spot price, and additional N-1 factors model the cost-of-carry.

When \code{GBM = FALSE}, the commodity model assumes that there is a long-term equilibrium the commodity price will tend towards over time, with model volatility a decreasing function of time. This is not the standard approach made in the commodity pricing literature (Cortazar and Naranjo, 2006).

State variables are thus assumed to follow the following processes:

When \code{GBM = TRUE}:
\mjdeqn{dx_{1,t} = \mu^*dt + \sigma_{1} dw_{1}t}{dx[1,t] = mu^* dt + sigma[1] dw[1]t}

When \code{GBM = FALSE}:
\mjdeqn{dx_{1,t} = - (\lambda_{1} + \kappa_{1}x_{1,t})dt + \sigma_{1} dw_{1}t}{dx[1,t] = - (lambda[1] + kappa[1] x[1,t]) dt + sigma[1] dw[t]t}

And:
\mjdeqn{dx_{i,t} =_{i\neq 1} - (\lambda_{i} + \kappa_{i}x_{i,t})dt + \sigma_{i} dw_{i}t}{dx[i,t] =_(i != 1) - (lambda[i] + kappa[i] x[i,t]dt + sigma[i] dw[i]t)}

where:
\mjdeqn{E(w_{i})E(w_{j}) = \rho_{i,j}}{E(w[i])E(w[j])}

Additionally, the deterministic seasonal function (if specified) is given by:

\mjdeqn{season(t) = \sum_{i=1} ( season_{i,1} cos(2i\pi) + season_{i,2} sin(2i\pi)}{season(t) = sum_{i=1} ( season_{i,1} cos(2.i.pi) + season_{i,2} sin(2.i.pi)}

The addition of deterministic, cyclical seasonality as a function of trigonometric variables was first suggested by Hannan, Terrell, and Tuckwell (1970) and first applied to model commodities by Sørensen (2002).

The following constant parameters are defined as:

\code{var} \mjeqn{\mu}{mu}:  long-term growth rate of the Brownian Motion process.

\code{var} \mjeqn{E}{E}: Constant equilibrium level.

\code{var} \mjeqn{\mu^*=\mu-\lambda_1}{mu^* = mu-lambda[1]}: Long-term risk-neutral growth rate

\code{var} \mjeqn{\lambda_{i}}{lambda[i]}: Risk premium of state variable \mjeqn{i}{i}.

\code{var} \mjeqn{\kappa_{i}}{kappa[i]}: Reversion rate of state variable \mjeqn{i}{i}.

\code{var} \mjeqn{\sigma_{i}}{sigma[i]}: Instantaneous volatility of state variable \mjeqn{i}{i}.

\code{var} \mjeqn{\rho_{i,j} \in [-1,1]}{rho[i,j] in [-1,1]}: Instantaneous correlation between state variables \mjeqn{i}{i} and \mjeqn{j}{j}.

Including additional factors within the spot-price process allow for additional flexibility (and possibly fit) to the term structure of a commodity.
The N-factor model nests simpler models within its framework, allowing for the fit of different N-factor models (applied to the same term structure data),
represented by the log-likelihood, to be directly compared with statistical testing possible through a chi-squared test. The AIC or BIC can also be used to compare models.

\bold{Disturbances - Measurement Error}:

The Kalman filtering algorithm assumes a given measure of measurement error or disturbance in the measurement equation (ie. matrix \mjeqn{H}{H}). Measurement errors can be interpreted as error in the
model's fit to observed prices, or as errors in the reporting of prices (Schwartz and Smith, 2000). These disturbances are typically assumed independent.

\code{var} \mjeqn{ME_i}{ME[i]} measurement error of contract \mjeqn{i}{i}.

where the measurement error of futures contracts \mjeqn{ME_i}{ME[i]} is equal to \code{'ME_'} [i] (i.e. \code{'ME_1'}, \code{'ME_2'}, ...) specified in arguments \code{parameter_values} and \code{parameter_names}.

There are three particular cases on how the measurement error of observations can be treated in the \code{NFCP_Kalman_filter} function:

\bold{Case 1:} Only one ME is specified. The Kalman filter assumes that the measurement error of observations are independent and identical.

\bold{Case 2:} One ME is specified for every observed futures contract. The Kalman filter assumes that the measurement error of observations are independent and unique.

\bold{Case 3:} A series of ME's are specified for a given grouping of maturities of futures contracts. The Kalman filter assumes that the measurement error of observations are independent and unique to their respective time-to-maturity.

Grouping of maturities for case 3 is specified through the \code{ME_TTM} argument. This is a vector that specifies the maximum maturity to consider for each respective ME parameter argument.

in other words, ME_1 is considered for observations with TTM less than ME_TTM[1], ME_2 is considered for observations with TTM less than ME_TTM[2], ..., etc.

The first case is clearly the simplest to estimate, but can be a restrictive assumption. The second case is clearly the most difficult to estimate, but can be an infeasible assumption when considering all available futures contracts that make up the term structure of a commodity.

Case 3 thus serves to ease the restriction of case 1, and allow the user to make the modeling of measurement error as simple or complex as desired for a given set of maturities.

\bold{Diffuse Kalman filtering}

If the initial values of the state vector are not supplied within the \code{parameter_names} and \code{parameter_values} vectors, a 'diffuse' assumption is used within the Kalman filtering algorithm.
Initial states of factors that follow an Ornstein-Uhlenbeck are assumed to equal zero.
The initial state of the first factor, given that it follows a Brownian motion, is assumed equal to the first element of \code{log_futures}. This is an
assumption that the initial estimate of the spot price is equal to the closest to maturity observed futures price.

The initial states of factors that follow an Ornstein-Uhlenbeck have a transient effect on future observations. This makes the diffuse assumption reasonable and further means that initial states cannot generally be accurately estimated.
}
\examples{
# Estimate a 'one-factor' geometric Brownian motion model:
Oil_1F_estimated_model <- NFCP_MLE(
## Arguments
log_futures = log(SS_oil$contracts)[1:20,1:5],
dt = SS_oil$dt,
futures_TTM= SS_oil$contract_maturities[1:20,1:5],
N_factors = 1, N_ME = 1,
## Genoud arguments:
pop.size = 4, print.level = 0, gr = NULL,
max.generations = 0)
}
\references{
Hannan, E. J., et al. (1970). "The seasonal adjustment of economic time series." \emph{International economic review}, 11(1): 24-52.

Schwartz, E. S., and J. E. Smith, (2000). Short-Term Variations and Long-Term Dynamics in Commodity Prices. \emph{Manage. Sci.}, 46, 893-911.

Sørensen, C. (2002). "Modeling seasonality in agricultural commodity futures." \emph{Journal of Futures Markets: Futures, Options, and Other Derivative Products} 22(5): 393-426.

Cortazar, G., and L. Naranjo, (2006). An N-factor Gaussian model of oil futures prices. \emph{Journal of Futures Markets: Futures, Options, and Other Derivative Products}, 26(3), 243-268.

Mebane, W. R., and J. S. Sekhon, (2011). Genetic Optimization Using Derivatives: The rgenoud Package for R.
\emph{Journal of Statistical Software}, 42(11), 1-26. URL http://www.jstatsoft.org/v42/i11/.
}
