% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dependencies.R
\name{stitch_contracts}
\alias{stitch_contracts}
\title{Stitch futures contracts}
\usage{
stitch_contracts(
  futures,
  futures_TTM = NULL,
  maturity_matrix = NULL,
  rollover_frequency = NULL,
  contract_numbers = NULL,
  verbose = FALSE
)
}
\arguments{
\item{futures}{Contract futures price data. Each row of \code{Futures} should represent one observation of futures prices and each column should represent one quoted futures contract. NA's in \code{Futures} are allowed, representing missing observations.}

\item{futures_TTM}{A \code{vector} of contract maturities to stitch}

\item{maturity_matrix}{The time-to-maturity (in years) for each contract at each given observation point. The dimensions of \code{maturity_matrix} should match those of \code{Futures}}

\item{rollover_frequency}{the frequency (in years) at which contracts should be rolled over}

\item{contract_numbers}{A \code{vector} of contract numbers offset from the closest-to-maturity contract at which to stitch contracts.}

\item{verbose}{\code{logical}. Should additional information be output? see \bold{details}}
}
\value{
\code{stitch_contracts} returns a matrix of stitched futures prices if \code{verbose = T} and a list with two or three objects otherwise (see below).

\tabular{ll}{

\code{prices} \tab A data frame of Stitched futures prices. Each row represents an observation of the specified contracts. \cr

\code{maturities} \tab A data frame of the time-to-maturity of observed futures prices. Each row represents an observation of the
specified contracts. Returned only when \bold{Method 1} is used (see \bold{Details})  and \code{verbose = T}. \cr

\code{tickers} \tab  A data frame of the named columns of observed futures prices (e.g. contract tickers). Returned only when \code{Futures} or \code{maturity_matrix} have named columns and \code{verbose = T}. \cr
}
}
\description{
Aggregate futures contract price data by stitching according to either approximate maturities and rollover frequency or contract number from closest maturity.
}
\details{
This function aggregates a set of futures contract data by stitching contract data over an observation period, resulting in a
set of futures observations that is 'complete' (ie. Does not feature missing observations). Aggregated futures
data benefit from several computational efficiencies compared to raw contract data, but results in the loss of futures price information.

There are two methods of the \code{stitch_contracts} function that can be utilized the stitch contracts:

\bold{Method 1}

\code{stitch_contracts(futures, contract_numbers, verbose = T)}
Futures data may be aggregated by stitching prices according to maturity matching. This method requires the inputs \code{futures_TTM}, \code{maturity_matrix} and \code{rollover_frequency}.
This method stitched contracts by matching the observation prices according to which contract has the closest time-to-maturity of the desired maturity specified
in \code{futures_TTM}. Contracts are rolled over at the frequency specified in \code{rollover_frequency}.

\bold{Method 2}

\code{stitch_contracts(futures, futures_TTM, maturity_matrix, rollover_frequency, verbose = T)}
Futures data may be stitched according to the contract numbers offset from the closest-to-maturity contract. This method requires only the
input \code{contract_numbers} specifying which contracts should be included. This method is most appropriate when the maturity of available
contracts are consistent (ie. contracts expire every month or three months).
}
\examples{


##These examples approximately replicate the Crude Oil data utilized within the
##prominent work of Schwartz and Smith (2000):

###Method 1 - Stitch crude oil contracts according to maturity matching:
SS_stitched_M1 <- stitch_contracts(futures = SS_oil$contracts,
                                  futures_TTM = c(1, 5, 9, 13, 17)/12,
                                  maturity_matrix = SS_oil$contract_maturities,
                                  rollover_frequency = 1/12, verbose = TRUE)

###Method 2 - Stitch crude oil contracts according to nearest contract numbers:
SS_stitched_M2 <- stitch_contracts(futures = SS_oil$contracts,
                                  contract_numbers = c(1, 5, 9, 13, 17), verbose = TRUE)

}
\references{
Schwartz, E. S., and J. E. Smith, (2000). Short-Term Variations and Long-Term Dynamics in Commodity Prices. \emph{Manage. Sci.}, 46, 893-911.

Cortazar, G., and L. Naranjo, (2006). An N-factor Gaussian model of oil futures prices. \emph{Journal of Futures Markets: Futures, Options, and Other Derivative Products}, 26(3), 243-268.
}
