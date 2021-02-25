## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 6,
  fig.align = 'center'
)

## ----setup--------------------------------------------------------------------
library(NFCP)

# Set seed for reproducibility:
set.seed(412)

## -----------------------------------------------------------------------------
SS.Oil.Two.Factor.parameters <- NFCP.Parameters(N.factors = 2,
                                                      GBM = TRUE,
                                                      Initial.State = FALSE,
                                                      S.Constant = FALSE,
                                                      N.contracts = 5)
## Print the vector of parameters of the specified commodity pricing model:
print(SS.Oil.Two.Factor.parameters)

## -----------------------------------------------------------------------------
###Method 1 - Stitch Crude Oil Contracts according to maturity matching:
SSOilStitched <- Stitch.Contracts(Futures = SS.Oil$Contracts,
TTM = c(1, 5, 9, 13, 17)/12, maturity.matrix = SS.Oil$Contract.Maturities,
rollover.frequency = 1/12, verbose = TRUE)

##Plot the Stitched Maturities:
matplot(as.Date(rownames(SSOilStitched$Maturities)), SSOilStitched$Maturities, 
        type = 'l', main = "Stitched Contract Maturities", 
        ylab = "Time To Maturity (Years)", xlab = "Date", col = 1)

## -----------------------------------------------------------------------------
print(SS.Oil$Two.Factor)

## -----------------------------------------------------------------------------
##Example 1 - Replicating the Schwartz and Smith (2000)
##Two-Factor Crude Oil commodity pricing model:

SS.Oil.2F <- NFCP.Kalman.filter(
 parameter.values = SS.Oil$Two.Factor,
 parameters = names(SS.Oil$Two.Factor),
 log.futures = log(SS.Oil$Stitched.Futures),
 TTM = SS.Oil$Stitched.TTM,
 dt = SS.Oil$dt,
 verbose = TRUE,
 debugging = TRUE)

## -----------------------------------------------------------------------------
### Assume constant white noise in parameters of 1%:
SS.Oil.Two.Factor <- SS.Oil$Two.Factor
SS.Oil.Two.Factor <- SS.Oil.Two.Factor[!grepl("contract", names(SS.Oil.Two.Factor))]
SS.Oil.Two.Factor["sigma.contracts"] <- 0.01

SS.Oil.2F.Contracts <- NFCP.Kalman.filter(
 parameter.values = SS.Oil.Two.Factor,
 parameters = names(SS.Oil.Two.Factor),
 log.futures = log(SS.Oil$Contracts),
 TTM = SS.Oil$Contract.Maturities,
 dt = SS.Oil$dt,
 verbose = TRUE,
 debugging = TRUE)

## -----------------------------------------------------------------------------
# Estimate a GBM model:
SS.Oil.1F <- NFCP.MLE(
      ## Arguments
      log.futures = log(SS.Oil$Stitched.Futures),
      dt = SS.Oil$dt,
      TTM= SS.Oil$Stitched.TTM,
      S.Constant = FALSE, 
      N.factors = 1,
      GBM = TRUE,
      cluster = NULL,
      Richardsons.Extrapolation = TRUE,
      ## Genoud arguments:
      pop.size = 1000, optim.method = "L-BFGS-B", print.level = 0, hessian = TRUE,
      max.generations = 100, solution.tolerance = 0.1)

##Print results:
print(cbind(`Estimated Parameters` = round(SS.Oil.1F$Estimated.Parameters,4), 
            `Standard Error` = round(SS.Oil.1F$Standard.Errors,4)))

## -----------------------------------------------------------------------------
print(c(`Log Likelihood: One-Factor` = SS.Oil.1F$MLE))

## -----------------------------------------------------------------------------
print(c(`Log Likelihood: Two-Factor` = SS.Oil.2F$LL))

## -----------------------------------------------------------------------------
##Replicate Table 3 of Schwartz and Smith (2000):
print(round(SS.Oil.2F$TSFit.Error,4))

## -----------------------------------------------------------------------------
Bias <- c(`One-Factor` = SS.Oil.1F$Filtered.Error["Bias"], 
          `Two-Factor` = SS.Oil.2F$Filtered.Error["Bias"])
RMSE <- c(`One-Factor` = SS.Oil.1F$Filtered.Error["RMSE"], 
          `Two-Factor` = SS.Oil.2F$Filtered.Error["RMSE"])
print(round(cbind(RMSE, Bias), 4))


## ---- out.width='.49\\linewidth'----------------------------------------------
##One Factor
matplot(as.Date(rownames(SS.Oil.1F$V)), SS.Oil.1F$V, type = 'l',
xlab = "", ylab = "Observation Error",
main = "Contract Observation Error: One-Factor Model"); legend("bottomright", 
colnames(SS.Oil.2F$V),col=seq_len(5),cex=0.8,fill=seq_len(5))

##Two-Factor
matplot(as.Date(rownames(SS.Oil.2F$V)), SS.Oil.2F$V, type = 'l',
xlab = "", ylab = "Observation Error", ylim = c(-0.3, 0.2),
main = "Contract Observation Error: Two-Factor Model"); legend("bottomright", 
colnames(SS.Oil.2F$V),col=seq_len(5),cex=0.8,fill=seq_len(5))

## -----------------------------------------------------------------------------
matplot(cbind(SS.Oil.1F$TSFit.Error["RMSE",], SS.Oil.2F$TSFit.Error["RMSE",]), 
     type = 'l', main = "Root Mean Squared Error of Futures Contracts", 
     xlab = "Contract", ylab = "RMSE"); legend("right",c("One-Factor", "Two-Factor"),
                                               col=1:2,cex=0.8,fill=1:2)

## -----------------------------------------------------------------------------
##Replicate Figure 4 of Schwartz and Smith (2000):
SS.Figure.4 <- cbind(`Equilibrium Price` =
                     exp(SS.Oil.2F$X[,1]),
                    `Spot Price` =
                     SS.Oil.2F$Y[,"filtered Spot"])

matplot(as.Date(rownames(SS.Figure.4)), SS.Figure.4, type = 'l',
xlab = "", ylab = "Oil Price ($/bbl, WTI)", col = 1,
 main = "Estimated Spot and Equilibrium Prices for the Futures Data")

## -----------------------------------------------------------------------------
Observation.Dates <- as.Date(rownames(SS.Oil$Contracts))

plot(Observation.Dates, sqrt(SS.Oil.2F$P_t[1,1,]), type = 'l', 
     xlab = "Date", ylab = "Std. Dev.")


## -----------------------------------------------------------------------------
plot(Observation.Dates, sqrt(SS.Oil.2F$P_t[2,2,]), type = 'l', 
     xlab = "Date", ylab = "Std. Dev.")

## -----------------------------------------------------------------------------
plot(as.Date(rownames(SS.Oil$Contracts)), sqrt(SS.Oil.2F.Contracts$P_t[1,1,]), 
     type = 'l', xlab = "Date", ylab = "Std. Dev.", 
     main = "Time Series of the Std. Dev for State Variable 1")

## -----------------------------------------------------------------------------
plot(x = Observation.Dates, sqrt(SS.Oil.2F.Contracts$P_t[2,2,]), 
     type = 'l', xlab = "Date", ylab = "Std. Dev.", 
     main = "Time Series of the Std. Dev for State Variable 2")

## -----------------------------------------------------------------------------
##Figure 1 and 2 of Schwartz and Smith (2000) was developed using Enron data
##and an assumption that mu was approximately 3% p.a.:
Enron.values <- c(0.0300875, 0.0161, 0.014, 1.19, 0.115, 0.158, 0.189)
names(Enron.values) <- NFCP.Parameters(2, TRUE, FALSE, FALSE, 0, FALSE)

## -----------------------------------------------------------------------------
## Replicate figure 1 of Schwartz and Smith (2000):

SS.Expected.Spot <- Spot.Price.Forecast(X.0 = c(2.857, 0.119),
                                           Enron.values,
                                           t = seq(0,9,1/12),
                                           Percentiles = c(0.1, 0.9))
##Factor one only:
Equilibrium.theta <- Enron.values[!names(Enron.values) %in%
                                 c("kappa_2", "lambda_2", "sigma_2", "rho_1_2")]
SS.Expected.Equilibrium <- Spot.Price.Forecast(X.0 = c(2.857, 0),
                                                  Equilibrium.theta,
                                                  t = seq(0,9,1/12),
                                                  Percentiles = c(0.1, 0.9))
SS.Figure.1 <- cbind(SS.Expected.Spot, SS.Expected.Equilibrium)
matplot(seq(0,9,1/12), SS.Figure.1, type = 'l', col = 1, lty = c(rep(1,3), rep(2,3)),
xlab = "Time (Years)", ylab = "Spot Price ($/bbl, WTI)",
main = "Probabilistic Forecasts for Spot and Equilibrium Prices")

## -----------------------------------------------------------------------------
## Replicate Figure 2 of Schwartz and Smith (2000):

#Forecast expected spot prices under the "True" stochastic process:
SS.Expected.Spot <- Spot.Price.Forecast(X.0 = c(2.857, 0.119),
                                        parameters = Enron.values,
                                        t = seq(0,9,1/12),
                                        Percentiles = c(0.1, 0.9))
#Forecast expected futures prices under the Risk-Neutral stochastic process:
SS.Futures.Curve <- Futures.Price.Forecast(X.0 = c(2.857, 0.119),
                                         parameters = Enron.values,
                                         TTM = seq(0,9,1/12))
SS.Figure.2 <- cbind(SS.Expected.Spot[,2], SS.Futures.Curve)
matplot(seq(0,9,1/12), log(SS.Figure.2), type = 'l', col = 1, 
        xlab = "Time (Years)", ylab = "Log(Price)", 
        main = "Futures Prices and Expected Spot Prices")

## -----------------------------------------------------------------------------
## Maximum Observed Maturity:
Max.Maturity <- max(tail(SS.Oil$Contract.Maturities,1), na.rm = TRUE)

##Estimated Futures Prices:

### One Factor (GBM):
Oil.TS.1F <- Futures.Price.Forecast(X.0 = SS.Oil.1F$X.t,
                                         parameters = SS.Oil.1F$Estimated.Parameters,
                                         TTM = seq(0,Max.Maturity,1/12))

### Two Factor:
Oil.TS.2F <- Futures.Price.Forecast(X.0 = SS.Oil.2F$X.t,
                                         parameters = SS.Oil$Two.Factor,
                                         TTM = seq(0,Max.Maturity,1/12))

matplot(seq(0,Max.Maturity,1/12), cbind(Oil.TS.1F, Oil.TS.2F), type = 'l', 
        xlab = "Maturity (Years)", ylab = "Futures Price ($)", 
        main = "Estimated and observed oil futures prices on 1995-02-14"); 
points(tail(SS.Oil$Contract.Maturities,1), tail(SS.Oil$Contracts,1))
legend("bottomleft", c("One-factor", "Two-Factor", "Observed"),
       col=2:4,cex=0.8,fill=c(1,2,0))


## -----------------------------------------------------------------------------
###Test the Volatility Term Structure Fit of the Schwartz-Smith Two-Factor Oil Model:
V_TSFit <- TSFit.Volatility(
 parameters = SS.Oil$Two.Factor,
 Futures = SS.Oil$Stitched.Futures,
 TTM = SS.Oil$Stitched.TTM,
 dt = SS.Oil$dt)

##Plot Results:
matplot(V_TSFit["Maturity",], cbind(SS.Oil.1F$TSFit.Volatility["Theoretical Volatility",], 
                                    V_TSFit["Theoretical Volatility",]), type = 'l',
xlab = "Maturity (Years)", ylab = "Volatility (%)", 
ylim = c(0, 0.5), main = "Volatility Term Structure of Futures Returns"); points(
V_TSFit["Maturity",], V_TSFit["Empirical Volatility",]); legend("bottomleft", 
                  c("Empirical", "One-Factor", "Two-Factor"),col=0:2,cex=0.8,fill=0:2)

## -----------------------------------------------------------------------------
##100 antithetic simulations of one year of monthly observations
Simulated.Spot.Prices <- Spot.Price.Simulate(
 X.0 = SS.Oil.2F$X.t,
 parameters = SS.Oil$Two.Factor,
 t = 1,
 dt = 1/12,
 n = 1e3,
 antithetic = TRUE,
 verbose = TRUE)

##Plot Price Paths:
matplot(seq(0,1,1/12), Simulated.Spot.Prices$Prices, type = 'l', 
        xlab = "Forecasting Horizon (Years)", 
        ylab = "Spot Price ($/bbl, WTI)", main = "Simulated Crude Oil prices")


## -----------------------------------------------------------------------------
##Not Run - Plot Antithetic Price Pairs:
matplot(seq(0,1,1/12), Simulated.Spot.Prices$Prices[,1:2], type = 'l', 
        xlab = "Forecasting Horizon (Years)", 
        ylab = "Spot Price ($/bbl, WTI)", 
        main = "Simulated Crude Oil Antithetic Price Pair")

## -----------------------------------------------------------------------------
##Plot 95% Prediction interval:
Prediction.interval <- rbind.data.frame(apply(Simulated.Spot.Prices$Prices, 1,
                       FUN = function(x) stats::quantile(x, probs = c(0.025, 0.975))),
                       Mean = rowMeans(Simulated.Spot.Prices$Prices))
matplot(seq(0,1,1/12), t(Prediction.interval), type = 'l', col = c(2,2,1),
lwd = 2, lty = c(2,2,1), xlab = "Forecasting Horizon (Years)", 
ylab = "Spot Price ($/bbl, WTI)", main = "Simulated Crude Oil 95% Confidence Interval")

## -----------------------------------------------------------------------------
## Simulate Crude Oil Contract Prices under a Two-Factor model

Simulated.Contracts <- Futures.Price.Simulate(X.0 = c(log(SS.Oil$Spot[1,1]), 0),
                                            parameters = SS.Oil.Two.Factor,
                                            dt = SS.Oil$dt,
                                            N.obs = nrow(SS.Oil$Contracts),
                                            TTM = SS.Oil$Contract.Maturities)

##Not Run - plot Simulated prices:
matplot(as.Date(rownames(SS.Oil$Contracts)), Simulated.Contracts$Futures, 
        type = 'l', ylab = "Futures Price ($/bbl, WTI)", xlab = "Observations", 
        main = "Simulated Futures Contracts")

## -----------------------------------------------------------------------------
## One-Factor European put option value: 
European.Oil.1F <- European.Option.Value(X.0 = SS.Oil.1F$X.t,
                                             parameters = SS.Oil.1F$Estimated.Parameters,
                                             t = 1,
                                             TTM = 1,
                                             K = 20,
                                             r = 0.05,
                                             call = FALSE)

## Two-Factor European put option value: 
European.Oil.2F <- European.Option.Value(X.0 = SS.Oil.2F$X.t,
                                             parameters = SS.Oil$Two.Factor,
                                             t = 1,
                                             TTM = 1,
                                             K = 20,
                                             r = 0.05,
                                             call = FALSE)
## Print results:
print(round(c(One.Factor = European.Oil.1F, Two.Factor = European.Oil.2F),3))


## -----------------------------------------------------------------------------
## One-Factor American put option value: 
American.Oil.1F <- American.Option.Value(X.0 = SS.Oil.1F$X.t,
                      parameters = SS.Oil.1F$Estimated.Parameters,
                      n = 1e5,
                      t = 1,
                      dt = 1/50,
                      K = 20,
                      r = 0.05)

## Two-Factor American put option value: 
American.Oil.2F <- American.Option.Value(X.0 = SS.Oil.2F$X.t,
                      parameters = SS.Oil$Two.Factor,
                      n = 1e5,
                      t = 1,
                      dt = 1/50,
                      K = 20,
                      r = 0.05,
                      verbose = FALSE,
                      orthogonal = "Power",
                      degree = 2)

print(round(c(One.Factor = American.Oil.1F, Two.Factor = American.Oil.2F),3))


## -----------------------------------------------------------------------------
## Exercise opportunities per year:
dt <- 1/50
## strike price :
K <- 40
## short-term interest rate:
rf <- 0.06
## 100,000 simulations (50% antithetic):
N.simulations <- 1e5
## Stock price volatility (variable):
sigma <- rep(c(rep(0.2,2),rep(0.4,2)),5)
## Initial Stock price (variable):
S0 <- sort(rep(seq(36,44,2),4))
## Option maturity (variable):
TTM <- rep(1:2, 10)

LSM.output <- matrix(0, 20, 4, 
                     dimnames = list(NULL, c("LSM American", "(s.e)", 
                                             "Closed form European", 
                                             "Early exercise value")))

## Cycle through the rows of the table:
for(i in 1:20){
  
## Stochastic model assumption:
Stockparameters <- c(mu_star = (rf - 0.5 * sigma[i]^2), sigma_1 = sigma[i])
    
## American option pricing through LSM Simulation
output <- American.Option.Value(X.0 = log(S0[i]),
                      parameters = Stockparameters,
                      n = N.simulations,
                      t = TTM[i],
                      dt = dt,
                      K = K,
                      r = rf,
                      orthogonal = "Laguerre",
                      degree = 3,
                      verbose = TRUE)

LSM.output[i,1] <- output$Value
LSM.output[i,2]  <- output$`Standard Error`

## European option pricing through the BSM PDE
LSM.output[i,3] <- European.Option.Value(X.0 = log(S0[i]), parameters = Stockparameters, 
                                  t = TTM[i], TTM = TTM[i], K = K, r = rf, call = FALSE)

## Early exercise value - the difference between American and European put values:
LSM.output[i,4] <- LSM.output[i,1] - LSM.output[i,3]

}


## -----------------------------------------------------------------------------
## Compile and print results:
LnS.Table1 <- cbind.data.frame(S = S0, sigma = sigma, T = TTM, LSM.output)
print(round(LnS.Table1,3))


