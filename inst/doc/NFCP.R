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
model_parameters_2F <- NFCP_parameters(N_factors = 2,
                                      GBM = TRUE,
                                      initial_states = FALSE,
                                      N_ME = 5)
## Print the vector of parameters of the specified commodity pricing model:
print(model_parameters_2F)

## -----------------------------------------------------------------------------
###Method 1 - Stitch Crude Oil Contracts according to maturity matching:
SS_oil_stitched <- stitch_contracts(futures = SS_oil$contracts,
futures_TTM = c(1, 5, 9, 13, 17)/12, maturity_matrix = SS_oil$contract_maturities,
rollover_frequency = 1/12, verbose = TRUE)

##Plot the Stitched Maturities:
matplot(as.Date(rownames(SS_oil_stitched$maturities)), SS_oil_stitched$maturities, 
        type = 'l', main = "Stitched Contract Maturities", 
        ylab = "Time To Maturity (Years)", xlab = "Date", col = 1)

## -----------------------------------------------------------------------------
print(SS_oil$two_factor)

## -----------------------------------------------------------------------------
##Example 1 - Replicating the Schwartz and Smith (2000)
##Two-Factor Crude Oil commodity pricing model:

SS_oil_2F <- NFCP_Kalman_filter(
 parameter_values = SS_oil$two_factor,
 parameter_names = names(SS_oil$two_factor),
 log_futures = log(SS_oil$stitched_futures),
 futures_TTM = SS_oil$stitched_TTM,
 dt = SS_oil$dt,
 verbose = TRUE,
 debugging = TRUE)


## -----------------------------------------------------------------------------
### Assume a constant measurement error in parameters of 1%:
SS_oil_2F_parameters <- SS_oil$two_factor
SS_oil_2F_parameters <- SS_oil_2F_parameters[!grepl("ME", names(SS_oil_2F_parameters))]
SS_oil_2F_parameters["ME_1"] <- 0.01

SS_oil_2F_contracts <- NFCP_Kalman_filter(
 parameter_values = SS_oil_2F_parameters,
 parameter_names = names(SS_oil_2F_parameters),
 log_futures = log(SS_oil$contracts),
 futures_TTM = SS_oil$contract_maturities,
 dt = SS_oil$dt,
 verbose = TRUE,
 debugging = TRUE)


## -----------------------------------------------------------------------------
print(SS_oil$two_factor[8:12])

## -----------------------------------------------------------------------------
# Estimate a GBM model:
SS_oil_1F <- NFCP_MLE(
      ## Arguments
      log_futures = log(SS_oil$stitched_futures),
      dt = SS_oil$dt,
      futures_TTM= SS_oil$stitched_TTM,
      N_ME = 3,
      ME_TTM = c(0.5, 1, 1.5),
      N_factors = 1,
      GBM = TRUE,
      cluster = NULL,
      Richardsons_extrapolation = TRUE,
      ## Genoud arguments:
      pop.size = 1000, optim.method = "L-BFGS-B", print.level = 0, hessian = TRUE,
      max.generations = 100, solution.tolerance = 0.1)

##Print results:
print(round(cbind(`Estimated Parameter` = SS_oil_1F$estimated_parameters, 
                  `Standard Error` = SS_oil_1F$standard_errors),4))

## -----------------------------------------------------------------------------
print(c(`Log-Likelihood: One-Factor` = SS_oil_1F$MLE))

## -----------------------------------------------------------------------------
print(c(`Log-Likelihood: Two-Factor` = SS_oil_2F$`Log-Likelihood`))

## -----------------------------------------------------------------------------
##Replicate Table 3 of Schwartz and Smith (2000):
print(round(SS_oil_2F[["Term Structure Fit"]],4))

## -----------------------------------------------------------------------------
CN_table3 <- matrix(nrow = 2, ncol = 2, dimnames = list(c("One-Factor","Two-Factor"), c("RMSE", "Bias")))
CN_table3[,"Bias"] <- c(SS_oil_1F$`Filtered Error`["Bias"], SS_oil_2F$`Filtered Error`["Bias"])
CN_table3[,"RMSE"] <- c(SS_oil_1F$`Filtered Error`["RMSE"], SS_oil_2F$`Filtered Error`["RMSE"])

print(round(CN_table3, 4))


## ---- out.width='.49\\linewidth'----------------------------------------------
##One Factor
matplot(as.Date(rownames(SS_oil_1F$V)), SS_oil_1F$V, type = 'l',
xlab = "", ylab = "Observation Error",
main = "Contract Observation Error: One-Factor Model"); legend("bottomright", 
colnames(SS_oil_2F$V),col=seq_len(5),cex=0.8,fill=seq_len(5))

##Two-Factor
matplot(as.Date(rownames(SS_oil_2F$V)), SS_oil_2F$V, type = 'l',
xlab = "", ylab = "Observation Error", ylim = c(-0.3, 0.2),
main = "Contract Observation Error: Two-Factor Model"); legend("bottomright", 
colnames(SS_oil_2F$V),col=seq_len(5),cex=0.8,fill=seq_len(5))

## -----------------------------------------------------------------------------
matplot(cbind(SS_oil_1F$`Term Structure Fit`["RMSE",], SS_oil_2F$`Term Structure Fit`["RMSE",]), 
     type = 'l', main = "Root Mean Squared Error of Futures Contracts", 
     xlab = "Contract", ylab = "RMSE"); legend("right",c("One-Factor", "Two-Factor"),
                                               col=1:2,cex=0.8,fill=1:2)

## -----------------------------------------------------------------------------
##Replicate Figure 4 of Schwartz and Smith (2000):
SS_figure_4 <- cbind(`Equilibrium Price` =
                     exp(SS_oil_2F$X[,1]),
                    `Spot Price` =
                     SS_oil_2F$Y[,"filtered Spot"])

matplot(as.Date(rownames(SS_figure_4)), SS_figure_4, type = 'l',
xlab = "", ylab = "Oil Price ($/bbl, WTI)", col = 1,
 main = "Estimated Spot and Equilibrium Prices for the Futures Data")

## -----------------------------------------------------------------------------
plot(as.Date(rownames(SS_oil$contracts)), sqrt(SS_oil_2F_contracts$P_t[1,1,]), 
     type = 'l', xlab = "Date", ylab = "Std. Dev.", 
     main = "Time Series of the Std. Dev for State Variable 1")

## -----------------------------------------------------------------------------
##Figure 1 and 2 of Schwartz and Smith (2000) was developed using Enron data
##and an assumption that mu was approximately 3% p.a.:
Enron_values <- c(0.0300875, 0.0161, 0.014, 1.19, 0.115, 0.158, 0.189)
names(Enron_values) <- NFCP_parameters(2, TRUE, FALSE, 0, FALSE)


## -----------------------------------------------------------------------------
## Replicate figure 1 of Schwartz and Smith (2000):

SS_expected_spot <- spot_price_forecast(x_0 = c(2.857, 0.119),
                                           Enron_values,
                                           t = seq(0,9,1/12),
                                           percentiles = c(0.1, 0.9))
##Factor one only:
equilibrium_theta <- Enron_values[!names(Enron_values) %in%
                                 c("kappa_2", "lambda_2", "sigma_2", "rho_1_2")]
SS_expected_equilibrium <- spot_price_forecast(x_0 = c(2.857, 0),
                                                  equilibrium_theta,
                                                  t = seq(0,9,1/12),
                                                  percentiles = c(0.1, 0.9))
SS_figure_1 <- cbind(SS_expected_spot, SS_expected_equilibrium)
matplot(seq(0,9,1/12), SS_figure_1, type = 'l', col = 1, lty = c(rep(1,3), rep(2,3)),
xlab = "Time (Years)", ylab = "Spot Price ($/bbl, WTI)",
main = "Probabilistic Forecasts for Spot and Equilibrium Prices")

## -----------------------------------------------------------------------------
## Replicate Figure 2 of Schwartz and Smith (2000):

#Forecast expected spot prices under the "True" stochastic process:
SS_expected_spot <- spot_price_forecast(x_0 = c(2.857, 0.119),
                                        parameters = Enron_values,
                                        t = seq(0,9,1/12),
                                        percentiles = c(0.1, 0.9))
#Forecast expected futures prices under the Risk-Neutral stochastic process:
SS_futures_curve <- futures_price_forecast(x_0 = c(2.857, 0.119),
                                         parameters = Enron_values,
                                         futures_TTM = seq(0,9,1/12))
SS_figure_2 <- cbind(SS_expected_spot[,2], SS_futures_curve)
matplot(seq(0,9,1/12), log(SS_figure_2), type = 'l', col = 1, 
        xlab = "Time (Years)", ylab = "Log(Price)", 
        main = "Futures Prices and Expected Spot Prices")

## -----------------------------------------------------------------------------
## Maximum Observed Maturity:
max_maturity <- max(tail(SS_oil$contract_maturities,1), na.rm = TRUE)

##Estimated Futures Prices:

### One Factor (GBM):
oil_TS_1F <- futures_price_forecast(x_0 = SS_oil_1F$x_t,
                                         parameters = SS_oil_1F$estimated_parameters,
                                         futures_TTM = seq(0,max_maturity,1/12))

### Two Factor:
oil_TS_2F <- futures_price_forecast(x_0 = SS_oil_2F$x_t,
                                         parameters = SS_oil$two_factor,
                                         futures_TTM = seq(0,max_maturity,1/12))

matplot(seq(0,max_maturity,1/12), cbind(oil_TS_1F, oil_TS_2F), type = 'l', 
        xlab = "Maturity (Years)", ylab = "Futures Price ($)", 
        main = "Estimated and observed oil futures prices on 1995-02-14"); 
points(tail(SS_oil$contract_maturities,1), tail(SS_oil$contracts,1))
legend("bottomleft", c("One-factor", "Two-Factor", "Observed"),
       col=2:4,cex=0.8,fill=c(1,2,0))


## -----------------------------------------------------------------------------
###Test the Volatility Term Structure Fit of the Schwartz-Smith Two-Factor Oil Model:
V_TSFit <- TSfit_volatility(
 parameters = SS_oil$two_factor,
 futures = SS_oil$stitched_futures,
 futures_TTM = SS_oil$stitched_TTM,
 dt = SS_oil$dt)

##Plot Results:
matplot(V_TSFit["Maturity",], cbind(SS_oil_1F$`Term Structure Volatility Fit`["Theoretical Volatility",], 
                                    V_TSFit["Theoretical Volatility",]), type = 'l',
xlab = "Maturity (Years)", ylab = "Volatility (%)", 
ylim = c(0, 0.5), main = "Volatility Term Structure of Futures Returns"); points(
V_TSFit["Maturity",], V_TSFit["Empirical Volatility",]); legend("bottomleft", 
                  c("Empirical", "One-Factor", "Two-Factor"),col=0:2,cex=0.8,fill=0:2)

## -----------------------------------------------------------------------------
##100 antithetic simulations of one year of monthly observations
simulated_spot_prices <- spot_price_simulate(
 x_0 = SS_oil_2F$x_t,
 parameters = SS_oil$two_factor,
 t = 1,
 dt = 1/12,
 N_simulations = 1e3,
 antithetic = TRUE,
 verbose = TRUE)

##Plot Price Paths:
matplot(seq(0,1,1/12), simulated_spot_prices$spot_prices, type = 'l', 
        xlab = "Forecasting Horizon (Years)", 
        ylab = "Spot Price ($/bbl, WTI)", main = "Simulated Crude Oil prices")


## -----------------------------------------------------------------------------
##Not Run - Plot Antithetic Price Pairs:
matplot(seq(0,1,1/12), simulated_spot_prices$spot_prices[,1:2], type = 'l', 
        xlab = "Forecasting Horizon (Years)", 
        ylab = "Spot Price ($/bbl, WTI)", 
        main = "Simulated Crude Oil Antithetic Price Pair")

## -----------------------------------------------------------------------------
##Plot 95% Prediction interval:
prediction_interval <- rbind.data.frame(apply(simulated_spot_prices$spot_prices, 1,
                       FUN = function(x) stats::quantile(x, probs = c(0.025, 0.975))),
                       Mean = rowMeans(simulated_spot_prices$spot_prices))
matplot(seq(0,1,1/12), t(prediction_interval), type = 'l', col = c(2,2,1),
lwd = 2, lty = c(2,2,1), xlab = "Forecasting Horizon (Years)", 
ylab = "Spot Price ($/bbl, WTI)", main = "Simulated Crude Oil 95% Confidence Interval")

## -----------------------------------------------------------------------------
## Simulate Crude Oil Contract Prices under a Two-Factor model

simulated_contracts <- futures_price_simulate(x_0 = c(log(SS_oil$spot[1,1]), 0),
                                            parameters = SS_oil_2F_parameters,
                                            dt = SS_oil$dt,
                                            N_obs = nrow(SS_oil$contracts),
                                            futures_TTM = SS_oil$contract_maturities)

##Not Run - plot Simulated prices:
matplot(as.Date(rownames(SS_oil$contracts)), simulated_contracts$futures_prices, 
        type = 'l', ylab = "Futures Price ($/bbl, WTI)", xlab = "Observations", 
        main = "Simulated Futures Contracts")

## -----------------------------------------------------------------------------
## One-Factor European put option value: 
European_oil_1F <- European_option_value(x_0 = SS_oil_1F$x_t,
                                         parameters = SS_oil_1F$estimated_parameters,
                                         futures_maturity = 1,
                                         option_maturity  = 1,
                                         K = 20,
                                         r = 0.05,
                                         call = FALSE)

## Two-Factor European put option value: 
European_oil_2F <- European_option_value(x_0 = SS_oil_2F$x_t,
                                         parameters = SS_oil$two_factor,
                                         futures_maturity = 1,
                                         option_maturity = 1,
                                         K = 20,
                                         r = 0.05,
                                         call = FALSE)
## Print results:
print(round(c("One Factor" = European_oil_1F, "Two Factor" = European_oil_2F),3))


## -----------------------------------------------------------------------------
## One-Factor American put option value: 
American_oil_1F <- American_option_value(x_0 = SS_oil_1F$x_t,
                      parameters = SS_oil_1F$estimated_parameters,
                      N_simulations = 1e5,
                      option_maturity = 1,
                      dt = 1/50,
                      K = 20,
                      r = 0.05)

## Two-Factor American put option value: 
American_oil_2F <- American_option_value(x_0 = SS_oil_2F$x_t,
                      parameters = SS_oil$two_factor,
                      N_simulations = 1e5,
                      option_maturity = 1,
                      dt = 1/50,
                      K = 20,
                      r = 0.05,
                      verbose = FALSE,
                      orthogonal = "Power",
                      degree = 2)

print(round(c("One Factor" = American_oil_1F, "Two Factor" = American_oil_2F),3))


## -----------------------------------------------------------------------------
## Exercise opportunities per year:
dt <- 1/50
## strike price :
K <- 40
## short-term interest rate:
rf <- 0.06
## 100,000 simulations (50% antithetic):
N_simulations <- 1e5
## Stock price volatility (variable):
sigma <- rep(c(rep(0.2,2),rep(0.4,2)),5)
## Initial Stock price (variable):
S0 <- sort(rep(seq(36,44,2),4))
## Option maturity (variable):
TTM <- rep(1:2, 10)

LSM_output <- matrix(0, 20, 4, 
                     dimnames = list(NULL, c("LSM American", "(s.e)", 
                                             "Closed form European", 
                                             "Early exercise value")))

## Cycle through the rows of the table:
for(i in 1:20){
  
## Stochastic model assumption:
stock_parameters <- c(mu_rn = (rf - 0.5 * sigma[i]^2), sigma_1 = sigma[i])
    
## American option pricing through LSM Simulation
output <- American_option_value(x_0 = log(S0[i]),
                      parameters = stock_parameters,
                      N_simulations = N_simulations,
                      option_maturity = TTM[i],
                      dt = dt,
                      K = K,
                      r = rf,
                      orthogonal = "Laguerre",
                      degree = 3,
                      verbose = TRUE)

LSM_output[i,1] <- output$Value
LSM_output[i,2]  <- output$`Standard Error`

## European option pricing through the BSM PDE
LSM_output[i,3] <- European_option_value(x_0 = log(S0[i]),
                                         parameters = stock_parameters,
                                         futures_maturity = TTM[i],
                                         option_maturity = TTM[i],
                                         K = K,
                                         r = rf,
                                         call = FALSE)

## Early exercise value - the difference between American and European put values:
LSM_output[i,4] <- LSM_output[i,1] - LSM_output[i,3]

}



## -----------------------------------------------------------------------------
## Compile and print results:
LnS_table_1 <- cbind.data.frame(S = S0, sigma = sigma, T = TTM, LSM_output)
print(round(LnS_table_1,3))


