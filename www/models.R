drug_concentration <- function(dose, ka, Fa, V, CL, t) {
  (dose * ka * Fa)/(V * ka - CL) * ( exp(-CL / V * t) - exp(-ka * t) )
}

# dose_response <- function(k1, Ct, EC50, n) {
#   -k1 * Ct^n / (Ct^n + EC50^n)
# }

# conc_start_growth_r <- (-(growth_r / k1) * EC50_r^n)/(1 + (growth_r / k1))^(1/n)


resistance_window <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),
       {
         # drug concentration at time t
         Ct <- (dose * ka * Fa)/(V * ka - CL) * ( exp(-CL / V * t) - exp(-ka * t) )
         
         # rationale for scaling the growth parameter
         # y'(t) = a * y(t) <=> y(t) = C * exp(a * t)
         # y(t + 48) = growth * y(t) <=> exp(a*t + a*48) = growth * exp(a*t) <=> a = log(growth) / 48
         
         # Sensitive parasites
         dS = log(growth_s)/48 * S - (-k1 * Ct^n / (Ct^n + EC50_s^n)) * S
         
         # Resistant parasites
         dR = log(growth_r)/48  * R - (-k1 * Ct^n / (Ct^n + EC50_r^n)) * R
         
         # outputs of the function
         list(c(dS, dR))
       }
  )
}