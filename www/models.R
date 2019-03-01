drug_concentration <- function(dose, ka, Fa, V, CL, t) {
  (dose * ka * Fa)/(V * ka - CL) * ( exp(-CL / V * t) - exp(-ka * t) )
}

# dose_response <- function(k1, Ct, EC50, n) {
#   -k1 * Ct^n / (Ct^n + EC50^n)
# }

resistance_window <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),
       {
         # drug concentration at time t
         Ct <- (dose * ka * Fa)/(V * ka - CL) * ( exp(-CL / V * t) - exp(-ka * t) )
         
         # Sensitive parasites
         dS <- 0.048 * growth_s^(1/48) * S - (-k1 * Ct^n / (Ct^n + EC50_s^n)) * S
         
         # Resistant parasites
         dR <- 0.048 * growth_r^(1/48) * R - (-k1 * Ct^n / (Ct^n + EC50_r^n)) * R
         
         # outputs of the function
         list(c(dS, dR))
       }
  )
}

derivative <- function(t, S, R, parameters){
  with(as.list(parameters),
       {
         Ct <- (dose * ka * Fa)/(V * ka - CL) * ( exp(-CL / V * t) - exp(-ka * t) )
         derivativeS = 0.048 * growth_s^(1/48) * S[t] - (-k1 * Ct^n / (Ct^n + EC50_s^n)) * S[t]
         derivativeR = 0.048 * growth_r^(1/48) * R[t] - (-k1 * Ct^n / (Ct^n + EC50_r^n)) * R[t]
         
         list(derivativeS, derivativeR)
       }
  )
}