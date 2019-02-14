drug_concentration <- function(dose, ka, Fa, V, CL, t) {
  (dose * ka * Fa)/(V * ka - CL) * ( exp(-CL / V * t) - exp(-ka * t) )
}

dose_response <- function(k1, Ct, EC50, n) {
  -k1 * Ct^n / (Ct^n + EC50^n)
}

resistance_window <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),
       {
         # drug concentration
         Ct <- (dose * ka * Fa)/(V * ka - CL) * ( exp(-CL / V * t) - exp(-ka * t) )
         
         dS <- growth * S - (-k1 * Ct^n / (Ct^n + EC50s^n) * S)
         dR <- growth * R - (-k1 * Ct^n / (Ct^n + EC50r^n) * R)
         
         # return the rate of change
         list(c(dS, dR))
       }
  )
}