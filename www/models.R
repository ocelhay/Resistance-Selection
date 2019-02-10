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
         
         # secondary infection at time t_secondary
         # the condition (t == t_secondary) below does not work; we create a "window of reinfection" instead
         # https://cran.r-project.org/web/packages/diffEq/vignettes/ODEinR.pdf
         ifelse(second_inf & (t > t_secondary) & (t <= t_secondary + 1),
                second_inf_S <- 10^12,
                second_inf_S <- 0)

         ifelse(second_inf & (t > t_secondary) & (t <= t_secondary + 1),
                second_inf_R <- 10^5,
                second_inf_R <- 0)
         
         # the secondary infection work only if there are remaining parasites!
           S <- S + second_inf_S
           R <- R + second_inf_R
         
         # attempt to factor the near-0 values with S >= 1, R >= 1
         # dS <- growth * (S >= 1) * S - (-k1 * Ct^n / (Ct^n + EC50s^n) * S) + second_inf_S
         # dR <- growth * (R >= 1) * R - (-k1 * Ct^n / (Ct^n + EC50r^n) * R) + second_inf_R
         
         dS <- growth * S - (-k1 * Ct^n / (Ct^n + EC50s^n) * S)
         dR <- growth * R - (-k1 * Ct^n / (Ct^n + EC50r^n) * R)
         
         # return the rate of change
         list(c(dS, dR))
       }
  )
}