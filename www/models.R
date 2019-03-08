drug_concentration <- function(dose, ka, Fa, V, CL, t) {
  (dose * ka * Fa)/(V * ka - CL) * ( exp(-CL / V * t) - exp(-ka * t) )
}

# dose_response <- function(k1, Ct, EC50, n) {
#   -k1 * Ct^n / (Ct^n + EC50^n)
# }

# conc_start_growth_r <- (-(growth_r / k1) * EC50_r^n)/(1 + (growth_r / k1))^(1/n)


parasite_function <- function(t, state, parameters, type) {
  with(as.list(c(state, parameters)),
       {
         # drug concentration at time t
         Ct <- (dose * ka * Fa)/(V * ka - CL) * (exp(-CL / V * t) - exp(-ka * t))
         
         if(type != 'S' & type != 'R') 
           stop('Type parasite incorrect!')
         
         # Sensitive parasites
         if(type == 'S') 
           dP = log(growth_s)/48 * P - (-k1 * Ct^n / (Ct^n + EC50_s^n)) * P
         
         # Resistant parasites
         if(type == 'R') 
           dP = log(growth_r)/48  * P - (-k1 * Ct^n / (Ct^n + EC50_r^n)) * P
         
         # outputs of the function
         list(dP)
       }
  )
}