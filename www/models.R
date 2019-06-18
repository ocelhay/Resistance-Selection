drug_concentration <- function(dose_1, t_dose_1 = 0, dose_2, t_dose_2, dose_3, t_dose_3, ka, ke, Fa, V, t) {
  
  ka <- ka / 48
  ke <- ke / 48
  
  # Ct_1 <-  (dose_1 * ka * Fa)/(V * ka - CL) * ( exp(-CL / V * (t - t_dose_1)) - exp(-ka * (t - t_dose_1)) )
  Ct_1 <-  ((Fa * dose_1 * ka) / (V * (ka - ke))) * (exp(-ke * (t - t_dose_1)) - exp(-ka * (t - t_dose_1)))
  Ct_1[Ct_1 < 0] <- 0
  
  
  ifelse(dose_2 == 0, Ct_2 <- 0, {
    # Ct_2 <-  (dose_2 * ka * Fa)/(V * ka - CL) * ( exp(-CL / V * (t - t_dose_2)) - exp(-ka * (t - t_dose_2)) )
    Ct_2 <-  ((Fa * dose_2 * ka) / (V * (ka - ke))) * (exp(-ke * (t - t_dose_2)) - exp(-ka * (t - t_dose_2)))
    Ct_2[Ct_2 < 0] <- 0
  })
  
  ifelse(dose_3 == 0, Ct_3 <- 0, {
    # Ct_3 <-  (dose_3 * ka * Fa)/(V * ka - CL) * ( exp(-CL / V * (t - t_dose_3)) - exp(-ka * (t - t_dose_3)) )
    Ct_3 <-  ((Fa * dose_3 * ka) / (V * (ka - ke))) * (exp(-ke * (t - t_dose_3)) - exp(-ka * (t - t_dose_3)))
    Ct_3[Ct_3 < 0] <- 0
  })
  
  
  return(Ct_1 + Ct_2 + Ct_3)
}

parasite_function <- function(t, state, parameters, type) {
  with(as.list(c(state, parameters)),
       {
         # drug concentration at time t
         Ct <- drug_concentration(dose_1, t_dose_1 = 0, dose_2, t_dose_2, dose_3, t_dose_3, ka, ke, Fa, V, t)
         
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