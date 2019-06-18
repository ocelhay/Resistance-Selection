shinyServer(
  function(input, output, session) {
    
    # Update default values based on inputs ----
    parameters = reactive({c(
      time_resistant = input$time_resistant,
      nb_resistant_t0 = as.numeric(input$nb_resistant_t0),
      nb_sensitive_t0 = as.numeric(input$nb_sensitive_t0),
      nb_resistant_sec = as.numeric(input$nb_resistant_sec),
      nb_sensitive_sec = as.numeric(input$nb_sensitive_sec),
      growth_s = input$growth_s,
      growth_r = input$growth_r,
      dose_1 = input$dose_1,
      dose_2 = input$dose_2,
      t_dose_2 = input$t_dose_2,
      dose_3 = input$dose_3,
      t_dose_3 = input$t_dose_3,
      ka = input$ka,
      ke = input$ke,
      Fa = input$Fa,
      V = input$V,
      k1 = input$k1,
      n = input$n,
      EC50_s = input$EC50_s,
      EC50_r = input$EC50_r,
      second_inf = input$second_inf,
      t_secondary = input$t_secondary)
    })
    
    
    # Welcomce modal
    # source('./www/welcome_modal.R', local = TRUE)
    
    
    # Modal help ----
    source('./www/about_modal.R', local = TRUE)
    
    # Warning on UI values ----
    output$message_rate <- renderText({
      if(input$growth_s <= input$growth_r) return(paste0(div(class = 'alertbox', icon('exclamation-triangle'), 
                                                             'It is expected that the multiplicaton rate of sensitive parasites is greater than the one or resistant parasites (fitness of cost).')))
      if(input$growth_s > input$growth_r) return('')
    })
    
    output$message_EC50 <- renderText({
      if(input$EC50_s > input$EC50_r) return(paste0(div(class = 'alertbox', icon('exclamation-triangle'), 
                                                        'It is expected that EC50 for resistant parasites is greater than EC50 for sensitive parasites.')))
      if(input$EC50_s <= input$EC50_r) return('')
    })
    
    output$time_hour <- renderText({paste0(input$t_secondary, ' hours = ', round(input$t_secondary/24, 1), ' days.')})
    
    
    # Reactive to save all results ----
    simul <- reactiveValues(
      simul_parasites = data.frame(NULL),
      updated = FALSE
    )
    
    # Simulate drug concentration over time ----
    simul_concentration <- reactive({
      tibble(times = c(seq(0, 1.99, by = 0.01), 2:240),
             Ct = drug_concentration(dose_1 = input$dose_1,
                                     t_dose_1 = 0,
                                     dose_2 = input$dose_2,
                                     t_dose_2 = input$t_dose_2,
                                     dose_3 = input$dose_3,
                                     t_dose_3 = input$t_dose_3,
                                     ka = input$ka, 
                                     ke = input$ke, 
                                     Fa = input$Fa, 
                                     V = input$V, 
                                     t = c(seq(0, 1.99, by = 0.01), 2:240))
      )
    })
    
    
    # Simulate parasites over time ----
    observe({
      list_parameters <- list(
        growth_s = input$growth_s,
        growth_r = input$growth_r,
        dose_1 = input$dose_1,
        dose_2 = input$dose_2,
        t_dose_2 = input$t_dose_2,
        dose_3 = input$dose_3,
        t_dose_3 = input$t_dose_3,
        ka = input$ka,
        ke = input$ke,
        Fa = input$Fa,
        V = input$V,
        k1 = input$k1,
        n = input$n,
        EC50_s = input$EC50_s,
        EC50_r = input$EC50_r)
      
      times <- seq(0, 240, by = 1)
      
      out <- tibble(
        time = times,
        S = rep(0, length(times)), 
        R = rep(0, length(times)))
      
      
      # Sensitive
      times_pre <- times < input$t_secondary
      times_post <- times >= input$t_secondary
      
      out$S[times_pre] <- ode(y = c(P = as.numeric(input$nb_sensitive_t0)), 
                              times = times[times_pre], 
                              func = parasite_function, 
                              parms = list_parameters,
                              type = 'S') %>%
        as_tibble() %>%
        pull(P)
      
      out$S[times_post] <- ode(y = c(P = last(out$S[times_pre]) + as.numeric(input$nb_sensitive_sec)), 
                               times = times[times_post], 
                               func = parasite_function, 
                               parms = list_parameters,
                               type = 'S') %>%
        as_tibble() %>%
        pull(P)
      
      # Resistant
      times_pre <- times < input$t_secondary
      times_post <- times >= input$t_secondary
      
      out$R[times_pre] <- ode(y = c(P = as.numeric(input$nb_resistant_t0)), 
                              times = times[times_pre], 
                              func = parasite_function, 
                              parms = list_parameters,
                              type = 'R') %>%
        as_tibble() %>%
        pull(P)
      
      out$R[times_post] <- ode(y = c(P = last(out$R[times_pre]) + as.numeric(input$nb_resistant_sec)), 
                               times = times[times_post], 
                               func = parasite_function, 
                               parms = list_parameters,
                               type = 'R') %>%
        as_tibble() %>%
        pull(P)
      
      
      out <- out %>%
        mutate(S = replace(S, cumsum(S < 1) >= 1, 0),
               R = replace(R, cumsum(R > 0 & R < 1) >= 1, 0))
      
      
      
      # Output ----
      
      adj_growth_r <- log(input$growth_r)/48
      mpc <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
      
      adj_growth_s <- log(input$growth_s)/48
      mic <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
      
      simul$simul_parasites <- out %>%
        mutate(Ct = simul_concentration()$Ct[c(1, 101, 201:439)],
               dS = c(0, diff(S)),
               dR = c(0, diff(R))) %>%
        mutate(prop_S = round(100* S / (R + S), 1),
               prop_R = round(100* R / (R + S), 1),
               msw = (Ct >= mic & Ct <= mpc),
               msw_open = c(0, diff(msw)) == 1,
               msw_close = c(0, diff(msw)) == -1)
      
      simul$updated <- TRUE
    })
    
    
    # Several PK parameters ----
    
    # half-life
    output$half_life <- renderText(paste0(div(class = 'output_animated', 
                                              span("The half-life of the drug is ", round(log(2) * input$ke, 2), ' hours.'))))
    
    # AUC
    output$auc <- renderText({
      # Approximation for integrating the function using the trapezoidal rule with basepoints x.
      auc_trapezoid <- trapz(simul_concentration()$times, simul_concentration()$Ct)
      return(paste0(div(class = 'output_animated', span("The AUC of the drug concentration is ", round(auc_trapezoid, 0), "."))))
    })
    
    # CMAX
    output$cmax <- renderText({
      cmax <- max(simul_concentration()$Ct)
      return(paste0(div(class = 'output_animated', span("Cmax for the drug concentration is ", round(cmax, 2), "mg/L."))))
    })
    
    # Compute TMAX
    output$tmax <- renderText({
      cmax <- max(simul_concentration()$Ct)
      tmax <- simul_concentration()$times[simul_concentration()$Ct == cmax]
      if(tmax > 2) return(paste0(div(class = 'output_animated', span("Tmax for the drug concentration is ", round(tmax, 1), "hours"))))
      if(tmax <= 2) return(paste0(div(class = 'output_animated', span("Tmax for the drug concentration is ", round(60*tmax, 0), "minutes"))))
    })
    
    # Mutant Selection Window, concentrations ----
    # MPC
    output$mpc <- renderText({
      adj_growth_r <- log(input$growth_r)/48
      conc_start_growth_r <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
      
      paste0(div(class = 'mpc2', span('Mutant Prevention Concentration = ', round(conc_start_growth_r, 2), ' mg/L.')))
    })
    
    # MIC
    output$mic <- renderText({
      adj_growth_s <- log(input$growth_s)/48
      conc_start_growth_s <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
      
      paste0(div(class = 'mic', span('Minimum Inhibitory Concentration = ', round(conc_start_growth_s, 2), ' mg/L.')))
    })
    
    
    
    
    # Mutant Selection Window, time ----
    output$mpc_time <- renderText({
      adj_growth_r <- log(input$growth_r)/48
      mpc <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
      msw_open <-  c(0, diff(simul_concentration()$Ct <= mpc)) == 1
      
      t_open <- round((simul_concentration()$times[msw_open == TRUE])/24, 1)
      paste0(div(class = 'mpc', span('attained at ' , paste0(t_open, collapse = " / "), ' days from treatment onset.')))
    })
    
    output$mic_time <- renderText({
      adj_growth_s <- log(input$growth_s)/48
      mic <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
      msw_close <-  c(0, diff(simul_concentration()$Ct >= mic)) == -1
      t_close <- round((simul_concentration()$times[msw_close == TRUE])/24, 1)
      paste0(div(class = 'mic2', span('attained at ', paste0(t_close, collapse = " / "), ' days from treatment onset.')))
    })
    
    output$window <- renderText({
      adj_growth_r <- log(input$growth_r)/48
      mpc <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
      msw_open <-  c(0, diff(simul_concentration()$Ct <= mpc)) == 1
      t_open <- round((simul_concentration()$times[msw_open == TRUE])/24, 1)
      
      adj_growth_s <- log(input$growth_s)/48
      mic <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
      msw_close <-  c(0, diff(simul_concentration()$Ct >= mic)) == -1
      t_close <- round((simul_concentration()$times[msw_close == TRUE])/24, 1)
      
      
      if(t_open[[1]] >= t_close[[1]]) return(paste0(div(class = 'output_animated', 'There is no Mutant Selection Window.')))
      if(length(t_open) > 1 | length(t_close) > 1) return(paste0(div(class = 'output_animated', 'There are several Mutant Selection Windows.')))
      
      return(paste0(div(class = 'output_animated', span('The Mutant Selection Window opens for ', round(24*(t_close - t_open), 2), ' hours.'))))
    })
    
    # Plot of Drug Concentration ----
    output$plot_1 <- renderPlot({
      
      # Elements for lines
      adj_growth_r <- log(input$growth_r)/48
      conc_start_growth_r <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
      mpc <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
      msw_open <-  c(0, diff(simul_concentration()$Ct <= mpc)) == 1
      
      t_open <- tibble(times = (simul_concentration()$times[msw_open == TRUE])/24)
      
      adj_growth_s <- log(input$growth_s)/48
      conc_start_growth_s <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
      mic <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
      msw_close <-  c(0, diff(simul_concentration()$Ct >= mic)) == -1
      
      t_close <- tibble(times = (simul_concentration()$times[msw_close == TRUE])/24)
      
      print(paste0("t_open = ", t_open))
      cmax <- max(simul_concentration()$Ct)
      
      if(t_open[[1]] <= t_close[[1]]){
      graph_concentration <- simul_concentration() %>%
        ggplot(aes(x = times / 24, y = Ct)) +
        geom_line(size = 1, colour = "grey") +
        scale_x_continuous(breaks = 0:10, minor_breaks = 0.5*1:10) +
        geom_hline(aes(yintercept = conc_start_growth_s, col = 'Minimum Inhibitory Concentration'), linetype = 'dashed') +
        geom_hline(aes(yintercept = conc_start_growth_r, col = 'Mutant Prevention Concentration'), linetype = 'dashed') +
        geom_vline(data = t_open, aes(xintercept = times, col = 'Mutant Prevention Concentration'), linetype = 'dashed') +
        geom_vline(data = t_close, aes(xintercept = times, col = 'Minimum Inhibitory Concentration'), linetype = 'dashed') +
        scale_color_manual(name = NULL, values = c(`Mutant Prevention Concentration` = '#e41a1c', `Minimum Inhibitory Concentration` = '#377eb8')) +
        geom_curve(aes(x = (t_open[[1]] + t_close[[1]])/2, y = cmax, xend = 8, yend = cmax), color = "black", arrow = arrow(type = "closed")) +
        geom_text(aes(x =  8, y = 1.1*cmax, size = 3, label = "Mutant Selection Window")) +
        labs(x = "Time (days)", y = 'Concentration (mg/L)', title = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.text = element_text(size = 13), legend.position = 'bottom')
      }
      
      if(t_open[[1]] > t_close[[1]]){
        graph_concentration <- simul_concentration() %>%
          ggplot(aes(x = times / 24, y = Ct)) +
          geom_line(size = 1, colour = "grey") +
          scale_x_continuous(breaks = 0:10, minor_breaks = 0.5*1:10) +
          geom_hline(aes(yintercept = conc_start_growth_s, col = 'Minimum Inhibitory Concentration'), linetype = 'dashed') +
          geom_hline(aes(yintercept = conc_start_growth_r, col = 'Mutant Prevention Concentration'), linetype = 'dashed') +
          geom_vline(data = t_open, aes(xintercept = times, col = 'Mutant Prevention Concentration'), linetype = 'dashed') +
          geom_vline(data = t_close, aes(xintercept = times, col = 'Minimum Inhibitory Concentration'), linetype = 'dashed') +
          scale_color_manual(name = NULL, values = c(`Mutant Prevention Concentration` = '#e41a1c', `Minimum Inhibitory Concentration` = '#377eb8')) +
          labs(x = "Time (days)", y = 'Concentration (mg/L)', title = NULL) +
          theme_minimal(base_size = 14) +
          theme(legend.text = element_text(size = 13), legend.position = 'bottom')
      }
      
      return(graph_concentration)
    })
    
    # Evolution of parasites ----
    output$plot_2 <- renderPlot({
      
      req(simul$updated)
      
      # Elements for lines
      adj_growth_r <- log(input$growth_r)/48
      conc_start_growth_r <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
      mpc <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
      msw_open <-  c(0, diff(simul_concentration()$Ct <= mpc)) == 1
      t_open <- (simul_concentration()$times[msw_open == TRUE])/24
      
      adj_growth_s <- log(input$growth_s)/48
      conc_start_growth_s <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
      mic <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
      msw_close <-  c(0, diff(simul_concentration()$Ct >= mic)) == -1
      t_close <- (simul_concentration()$times[msw_close == TRUE])/24
      
      
      ggplot(data = simul$simul_parasites, aes(x = time / 24)) +
        geom_line(aes(y = S, colour = 'Sensitive Parasites'), size = 1.2) +
        geom_line(aes(y = R, colour = 'Resistant Parasites'), size = 1.2) +
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x)),
                      limits = c(1, 10^12)) +
        scale_x_continuous(breaks = 0:10, minor_breaks = 0.5*1:10) +
        geom_vline(xintercept = t_open, colour = '#e41a1c', linetype = 'dashed') +
        geom_vline(xintercept = t_close, colour = '#377eb8', linetype = 'dashed') +
        labs(x = "Time (days)", y = "Parasites (Log scale)", title = NULL) +
        scale_colour_manual(name = NULL, values = cols) +
        theme_minimal(base_size = 14) +
        theme(legend.text = element_text(size = 13), legend.position = 'bottom')
    })
    
    output$plot_3 <- renderPlot({
      
      req(simul$updated)
      
      ggplot(data = simul$simul_parasites, aes(x = time / 24)) +
        scale_y_continuous(limits = c(0, 100)) +
        geom_point(aes(y = prop_S), size = 1.2, col = '#377eb8') +
        geom_point(aes(y = prop_R), size = 1.2, col = '#e41a1c') +
        geom_hline(yintercept = 10, lty = 2) +
        labs(x = "Time (days)", y = "Proportion", title = "Proportion of Sensitive/Resistant") +
        scale_x_continuous(breaks = 0:10, minor_breaks = 0.5*1:10) +
        theme_minimal(base_size = 14)
    })
    
    # Total time for which R >= 0.1*S
    output$danger_time <- renderText({
      req(simul$updated)
      
      danger_days <- round(sum(simul$simul_parasites$prop_R > 10) / 24, 1)
      if(is.na(danger_days)) return(paste0(span(icon('check'), 'All parasites are eliminated by the drug.')))
      if(danger_days == 0) return(paste0('Resistant parasites are dominated by sensitive parasites.'))
      if(danger_days > 0) return(paste0(div(class = 'alertbox', icon('exclamation-triangle'), "Potential for selection of resistance", br(), "for ",  danger_days, " days, 10% or more of total parasites are resistant.")))
    })
    
  }
)