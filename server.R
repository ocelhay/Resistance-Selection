shinyServer(
function(input, output) {
  
  # update default values based on inputs
  parameters = reactiveValues(
    time_resistant = init_param$time_resistant,
    nb_resistant = init_param$nb_resistant,
    growth_s = init_param$growth_s,
    growth_r = init_param$growth_r,
    dose = init_param$dose,
    ka = init_param$ka,
    Fa = init_param$Fa,
    V = init_param$V,
    CL = init_param$CL,
    k1 = init_param$k1,
    n = init_param$n,
    EC50_s = init_param$EC50_s,
    EC50_r = init_param$EC50_r,
    second_inf = init_param$second_inf,
    t_secondary = init_param$t_secondary)
  
  observe({
    parameters$time_resistant = input$time_resistant
    parameters$nb_resistant = as.numeric(input$nb_resistant)
    parameters$growth_s = input$growth_s
    parameters$growth_r = input$growth_r
    parameters$dose = input$dose
    parameters$ka = input$ka
    parameters$Fa = input$Fa
    parameters$V = input$V
    parameters$CL = input$CL
    parameters$k1 = input$k1
    parameters$n = input$n
    parameters$EC50_s = input$EC50_s
    parameters$EC50_r = input$EC50_r
    parameters$second_inf = input$second_inf
    parameters$t_secondary = input$t_secondary
  })
  
  
  # Simulate drug concentration over time =====================================

  simul_concentration <- reactive({
    tibble(times = c(seq(0, 1.99, by = 0.01), 2:240),
           Ct = drug_concentration(dose = parameters$dose, ka = parameters$ka, Fa = parameters$Fa, V = parameters$V, 
                       CL = parameters$CL, t = c(seq(0, 1.99, by = 0.01), 2:240))
    )
  })
  
  
  # Simulate parasites over time ==============================================
  
  simul_parasites <- reactive({
    
    list_parameters <- list(
      growth_s = parameters$growth_s,
      growth_r = parameters$growth_r,
      dose = parameters$dose,
      ka = parameters$ka,
      Fa = parameters$Fa,
      V = parameters$V,
      CL = parameters$CL,
      k1 = parameters$k1,
      n = parameters$n,
      EC50_s = parameters$EC50_s,
      EC50_r = parameters$EC50_r)
    
    times <- seq(0, 240, by = 1)
    
    out <- tibble(
      time = times,
      S = rep(0, length(times)), 
      R = rep(0, length(times)))
    
    # NO secondary infection ==================================================
    if (! parameters$second_inf) {
  
      # Sensitive
      out$S <- ode(y = c(P = 10^12), 
                   times = times, 
                   func = parasite_function, 
                   parms = list_parameters,
                   type = 'S') %>%
        as.tibble() %>%
        pull(P)
      
      # Resistant
      out$R[input$time_resistant:length(times)] <- 
        ode(y = c(P = parameters$nb_resistant), 
            times = times[input$time_resistant:length(times)], 
            func = parasite_function, 
            parms = list_parameters,
            type = 'R') %>%
        as.tibble() %>%
        pull(P)
      
      out <- out %>%
        mutate(S = replace(S, cumsum(S < 1) >= 1, 0),
               R = replace(R, cumsum(R > 0 & R < 1) >= 1, 0))
    }
    
    # Secondary infection =====================================================
    if (parameters$second_inf) {
      
      out <- tibble(
        time = times,
        S = rep(0, length(times)), 
        R = rep(0, length(times)))
      
      # Sensitive
      times_pre <- times < parameters$t_secondary
      times_post <- times >= parameters$t_secondary
      
      out$S[times_pre] <- ode(y = c(P = 10^12), 
                   times = times[times_pre], 
                   func = parasite_function, 
                   parms = list_parameters,
                   type = 'S') %>%
        as.tibble() %>%
        pull(P)
      
      out$S[times_post] <- ode(y = c(P = last(out$S[times_pre]) + 10^12), 
                              times = times[times_post], 
                              func = parasite_function, 
                              parms = list_parameters,
                              type = 'S') %>%
        as.tibble() %>%
        pull(P)
      
      # Resistant
      times_pre <- times >= input$time_resistant & times < parameters$t_secondary
      times_post <- times >= parameters$t_secondary
      
      out$R[times_pre] <- ode(y = c(P = parameters$nb_resistant), 
                              times = times[times_pre], 
                              func = parasite_function, 
                              parms = list_parameters,
                              type = 'R') %>%
        as.tibble() %>%
        pull(P)
      
      out$R[times_post] <- ode(y = c(P = last(out$R[times_pre]) + 10^5), 
                               times = times[times_post], 
                               func = parasite_function, 
                               parms = list_parameters,
                               type = 'R') %>%
        as.tibble() %>%
        pull(P)
      
      out <- out %>%
        mutate(S = replace(S, cumsum(S < 1) >= 1, 0),
               R = replace(R, cumsum(R > 0 & R < 1) >= 1, 0))
    }
    
    
    
    # Add elements to output ==================================================
    
    adj_growth_r <- log(input$growth_r)/48
    mpc <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
    
    adj_growth_s <- log(input$growth_s)/48
    mic <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
    
    out <- out %>%
      mutate(Ct = simul_concentration()$Ct[c(1, 101, 201:439)],
      # mutate(Ct = (list_parameters$dose * list_parameters$ka * list_parameters$Fa)/(list_parameters$V * list_parameters$ka - list_parameters$CL) * ( exp(-list_parameters$CL / list_parameters$V * times) - exp(-list_parameters$ka * times)),
             dS = c(0, diff(S)),
             dR = c(0, diff(R))) %>%
      mutate(prop_R = round(100* R / (R + S), 1),
             # growth_S = (dS > 0),
             # growth_R = (dR > 0),
             # growth_RsupS = (dR > dS),
             msw = (Ct >= mic & Ct <= mpc),
             msw_open = c(0, diff(msw)) == 1,
             msw_close = c(0, diff(msw)) == -1)
    
    return(out)
  })
  
  # Append result of simulations to data frame every time that there is a new simulation 
  # (to show if for a specific initial dose there is a window opening)
  # START DISCARDED but kept just in case
  # simul <- reactiveValues(new = FALSE, results = tibble())
  # 
  # observeEvent(simul_parasites(), {
  #   simul$new <- TRUE
  #   simul$results <- bind_rows(simul$results,
  #                              tibble(dose = parameters$dose, 
  #                                     status = ifelse (any(simul_parasites()$R > simul_parasites()$S), "Selection", "No Selection")))
  # })
  
  # A change for these parameters should triger a new graph
  # observeEvent({
  #   input$time_resistant
  #   input$nb_resistant
  #   input$growth_s
  #   input$growth_r
  #   input$ka
  #   input$Fa
  #   input$V
  #   input$CL
  #   input$k1
  #   input$n
  #   input$EC50_s
  #   input$EC50_r
  #   input$second_inf
  #   input$t_secondary}, {
  #     simul$new <- FALSE
  #     simul$results <- tibble()
  #   }, ignoreInit = TRUE)
  # END DISCARDED but kept just in case
  
  
  # Output window of selection graph ==========================================
  # output$window_selection <- renderPlot({
  #   req(simul$new)
  #   
  #   ggplot(data = simul$results, aes(x = dose, y = 0)) +
  #     geom_point(aes(colour = status), shape = 25, size = 2) +
  #     geom_text(aes(label = status, colour = status), fontface = "bold", angle = 30, hjust = 0, vjust = -0.5, size = 5, show.legend = FALSE) +
  #     scale_x_continuous(limits = c(50, 1100)) +
  #     scale_y_continuous(limits = c(0, 1)) +
  #     scale_colour_manual(values = c('No Selection' = '#377eb8', 'Selection' = '#e41a1c')) +
  #     labs(x = "Dose (mg)", y = NULL) +
  #     theme_classic(base_size = 13) +
  #     guides(colour = FALSE) +
  #     theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), panel.border = element_blank())
  # })
  
  
  # Output half-life =======================================================
  output$half_life <- renderText(paste0("The half-life of the drug is ", round(log(2) * parameters$V / parameters$CL, 2), ' hours.'))
  
  # MPC, Concentration at which we seee a growth of R
  output$conc_growth_r <- renderText({
    adj_growth_r <- log(input$growth_r)/48
    conc_start_growth_r <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
    print(conc_start_growth_r)
    paste0("Mutant Prevention Concentration (MPC) — resistant parasites will grow if the drug concentration is less than ", round(conc_start_growth_r, 2), ' mg/L')
  })
  
  # Time MPC
  output$time_mpc <- renderText({
    t_open <- (simul_parasites()$time[simul_parasites()$msw_open == TRUE])/24
    return(paste0('The MPC is reached at time ', round(t_open, 2), ' days'))
  })
  
  # MIC, Concentration at which we seee a growth of S
  output$conc_growth_s <- renderText({
    adj_growth_s <- log(input$growth_s)/48
    conc_start_growth_s <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
    print(conc_start_growth_s)
    paste0("Minimum Inhibitory Concentration (MIC) — sensitive parasites will grow if the drug concentration is less than ", round(conc_start_growth_s, 2), ' mg/L')
  })
  
  # Time MIC
  output$time_mic <- renderText({
    t_close <- (simul_parasites()$time[simul_parasites()$msw_close == TRUE])/24
    return(paste0('The MIC is reached at time ', round(t_close, 2), ' days'))
  })
  
  # Duration MSW
  output$duration_msw <- renderText({
    t_open <- (simul_parasites()$time[simul_parasites()$msw_open == TRUE])/24
    t_close <- (simul_parasites()$time[simul_parasites()$msw_close == TRUE])/24
    return(paste0('The Mutant Selection Window opens for ', round(24*(t_close - t_open), 2), ' hours.'))
  })
  
  # Total time for which R >= 0.1*S
  output$danger_time <- renderText({
  danger_days <- round(10*sum(simul_parasites()$prop_R > 0.1) / 240, 1)
  paste0("For ", danger_days, " days, the ratio R on S is above 10%")
  })
    
    
  
  # Messages warning user of odd parameter choice ==========================
  output$message_rate <- renderText({
    if(input$growth_s <= input$growth_r) return(paste0(div(class = 'alertbox', 'It is expected that the multiplicaton rate of sensitive parasites is greater than the one or resistant parasites (fitness of cost).')))
    if(input$growth_s > input$growth_r) return('')
  })
  
  output$message_EC50 <- renderText({
    if(input$EC50_s > input$EC50_r) return(paste0(div(class = 'alertbox', 'It is expected that EC50 for resistant parasites is greater than EC50 for sensitive parasites.')))
    if(input$EC50_s <= input$EC50_r) return('')
  })
  
  
  # Output combined graphs ====================================================
  output$combined_graphs <- renderPlot({
    req(simul_concentration(), simul_parasites())
    
    # Elements for lines
    adj_growth_s <- log(input$growth_s)/48
    conc_start_growth_s <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
    adj_growth_r <- log(input$growth_r)/48
    conc_start_growth_r <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
    t_open <- (simul_parasites()$time[simul_parasites()$msw_open == TRUE])/24
    t_close <- (simul_parasites()$time[simul_parasites()$msw_close == TRUE])/24
    
    # Graph concentration
    if(input$zoom_y == FALSE){
      graph_concentration <- simul_concentration() %>%
        ggplot(aes(x = times / 24, y = Ct)) +
        geom_line(size = 1, colour = "grey") +
        scale_x_continuous(breaks = 0:10, minor_breaks = 0.5*1:10) +
        geom_hline(yintercept = conc_start_growth_s, lty = 4, col = '#8e44ad') +
        geom_label(x = 0, y = conc_start_growth_s, label = 'MIC', col = '#8e44ad') +
        geom_hline(yintercept = conc_start_growth_r, lty = 4, col = '#8e44ad') +
        geom_label(x = 0, y = conc_start_growth_r, label = 'MPC', col = '#8e44ad') +
        geom_vline(xintercept = t_open) +
        geom_vline(xintercept = t_close) +
        labs(x = "Time (days)", y = "Drug concentration (mg/L)") +
        theme_minimal(base_size = 14)
    }

    if(input$zoom_y ==  TRUE){
      print(max(conc_start_growth_r, conc_start_growth_s))
      
      graph_concentration <- simul_concentration() %>%
        ggplot(aes(x = times / 24, y = Ct)) +
        geom_line(size = 1, colour = "grey") +
        scale_x_continuous(breaks = 0:10, minor_breaks = 0.5*1:10) +
        geom_hline(yintercept = conc_start_growth_s, lty = 4, col = '#8e44ad') +
        geom_label(x = 0, y = conc_start_growth_s, label = 'MIC', col = '#8e44ad') +
        geom_hline(yintercept = conc_start_growth_r, lty = 4, col = '#8e44ad') +
        geom_label(x = 0, y = conc_start_growth_r, label = 'MPC', col = '#8e44ad') +
        geom_vline(xintercept = t_open) +
        geom_vline(xintercept = t_close) +
        labs(x = "Time (days)", y = "Drug concentration (mg/L)") +
        theme_minimal(base_size = 14) + 
        facet_zoom(y = (Ct < 1.5 * max(conc_start_growth_r, conc_start_growth_s)), horizontal = TRUE)
    }
    
    
    graph_parasites <- ggplot(data = simul_parasites(), aes(x = time / 24)) +
      geom_line(aes(y = S, colour = 'Sensitive Parasites'), size = 1.2) +
      geom_line(aes(y = R, colour = 'Resistant Parasites'), size = 1.2) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      scale_x_continuous(breaks = 0:10) +
      geom_hline(yintercept = 1, lty = 2) +
      geom_vline(xintercept = t_open) +
      geom_vline(xintercept = t_close) +
      labs(x = "Time (days)", y = "Parasites (Log scale)") +
      scale_colour_manual(name = NULL, values = cols) +
      theme_minimal(base_size = 14) +
      theme(legend.text = element_text(size = 13), legend.position = 'top')
    
    graph_proportion_R <- ggplot(data = simul_parasites(), aes(x = time / 24)) +
      scale_y_continuous(limits = c(0, 100)) +
      geom_line(aes(y = prop_R), size = 1.2) +
      geom_hline(yintercept = 10, lty = 2) +
      labs(x = "Time (days)", y = "Proportion of resistant \n on total parasites (%)") +
      theme_minimal(base_size = 14)
    
    
    
    # graph_growthS <- ggplot(data = simul_parasites(), aes(x = time/24)) +
    #   geom_point(aes(y = growth_S), size = 1.2) +
    #   labs(x = NULL, y = NULL, title = "Growth of Sensitive?") +
    #   theme_minimal(base_size = 14)
    
    # graph_growthS <- plot(x = simul_parasites()$time, y = simul_parasites()$growth_S, type = 'p', main = 'Growth of Sensitive')
    
    # graph_growthR <- ggplot(data = simul_parasites(), aes(x = time/24)) +
    #   geom_point(aes(y = growth_R), size = 1.2) +
    #   labs(x = NULL, y = NULL, title = "Growth of Resistant?") +
    #   theme_minimal(base_size = 14)
    # 
    # graph_window_1 <- ggplot(data = simul_parasites(), aes(x = time/24)) +
    #   geom_point(aes(y = window_1), size = 1.2) +
    #   labs(x = NULL, y = NULL, title = "Is the Window v1 Open?", subtitle = "Growth of Resistant AND Resistant grow faster than Sensitive") +
    #   theme_minimal(base_size = 14)
    # 
    # graph_window_2 <- ggplot(data = simul_parasites(), aes(x = time/24)) +
    #   geom_point(aes(y = window_2), size = 1.2) +
    #   labs(x = NULL, y = NULL, title = "Is the Window v2 Open?", subtitle = "Growth of Resistant AND Resistant outnumber Sensitive") +
    #   theme_minimal(base_size = 14)
    
    
    # Combined graphs
    # graph_concentration + graph_parasites + graph_growthS + graph_growthR + graph_window_1 + graph_window_2 +
    #   plot_layout(ncol = 1, heights = c(3, 4, 1, 1, 1, 1))
    
    graph_concentration + graph_parasites + graph_proportion_R +
      plot_layout(ncol = 1, heights = c(3, 4, 2))
  })
}
)