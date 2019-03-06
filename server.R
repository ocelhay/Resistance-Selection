shinyServer(
function(input, output) {
  
  # update default values based on inputs
  parameters = reactiveValues(growth_s = init_param$growth_s,
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
  
  
  # Run simulations ===========================================================

  # times = c(seq(0, 1.99, by = 0.01), 2:240)
  # only for plot
  simul_concentration <- reactive({
    drug_concentration(dose = parameters$dose, ka = parameters$ka, Fa = parameters$Fa, V = parameters$V, CL = parameters$CL, t = times)
  })
  
  # times = seq(0, 240, by = 1)
  simul_parasites <- reactive({
    list_parameters <- list(growth_s = parameters$growth_s,
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
    # Start time at 0
    times <- seq(0, 240, by = 1)
    
    
    # No secondary infection
    if (!parameters$second_inf) {
      out <- ode(y = state, times = times, func = resistance_window, parms = list_parameters)
      out <- as.data.frame(out)
      
      # there should be no come back from S < 1 or R < 1
      if(any(out$S < 1)) out$S[min(which(out$S < 1)):length(out$S)] <- 1
      if(any(out$R < 1)) out$R[min(which(out$R < 1)):length(out$R)] <- 1
    }
    
    # Secondary infection
    if (parameters$second_inf) {
      times_pre <- times[times < parameters$t_secondary]
      out_pre <- ode(y = state, times = times_pre, func = resistance_window, parms = list_parameters)
      out_pre <- as.data.frame(out_pre)
      
      # there should be no come back from S < 1 or R < 1
      if(any(out_pre$S < 1)) out_pre$S[min(which(out_pre$S < 1)):length(out_pre$S)] <- 1
      if(any(out_pre$R < 1)) out_pre$R[min(which(out_pre$R < 1)):length(out_pre$R)] <- 1
      
      times_post <- times[times >= parameters$t_secondary]
      state_post <- c(S = (tail(out_pre$S, n = 1) + 10^12), R = (tail(out_pre$R, n = 1) + 10^5))
      out_post <- ode(y = state_post, times = times_post, func = resistance_window, parms = list_parameters)
      out_post <- as.data.frame(out_post)
      
      # there should be no come back from S < 1 or R < 1
      if(any(out_post$S < 1)) out_post$S[min(which(out_post$S < 1)):length(out_post$S)] <- 1
      if(any(out_post$R < 1)) out_post$R[min(which(out_post$R < 1)):length(out_post$R)] <- 1
      
      out <- bind_rows(out_pre, out_post)
    }
    
    # Add derivative and absolute and relative growth indicators
    out$Ct <- (list_parameters$dose * list_parameters$ka * list_parameters$Fa)/(list_parameters$V * list_parameters$ka - list_parameters$CL) * ( exp(-list_parameters$CL / list_parameters$V * times) - exp(-list_parameters$ka * times) )
    
    out <- out %>%
      # mutate(dS = log(list_parameters$growth_s)/48 * S - (-list_parameters$k1 * Ct^list_parameters$n / (Ct^list_parameters$n + list_parameters$EC50_s^list_parameters$n)) * S,
      #        dR = log(list_parameters$growth_r)/48  * R - (-list_parameters$k1 * Ct^list_parameters$n / (Ct^list_parameters$n + list_parameters$EC50_r^list_parameters$n)) * R) %>%
      mutate(dS = c(0, diff(S)),
             dR = c(0, diff(R))) %>%
      mutate(
        RsupS = (R > S),
        growth_S = (dS > 0),
        growth_R = (dR > 0),
        growth_RsupS = (dR > dS)) %>%
      mutate(window_1 = growth_R & growth_RsupS,
             window_2 = growth_R & RsupS)
    
    print(out)
    
    return(out)
  })
  
  # Append result of simulations to data frame every time that there is a new simulation 
  # (to show if for a specific initial dose there is a window opening)
  # START DISCARDED but kept just in case
  simul <- reactiveValues(new = FALSE, results = tibble())
  
  observeEvent(simul_parasites(), {
    simul$new <- TRUE
    simul$results <- bind_rows(simul$results,
                               tibble(dose = parameters$dose, 
                                      status = ifelse (any(simul_parasites()$R > simul_parasites()$S), "Selection", "No Selection")))
  })
  
  # A change for these parameters should triger a new graph
  observeEvent({
    input$growth_s
    input$growth_r
    input$ka
    input$Fa
    input$V
    input$CL
    input$k1
    input$n
    input$EC50_s
    input$EC50_r
    input$second_inf
    input$t_secondary}, {
      simul$new <- FALSE
      simul$results <- tibble()
    }, ignoreInit = TRUE)
  # END DISCARDED but kept just in case
  
  
  # Output window of selection graph ==========================================
  output$window_selection <- renderPlot({
    req(simul$new)
    
    ggplot(data = simul$results, aes(x = dose, y = 0)) +
      geom_point(aes(colour = status), shape = 25, size = 2) +
      geom_text(aes(label = status, colour = status), fontface = "bold", angle = 30, hjust = 0, vjust = -0.5, size = 5, show.legend = FALSE) +
      scale_x_continuous(limits = c(50, 1100)) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_colour_manual(values = c('No Selection' = '#377eb8', 'Selection' = '#e41a1c')) +
      labs(x = "Dose (mg)", y = NULL) +
      theme_classic(base_size = 13) +
      guides(colour = FALSE) +
      theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), panel.border = element_blank())
  })
  
  
  # Output half-life =======================================================
  output$half_life <- renderText(paste0("Based on provided parameters values, the half-life is ", round(0.693 * parameters$V / parameters$CL, 2), ' hours.'))
  
  # Concentration at which we seee a growth of R
  output$conc_growth_r <- renderText({
    adj_growth_r <- log(input$growth_r)/48
    conc_start_growth_r <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
    print(conc_start_growth_r)
    paste0("Based on parasites attributes (EC50r...), resistant parasites will grow if the drug concentration is less than ", round(conc_start_growth_r, 2), ' mg/L')
  })
  
  # Concentration at which we seee a growth of R
  output$conc_growth_s <- renderText({
    adj_growth_s <- log(input$growth_s)/48
    conc_start_growth_s <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
    print(conc_start_growth_s)
    paste0("Based on parasites attributes (EC50s...), sensitive parasites will grow if the drug concentration is less than ", round(conc_start_growth_s, 2), ' mg/L')
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
  
  
  # Output: selection =========================================================
  # output$text_selection <- renderText({
  #   req(simul_parasites())
  #   
  #   if (any(simul_parasites()$R > simul_parasites()$S)) return(paste0(p("The model predicts ", span(class = 'resistant', 'the selection of resistant parasites.'))))
  #   if (! any(simul_parasites()$R > simul_parasites()$S)) return(paste0(p("The model predicts ", span(class = 'sensitive', 'no selection'))))
  # })
  
  
  # Output combined graphs ====================================================
  output$combined_graphs <- renderPlot({
    req(simul$results, simul_concentration(), simul_parasites())
    
    
    
    
    
    # Dose response ===========================================================
    
    # graph_dose_response <- 
    #   data.frame(t = times,
    #              fC_S = dose_response(k1 = parameters$k1, Ct = simul_concentration(), 
    #                                   EC50 = parameters$EC50s, n = parameters$n),
    #              fC_R = dose_response(k1 = parameters$k1, Ct = simul_concentration(), 
    #                                   EC50 = parameters$EC50r, n = parameters$n)) %>%
    #   ggplot(aes(x = times)) +
    #   geom_line(aes(y = fC_S), colour = "#377eb8", linetype = "dashed") +
    #   geom_line(aes(y = fC_R), colour = "#e41a1c", linetype = "dashed") +
    #   labs(x = "Time (hours)", y = "Parasite killing rates") +
    #   theme_classic(base_size = 13)
    
    
    # Parasites ===============================================================
  
    
    # graph_parasites <- ggplot(data = simul_parasites(), aes(x = time)) +
    #   geom_line(aes(y = log10(S), colour = 'Sensitive Parasites'), size = 1.2) +
    #   geom_line(aes(y = log10(R), colour = 'Resistant Parasites'), size = 1.2) +
    #   scale_y_continuous(limits = c(0, 13), breaks = 2*1:6) +
    #   labs(x = "Time (hours)", y = "Parasites (Log scale)") +
    #   geom_vline(xintercept = 24*1:6) +
    #   scale_colour_manual(name = NULL, values = cols) +
    #   theme_minimal(base_size = 14) +
    #   theme(legend.text = element_text(size = 13), legend.position = 'top')
    
    # Labels for window opening and closing
    # window_open <- data_frame(
    #   x = which(c(0, diff(simul_parasites()$window_2)) == 1),
    #   y = 10^12,
    #   y2 = 0.9 * max(simul_parasites()$Ct),
    #   label = 'OPEN (v2)') %>%
    #   mutate(mpc = simul_parasites()$Ct[x])
    # 
    # print(window_open)
    # 
    # window_close <- data_frame(
    #   x = which(c(0, diff(simul_parasites()$window_2)) == -1),
    #   y = 10^12,
    #   y2 = 0.9 * max(simul_parasites()$Ct),
    #   label = 'CLOSE (v2)') %>%
    #   mutate(mic = simul_parasites()$Ct[x])
    # 
    # print(window_open)
    
    
    adj_growth_s <- log(input$growth_s)/48
    conc_start_growth_s <- (-(adj_growth_s / input$k1) * (input$EC50_s ^ input$n))/(1 + (adj_growth_s / input$k1))
    
    adj_growth_r <- log(input$growth_r)/48
    conc_start_growth_r <- (-(adj_growth_r / input$k1) * (input$EC50_r ^ input$n))/(1 + (adj_growth_r / input$k1))
    
    if(input$limit_y_concentration == 1000)
      graph_concentration <- data.frame(t = times, Ct = simul_concentration()) %>%
      ggplot(aes(x = t / 24, y = Ct)) +
      geom_line(size = 1, colour = "grey") +
      scale_x_continuous(breaks = 0:10, minor_breaks = 0.5*1:10) +
      geom_hline(yintercept = conc_start_growth_s, lty = 4, col = '#8e44ad') +
      geom_label(x = 0, y = conc_start_growth_s, label = 'MIC', lty = 4, col = '#8e44ad') +
      geom_hline(yintercept = conc_start_growth_r, lty = 4, col = '#8e44ad') +
      geom_label(x = 0, y = conc_start_growth_r, label = 'MPC', lty = 4, col = '#8e44ad') +
      
      # geom_vline(data = window_open, aes(xintercept = x /24), lty = 2) +
      # geom_label(data = window_open, aes(x = x /24, y = y2, label = label)) +
      # geom_hline(data = window_open, aes(yintercept = mpc), lty = 4, col = '#8e44ad') +
      # geom_label(data = window_open, aes(x = 0, y = mpc, label = 'MPC')) +
      # 
      # geom_vline(data = window_close, aes(xintercept = x /24), lty = 2) +
      # geom_label(data = window_close, aes(x = x / 24, y = y2, label = label)) +
      # geom_hline(data = window_close, aes(yintercept = mic), lty = 4, col = '#8e44ad') +
      # geom_label(data = window_close, aes(x = 0, y = mic, label = 'MIC')) +
      
    labs(x = "Time (days)", y = "Drug concentration (mg/L)") +
      theme_minimal(base_size = 14)

    if(input$limit_y_concentration != 1000)
      graph_concentration <- data.frame(t = times, Ct = simul_concentration()) %>%
      ggplot(aes(x = t / 24, y = Ct)) +
      geom_line(size = 1, colour = "grey") +
      scale_x_continuous(breaks = 0:10, minor_breaks = 0.5*1:10) +
      geom_hline(yintercept = conc_start_growth_s, lty = 4, col = '#8e44ad') +
      geom_label(x = 0, y = conc_start_growth_s, label = 'MIC', lty = 4, col = '#8e44ad') +
      geom_hline(yintercept = conc_start_growth_r, lty = 4, col = '#8e44ad') +
      geom_label(x = 0, y = conc_start_growth_r, label = 'MPC', lty = 4, col = '#8e44ad') +
      
      # geom_vline(data = window_open, aes(xintercept = x /24), lty = 2) +
      # geom_label(data = window_open, aes(x = x /24, y = y2, label = label)) +
      # geom_hline(data = window_open, aes(yintercept = mpc), lty = 4, col = '#8e44ad') +
      # geom_label(data = window_open, aes(x = 0, y = mpc, label = 'MPC')) +
      # 
      # geom_vline(data = window_close, aes(xintercept = x /24), lty = 2) +
      # geom_label(data = window_close, aes(x = x / 24, y = y2, label = label)) +
      # geom_hline(data = window_close, aes(yintercept = mic), lty = 4, col = '#8e44ad') +
      # geom_label(data = window_close, aes(x = 0, y = mic, label = 'MIC')) +
      
    labs(x = "Time (days)", y = "Drug concentration (mg/L)") +
      theme_minimal(base_size = 14) + 
      scale_y_continuous(limits = c(0, input$limit_y_concentration))
    
    
    graph_parasites <- ggplot(data = simul_parasites(), aes(x = time / 24)) +
      geom_line(aes(y = S, colour = 'Sensitive Parasites'), size = 1.2) +
      geom_line(aes(y = R, colour = 'Resistant Parasites'), size = 1.2) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      scale_x_continuous(breaks = 0:10) +
      geom_hline(yintercept = 1, lty = 2) +
      # geom_vline(data = window_open, aes(xintercept = x / 24), lty = 2) +
      # geom_label(data = window_open, aes(x = x / 24, y = y, label = label)) +
      # geom_vline(data = window_close, aes(xintercept = x / 24), lty = 2) +
      # geom_label(data = window_close, aes(x = x / 24, y = y, label = label)) +
      labs(x = "Time (days)", y = "Parasites (Log scale)") +
      scale_colour_manual(name = NULL, values = cols) +
      theme_minimal(base_size = 14) +
      theme(legend.text = element_text(size = 13), legend.position = 'top')
    
    
    
    graph_growthS <- ggplot(data = simul_parasites(), aes(x = time/24)) +
      geom_point(aes(y = growth_S), size = 1.2) +
      labs(x = NULL, y = NULL, title = "Growth of Sensitive?") +
      theme_minimal(base_size = 14)
    
    # graph_growthS <- plot(x = simul_parasites()$time, y = simul_parasites()$growth_S, type = 'p', main = 'Growth of Sensitive')
    
    graph_growthR <- ggplot(data = simul_parasites(), aes(x = time/24)) +
      geom_point(aes(y = growth_R), size = 1.2) +
      labs(x = NULL, y = NULL, title = "Growth of Resistant?") +
      theme_minimal(base_size = 14)
    
    graph_window_1 <- ggplot(data = simul_parasites(), aes(x = time/24)) +
      geom_point(aes(y = window_1), size = 1.2) +
      labs(x = NULL, y = NULL, title = "Is the Window v1 Open?", subtitle = "Growth of Resistant AND Resistant grow faster than Sensitive") +
      theme_minimal(base_size = 14)
    
    graph_window_2 <- ggplot(data = simul_parasites(), aes(x = time/24)) +
      geom_point(aes(y = window_2), size = 1.2) +
      labs(x = NULL, y = NULL, title = "Is the Window v2 Open?", subtitle = "Growth of Resistant AND Resistant outnumber Sensitive") +
      theme_minimal(base_size = 14)
    
    
    # Combined graphs
    graph_concentration + graph_parasites + graph_growthS + graph_growthR + graph_window_1 + graph_window_2 +
      plot_layout(ncol = 1, heights = c(3, 4, 1, 1, 1, 1))
    
    # graph_parasites + graph_concentration +
    #   plot_layout(ncol = 1, height = c(4, 2))
  })
}
)