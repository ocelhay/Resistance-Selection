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

  
  simul_concentration <- reactive({
    drug_conc = drug_concentration(dose = parameters$dose, ka = parameters$ka, Fa = parameters$Fa, V = parameters$V, CL = parameters$CL,
                                   t = times)
    return(drug_conc)
  })
  
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
    times <- seq(0, 179, by = 1)
    
    
    # No secondary infection
    if (!parameters$second_inf) {
      out <- ode(y = state, times = times, func = resistance_window, parms = list_parameters)
      out <- as.data.frame(out)
    }
    
    # Secondary infection
    if (parameters$second_inf) {
      times_pre <- times[times < parameters$t_secondary]
      out_pre <- ode(y = state, times = times_pre, func = resistance_window, parms = list_parameters)
      out_pre <- as.data.frame(out_pre)
      
      times_post <- times[times >= parameters$t_secondary]
      state_post <- c(S = (tail(out_pre$S, n = 1) + 10^12), R = (tail(out_pre$R, n = 1) + 10^5))
      out_post <- ode(y = state_post, times = times_post, func = resistance_window, parms = list_parameters)
      out_post <- as.data.frame(out_post)
      
      out <- bind_rows(out_pre, out_post)
    }
    
    # Limit the range of the y-axis (and make the growth somehow limited)
    out$S <- ifelse(out$S <= 1, 1, out$S) # (log10(1) = 0)
    out$R <- ifelse(out$R <= 1, 1, out$R)
    
    deriv <- derivative(t = times, S = out$S, R = out$R, parameters = list_parameters)
    
    print(deriv)
    out$dS <- deriv[[1]]
    out$dR <- deriv[[2]]
    
    return(out)
  })
  
  # Append result of simulations to data frame every time that there is a new simulation
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
    input$t_secondary},{
      simul$new <- FALSE
      simul$results <- tibble()
    },
    ignoreInit = TRUE)
  
  
  # Output window of selection graph ==========================================
  output$window_selection <- renderPlot({
    req(simul$new)
    
    ggplot(data = simul$results, aes(x = dose, y = 0)) +
      geom_point(aes(colour = status), shape = 25, size = 2) +
      # geom_segment(aes(xend = dose, y = -0.1, yend = 0), linetype = 2) +
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
  output$text_selection <- renderText({
    req(simul_parasites())
    
    if (any(simul_parasites()$R > simul_parasites()$S)) return(paste0(p("The model predicts ", span(class = 'resistant', 'the selection of resistant parasites.'))))
    if (! any(simul_parasites()$R > simul_parasites()$S)) return(paste0(p("The model predicts ", span(class = 'sensitive', 'no selection'))))
  })
  
  
  # Output combined graphs ====================================================
  output$combined_graphs <- renderPlot(height = 650, {
    req(simul$results, simul_concentration(), simul_parasites())
    
    # Drug concentration ======================================================
    
    graph_concentration <- data.frame(t = times, Ct = simul_concentration()) %>%
      ggplot(aes(x = t/24, y = Ct)) +
      geom_line(size = 1, colour = "grey") +
      scale_x_continuous(breaks = 0:10, minor_breaks = 0.5*1:10) +
      labs(x = "Time (days)", y = "Drug concentration") +
      theme_minimal(base_size = 14)
    
    
    
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
    cols <- c('Sensitive Parasites' = '#377eb8', 'Resistant Parasites' = '#e41a1c')

    
    # graph_parasites <- ggplot(data = simul_parasites(), aes(x = time)) +
    #   geom_line(aes(y = log10(S), colour = 'Sensitive Parasites'), size = 1.2) +
    #   geom_line(aes(y = log10(R), colour = 'Resistant Parasites'), size = 1.2) +
    #   scale_y_continuous(limits = c(0, 13), breaks = 2*1:6) +
    #   labs(x = "Time (hours)", y = "Parasites (Log scale)") +
    #   geom_vline(xintercept = 24*1:6) +
    #   scale_colour_manual(name = NULL, values = cols) +
    #   theme_minimal(base_size = 14) +
    #   theme(legend.text = element_text(size = 13), legend.position = 'top')
    
    graph_parasites <- ggplot(data = simul_parasites(), aes(x = time/24)) +
      geom_line(aes(y = log10(S), colour = 'Sensitive Parasites'), size = 1.2) +
      geom_line(aes(y = log10(R), colour = 'Resistant Parasites'), size = 1.2) +
      scale_y_continuous(limits = c(0, 13), breaks = 2*1:6) +
      labs(x = "Time (days)", y = "Parasites (Log scale)") +
      scale_colour_manual(name = NULL, values = cols) +
      theme_minimal(base_size = 14) +
      theme(legend.text = element_text(size = 13), legend.position = 'top')
    
    
    graph_deriv <- ggplot(data = simul_parasites(), aes(x = time/24)) +
      geom_line(aes(y = dS, colour = 'Sensitive Parasites'), size = 1.2) +
      geom_line(aes(y = dR, colour = 'Resistant Parasites'), size = 1.2) +
      scale_colour_manual(name = NULL, values = cols) +
      theme_minimal(base_size = 14) +
      theme(legend.text = element_text(size = 13), legend.position = 'top')
    
    
    # Combined graphs
    graph_parasites + graph_deriv + graph_concentration +
      plot_layout(ncol = 1, height = c(4, 2, 2))
  })
}
)