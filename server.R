shinyServer(
function(input, output) {
  
  # update default values based on inputs
  parameters = reactiveValues(growth = init_param$growth,
                              dose = init_param$dose,
                              ka = init_param$ka,
                              Fa = init_param$Fa,
                              V = init_param$V,
                              CL = init_param$CL,
                              k1 = init_param$k1,
                              n = init_param$n,
                              EC50s = init_param$EC50s,
                              EC50r = init_param$EC50r,
                              second_inf = init_param$second_inf,
                              t_secondary = init_param$t_secondary)
  
  observe({
    parameters$growth = input$growth
    parameters$dose = input$dose
    parameters$ka = input$ka
    parameters$Fa = input$Fa
    parameters$V = input$V
    parameters$CL = input$CL
    parameters$k1 = input$k1
    parameters$n = input$n
    parameters$EC50s = input$EC50s
    parameters$EC50r = input$EC50r
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
    list_parameters <- list(growth = parameters$growth,
                            dose = parameters$dose,
                            ka = parameters$ka,
                            Fa = parameters$Fa,
                            V = parameters$V,
                            CL = parameters$CL,
                            k1 = parameters$k1,
                            n = parameters$n,
                            EC50s = parameters$EC50s,
                            EC50r = parameters$EC50r)
    
    # No secondary infection
    if (!parameters$second_inf) {
      out <- ode(y = state, times = times, func = resistance_window, parms = list_parameters, method = "adams")
      out <- as.data.frame(out)
    }
    
    # Secondary infection
    if (parameters$second_inf) {
      times_pre <- times[times < parameters$t_secondary]
      out_pre <- ode(y = state, times = times_pre, func = resistance_window, parms = list_parameters, method = "adams")
      out_pre <- as.data.frame(out_pre)
      
      times_post <- times[times >= parameters$t_secondary]
      state_post <- c(S = (tail(out_pre$S, n = 1) + 10^12), R = (tail(out_pre$R, n = 1) + 10^5))
      out_post <- ode(y = state_post, times = times_post, func = resistance_window, parms = list_parameters, method = "adams")
      out_post <- as.data.frame(out_post)
      
      out <- bind_rows(out_pre, out_post)
    }
    
    # Limit the range of the y-axis (and make the growth somehow limited)
    out$S <- ifelse(out$S <= 1, 1, out$S) # (log10(1) = 0)
    out$R <- ifelse(out$R <= 1, 1, out$R)
    
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
    input$growth
    input$ka
    input$Fa
    input$V
    input$CL
    input$k1
    input$n
    input$EC50s
    input$EC50r
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
  output$half_life <- renderText(paste0("Half-life of the drug = ", round(0.693 * parameters$V / parameters$CL, 2), ' hours.'))
  
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
      ggplot(aes(x = t, y = Ct)) +
      geom_line(size = 1, colour = "grey") +
      labs(x = "Time (hours)", y = "Drug concentration") +
      theme_classic(base_size = 13)
    
    
    
    # Dose response ===========================================================
    
    graph_dose_response <- 
      data.frame(t = times,
                 fC_S = dose_response(k1 = parameters$k1, Ct = simul_concentration(), 
                                      EC50 = parameters$EC50s, n = parameters$n),
                 fC_R = dose_response(k1 = parameters$k1, Ct = simul_concentration(), 
                                      EC50 = parameters$EC50r, n = parameters$n)) %>%
      ggplot(aes(x = times)) +
      geom_line(aes(y = fC_S), colour = "#377eb8", linetype = "dashed") +
      geom_line(aes(y = fC_R), colour = "#e41a1c", linetype = "dashed") +
      labs(x = "Time (hours)", y = "Parasite killing rates") +
      theme_classic(base_size = 13)
    
    
    # Parasites ===============================================================
    cols <- c('Sensitive Parasites' = '#377eb8', 'Resistant Parasites' = '#e41a1c')
    
    graph_parasites <- ggplot(data = simul_parasites(), aes(x = time)) +
      geom_line(aes(y = log10(S), colour = 'Sensitive Parasites'), size = 1.2) +
      geom_line(aes(y = log10(R), colour = 'Resistant Parasites'), size = 1.2) +
      scale_y_continuous(limits = c(0, 13)) +
      labs(x = "Time (hours)", y = "Parasites (Log scale)") +
      scale_colour_manual(name = NULL, values = cols) +
      theme_classic(base_size = 14) +
      theme(legend.text = element_text(size = 13), legend.position = 'top')
    
    
    # Combined graphs
    graph_parasites + graph_concentration + graph_dose_response +
      plot_layout(ncol = 1, height = c(4, 2, 2))
  })
}
)