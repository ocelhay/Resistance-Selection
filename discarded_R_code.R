# Output: selection =========================================================
# output$text_selection <- renderText({
#   req(simul_parasites())
#   
#   if (any(simul_parasites()$R > simul_parasites()$S)) return(paste0(p("The model predicts ", span(class = 'resistant', 'the selection of resistant parasites.'))))
#   if (! any(simul_parasites()$R > simul_parasites()$S)) return(paste0(p("The model predicts ", span(class = 'sensitive', 'no selection'))))
# })

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


# geom_vline(data = window_open, aes(xintercept = x /24), lty = 2) +
# geom_label(data = window_open, aes(x = x /24, y = y2, label = label)) +
# geom_hline(data = window_open, aes(yintercept = mpc), lty = 4, col = '#8e44ad') +
# geom_label(data = window_open, aes(x = 0, y = mpc, label = 'MPC')) +
# 
# geom_vline(data = window_close, aes(xintercept = x /24), lty = 2) +
# geom_label(data = window_close, aes(x = x / 24, y = y2, label = label)) +
# geom_hline(data = window_close, aes(yintercept = mic), lty = 4, col = '#8e44ad') +
# geom_label(data = window_close, aes(x = 0, y = mic, label = 'MIC')) +



# geom_vline(data = window_open, aes(xintercept = x /24), lty = 2) +
# geom_label(data = window_open, aes(x = x /24, y = y2, label = label)) +
# geom_hline(data = window_open, aes(yintercept = mpc), lty = 4, col = '#8e44ad') +
# geom_label(data = window_open, aes(x = 0, y = mpc, label = 'MPC')) +
# 
# geom_vline(data = window_close, aes(xintercept = x /24), lty = 2) +
# geom_label(data = window_close, aes(x = x / 24, y = y2, label = label)) +
# geom_hline(data = window_close, aes(yintercept = mic), lty = 4, col = '#8e44ad') +
# geom_label(data = window_close, aes(x = 0, y = mic, label = 'MIC')) +
