#### Introduction ####
# This script contains some exploratory models
# about COVID-19.

#### Standard SEIR model ####

# No demographics, no mortality

# Differential equations
# dS/dt = - beta * S * I / N
# dE/dt = (beta * S * I / N) - alpha * E
# dI/dt = alpha * E - gamma * I
# dR/dt = gamma * I

# Rough COVID-19 parameters:
# - R0 = 1.5 (assumes some NPIs)
# - infectious 2 days before Sx
# - incubation period ~ 5 days
# - latent period ~ 3 days
# - duration of infectiousness ~ 10 days

#### load packages ####
library(deSolve)
library(reshape2)
library(ggplot2)
library(tidyverse)

# basic parameters
parameters <- c(gamma = 0.1,  # recovery rate; implies avg. duration = 10d
                beta = 0.6, # R0 = 6 and R0 = beta/gamma implies beta = 0.6,
                alpha = 0.33 # defines latent period of 3 days
                )

# Define time points (days) - model 6 months 
times <- seq(from = 0, to = 180, by = 1)

# Initial state
initial_state <- c(S = (10^6) - 1,
                   E = 0,
                   I = 1,
                   R = 0)

# SEIR model
seir_model <- function(time, state, parameters){
  with(as.list(c(state, parameters)),{
    # Calculate N
    N <- sum(S, E, I, R)
    
    # differential equations
    dS = - beta * I/N * S
    dE = (beta * I/N * S) - (alpha * E)
    dI = (alpha * E) - (gamma * I)
    dR = (gamma * I)
    
    # Return output
    return(list(c(dS, dE, dI, dR)))
  })
}

# Evaluate the model at each time point
out_1 <- ode(y = initial_state,
             times = times,
             func = seir_model,
             parms = parameters)

# Convert to proportions & calculate Reff
out_1 <- data.frame(out_1)

out_1 <- mutate(out_1,
                N = S+I+R,
                Sp = S/N,
                Ep = E/N,
                Ip = I/N,
                Rp = R/N,
                Reff = (parameters["beta"]/parameters["gamma"]) * Sp)

out_1_long <- melt(out_1, id = "time")

# Plot the standard SEIR model
g1 <- out_1_long %>% 
  filter(variable %in% c("Sp", "Ep", "Ip", "Rp")) %>% 
  ggplot(aes(x = time,
             y = value,
             group = variable,
             colour = variable)) +
    geom_line() +
    labs(x = "time (days)",
         y = NULL,
         title = "Proportion in each compartment") +
    theme_classic() +
    theme(plot.title = element_text(size = 10, face = "bold"),
          legend.position = "top")

# Plot Reff
g2 <- out_1_long %>% 
  filter(variable == "Reff") %>% 
  ggplot(aes(x = time, y = value)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 3)) +
  labs(x = "time (days)",
       y = NULL,
       title = "Effective reproduction rate") +
  theme_classic() +
  theme(plot.title = element_text(size = 10, face = "bold"))

# get time when Reff is closest to 1.5
int_time <- 50 

#### model with intervention ####

seir_int_model <- function(time, state, parameters){
  with(as.list(c(state, parameters)),{
    # Calculate N
    N <- sum(S, E, I, R)
    
    # adjust beta down
    if(time >= int_time){beta <- 0.2}
    
    # differential equations
    dS = - beta * I/N * S
    dE = (beta * I/N * S) - (alpha * E)
    dI = (alpha * E) - (gamma * I)
    dR = (gamma * I)
    
    # Return output
    return(list(c(dS, dE, dI, dR)))
  })
}

# Evaluate the model at each time point
out_2 <- ode(y = initial_state,
             times = times,
             func = seir_int_model,
             parms = parameters)

# Convert to proportions & calculate Reff
out_2 <- data.frame(out_2)

out_2 <- mutate(out_2,
                N = S+I+R,
                Sp = S/N,
                Ep = E/N,
                Ip = I/N,
                Rp = R/N,
                Reff = (parameters["beta"]/parameters["gamma"]) * Sp)

out_2_long <- melt(out_2, id = "time")

# Plot the standard SEIR model
g3 <- out_2_long %>% 
  filter(variable %in% c("Sp", "Ep", "Ip", "Rp")) %>% 
  ggplot(aes(x = time,
             y = value,
             group = variable,
             colour = variable)) +
  geom_line() +
  labs(x = "time (days)",
       y = NULL,
       title = "Proportion in each compartment") +
  theme_classic() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        legend.position = "top")

# Plot Reff
g4 <- out_2_long %>% 
  filter(variable == "Reff") %>% 
  ggplot(aes(x = time, y = value)) +
  geom_line() +
  labs(x = "time (days)",
       y = NULL,
       title = "Effective reproduction rate") +
  theme_classic() +
  theme(plot.title = element_text(size = 10, face = "bold"))

#### Plot infection prevalence in two models ####
prevs <- data.frame(out_1$time,
                    out_1$Ip,
                    out_2$Ip)
prevs <- rename(prevs,
                time = out_1.time,
                prev_unmitigated = out_1.Ip,
                prev_mitigated = out_2.Ip)

prevs_long <- melt(prevs,
                   id = "time")

g5 <- ggplot(data = prevs_long,
             aes(x = time,
                 y = value * 100,
                 group = variable,
                 colour = variable)) +
  geom_line() +
  scale_colour_discrete(labels = c("unmitigated",
                                 "mitigated")) +
  labs(x = "time (days)",
       y = "prevalence (%)",
       title = "Prevalence in mitigated vs unmitigated SEIR models",
       colour = NULL) +
  theme_classic() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10)) +
  theme(legend.position = "top")

g5
