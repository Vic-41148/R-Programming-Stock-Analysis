# Install packages if needed
install.packages(c("deSolve", "ggplot2"))

# Load packages
library(deSolve)
library(ggplot2)

# Step 1: Define the SIR differential equations
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}

# Step 2: Set initial values
N <- 1000         # total population
I0 <- 1           # initial infected
R0 <- 0           # initial recovered
S0 <- N - I0 - R0 # initial susceptible

# Parameters
beta <- 0.3       # infection rate
gamma <- 0.1      # recovery rate

# Time frame
time <- seq(0, 160, by = 1)

# Initial state and parameters
init_state <- c(S = S0, I = I0, R = R0)
parameters <- c(beta = beta, gamma = gamma)

# Step 3: Solve using ode()
sir_output <- ode(y = init_state, times = time, func = sir_model, parms = parameters)
sir_output <- as.data.frame(sir_output)
sir_output$time <- as.integer(sir_output$time)

# Step 4: Plot using ggplot2
sir_data <- reshape2::melt(sir_output, id = "time")

ggplot(sir_data, aes(x = time, y = value, color = variable)) +
  geom_line(size = 1.2) +
  labs(
    title = "SIR Model of Disease Spread",
    x = "Time (days)",
    y = "Number of Individuals",
    color = "Compartment"
  ) +
  theme_minimal()
