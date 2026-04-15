# -------------------------------
# Monte Carlo Integration - Small Multiple
# -------------------------------

# Load libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# -------------------------------
# Step 1: Monte Carlo Simulation Function
# -------------------------------
mc_simulation <- function(n, x_min, x_max, y_min, y_max){
  x_vals <- runif(n, min = x_min, max = x_max)
  y_vals <- runif(n, min = y_min, max = y_max)
  data.frame(x = x_vals, y = y_vals)
}

# -------------------------------
# Step 2: Flag points and compute integral estimate
# -------------------------------
flag_points_and_estimate <- function(sim_data){
  sim_data_flagged <- sim_data %>%
    mutate(
      f_x = dbeta(x, shape1 = 2, shape2 = 2),
      flag = ifelse(y <= f_x, "on/below", "above")
    )
  
  p <- mean(sim_data_flagged$flag == "on/below")  # proportion of points on/below curve
  rect_area <- (1 - 0) * (1.5 - 0)  # rectangle bounds
  estimate <- p * rect_area          # Monte Carlo estimate of integral
  
  list(data = sim_data_flagged, estimate = estimate)
}

# -------------------------------
# Step 3: Plotting function
# -------------------------------
plot_mc <- function(sim_data_flagged, estimate){
  ggplot(sim_data_flagged, aes(x = x, y = y, color = flag)) +
    geom_point(alpha = 0.6, size = 1.5) +
    stat_function(
      fun = dbeta,
      args = list(shape1 = 2, shape2 = 2),
      xlim = c(0, 1),
      color = "blue",
      linewidth = 1
    ) +
    labs(
      title = paste("Monte Carlo Integration (n =", nrow(sim_data_flagged), ")"),
      subtitle = paste("Estimated Integral =", round(estimate, 4)),
      x = "x",
      y = "y",
      color = "Flag"
    ) +
    theme_minimal()
}

# -------------------------------
# Step 4: Generate simulations at different resolutions
# -------------------------------
sim100 <- mc_simulation(100, 0, 1, 0, 1.5)
sim500 <- mc_simulation(500, 0, 1, 0, 1.5)
sim1000 <- mc_simulation(1000, 0, 1, 0, 1.5)
sim10000 <- mc_simulation(10000, 0, 1, 0, 1.5)

# -------------------------------
# Step 5: Flag points and compute estimates
# -------------------------------
flag100 <- flag_points_and_estimate(sim100)
flag500 <- flag_points_and_estimate(sim500)
flag1000 <- flag_points_and_estimate(sim1000)
flag10000 <- flag_points_and_estimate(sim10000)

# -------------------------------
# Step 6: Create individual plots
# -------------------------------
plot100 <- plot_mc(flag100$data, flag100$estimate)
plot500 <- plot_mc(flag500$data, flag500$estimate)
plot1000 <- plot_mc(flag1000$data, flag1000$estimate)
plot10000 <- plot_mc(flag10000$data, flag10000$estimate)

# -------------------------------
# Step 7: Combine plots into small multiple
# -------------------------------
plot100 + plot500 + plot1000 + plot10000