# Part 1: Statistical Analysis 
# Load necessary libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(boot)


# Read the data
data <- read.csv("ScanRecords.csv")

# Convert Date column to Date type
data$Date <- as.Date(data$Date)

# Separate data for Type 1 and Type 2 patients
type1_data <- data[data$PatientType == "Type 1", ]
type2_data <- data[data$PatientType == "Type 2", ]

type1_daily_counts <- table(type1_data$Date)
type2_daily_counts <- table(type2_data$Date)

# Estimate lambda for Type 1 (Poisson distribution parameter)
lambda_hat <- mean(type1_daily_counts)

# Parametric Poisson Bootstrap for Type 1 Daily Counts
parametric_poisson_bootstrap <- function(data, indices) {
  # Resample daily counts from Poisson with estimated lambda
  rpois(n = length(indices), lambda = lambda_hat)
}

# Parametric Bootstrap for Type 1 Scan Durations (Normal)
parametric_scan_bootstrap <- function(data, indices) {
  sampled_data <- data[indices, ]
  mean_duration <- mean(sampled_data$Duration)
  sd_duration <- sd(sampled_data$Duration)
  rnorm(n = nrow(sampled_data), mean = mean_duration, sd = sd_duration)
}

# Non-Parametric Bootstrap for Type 2 Daily Counts
non_parametric_count_bootstrap <- function(data, indices) {
  sampled_counts <- data[indices]
  sampled_counts
}

# Non-Parametric Bootstrap for Type 2 Scan Durations
non_parametric_scan_bootstrap <- function(data, indices) {
  sampled_data <- data[indices, ]
  sampled_data$Duration
}

# Combined Bootstrap Function
mixed_bootstrap <- function(data, indices) {
  # Separate indices for Type 1 and Type 2
  count_indices <- 1:length(type1_daily_counts)
  scan_indices <- indices[indices <= nrow(type1_data)]
  type2_scan_indices <- indices[indices > nrow(type1_data)] - nrow(type1_data)
  
  # Resample daily counts for Type 1 (Poisson) and Type 2 (Non-Parametric)
  type1_daily_sample <- parametric_poisson_bootstrap(type1_daily_counts, count_indices)
  type2_daily_sample <- non_parametric_count_bootstrap(type2_daily_counts, count_indices)
  
  # Resample scan durations for Type 1 (Normal) and Type 2 (Non-Parametric)
  type1_scan_sample <- parametric_scan_bootstrap(type1_data, scan_indices)
  type2_scan_sample <- non_parametric_scan_bootstrap(type2_data, type2_scan_indices)
  
  # Combine statistics
  c(
    mean(type1_daily_sample), sd(type1_daily_sample),  # Type 1 Daily Counts
    mean(type1_scan_sample), sd(type1_scan_sample), median(type1_scan_sample),  # Type 1 Scan Durations
    mean(type2_daily_sample), sd(type2_daily_sample),  # Type 2 Daily Counts
    mean(type2_scan_sample), sd(type2_scan_sample), median(type2_scan_sample)  # Type 2 Scan Durations
  )
}

# Run the Mixed Bootstrap
set.seed(123)
combined_data <- rbind(type1_data, type2_data)
results <- boot(data = combined_data, statistic = mixed_bootstrap, R = 1000)

# Extract statistics
bootstrap_means <- colMeans(results$t)
bootstrap_cis <- apply(results$t, 2, quantile, probs = c(0.025, 0.975))

# Organize results
bootstrap_results <- data.frame(
  Statistic = c(
    "Mean Type 1 Daily Count", "SD Type 1 Daily Count",
    "Mean Type 1 Duration", "SD Type 1 Duration", "Median Type 1 Duration",
    "Mean Type 2 Daily Count", "SD Type 2 Daily Count",
    "Mean Type 2 Duration", "SD Type 2 Duration", "Median Type 2 Duration"
  ),
  Estimate = bootstrap_means,
  CI_Lower = bootstrap_cis[1, ],
  CI_Upper = bootstrap_cis[2, ]
)

# Print the results
print(bootstrap_results)

###----------------------------------------------------------------------------###
# Part 2: Scheduling
# Load required libraries
library(simmer)
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(1234)

# Parameters
operating_hours <- 9 * 60  # Total minutes from 8:00 to 17:00
type1_initial_slot <- 30   # Initial time slot for Type 1 patients in minutes
type2_initial_slot <- 45   # Initial time slot for Type 2 patients in minutes
fixed_slot <- 40           # Fixed time slot for Policy 2
type1_daily_rate <- 16.5   # Estimated daily arrival rate for Type 1 patients
type2_daily_rate <- 10.4   # Estimated daily arrival rate for Type 2 patients

# Arrival time distributions (Exponential based on daily rates)
type1_arrival <- function() rexp(1, rate = type1_daily_rate / operating_hours)
type2_arrival <- function() rexp(1, rate = type2_daily_rate / operating_hours)

# Simulate Policy 1 (Dedicated machines with different time slots)
simulate_policy1 <- function() {
  env <- simmer("Policy 1: Dedicated Facilities")
  type1_trajectory <- trajectory("Type 1 Patients") %>%
    seize("Facility_1") %>%
    timeout(type1_initial_slot) %>%
    release("Facility_1")
  
  type2_trajectory <- trajectory("Type 2 Patients") %>%
    seize("Facility_2") %>%
    timeout(type2_initial_slot) %>%
    release("Facility_2")
  
  env %>%
    add_resource("Facility_1", capacity = 1) %>%
    add_resource("Facility_2", capacity = 1) %>%
    add_generator("Type 1 Generator", type1_trajectory, type1_arrival) %>%
    add_generator("Type 2 Generator", type2_trajectory, type2_arrival) %>%
    run(until = operating_hours)
  
  list(
    utilization = get_mon_resources(env),
    arrivals = get_mon_arrivals(env)
  )
}

# Function to evaluate performance
evaluate_performance <- function(results) {
  utilization <- results$utilization
  arrivals <- results$arrivals
  
  utilization_facility_1 <- ifelse(
    nrow(utilization[utilization$resource == "Facility_1", ]) > 0,
    sum(utilization$server[utilization$resource == "Facility_1"]) / 
      nrow(utilization[utilization$resource == "Facility_1", ]),
    NA
  )
  
  utilization_facility_2 <- ifelse(
    nrow(utilization[utilization$resource == "Facility_2", ]) > 0,
    sum(utilization$server[utilization$resource == "Facility_2"]) / 
      nrow(utilization[utilization$resource == "Facility_2", ]),
    NA
  )
  
  list(
    utilization_facility_1 = utilization_facility_1,
    utilization_facility_2 = utilization_facility_2
  )
}

# Simulate Policy 2 (Fixed time slot for both machines)
simulate_policy2 <- function(fixed_slot) {
  env <- simmer("Policy 2: Fixed Slot Allocation")
  fixed_trajectory <- trajectory("Fixed Patients") %>%
    seize("Facility_1", amount = 1) %>%
    timeout(fixed_slot) %>%
    release("Facility_1", amount = 1)
  
  fixed_trajectory_2 <- trajectory("Fixed Patients 2") %>%
    seize("Facility_2", amount = 1) %>%
    timeout(fixed_slot) %>%
    release("Facility_2", amount = 1)
  
  env %>%
    add_resource("Facility_1", capacity = 1) %>%
    add_resource("Facility_2", capacity = 1) %>%
    add_generator("Patient Generator 1", fixed_trajectory, function() type1_arrival()) %>%
    add_generator("Patient Generator 2", fixed_trajectory_2, function() type2_arrival()) %>%
    run(until = operating_hours)
  
  list(
    utilization = get_mon_resources(env),
    arrivals = get_mon_arrivals(env)
  )
}

# Function to calculate throughput
calculate_throughput <- function(arrivals) {
  nrow(arrivals)  # Total patients processed
}

# Function to calculate waiting times
calculate_waiting_times <- function(arrivals) {
  waiting_times <- arrivals$end_time - arrivals$start_time - arrivals$activity_time
  list(
    avg_waiting_time = mean(waiting_times),
    max_waiting_time = max(waiting_times),
    waiting_times = waiting_times
  )
}

# Function to calculate idle times
calculate_idle_times <- function(utilization, operating_hours) {
  utilization <- utilization %>%
    group_by(resource) %>%
    summarize(
      total_idle_time = sum((1 - server / capacity) * diff(c(0, time)), na.rm = TRUE),
      total_time = operating_hours
    )
  
  utilization %>%
    mutate(idle_percentage = (total_idle_time / total_time) * 100)
}

# Plot queue length over time
plot_queue_over_time <- function(utilization) {
  ggplot(utilization, aes(x = time, y = queue, color = resource)) +
    geom_line() +
    labs(
      title = "Queue Length Over Time",
      x = "Time (minutes)",
      y = "Queue Length"
    ) +
    theme_minimal()
}

# Plot utilization over time
plot_utilization_over_time <- function(utilization) {
  utilization <- utilization %>%
    mutate(utilization_rate = server / capacity)
  
  ggplot(utilization, aes(x = time, y = utilization_rate, color = resource)) +
    geom_line() +
    labs(
      title = "Utilization Rate Over Time",
      x = "Time (minutes)",
      y = "Utilization Rate"
    ) +
    theme_minimal()
}

# Plot waiting time distribution
plot_waiting_times <- function(waiting_times) {
  df <- data.frame(WaitingTime = waiting_times)
  ggplot(df, aes(x = WaitingTime)) +
    geom_histogram(binwidth = 2, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(
      title = "Distribution of Patient Waiting Times",
      x = "Waiting Time (minutes)",
      y = "Frequency"
    ) +
    theme_minimal()
}

# Run simulations for both policies
policy1_results <- simulate_policy1()
policy2_results <- simulate_policy2(fixed_slot)

# Throughput
policy1_throughput <- calculate_throughput(policy1_results$arrivals)
policy2_throughput <- calculate_throughput(policy2_results$arrivals)

# Waiting Times
policy1_waiting <- calculate_waiting_times(policy1_results$arrivals)
policy2_waiting <- calculate_waiting_times(policy2_results$arrivals)

# Print Metrics
cat("Policy 1 Metrics:\n")
cat(paste("Throughput:", policy1_throughput, "\n"))
cat(paste("Average Waiting Time:", policy1_waiting$avg_waiting_time, "\n"))
cat(paste("Maximum Waiting Time:", policy1_waiting$max_waiting_time, "\n"))


cat("Policy 2 Metrics:\n")
cat(paste("Throughput:", policy2_throughput, "\n"))
cat(paste("Average Waiting Time:", policy2_waiting$avg_waiting_time, "\n"))
cat(paste("Maximum Waiting Time:", policy2_waiting$max_waiting_time, "\n"))

# Generate Visualizations
cat("Policy 1 Plots:\n")
plot_queue_over_time(policy1_results$utilization)
plot_utilization_over_time(policy1_results$utilization)
plot_waiting_times(policy1_waiting$waiting_times)

cat("Policy 2 Plots:\n")
plot_queue_over_time(policy2_results$utilization)
plot_utilization_over_time(policy2_results$utilization)
plot_waiting_times(policy2_waiting$waiting_times)

###---------------------------------------------------------------------------###
# Define thresholds and limits for utilization adjustment
# Define thresholds and limits for utilization adjustment
utilization_min <- 0.8  # Minimum acceptable utilization rate (80%)
utilization_max <- 0.95 # Maximum acceptable utilization rate (95%)
max_iterations <- 20    # Limit the number of iterations

# Initialize time slots and track performance
current_type1_slot <- type1_initial_slot
current_type2_slot <- type2_initial_slot
current_fixed_slot <- fixed_slot

# Initial check for both policies before starting the loop

set.seed(1234)
policy1_results <- simulate_policy1()
policy1_performance <- evaluate_performance(policy1_results)
policy1_meets_criteria <- all(
  !is.na(policy1_performance$utilization_facility_1) && 
    policy1_performance$utilization_facility_1 >= utilization_min && 
    policy1_performance$utilization_facility_1 <= utilization_max,
  !is.na(policy1_performance$utilization_facility_2) && 
    policy1_performance$utilization_facility_2 >= utilization_min && 
    policy1_performance$utilization_facility_2 <= utilization_max
)

set.seed(1234)
policy2_results <- simulate_policy2(current_fixed_slot)
policy2_performance <- evaluate_performance(policy2_results)
policy2_meets_criteria <- all(
  !is.na(policy2_performance$utilization_facility_1) && 
    policy2_performance$utilization_facility_1 >= utilization_min && 
    policy2_performance$utilization_facility_1 <= utilization_max,
  !is.na(policy2_performance$utilization_facility_2) && 
    policy2_performance$utilization_facility_2 >= utilization_min && 
    policy2_performance$utilization_facility_2 <= utilization_max
)

# If both policies meet criteria initially, no need for adjustments
if (policy1_meets_criteria && policy2_meets_criteria) {
  cat("Both policies meet criteria initially. No adjustments needed.\n")
} else {
  # Start iterative adjustments only if needed
  for (iteration in 1:max_iterations) {
    cat(sprintf("\nIteration #%d:\n", iteration))
    
    # Adjust Policy 1 only if it does not meet criteria
    if (!policy1_meets_criteria) {
      # Simulate and evaluate Policy 1
      set.seed(1234)
      policy1_results <- simulate_policy1()
      policy1_performance <- evaluate_performance(policy1_results)
      
      # Calculate and report waiting times for Policy 1
      policy1_waiting <- calculate_waiting_times(policy1_results$arrivals)
      cat(sprintf("  Policy 1 Average Waiting Time: %.2f minutes\n", policy1_waiting$avg_waiting_time))
      cat(sprintf("  Policy 1 Maximum Waiting Time: %.2f minutes\n", policy1_waiting$max_waiting_time))
      
      # Report utilization rates
      cat(sprintf("  Policy 1 Utilization - Facility 1: %.2f%%\n", 
                  policy1_performance$utilization_facility_1 * 100))
      cat(sprintf("  Policy 1 Utilization - Facility 2: %.2f%%\n", 
                  policy1_performance$utilization_facility_2 * 100))
      
      # Adjust timeslot lengths for Policy 1
      if (!is.na(policy1_performance$utilization_facility_1)) {
        if (policy1_performance$utilization_facility_1 < utilization_min) {
          current_type1_slot <- current_type1_slot - 5
          cat("  Adjusting Type 1 Slot: Decreasing by 5 minutes (Low Utilization)\n")
        } else if (policy1_performance$utilization_facility_1 > utilization_max) {
          current_type1_slot <- current_type1_slot + 5
          cat("  Adjusting Type 1 Slot: Increasing by 5 minutes (High Utilization)\n")
        }
      }
      
      if (!is.na(policy1_performance$utilization_facility_2)) {
        if (policy1_performance$utilization_facility_2 < utilization_min) {
          current_type2_slot <- current_type2_slot - 5
          cat("  Adjusting Type 2 Slot: Decreasing by 5 minutes (Low Utilization)\n")
        } else if (policy1_performance$utilization_facility_2 > utilization_max) {
          current_type2_slot <- current_type2_slot + 5
          cat("  Adjusting Type 2 Slot: Increasing by 5 minutes (High Utilization)\n")
        }
      }
      
      # Recheck Policy 1 criteria after adjustments
      policy1_meets_criteria <- all(
        !is.na(policy1_performance$utilization_facility_1) && 
          policy1_performance$utilization_facility_1 >= utilization_min && 
          policy1_performance$utilization_facility_1 <= utilization_max,
        !is.na(policy1_performance$utilization_facility_2) && 
          policy1_performance$utilization_facility_2 >= utilization_min && 
          policy1_performance$utilization_facility_2 <= utilization_max
      )
    }
    
    # Adjust Policy 2 only if it does not meet criteria
    if (!policy2_meets_criteria) {
      # Simulate and evaluate Policy 2
      set.seed(1234)
      policy2_results <- simulate_policy2(current_fixed_slot)
      policy2_performance <- evaluate_performance(policy2_results)
      
      # Calculate and report waiting times for Policy 2
      policy2_waiting <- calculate_waiting_times(policy2_results$arrivals)
      cat(sprintf("  Policy 2 Average Waiting Time: %.2f minutes\n", policy2_waiting$avg_waiting_time))
      cat(sprintf("  Policy 2 Maximum Waiting Time: %.2f minutes\n", policy2_waiting$max_waiting_time))
      
      # Report utilization rates
      cat(sprintf("  Policy 2 Utilization - Facility 1: %.2f%%\n", 
                  policy2_performance$utilization_facility_1 * 100))
      cat(sprintf("  Policy 2 Utilization - Facility 2: %.2f%%\n", 
                  policy2_performance$utilization_facility_2 * 100))
      
      # Adjust fixed slot length for Policy 2
      if (!is.na(policy2_performance$utilization_facility_1) && 
          policy2_performance$utilization_facility_1 < utilization_min || 
          policy2_performance$utilization_facility_2 < utilization_min) {
        current_fixed_slot <- current_fixed_slot - 5
        cat("  Adjusting Fixed Slot: Decreasing by 5 minutes (Low Utilization)\n")
      } else if (!is.na(policy2_performance$utilization_facility_1) && 
                 policy2_performance$utilization_facility_1 > utilization_max || 
                 policy2_performance$utilization_facility_2 > utilization_max) {
        current_fixed_slot <- current_fixed_slot + 5
        cat("  Adjusting Fixed Slot: Increasing by 5 minutes (High Utilization)\n")
      }
      
      # Recheck Policy 2 criteria after adjustments
      policy2_meets_criteria <- all(
        !is.na(policy2_performance$utilization_facility_1) && 
          policy2_performance$utilization_facility_1 >= utilization_min && 
          policy2_performance$utilization_facility_1 <= utilization_max,
        !is.na(policy2_performance$utilization_facility_2) && 
          policy2_performance$utilization_facility_2 >= utilization_min && 
          policy2_performance$utilization_facility_2 <= utilization_max
      )
    }
    
    # Print intermediate results
    cat(sprintf("  Type 1 Slot: %d minutes, Type 2 Slot: %d minutes, Fixed Slot: %d minutes\n", 
                current_type1_slot, current_type2_slot, current_fixed_slot))
    
    # Stop the loop if both policies meet criteria
    if (policy1_meets_criteria && policy2_meets_criteria) {
      cat("Optimization successful: Both policies meet criteria.\n")
      break
    }
    
    # Prevent excessive adjustments
    if (current_type1_slot < 5 || current_type2_slot < 5 || current_fixed_slot < 5 ||
        current_type1_slot > 60 || current_type2_slot > 60 || current_fixed_slot > 60) {
      cat("Stopping: Slot adjustments exceeded reasonable bounds.\n")
      break
    }
  }
}

# Print final adjusted slots
cat("\nFinal Adjusted Slots:\n")
cat(sprintf("  Type 1 Slot: %d minutes\n", current_type1_slot))
cat(sprintf("  Type 2 Slot: %d minutes\n", current_type2_slot))
cat(sprintf("  Fixed Slot: %d minutes\n", current_fixed_slot))
