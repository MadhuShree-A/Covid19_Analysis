library(dplyr)
library(ggplot2)


data <- read.csv("D:\\SEMESTER 3\\R programming lab\\package\\country_wise_latest.csv")


colnames(data) <- make.names(colnames(data), unique = TRUE)
print(colnames(data))



manual_mean <- function(x) {
  sum(x, na.rm = TRUE) / length(na.omit(x))
}


manual_median <- function(x) {
  x <- na.omit(x)  
  sorted_x <- sort(x)  
  n <- length(sorted_x)
  if (n == 0) return(NA)  
  if (n %% 2 == 0) {
    return((sorted_x[n / 2] + sorted_x[n / 2 + 1]) / 2)
  } else {
    return(sorted_x[(n + 1) / 2])
  }
}

manual_sd <- function(x) {
  mean_x <- manual_mean(x)
  n <- length(na.omit(x))
  sqrt(sum((x - mean_x)^2, na.rm = TRUE) / (n - 1))
}


manual_correlation <- function(x, y) {
  x_mean <- manual_mean(x)
  y_mean <- manual_mean(y)
  numerator <- sum((x - x_mean) * (y - y_mean), na.rm = TRUE)
  denominator <- sqrt(sum((x - x_mean)^2, na.rm = TRUE)) * sqrt(sum((y - y_mean)^2, na.rm = TRUE))
  numerator / denominator
}


manual_regression <- function(x, y) {
  x_mean <- manual_mean(x)
  y_mean <- manual_mean(y)
  
  slope <- sum((x - x_mean) * (y - y_mean), na.rm = TRUE) / sum((x - x_mean)^2, na.rm = TRUE)
  intercept <- y_mean - slope * x_mean
  
  list(intercept = intercept, slope = slope)
}

manual_t_test <- function(x, y) {
  mean_x <- manual_mean(x)
  mean_y <- manual_mean(y)
  sd_x <- manual_sd(x)
  sd_y <- manual_sd(y)
  n_x <- length(na.omit(x))
  n_y <- length(na.omit(y))
  
  (mean_x - mean_y) / sqrt((sd_x^2 / n_x) + (sd_y^2 / n_y))
}


manual_anova <- function(dependent_var, group_var) {
  group_means <- tapply(dependent_var, group_var, manual_mean)
  overall_mean <- manual_mean(dependent_var)
  
  ss_between <- sum(table(group_var) * (group_means - overall_mean)^2)
  ss_within <- sum((dependent_var - group_means[group_var])^2, na.rm = TRUE)
  
  df_between <- length(unique(group_var)) - 1
  df_within <- length(dependent_var) - length(unique(group_var))
  
  ms_between <- ss_between / df_between
  ms_within <- ss_within / df_within
  
  f_statistic <- ms_between / ms_within
  return(f_statistic)
}


descriptive_stats <- function(data) {
  summary_stats <- data %>%
    group_by(WHO.Region) %>%
    summarise(
      mean_new_cases = manual_mean(New.cases),
      median_new_cases = manual_median(New.cases),
      sd_new_cases = manual_sd(New.cases),
      mean_deaths_per_100 = manual_mean(Deaths...100.Cases),
      median_deaths_per_100 = manual_median(Deaths...100.Cases),
      sd_deaths_per_100 = manual_sd(Deaths...100.Cases),
      mean_recovered_per_100 = manual_mean(Recovered...100.Cases),
      median_recovered_per_100 = manual_median(Recovered...100.Cases),
      sd_recovered_per_100 = manual_sd(Recovered...100.Cases)
    )
  
  print(summary_stats)
  
  summary_stats <- summary_stats[!is.na(summary_stats$WHO.Region), ]
  
  
  ggplot(summary_stats, aes(x = WHO.Region, y = mean_new_cases, fill = WHO.Region)) +
    geom_bar(stat = "identity", width = 0.7) +  # Adjust bar width
    labs(title = "Mean New Cases by WHO Region", x = "WHO Region", y = "Mean New Cases") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotates x-axis labels for better readability
}



correlation_analysis <- function(data) {
  cor_confirmed_deaths <- manual_correlation(data$Confirmed, data$Deaths...100.Cases)
  cor_confirmed_recovered <- manual_correlation(data$Confirmed, data$Recovered...100.Cases)
  
  cat("Pearson Correlation between Confirmed and Deaths / 100 Cases:", cor_confirmed_deaths, "\n")
  cat("Pearson Correlation between Confirmed and Recovered / 100 Cases:", cor_confirmed_recovered, "\n")
  
  
  ggplot(data, aes(x = Confirmed, y = Deaths...100.Cases)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "blue") +
    labs(title = "Scatter Plot: Confirmed vs Deaths / 100 Cases",
         x = "Confirmed Cases", y = "Deaths / 100 Cases") +
    theme_minimal()
}


regression_analysis <- function(data) {
  regression_result <- manual_regression(data$New.cases, data$Confirmed)
  cat("Intercept:", regression_result$intercept, "\n")
  cat("Slope:", regression_result$slope, "\n")
  
  ggplot(data, aes(x = New.cases, y = Confirmed)) +
    geom_point() +
    geom_abline(intercept = regression_result$intercept,
                slope = regression_result$slope,
                col = "red") +
    labs(title = "Linear Regression: New Cases vs Confirmed Cases",
         x = "New Cases", y = "Confirmed Cases") +
    theme_minimal()
}



hypothesis_test <- function(data) {
  africa_data <- subset(data, WHO.Region == "Africa")$Deaths...100.Cases
  europe_data <- subset(data, WHO.Region == "Europe")$Deaths...100.Cases
  
  t_stat <- manual_t_test(africa_data, europe_data)
  
  cat("t-statistic:", t_stat, "\n")
  
  ggplot(data %>% filter(WHO.Region %in% c("Africa", "Europe")),
         aes(x = WHO.Region, y = Deaths...100.Cases, fill = WHO.Region)) +
    geom_boxplot() +
    labs(title = "Comparison of Deaths / 100 Cases: Africa vs Europe",
         x = "WHO Region", y = "Deaths / 100 Cases") +
    theme_minimal()
}


new_cases_hypothesis_test <- function(data) {
  africa_data_new_cases <- subset(data, WHO.Region == "Africa")$New.cases
  europe_data_new_cases <- subset(data, WHO.Region == "Europe")$New.cases
  
  t_stat_new_cases <- manual_t_test(africa_data_new_cases, europe_data_new_cases)
  
  cat("t-statistic for new cases:", t_stat_new_cases, "\n")
  
  ggplot(data %>% filter(WHO.Region %in% c("Africa", "Europe")),
         aes(x = WHO.Region, y = New.cases, fill = WHO.Region)) +
    geom_boxplot() +
    labs(title = "Comparison of New Cases: Africa vs Europe",
         x = "WHO Region", y = "New Cases") +
    theme_minimal()
}

anova_deaths_across_regions <- function(data) {
  death_data <- data %>% select(Deaths...100.Cases, WHO.Region) %>% na.omit()
  anova_result <- aov(Deaths...100.Cases ~ WHO.Region, data = death_data)
  
  
  split_by_region <- split(death_data$Deaths...100.Cases, death_data$WHO.Region)
  
  
  
  europe_data <- split_by_region$Europe
  africa_data <- split_by_region$Africa
  americas_data <- split_by_region$Americas
  EMedi <- split_by_region$'Eastern Mediterranean'
  SEAsia  <-split_by_region$'South-East Asia'
  WPaci <- split_by_region$'Western Pacific'
  
  k = 6 
  
  N = length(europe_data)+length(africa_data)+length(americas_data)+length(EMedi)+length(SEAsia)+length(WPaci)
  
  
  dataTotal = sum(europe_data)+sum(africa_data)+sum(americas_data)+sum(EMedi)+sum(SEAsia)+sum(WPaci)
  
  CF = dataTotal*dataTotal/N
  
  dataTotal2 = sum(europe_data^2)+sum(africa_data^2)+sum(americas_data^2)+sum(EMedi^2)+sum(SEAsia^2)+sum(WPaci^2)
  SST = dataTotal2 - CF
  
  SSB <- (sum(europe_data)^2 / length(europe_data) +
            sum(africa_data)^2 / length(africa_data) +
            sum(americas_data)^2 / length(americas_data) +
            sum(EMedi)^2 / length(EMedi) +
            sum(SEAsia)^2 / length(SEAsia) +
            sum(WPaci)^2 / length(WPaci)) - CF
  SSE = SST - SSB
  
  dofB = k-1
  dofE = N-k
  dofT = N-1
  
  MSB = SSB/dofB
  MSE = SSE/dofE
  Fratio = MSB/MSE
  
  print(paste("SST = ",SST))
  print(paste("SSB = ",SSB))
  print(paste("SSE = ",SSE))
  
  print(paste("Dof(between) = ",dofB))
  print(paste("Dof(Error) = ",dofE))
  print(paste("Dof(Total) = ",dofT))
  
  print(paste("MSB = ",MSB))
  print(paste("MSE = ",MSE))
  
  print(paste("Fratio = ",Fratio))
  
  alpha = 0.05
  fvalue = qf(1-0.05,dofB,dofE)
  
  
  
  if(Fratio<fvalue)
  {
    print("Ho accepted")
  }else
  {
    print("Ho rejected")
  }
  
  response <- c(europe_data,africa_data,americas_data,EMedi,SEAsia,WPaci)
  group <- factor(rep(c("Europe", "Africa", "Americas", "Eastern Mediterranean", "South-East Asia", "Western Pacific"),
                      times = c(length(europe_data), length(africa_data), length(americas_data), length(EMedi), length(SEAsia), length(WPaci))))
  
  anova_result <- aov(response ~ group)
  
  cat("\nOne-Way ANOVA Results:\n")
  print(summary(anova_result))
  
  
  
  ggplot(death_data, aes(x = Deaths...100.Cases, fill = WHO.Region)) +
    geom_histogram(position = "identity", alpha = 0.05, bins = 30) +
    facet_wrap(~ WHO.Region) +
    labs(title = "Distribution of Deaths per 100 Cases by WHO Region",
         x = "Deaths per 100 Cases", y = "Frequency") +
    theme_minimal()
}


descriptive_plot <- descriptive_stats(data)
print(descriptive_plot)  

correlation_plot <- correlation_analysis(data)
print(correlation_plot)  

regression_plot <- regression_analysis(data)
print(regression_plot)  

hypothesis_plot <- hypothesis_test(data)
print(hypothesis_plot)  

new_cases_plot <- new_cases_hypothesis_test(data)
print(new_cases_plot)  

anova_plot <- anova_deaths_across_regions(data)
print(anova_plot)  