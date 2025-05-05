
# Install and load required package
install.packages("cmprsk")
library("cmprsk")

# Load data
attach(CFSME)  # Replace with your actual dataset if different
data <- CFSME

# Define outcomes of interest
columns_of_interest <- c("CFS.ME")

# Define outcome labels
column_meanings <- c(
  "CFS.ME" = "Chronic Fatigue Syndrome/Myalgic Encephalomyelitis"
)

# Define whether to exclude pre-existing conditions (1 = exclude, 0 = don't exclude)
exclude_pre <- c(
  "CFS.ME" = 0
)

# Covariates to adjust for
covariates <- c("covid.hosp", "covid.non.hosp", "AGE", "MALE", "BLACK", "ASIAN", "OTHER_RACE", "HISPANIC",
                "HTN", "DIABETES_II", "COPD", "ASTHMA", "CKD", "LIVER", "CVD", "AUTOIMMUNE", "ANXT", "DPRSN", "Vaccine",
                "Medicaid", "Medicare", "Self.Pay", "Bottom.25q", "X25.50q", "X50.75q")  # <- Adjust this list as needed

# Loop through outcomes
for (column in columns_of_interest) {
  
  outcome_name <- column_meanings[column]
  
  # Determine subset based on whether to exclude pre-existing diagnoses
  subset_condition <- if (exclude_pre[column] == 1) {
    data[[paste0("PRE_", column)]] == 0
  } else {
    rep(TRUE, nrow(data))
  }
  
  # Initialize results lists
  univariate_results <- list()
  multivariate_results <- list()
  
  # ---- UNIVARIATE MODELS ----
  for (cov in covariates) {
    fg_uni <- crr(
      ftime = data[[paste0(column, "_TIME")]][subset_condition],
      fstatus = data[[column]][subset_condition],
      cov1 = as.matrix(data[subset_condition, cov]),
      failcode = 1,
      cencode = 0
    )
    
    # Extract HR and confidence intervals
    sum_uni <- summary(fg_uni)
    hr <- sprintf("%.2f", sum_uni$coef[1, "exp(coef)"])
    ci_lower <- sprintf("%.2f", sum_uni$conf.int[1, "2.5%"])
    ci_upper <- sprintf("%.2f", sum_uni$conf.int[1, "97.5%"])
    z <- sum_uni$coef[1, "z"]
    p <- 2 * (1 - pnorm(abs(z)))
    p_fmt <- ifelse(p < 0.005, "<0.005", sprintf("%.3f", p))
    hr_ci <- paste0(hr, " [", ci_lower, ", ", ci_upper, "]")
    
    # Append univariate results
    univariate_results[[length(univariate_results) + 1]] <- data.frame(
      Outcome = outcome_name,
      Model = "Univariate",
      Covariate = cov,
      HR_95CI = hr_ci,
      P_value = p_fmt
    )
  }
  
  # ---- MULTIVARIATE MODEL ----
  fg_multi <- crr(
    ftime = data[[paste0(column, "_TIME")]][subset_condition],
    fstatus = data[[column]][subset_condition],
    cov1 = as.matrix(data[subset_condition, covariates]),
    failcode = 1,
    cencode = 0
  )
  
  sum_multi <- summary(fg_multi)
  
  # Loop through covariates and extract HRs
  for (i in seq_along(covariates)) {
    cov <- covariates[i]
    hr <- sprintf("%.2f", sum_multi$coef[i, "exp(coef)"])
    ci_lower <- sprintf("%.2f", sum_multi$conf.int[i, "2.5%"])
    ci_upper <- sprintf("%.2f", sum_multi$conf.int[i, "97.5%"])
    z <- sum_multi$coef[i, "z"]
    p <- 2 * (1 - pnorm(abs(z)))
    p_fmt <- ifelse(p < 0.005, "<0.005", sprintf("%.3f", p))
    hr_ci <- paste0(hr, " [", ci_lower, ", ", ci_upper, "]")
    
    # Append multivariate results
    multivariate_results[[length(multivariate_results) + 1]] <- data.frame(
      Outcome = outcome_name,
      Model = "Multivariate",
      Covariate = cov,
      HR_95CI = hr_ci,
      P_value = p_fmt
    )
  }
  
  # Combine and export results to CSV
  all_results <- do.call(rbind, c(univariate_results, multivariate_results))
  write.csv(all_results, paste0("fine_gray_", column, "_results.csv"), row.names = FALSE)
}
