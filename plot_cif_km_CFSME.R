
# ---------- Load Packages ----------
install.packages(c("cmprsk", "nnet", "survival"))
library(cmprsk)
library(nnet)
library(survival)

# ---------- Attach Data ----------
attach(CFSME)
data <- CFSME
data$ID <- 1:nrow(data)

# ---------- Setup ----------
columns_of_interest <- c("CFS.ME")
column_meanings <- c(
  "CFS.ME" = "Chronic Fatigue Syndrome/Myalgic Encephalomyelitis"
)

covariates <- c()

# ---------- Toggle Pre-Exclusion for Each Outcome ----------
apply_pre_exclusion <- c(
  "CFS.ME" = FALSE
)

# ---------- Choose Plot Type for Each Outcome ----------
plot_type_per_outcome <- c(
  "CFS.ME" = "CIF"
)

# ---------- Loop Through Outcomes ----------
for (outcome in columns_of_interest) {
  
  label <- column_meanings[outcome]
  time_col <- paste0(outcome, "_TIME")
  status_col <- outcome
  group_var <- "covid"
  plot_type <- plot_type_per_outcome[[outcome]]
  
  subset_condition <- if (apply_pre_exclusion[[outcome]]) {
    data[[paste0("PRE_", outcome)]] == 0
  } else {
    rep(TRUE, nrow(data))
  }
  
  data_sub <- data[subset_condition, ]
  data_weighted <- data_sub  # no IPW applied
  
  if (plot_type == "CIF") {
    # ---------- CIF ----------
    cif_out <- cuminc(
      ftime = data_weighted[[time_col]],
      fstatus = data_weighted[[status_col]],
      group = data_weighted[[group_var]],
      cencode = 0
    )
    cif_primary <- cif_out[grep("1$", names(cif_out))]
    
    plot(NULL, xlim = c(0, 45), ylim = c(0, 0.02),
         xlab = "Time Since Index Date (Months)",
         ylab = paste("Cumulative Incidence of", label),
         main = paste("Cumulative Incidence Function for", label))
    
    for (i in seq_along(cif_primary)) {
      lwd_val <- if (i == 1) 1 else if (i == 2) 2 else 4
      lines(cif_primary[[i]]$time, cif_primary[[i]]$est, col = "black", lwd = lwd_val)
    }
    
    legend("topleft",
           legend = c("COVID+ Hospitalized", "COVID+ Non-Hospitalized", "COVID–"),
           lwd = c(4, 2, 1),
           col = "black",
           bty = "n")
    
  } else if (plot_type == "KM") {
    # ---------- KM ----------
    covid_levels <- sort(unique(as.integer(as.character(data_weighted$covid))))
    plotted <- FALSE
    ymin <- 1
    ymax <- 0
    
    for (i in covid_levels) {
      df_group <- data_weighted[data_weighted$covid == i, ]
      if (sum(df_group[[status_col]] == 1, na.rm = TRUE) == 0) next
      
      surv_obj <- Surv(df_group[[time_col]], df_group[[status_col]] == 1)
      km_fit <- survfit(surv_obj ~ 1)
      
      ymin <- min(ymin, min(km_fit$surv, na.rm = TRUE))
      ymax <- max(ymax, max(km_fit$surv, na.rm = TRUE))
      
      lwd_val <- if (i == 0) 1 else if (i == 1) 2 else 4
      
      if (!plotted) {
        plot(km_fit$time, km_fit$surv, type = "s", col = "black", lwd = lwd_val,
             xlab = "Time Since Index Date (Months)",
             ylab = paste("Survival Probability for", label),
             main = paste("Kaplan-Meier Curve for", label),
             xlim = c(0, 45), ylim = c(0.90, 1))
        plotted <- TRUE
      } else {
        lines(km_fit$time, km_fit$surv, col = "black", lwd = lwd_val)
      }
    }
    
    if (plotted) {
      legend("bottomleft",
             legend = c("COVID+ Hospitalized", "COVID+ Non-Hospitalized", "COVID–"),
             lwd = c(4, 2, 1),
             col = "black",
             bty = "n")
    }
  }
}
