# Created February 28 2025
# R version 4.4.2
# Juneau Wang
R.Version()


##### IMPORT THE DATA #####
#library(ez)
library(tidyverse)
library(lme4)
library(car)
#library(effectsize)
sem <- function(x) sd(x)/sqrt(length(x))

# Import raw data and set factors
data_e1 <- read.csv("RStudio_input_alls_expt1_ta-auditory_20230920.csv")
data_e1$expt = "aud-e1"
data_e1$universalID <- factor(data_e1$universalID)
data_e1$axis <- factor(data_e1$axis)
data_e1$target <- factor(data_e1$target)
data_e1$validity <- factor(data_e1$validity)

data_e2 <- read.csv("RStudio_input_alls_expt2_ta-auditory_20230920.csv")
data_e2$expt = "aud-e2"
data_e2$universalID <- factor(data_e2$universalID)
data_e2$axis <- factor(data_e2$axis)
data_e2$target <- factor(data_e2$target)
data_e2$validity <- factor(data_e2$validity)

### Exclude 3sd lognormal reaction time trials
exclude = 1
if (exclude == 1) {
  # Remove outliers: 3 standard deviations beyond the mean for each subject
  data_e1 <- data_e1 %>%
    group_by(universalID) %>%
    filter(abs(log(rxntime) - mean(log(rxntime), na.rm = TRUE)) <= 6 * sd(log(rxntime), na.rm = TRUE)) %>%
    ungroup()
  
  data_e2 <- data_e2 %>%
    group_by(universalID) %>%
    filter(abs(log(rxntime) - mean(log(rxntime), na.rm = TRUE)) <= 6 * sd(log(rxntime), na.rm = TRUE)) %>%
    ungroup()
} else {
  
}

# Combine both experiments' trial-by-trial data
data_both <- rbind(data_e1, data_e2)

# Group the data into mean accuracies
data_e1_grouped <- data_e1 %>% 
  group_by(universalID, axis, target, validity, expt) %>% 
  summarize(accuracy = mean(accuracy), rxntime = mean(rxntime))
data_e2_grouped <- data_e2 %>% 
  group_by(universalID, axis, target, validity, expt) %>% 
  summarize(accuracy = mean(accuracy), rxntime = mean(rxntime))

# Average the data across axes
data_e1_g_ave <- data_e1_grouped %>% 
  group_by(universalID, target, validity, expt) %>% 
  summarize(accuracy = mean(accuracy), rxntime = mean(rxntime))
data_e2_g_ave <- data_e2_grouped %>% 
  group_by(universalID, target, validity, expt) %>% 
  summarize(accuracy = mean(accuracy), rxntime = mean(rxntime))

# Combine both experiments' axes-averaged data
data_both_g_ave <- rbind(data_e1_g_ave, data_e2_g_ave)

##### Export simple grouped means and sems from each experiment #####
## originally from ta-auditory-binomial-glmms.R, here is simplified (stats tests separate dataframe)

## Both experiments
data_both_g_ave_labels <- c("target", "validity", "mean_accuracy", "sem_accuracy", "mean_rxntime", "sem_rxntime")
data_both_g_ave_means <- data.frame(matrix(nrow = nlevels(data_both_g_ave$target)*nlevels(data_both_g_ave$validity), ncol = length(data_both_g_ave_labels)))
colnames(data_both_g_ave_means) <- data_both_g_ave_labels
row_counter = 1
for (iTarget in 1:nlevels(data_both_g_ave$target)){
  for (iValidity in 1:nlevels((data_both_g_ave$validity))){
    data_both_g_ave_means[row_counter,] <- c(
      iTarget,
      iValidity,
      mean(subset(data_both_g_ave, target == iTarget & validity == iValidity)$accuracy),
      sem(subset(data_both_g_ave, target == iTarget & validity == iValidity)$accuracy),
      mean(subset(data_both_g_ave, target == iTarget & validity == iValidity)$rxntime),
      sem(subset(data_both_g_ave, target == iTarget & validity == iValidity)$rxntime)
    )
    row_counter = row_counter + 1
  }
}

## Experiment 1
data_e1_g_ave_labels <- c("target", "validity", "mean_accuracy", "sem_accuracy", "mean_rxntime", "sem_rxntime")
data_e1_g_ave_means <- data.frame(matrix(nrow = nlevels(data_e1_g_ave$target)*nlevels(data_e1_g_ave$validity), ncol = length(data_e1_g_ave_labels)))
colnames(data_e1_g_ave_means) <- data_e1_g_ave_labels
row_counter = 1
for (iTarget in 1:nlevels(data_e1_g_ave$target)){
  for (iValidity in 1:nlevels((data_e1_g_ave$validity))){
    data_e1_g_ave_means[row_counter,] <- c(
      iTarget,
      iValidity,
      mean(subset(data_e1_g_ave, target == iTarget & validity == iValidity)$accuracy),
      sem(subset(data_e1_g_ave, target == iTarget & validity == iValidity)$accuracy),
      mean(subset(data_e1_g_ave, target == iTarget & validity == iValidity)$rxntime),
      sem(subset(data_e1_g_ave, target == iTarget & validity == iValidity)$rxntime)
    )
    row_counter = row_counter + 1
  }
}


## Experiment 2
data_e2_g_ave_labels <- c("target", "validity", "mean_accuracy", "sem_accuracy", "mean_rxntime", "sem_rxntime")
data_e2_g_ave_means <- data.frame(matrix(nrow = nlevels(data_e2_g_ave$target)*nlevels(data_e2_g_ave$validity), ncol = length(data_e2_g_ave_labels)))
colnames(data_e2_g_ave_means) <- data_e2_g_ave_labels
row_counter = 1
for (iTarget in 1:nlevels(data_e2_g_ave$target)){
  for (iValidity in 1:nlevels((data_e2_g_ave$validity))){
    data_e2_g_ave_means[row_counter,] <- c(
      iTarget,
      iValidity,
      mean(subset(data_e2_g_ave, target == iTarget & validity == iValidity)$accuracy),
      sem(subset(data_e2_g_ave, target == iTarget & validity == iValidity)$accuracy),
      mean(subset(data_e2_g_ave, target == iTarget & validity == iValidity)$rxntime),
      sem(subset(data_e2_g_ave, target == iTarget & validity == iValidity)$rxntime)
    )
    row_counter = row_counter + 1
  }
}

write.csv(data_both_g_ave_means, "R_data_both_g_ave_means.csv", row.names = FALSE)
write.csv(data_e1_g_ave_means, "R_data_e1_g_ave_means.csv", row.names = FALSE)
write.csv(data_e2_g_ave_means, "R_data_e2_g_ave_means.csv", row.names = FALSE)


##### New: exporting subject acc and rt means for each expt
## Exp 1 n = 10, Exp 2 n = 10, both n = 20

# Group the data into mean accuracies

write.csv(data_both_g_ave, "R_data_both_subj_ave_means.csv", row.names = FALSE)
write.csv(data_e1_g_ave, "R_data_e1_subj_ave_means.csv", row.names = FALSE)
write.csv(data_e2_g_ave, "R_data_e2_subj_ave_means.csv", row.names = FALSE)


##### Analyzing which distribution best fits reaction time data #####

library(fitdistrplus)
library(statmod)  # Needed for Inverse Gaussian
library(ggplot2)
library(dplyr)
library(tidyr)

# Create a unique subject-experiment list
subject_expt_pairs <- unique(data_both[, c("universalID", "expt")])

# Initialize storage for AIC/BIC results
aic_bic_results <- data.frame()
best_fits <- data.frame()

# Function to fit distributions and generate plots
analyze_rt_distribution <- function(data_subset, subject_id, expt_label) {
  rt_data <- data_subset$rxntime  # Extract RT values
  
  # Fit distributions
  fit_gaussian <- fitdist(rt_data, "norm")
  fit_gamma <- fitdist(rt_data, "gamma")
  fit_invgauss <- fitdist(rt_data, "invgauss")
  fit_lognormal <- fitdist(rt_data, "lnorm")
  
  # Store fits in a list
  fits <- list(Gaussian = fit_gaussian, Gamma = fit_gamma, 
               Inverse_Gaussian = fit_invgauss, Lognormal = fit_lognormal)
  
  # Compute AIC/BIC for each distribution
  aic_values <- sapply(fits, function(fit) AIC(fit))
  bic_values <- sapply(fits, function(fit) BIC(fit))
  
  # Store AIC/BIC results in a dataframe
  aic_bic_df <- data.frame(
    universalID = subject_id,
    expt = expt_label,
    Distribution = names(aic_values),
    AIC = aic_values,
    BIC = bic_values
  )
  
  # Find the best-fitting distribution (lowest AIC)
  best_fit <- aic_bic_df[which.min(aic_bic_df$AIC), ]
  
  # Store results
  aic_bic_results <<- rbind(aic_bic_results, aic_bic_df)
  best_fits <<- rbind(best_fits, best_fit)
  
  # Create Q-Q plot with all four distributions on one plot
  qq_df <- data.frame(
    sample_quantiles = sort(rt_data),
    Gaussian = qnorm(ppoints(length(rt_data)), mean = fit_gaussian$estimate[1], sd = fit_gaussian$estimate[2]),
    Gamma = qgamma(ppoints(length(rt_data)), shape = fit_gamma$estimate[1], rate = fit_gamma$estimate[2]),
    Inverse_Gaussian = qinvgauss(ppoints(length(rt_data)), mean = fit_invgauss$estimate[1], shape = fit_invgauss$estimate[2]),
    Lognormal = qlnorm(ppoints(length(rt_data)), meanlog = fit_lognormal$estimate[1], sdlog = fit_lognormal$estimate[2])
  )
  
  # Convert data to long format for ggplot
  qq_long <- qq_df %>% pivot_longer(cols = -sample_quantiles, names_to = "Distribution", values_to = "Theoretical_Quantiles")
  
  qq_plot <- ggplot(qq_long, aes(x = Theoretical_Quantiles, y = sample_quantiles, color = Distribution)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(title = paste("Q-Q Plot:", subject_id, "-", expt_label),
         x = "Theoretical Quantiles", y = "Sample Quantiles")
  
  # Create density plot comparing empirical and fitted distributions
  density_plot <- ggplot(data.frame(rt_data), aes(x = rt_data)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "gray", alpha = 0.5) +
    stat_function(fun = dnorm, args = list(mean = fit_gaussian$estimate[1], sd = fit_gaussian$estimate[2]), color = "blue", linetype = "dashed") +
    stat_function(fun = dgamma, args = list(shape = fit_gamma$estimate[1], rate = fit_gamma$estimate[2]), color = "red") +
    stat_function(fun = dinvgauss, args = list(mean = fit_invgauss$estimate[1], shape = fit_invgauss$estimate[2]), color = "green") +
    stat_function(fun = dlnorm, args = list(meanlog = fit_lognormal$estimate[1], sdlog = fit_lognormal$estimate[2]), color = "purple") +
    theme_minimal() +
    labs(title = paste("RT Distribution Fit:", subject_id, "-", expt_label),
         x = "Reaction Time", y = "Density",
         subtitle = "Blue = Gaussian, Red = Gamma, Green = Inverse Gaussian, Purple = Lognormal")
  
  return(list(qq_plot = qq_plot, density_plot = density_plot))
}

# Loop through each subject-experiment pair and generate plots
plots_list <- list()

for (i in 1:nrow(subject_expt_pairs)) {
  subject_id <- subject_expt_pairs$universalID[i]
  expt_label <- subject_expt_pairs$expt[i]
  
  # Subset data for the current subject-experiment pair
  data_subset <- subset(data_both, universalID == subject_id & expt == expt_label)
  
  # Run analysis and store plots
  plots_list[[paste(subject_id, expt_label, sep = "_")]] <- analyze_rt_distribution(data_subset, subject_id, expt_label)
}

# Compute overall average AIC/BIC per distribution
avg_aic_bic <- aic_bic_results %>%
  group_by(Distribution) %>%
  summarise(Average_AIC = mean(AIC), Average_BIC = mean(BIC))

# Print the best-fitting distribution for each subject-experiment pair
print(best_fits)

# Print average AIC/BIC results
print(avg_aic_bic)

# Display plots for a single subject-experiment pair (replace with any valid ID)
example_pair <- names(plots_list)[1]
print(plots_list[[example_pair]]$qq_plot)
print(plots_list[[example_pair]]$density_plot)


##### USE THE LOG REACTION TIME FOR ANALYSIS #####

library(MASS)  # For fitting distributions
library(fitdistrplus)  # For model comparison
library(ggplot2)  # For visualization
library(statmod)

# Load your RT data
rt_data <- data_both$rxntime

# Fit distributions
fit_gaussian <- fitdist(rt_data, "norm")
fit_gamma <- fitdist(rt_data, "gamma")
fit_invgauss <- fitdist(rt_data, "invgauss")
fit_lognormal <- fitdist(rt_data, "lnorm")

# Print parameter estimates
fit_gaussian
fit_gamma
fit_invgauss
fit_lognormal

aic_values <- c(
  AIC(fit_gaussian),
  AIC(fit_gamma),
  AIC(fit_invgauss),
  AIC(fit_lognormal)
)

bic_values <- c(
  BIC(fit_gaussian),
  BIC(fit_gamma),
  BIC(fit_invgauss),
  BIC(fit_lognormal)
)

# Create a comparison dataframe
dist_comparison <- data.frame(
  Distribution = c("Gaussian", "Gamma", "Inverse Gaussian", "Lognormal"),
  AIC = aic_values,
  BIC = bic_values
)

# Display results
print(dist_comparison)


##### ADDING LOG RXN TO DATAFRAMES #####
data_e1$log_rxntime <- log(data_e1$rxntime)
data_e2$log_rxntime <- log(data_e2$rxntime)
data_both$log_rxntime <- log(data_both$rxntime)


##### BGLMMs and LOGNORMAL LMMS #####

### LME4 GLMM via binomial
### Now onto trial-by-trial data
### Using Type II Anova() tests


# Define datasets
datasets <- list(
  "both" = data_both,
  "e1" = data_e1,
  "e2" = data_e2
)

# Define model formulas (adjusted for `expt`)
formulas <- list(
  "acc_both" = "accuracy ~ validity * target * expt + (1|universalID)",  # Expt included for both
  "acc_single" = "accuracy ~ validity * target + (1|universalID)",        # No expt for single experiments
  "acc_both_t" = "accuracy ~ validity * expt + (1|universalID)",          # Target 1/2, expt included
  "acc_single_t" = "accuracy ~ validity + (1|universalID)",               # Target 1/2, no expt
  
  "rxn_both" = "log_rxntime ~ validity * target * expt + (1|universalID)",  # Log RT, Expt included
  "rxn_single" = "log_rxntime ~ validity * target + (1|universalID)",        # Log RT, No expt
  "rxn_both_t" = "log_rxntime ~ validity * expt + (1|universalID)",          # Log RT, Target 1/2, expt included
  "rxn_single_t" = "log_rxntime ~ validity + (1|universalID)"                # Log RT, Target 1/2, no expt
)

# Initialize lists to store models and ANOVA results
models <- list()
anova_results <- list()

# Loop over datasets and fit models dynamically
for (ds_name in names(datasets)) {
  data <- datasets[[ds_name]]
  
  # Select the correct formula based on whether `expt` is present
  if (ds_name == "both") {
    models[[paste0("acc_", ds_name)]] <- glmer(as.formula(formulas$acc_both), data = data, family = binomial)
    models[[paste0("rxn_", ds_name)]] <- lmer(as.formula(formulas$rxn_both), data = data)
  } else {
    models[[paste0("acc_", ds_name)]] <- glmer(as.formula(formulas$acc_single), data = data, family = binomial)
    models[[paste0("rxn_", ds_name)]] <- lmer(as.formula(formulas$rxn_single), data = data)
  }
  
  # Store ANOVA results
  anova_results[[paste0("aov_acc_", ds_name)]] <- Anova(models[[paste0("acc_", ds_name)]])
  anova_results[[paste0("aov_rxn_", ds_name)]] <- Anova(models[[paste0("rxn_", ds_name)]])
  
  # Fit target-specific models (Target 1 and Target 2)
  for (target in 1:2) {
    target_data <- data[data$target == target, ]
    
    # Choose the correct formula based on experiment inclusion
    model_name_acc <- paste0("acc_", ds_name, "_t", target)
    model_name_rxn <- paste0("rxn_", ds_name, "_t", target)
    formula_acc <- ifelse(ds_name == "both", "acc_both_t", "acc_single_t")
    formula_rxn <- ifelse(ds_name == "both", "rxn_both_t", "rxn_single_t")
    
    # Fit GLMM and LMM models for Target 1/2
    models[[model_name_acc]] <- glmer(as.formula(formulas[[formula_acc]]), data = target_data, family = binomial)
    models[[model_name_rxn]] <- lmer(as.formula(formulas[[formula_rxn]]), data = target_data)
    
    # Store ANOVA results
    anova_results[[paste0("aov_", model_name_acc)]] <- Anova(models[[model_name_acc]])
    anova_results[[paste0("aov_", model_name_rxn)]] <- Anova(models[[model_name_rxn]])
  }
}

# Print summaries for all models
lapply(models, summary)





#### Pairwise comparisons


# Define validity pair comparisons
validity_pairs <- list(
  "vi" = c(1, 2),  # Valid vs Invalid
  "vn" = c(1, 3),  # Valid vs Neutral
  "ni" = c(2, 3)   # Neutral vs Invalid
)

# Initialize lists to store models and ANOVA results
models <- list()
anova_results_paired <- list()

# Loop over datasets and fit models dynamically
for (ds_name in names(datasets)) {
  data <- datasets[[ds_name]]
  
  # Loop through targets (1 and 2)
  for (target in 1:2) {
    
    # Loop through each validity pair (1 vs 2, 1 vs 3, 2 vs 3)
    for (pair_name in names(validity_pairs)) {
      pair_values <- validity_pairs[[pair_name]]
      
      # Subset data for the current target and validity pair
      target_data <- data[data$target == target & data$validity %in% pair_values, ]
      
      # Generate model names
      model_name_acc <- paste0("acc_", ds_name, "_t", target, "_", pair_name)
      model_name_rxn <- paste0("rxn_", ds_name, "_t", target, "_", pair_name)
      
      # Choose formula (Experiment 2 and 1 do not include `expt`)
      formula_acc <- ifelse(ds_name == "both",
                            "accuracy ~ validity * expt + (1|universalID)",
                            "accuracy ~ validity + (1|universalID)")
      
      formula_rxn <- ifelse(ds_name == "both",
                            "log_rxntime ~ validity * expt + (1|universalID)",
                            "log_rxntime ~ validity + (1|universalID)")
      
      # Fit GLMM and LMM models
      models[[model_name_acc]] <- glmer(as.formula(formula_acc), data = target_data, family = binomial)
      models[[model_name_rxn]] <- lmer(as.formula(formula_rxn), data = target_data)
      
      # Store ANOVA results
      anova_results_paired[[paste0("aov_", model_name_acc)]] <- Anova(models[[model_name_acc]])
      anova_results_paired[[paste0("aov_", model_name_rxn)]] <- Anova(models[[model_name_rxn]])
      
      # Debugging prints (remove after testing)
      print(paste("Processed:", model_name_acc, "and", model_name_rxn))
      print(dim(target_data))  # Should be smaller than the original dataset
    }
  }
}

# Print summaries for all models
lapply(models, summary)


### ONE MORE TIME? Comparing Aud Exp data vs. Visual data:
# April 2 2025
# Take Valid - Invalid per subject (n = 10) per expt (Aud exp 1, exp 2, Vis)
# Figure out the distribution of these valid-invalid values
# Do an lmm on this data 10 v 10 v 10
# Do an lmm on pair comparisons 10 v 10, 3 times
# Do this for both accuracy and reaction time
# Also note validity, expt, and validity:expt effects

data_both_g_ave

datavis <- read.csv("TemporalAttention_E0_cb_AllData.csv")

datavis0 <- datavis # store a safe copy of data

datavis$subject <- datavis$subject + 1000
names(datavis)[names(datavis) == 'early.late'] <- 'target'
names(datavis)[names(datavis) == 'RT'] <- 'rxntime'
names(datavis)[names(datavis) == 'subject'] <- 'universalID'
datavis[datavis == "early"] <- 1
datavis[datavis == "late"] <- 2
datavis[datavis == "valid"] <- 1
datavis[datavis == "invalid"] <- 2
datavis[datavis == "neutral"] <- 3
datavis$target <- as.integer(as.character(datavis$target))
datavis$validity <- as.integer(as.character(datavis$validity))
datavis$expt <- "visual"

datavis$universalID <- factor(datavis$universalID)
datavis$contrast <- factor(datavis$contrast)
datavis$target <- factor(datavis$target)
datavis$validity <- factor(datavis$validity)
datavis$expt <- factor(datavis$expt)

# contrasts separately
datavis_contrast64 <- subset(datavis, contrast==64)
datavis_contrast100 <- subset(datavis, contrast==100)

# average across contrasts
datavis_g_ave <- datavis_contrast64
datavis_g_ave$accuracy <- (datavis_contrast64$accuracy + datavis_contrast100$accuracy)/2
datavis_g_ave$rxntime <- (datavis_contrast64$rxntime + datavis_contrast100$rxntime)/2
datavis_g_ave = subset(datavis_g_ave, select = -c(contrast))

# Filter for only validity 1 and 2
datavis_VI <- datavis_g_ave %>%
  filter(validity %in% c(1, 2))

# Subtract directly
datavis_diff <- datavis_VI %>%
  filter(validity %in% c(1, 2)) %>%
  group_by(universalID, target, expt) %>%
  summarize(
    accuracy_diff = accuracy[validity == 1] - accuracy[validity == 2],
    rxntime_diff = rxntime[validity == 1] - rxntime[validity == 2],
    .groups = "drop"
  )

# Filter auditory data for only validity 1 and 2
data_both_VI <- data_both_g_ave %>%
  filter(validity %in% c(1, 2))

# Subtract directly using same logic as visual
dataaud_diff <- data_both_VI %>%
  group_by(universalID, target, expt) %>%
  summarize(
    accuracy_diff = accuracy[validity == 1] - accuracy[validity == 2],
    rxntime_diff = rxntime[validity == 1] - rxntime[validity == 2],
    .groups = "drop"
  )

# Combine the visual and auditory dataframes of Valid Invalid difference
dataAV <- rbind(dataaud_diff, datavis_diff)
write.csv(dataAV, "data_sensory_differences_VI_accrt.csv", row.names = FALSE)

# Create a list of unique experiments
expt_list_diff <- unique(dataAV$expt)

# Initialize storage for AIC/BIC results (specific to diff)
aic_bic_results_diff <- data.frame()
best_fits_diff <- data.frame()

# Function to analyze difference distributions per experiment
analyze_diff_distribution <- function(data_subset, expt_label) {
  ### --- Reaction Time Difference --- ###
  rt_diff <- data_subset$rxntime_diff
  rt_shift <- -min(rt_diff) + 0.0001
  rt_pos <- rt_diff + rt_shift
  
  fit_rt_norm <- fitdist(rt_diff, "norm")
  fit_rt_gamma <- fitdist(rt_pos, "gamma")
  fit_rt_invgauss <- fitdist(rt_pos, "invgauss")
  fit_rt_lnorm <- fitdist(rt_pos, "lnorm")
  
  fits_rt <- list(Gaussian = fit_rt_norm, Gamma = fit_rt_gamma,
                  Inverse_Gaussian = fit_rt_invgauss, Lognormal = fit_rt_lnorm)
  
  aic_rt <- sapply(fits_rt, AIC)
  bic_rt <- sapply(fits_rt, BIC)
  
  aic_bic_rt_df <- data.frame(
    universalID = "group",
    expt = expt_label,
    Measure = "rxntime_diff",
    Distribution = names(aic_rt),
    AIC = aic_rt,
    BIC = bic_rt,
    n = length(rt_diff)
  )
  
  best_rt <- aic_bic_rt_df[which.min(aic_bic_rt_df$AIC), ]
  aic_bic_results_diff <<- rbind(aic_bic_results_diff, aic_bic_rt_df)
  best_fits_diff <<- rbind(best_fits_diff, best_rt)
  
  ### --- Accuracy Difference --- ###
  acc_diff <- data_subset$accuracy_diff
  acc_shift <- -min(acc_diff) + 0.0001
  acc_pos <- acc_diff + acc_shift
  
  fit_acc_norm <- fitdist(acc_diff, "norm")
  fit_acc_gamma <- fitdist(acc_pos, "gamma")
  fit_acc_invgauss <- fitdist(acc_pos, "invgauss")
  fit_acc_lnorm <- fitdist(acc_pos, "lnorm")
  
  fits_acc <- list(Gaussian = fit_acc_norm, Gamma = fit_acc_gamma,
                   Inverse_Gaussian = fit_acc_invgauss, Lognormal = fit_acc_lnorm)
  
  aic_acc <- sapply(fits_acc, AIC)
  bic_acc <- sapply(fits_acc, BIC)
  
  aic_bic_acc_df <- data.frame(
    universalID = "group",
    expt = expt_label,
    Measure = "accuracy_diff",
    Distribution = names(aic_acc),
    AIC = aic_acc,
    BIC = bic_acc,
    n = length(acc_diff)
  )
  
  best_acc <- aic_bic_acc_df[which.min(aic_bic_acc_df$AIC), ]
  aic_bic_results_diff <<- rbind(aic_bic_results_diff, aic_bic_acc_df)
  best_fits_diff <<- rbind(best_fits_diff, best_acc)
  
  ### --- Q-Q and Density Plots --- ###
  # RT plots
  qq_rt_df <- data.frame(
    sample_quantiles = sort(rt_diff),
    Gaussian = qnorm(ppoints(length(rt_diff)), mean = fit_rt_norm$estimate[1], sd = fit_rt_norm$estimate[2]),
    Gamma = qgamma(ppoints(length(rt_diff)), shape = fit_rt_gamma$estimate[1], rate = fit_rt_gamma$estimate[2]) - rt_shift,
    Inverse_Gaussian = qinvgauss(ppoints(length(rt_diff)), mean = fit_rt_invgauss$estimate[1], shape = fit_rt_invgauss$estimate[2]) - rt_shift,
    Lognormal = qlnorm(ppoints(length(rt_diff)), meanlog = fit_rt_lnorm$estimate[1], sdlog = fit_rt_lnorm$estimate[2]) - rt_shift
  )
  qq_rt_long <- pivot_longer(qq_rt_df, cols = -sample_quantiles, names_to = "Distribution", values_to = "Theoretical_Quantiles")
  qq_plot_rt <- ggplot(qq_rt_long, aes(x = Theoretical_Quantiles, y = sample_quantiles, color = Distribution)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(title = paste("Q-Q Plot (RT Diff):", expt_label),
         x = "Theoretical Quantiles", y = "Sample Quantiles")
  
  density_plot_rt <- ggplot(data.frame(rt_diff), aes(x = rt_diff)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "gray", alpha = 0.5) +
    stat_function(fun = dnorm, args = list(mean = fit_rt_norm$estimate[1], sd = fit_rt_norm$estimate[2]), color = "blue", linetype = "dashed") +
    stat_function(fun = dgamma, args = list(shape = fit_rt_gamma$estimate[1], rate = fit_rt_gamma$estimate[2]), color = "red",
                  inherit.aes = FALSE, aes(x = rt_diff + rt_shift)) +
    stat_function(fun = dinvgauss, args = list(mean = fit_rt_invgauss$estimate[1], shape = fit_rt_invgauss$estimate[2]), color = "green",
                  inherit.aes = FALSE, aes(x = rt_diff + rt_shift)) +
    stat_function(fun = dlnorm, args = list(meanlog = fit_rt_lnorm$estimate[1], sdlog = fit_rt_lnorm$estimate[2]), color = "purple",
                  inherit.aes = FALSE, aes(x = rt_diff + rt_shift)) +
    theme_minimal() +
    labs(title = paste("RT Diff Fit:", expt_label),
         x = "RT Difference", y = "Density")
  
  # Accuracy plots
  qq_acc_df <- data.frame(
    sample_quantiles = sort(acc_diff),
    Gaussian = qnorm(ppoints(length(acc_diff)), mean = fit_acc_norm$estimate[1], sd = fit_acc_norm$estimate[2]),
    Gamma = qgamma(ppoints(length(acc_diff)), shape = fit_acc_gamma$estimate[1], rate = fit_acc_gamma$estimate[2]) - acc_shift,
    Inverse_Gaussian = qinvgauss(ppoints(length(acc_diff)), mean = fit_acc_invgauss$estimate[1], shape = fit_acc_invgauss$estimate[2]) - acc_shift,
    Lognormal = qlnorm(ppoints(length(acc_diff)), meanlog = fit_acc_lnorm$estimate[1], sdlog = fit_acc_lnorm$estimate[2]) - acc_shift
  )
  qq_acc_long <- pivot_longer(qq_acc_df, cols = -sample_quantiles, names_to = "Distribution", values_to = "Theoretical_Quantiles")
  qq_plot_acc <- ggplot(qq_acc_long, aes(x = Theoretical_Quantiles, y = sample_quantiles, color = Distribution)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(title = paste("Q-Q Plot (Accuracy Diff):", expt_label),
         x = "Theoretical Quantiles", y = "Sample Quantiles")
  
  density_plot_acc <- ggplot(data.frame(acc_diff), aes(x = acc_diff)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "gray", alpha = 0.5) +
    stat_function(fun = dnorm, args = list(mean = fit_acc_norm$estimate[1], sd = fit_acc_norm$estimate[2]), color = "blue", linetype = "dashed") +
    stat_function(fun = dgamma, args = list(shape = fit_acc_gamma$estimate[1], rate = fit_acc_gamma$estimate[2]), color = "red",
                  inherit.aes = FALSE, aes(x = acc_diff + acc_shift)) +
    stat_function(fun = dinvgauss, args = list(mean = fit_acc_invgauss$estimate[1], shape = fit_acc_invgauss$estimate[2]), color = "green",
                  inherit.aes = FALSE, aes(x = acc_diff + acc_shift)) +
    stat_function(fun = dlnorm, args = list(meanlog = fit_acc_lnorm$estimate[1], sdlog = fit_acc_lnorm$estimate[2]), color = "purple",
                  inherit.aes = FALSE, aes(x = acc_diff + acc_shift)) +
    theme_minimal() +
    labs(title = paste("Accuracy Diff Fit:", expt_label),
         x = "Accuracy Difference", y = "Density")
  
  return(list(
    qq_plot_rt = qq_plot_rt,
    density_plot_rt = density_plot_rt,
    qq_plot_acc = qq_plot_acc,
    density_plot_acc = density_plot_acc
  ))
}

# Loop through each experiment and analyze all data from that condition
plots_list_diff <- list()

for (expt_label in expt_list_diff) {
  data_subset <- filter(dataAV, expt == expt_label)
  plots_list_diff[[expt_label]] <- analyze_diff_distribution(data_subset, expt_label)
}

# Summary of AIC/BIC per distribution
avg_aic_bic_diff <- aic_bic_results_diff %>%
  group_by(Measure, Distribution) %>%
  summarise(Average_AIC = mean(AIC), Average_BIC = mean(BIC), .groups = "drop")

# View results
print(best_fits_diff)
print(avg_aic_bic_diff)

# Display plots for a specific experiment
example_pair_diff <- names(plots_list_diff)[1]
print(plots_list_diff[[example_pair_diff]]$qq_plot_rt)
print(plots_list_diff[[example_pair_diff]]$density_plot_rt)
print(plots_list_diff[[example_pair_diff]]$qq_plot_acc)
print(plots_list_diff[[example_pair_diff]]$density_plot_acc)

# Conclusion: Gaussian
# Define function to fit 8 Gaussian LMMs for accuracy and reaction time
fit_diff_lmms_by_target <- function(dataAV) {
  comparisons <- list(
    "aud_e1_vs_e2_vs_visual" = c("aud-e1", "aud-e2", "visual"),
    "aud_e1_vs_e2" = c("aud-e1", "aud-e2"),
    "aud_e2_vs_visual" = c("aud-e2", "visual"),
    "aud_e1_vs_visual" = c("aud-e1", "visual")
  )
  
  models <- list()
  anovas <- list()
  
  for (target_label in c("all", "1", "2")) {
    # Filter by target
    if (target_label == "all") {
      target_data <- dataAV
    } else {
      target_data <- filter(dataAV, target == target_label)
    }
    
    for (label in names(comparisons)) {
      expt_levels <- comparisons[[label]]
      data_subset <- filter(target_data, expt %in% expt_levels)
      
      # Factorize experiment levels
      data_subset$expt <- factor(data_subset$expt, levels = expt_levels)
      
      n_obs <- nrow(data_subset)
      n_ids <- length(unique(data_subset$universalID))
      suffix <- ifelse(target_label == "all", "", paste0("_t", target_label))
      
      # Reaction time model
      if (n_obs > n_ids) {
        model_rxn <- lmer(rxntime_diff ~ expt + (1|universalID), data = data_subset)
      } else {
        model_rxn <- lm(rxntime_diff ~ expt, data = data_subset)
      }
      models[[paste0("rxn_", label, suffix)]] <- model_rxn
      anovas[[paste0("rxn_", label, suffix)]] <- Anova(model_rxn)
      
      # Accuracy model
      if (n_obs > n_ids) {
        model_acc <- lmer(accuracy_diff ~ expt + (1|universalID), data = data_subset)
      } else {
        model_acc <- lm(accuracy_diff ~ expt, data = data_subset)
      }
      models[[paste0("acc_", label, suffix)]] <- model_acc
      anovas[[paste0("acc_", label, suffix)]] <- Anova(model_acc)
    }
  }
  
  return(list(models = models, anovas = anovas))
}

diff_lmm_results <- fit_diff_lmms_by_target(dataAV)

# Model summaries
lapply(diff_lmm_results$models, summary)

# ANOVA tables
lapply(diff_lmm_results$anovas, print)

# Clean extractor for Anova results
extract_anova_summary <- function(anova_list) {
  results <- lapply(names(anova_list), function(name) {
    tbl <- anova_list[[name]]
    
    if ("Chisq" %in% colnames(tbl)) {
      stat <- tbl["expt", "Chisq"]
      df <- tbl["expt", "Df"]
      p <- tbl["expt", "Pr(>Chisq)"]
    } else if ("F value" %in% colnames(tbl)) {
      stat <- tbl["expt", "F value"]
      df <- tbl["expt", "Df"]
      p <- tbl["expt", "Pr(>F)"]
    } else {
      stat <- df <- p <- NA
    }
    
    sig <- symnum(p,
                  corr = FALSE, na = FALSE,
                  cutpoints = c(0, .001, .01, .05, .1, 1),
                  symbols = c("***", "**", "*", ".", ""))
    
    data.frame(
      Model = name,
      Statistic = round(stat, 3),
      Df = df,
      p_value = signif(p, 3),
      Signif = as.character(sig)
    )
  })
  
  do.call(rbind, results)
}

anova_summary_df <- extract_anova_summary(diff_lmm_results$anovas)
print(anova_summary_df)



library(MuMIn)  # for R²
library(broom.mixed)  # optional if you want tidy output

extract_anova_extended_summary <- function(models, anovas) {
  results <- lapply(names(anovas), function(name) {
    model <- models[[name]]
    anova_tbl <- anovas[[name]]
    
    # Determine model class
    is_lmm <- inherits(model, "lmerMod")
    is_lm  <- inherits(model, "lm")
    
    # Extract test statistic
    if ("Chisq" %in% colnames(anova_tbl)) {
      stat <- anova_tbl["expt", "Chisq"]
      df <- anova_tbl["expt", "Df"]
      p <- anova_tbl["expt", "Pr(>Chisq)"]
    } else if ("F value" %in% colnames(anova_tbl)) {
      stat <- anova_tbl["expt", "F value"]
      df <- anova_tbl["expt", "Df"]
      p <- anova_tbl["expt", "Pr(>F)"]
    } else {
      stat <- df <- p <- NA
    }
    
    sig <- symnum(p,
                  corr = FALSE, na = FALSE,
                  cutpoints = c(0, .001, .01, .05, .1, 1),
                  symbols = c("***", "**", "*", ".", ""))
    
    # R squared
    if (is_lmm) {
      r2_vals <- MuMIn::r.squaredGLMM(model)
      r2_marginal <- round(r2_vals[1], 3)
      r2_conditional <- round(r2_vals[2], 3)
    } else if (is_lm) {
      r2_marginal <- round(summary(model)$r.squared, 3)
      r2_conditional <- NA
    } else {
      r2_marginal <- r2_conditional <- NA
    }
    
    # Coefficient for one expt level
    if (is_lmm) {
      coefs <- fixef(model)
    } else if (is_lm) {
      coefs <- coef(model)
    } else {
      coefs <- NA
    }
    
    coef_expt <- if (any(grepl("^expt", names(coefs)))) {
      round(coefs[grep("^expt", names(coefs))[1]], 3)
    } else {
      NA
    }
    
    data.frame(
      Model = name,
      Statistic = round(stat, 3),
      Df = df,
      p_value = signif(p, 3),
      Signif = as.character(sig),
      R2_marginal = r2_marginal,
      R2_conditional = r2_conditional,
      Coef_expt = coef_expt
    )
  })
  
  do.call(rbind, results)
}

anova_summary_extended_df <- extract_anova_extended_summary(diff_lmm_results$models, diff_lmm_results$anovas)
print(anova_summary_extended_df)

## Now threshold analysis ##

thresholds <- read.csv('data/thresholds_with_averages.csv')

library(lme4)

# Manually reshape to long format
exp1_rows <- data.frame(
  universalID = thresholds$SubjectNum,
  Experiment = "aud-e1",
  AvgThreshold = thresholds$Avg_Exp1
)

exp2_rows <- data.frame(
  universalID = thresholds$SubjectNum,
  Experiment = "aud-e2",
  AvgThreshold = thresholds$Avg_Exp2
)

# Combine and drop NA threshold rows
thresholds_long <- rbind(exp1_rows, exp2_rows)
thresholds_long <- thresholds_long[!is.na(thresholds_long$AvgThreshold), ]

# Convert experiment to factor
thresholds_long$Experiment <- factor(thresholds_long$Experiment)

# Fit the linear mixed model
model.thresholds <- lmer(AvgThreshold ~ Experiment + (1 | universalID), data = thresholds_long)

# View results
summary(model.thresholds)
Anova(model.thresholds)
r.squaredGLMM(model.thresholds)

##### GLMMs with Lognormal Family for Reaction Time #####

# These GLMMs model raw rxntime using a log link, suited for skewed, positive-only distributions
# and provide a comparison to the existing LMMs on log_rxntime

##### GLMM Gamma(log) models for raw rxntime #####

gamma_models <- list()
gamma_anovas <- list()

# Loop over datasets and fit Gamma(log) GLMMs
for (ds_name in names(datasets)) {
  data <- datasets[[ds_name]]
  
  # Fit main model
  formula_rxn <- if (ds_name == "both") {
    "rxntime ~ validity * target * expt + (1|universalID)"
  } else {
    "rxntime ~ validity * target + (1|universalID)"
  }
  gamma_models[[paste0("rxn_gamma_", ds_name)]] <- glmer(as.formula(formula_rxn), data = data, family = Gamma(link = "log"))
  gamma_anovas[[paste0("aov_rxn_gamma_", ds_name)]] <- Anova(gamma_models[[paste0("rxn_gamma_", ds_name)]])
  
  # Fit target-specific models (Target 1 and 2)
  for (target in 1:2) {
    target_data <- data[data$target == target, ]
    
    formula_rxn_t <- if (ds_name == "both") {
      "rxntime ~ validity * expt + (1|universalID)"
    } else {
      "rxntime ~ validity + (1|universalID)"
    }
    
    model_name <- paste0("rxn_gamma_", ds_name, "_t", target)
    gamma_models[[model_name]] <- glmer(as.formula(formula_rxn_t), data = target_data, family = Gamma(link = "log"))
    gamma_anovas[[paste0("aov_", model_name)]] <- Anova(gamma_models[[model_name]])
  }
}

# Print model summaries
lapply(gamma_models, summary)

##### Gamma(log) GLMMs for pairwise validity comparisons on raw rxntime #####

# Initialize lists to store models and ANOVA results
gamma_models <- list()
gamma_anova_results_paired <- list()

# Loop over datasets and fit models dynamically
for (ds_name in names(datasets)) {
  data <- datasets[[ds_name]]
  
  # Loop through targets (1 and 2)
  for (target in 1:2) {
    
    # Loop through each validity pair (1 vs 2, 1 vs 3, 2 vs 3)
    for (pair_name in names(validity_pairs)) {
      pair_values <- validity_pairs[[pair_name]]
      
      # Subset data for the current target and validity pair
      target_data <- data[data$target == target & data$validity %in% pair_values, ]
      
      # Generate model name
      model_name_rxn <- paste0("rxn_gamma_", ds_name, "_t", target, "_", pair_name)
      
      # Choose formula (Experiment 2 and 1 do not include `expt`)
      formula_rxn <- ifelse(ds_name == "both",
                            "rxntime ~ validity * expt + (1|universalID)",
                            "rxntime ~ validity + (1|universalID)")
      
      # Fit Gamma GLMM with log link
      gamma_models[[model_name_rxn]] <- glmer(as.formula(formula_rxn), data = target_data, family = Gamma(link = "log"))
      
      # Store ANOVA results
      gamma_anova_results_paired[[paste0("aov_", model_name_rxn)]] <- Anova(gamma_models[[model_name_rxn]])
      
      # Debugging prints (remove if desired)
      print(paste("Processed Gamma GLMM:", model_name_rxn))
      print(dim(target_data))
    }
  }
}

##### AIC/BIC Comparison between LMM (log rxntime) and GLMM (gamma-log) #####
# Filter only rxn models that have matching gamma models
lmm_rxn_names <- names(models)[grepl("^rxn_", names(models))]
gamma_rxn_names <- names(gamma_models)

# Intersect only those that match up after the prefix swap
aic_bic_comparison <- data.frame()

for (lmm_name in lmm_rxn_names) {
  gamma_name <- sub("^rxn_", "rxn_gamma_", lmm_name)
  
  if (gamma_name %in% gamma_rxn_names) {
    lmm_model <- models[[lmm_name]]
    gamma_model <- gamma_models[[gamma_name]]
    
    comparison_row <- data.frame(
      Model = lmm_name,
      AIC_LMM = AIC(lmm_model),
      BIC_LMM = BIC(lmm_model),
      AIC_Gamma = AIC(gamma_model),
      BIC_Gamma = BIC(gamma_model)
    )
    
    aic_bic_comparison <- rbind(aic_bic_comparison, comparison_row)
  }
}

print(aic_bic_comparison)

cat("LMM reaction time model names:\n")
print(names(models)[grepl("^rxn_", names(models))])

cat("\nGamma GLMM reaction time model names:\n")
print(names(gamma_models))

# ----- Compute and plot average AIC/BIC from comparison -----

# Compute averages
avg_aic_lmm <- mean(aic_bic_comparison$AIC_LMM)
avg_bic_lmm <- mean(aic_bic_comparison$BIC_LMM)
avg_aic_gamma <- mean(aic_bic_comparison$AIC_Gamma)
avg_bic_gamma <- mean(aic_bic_comparison$BIC_Gamma)

cat("\nNumber of matched models:", nrow(aic_bic_comparison), "\n")
cat("Average AIC:\n  LMM:", avg_aic_lmm, "\n  Gamma GLMM:", avg_aic_gamma, "\n")
cat("Average BIC:\n  LMM:", avg_bic_lmm, "\n  Gamma GLMM:", avg_bic_gamma, "\n")

# Plot
library(ggplot2)
library(tidyr)

aic_bic_long <- pivot_longer(
  aic_bic_comparison,
  cols = c(AIC_LMM, AIC_Gamma, BIC_LMM, BIC_Gamma),
  names_to = c("Metric", "ModelType"),
  names_sep = "_",
  values_to = "Value"
)

ggplot(aic_bic_long, aes(x = Model, y = Value, fill = ModelType)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
  labs(title = "AIC/BIC: LMM (log RT) vs Gamma GLMM",
       x = "Model",
       y = "Score",
       fill = "Model Type")



### Who knew we could just do a simple ANOVA on the target-validity-subject avgs
## ... June 2025

dataAV_condition_avgs <- rbind(data_both_g_ave, datavis_g_ave)

aov_condavgs_rxn_a1v <- Anova(lmer(rxntime ~ expt * target * validity + (1|universalID), data = subset(dataAV_condition_avgs, expt %in% c("aud-e1", "visual"))))
aov_condavgs_rxn_a1v_t1 <- Anova(lmer(rxntime ~ expt * validity + (1|universalID), data = subset(dataAV_condition_avgs, expt %in% c("aud-e1", "visual") & target == 1)))
aov_condavgs_rxn_a1v_t2 <- Anova(lmer(rxntime ~ expt * validity + (1|universalID), data = subset(dataAV_condition_avgs, expt %in% c("aud-e1", "visual") & target == 2)))

aov_condavgs_rxn_a2v <- Anova(lmer(rxntime ~ expt * target * validity + (1|universalID), data = subset(dataAV_condition_avgs, expt %in% c("aud-e2", "visual"))))
aov_condavgs_rxn_a2v_t1 <- Anova(lmer(rxntime ~ expt * validity + (1|universalID), data = subset(dataAV_condition_avgs, expt %in% c("aud-e2", "visual") & target == 1)))
aov_condavgs_rxn_a2v_t2 <- Anova(lmer(rxntime ~ expt * validity + (1|universalID), data = subset(dataAV_condition_avgs, expt %in% c("aud-e2", "visual") & target == 2)))

aov_condavgs_rxn_a12 <- Anova(lmer(rxntime ~ expt * target * validity + (1|universalID), data = subset(dataAV_condition_avgs, expt %in% c("aud-e1", "aud-e2"))))
aov_condavgs_rxn_a12_t1 <- Anova(lmer(rxntime ~ expt * validity + (1|universalID), data = subset(dataAV_condition_avgs, expt %in% c("aud-e1", "aud-e2") & target == 1)))
aov_condavgs_rxn_a12_t2 <- Anova(lmer(rxntime ~ expt * validity + (1|universalID), data = subset(dataAV_condition_avgs, expt %in% c("aud-e1", "aud-e2") & target == 2)))

library(tidyverse)
library(lme4)
library(car)

# Combine data
dataAV_condition_avgs <- rbind(data_both_g_ave, datavis_g_ave)

# Validity pairs for pairwise models
validity_pairs <- list(
  vi = c("1", "2"),
  vn = c("1", "3"),
  ni = c("2", "3"),
  all = c("1", "2", "3")
)

# # Validity pairs for pairwise models
# validity_pairs <- list(
#   v = c("1"),
#   n = c("3"),
#   i = c("2"),
#   all = c("1", "2", "3")
# )

# All model combinations to loop over
dvs <- c("rxntime", "accuracy")
targets <- c("all", "1", "2")
expt_sets <- list(
  a1v = c("aud-e1", "visual"),
  a2v = c("aud-e2", "visual"),
  a12 = c("aud-e1", "aud-e2")
)

# Initialize model list
all_condavg_models <- list()

# Function to generate and store models
for (dv in dvs) {
  for (targ in targets) {
    for (exp_name in names(expt_sets)) {
      for (val_name in names(validity_pairs)) {
        
        df <- dataAV_condition_avgs %>%
          filter(expt %in% expt_sets[[exp_name]],
                 validity %in% validity_pairs[[val_name]]) %>%
          { if (targ != "all") filter(., target == targ) else . }
        
        if (nrow(df) < 10) next
        
        formula <- if (targ == "all" && val_name == "all") {
          as.formula(paste0(dv, " ~ expt * target * validity + (1|universalID)"))
        } else if (val_name == "all") {
          as.formula(paste0(dv, " ~ expt * validity + (1|universalID)"))
        } else if (targ == "all") {
          as.formula(paste0(dv, " ~ expt * target + (1|universalID)"))
        } else {
          as.formula(paste0(dv, " ~ expt + (1|universalID)"))
        }
        
        model <- tryCatch({
          fit <- lmer(formula, data = df)
          if (isSingular(fit, tol = 1e-4)) {
            message("Singular fit for: ", dv, " / ", exp_name, " / ", val_name, " / t", targ)
          }
          Anova(fit)
        }, error = function(e) {
          message("Model error for: ", dv, " / ", exp_name, " / ", val_name, " / t", targ)
          return(NULL)
        })
        
        # Save in list with clear name
        model_name <- paste(dv, exp_name, val_name, if (targ != "all") paste0("t", targ) else "all", sep = "_")
        all_condavg_models[[model_name]] <- model
      }
    }
  }
}

### Per workshop, plot the fits on the distributions if you transform


##### NEW ANALYSIS: VN & NI diffs (accuracy + rxntime) per subject × expt × target #####

# Combine auditory + visual (axes-averaged)
g_ave_all <- dplyr::bind_rows(
  dplyr::select(data_both_g_ave, universalID, expt, target, validity, accuracy, rxntime),
  dplyr::select(datavis_g_ave,  universalID, expt, target, validity, accuracy, rxntime)
)

# Compute Valid - Neutral (VN) and Neutral - Invalid (NI) per subj/expt/target
VN_NI_diffs <- g_ave_all %>%
  dplyr::group_by(universalID, expt, target) %>%
  dplyr::summarise(
    VN_acc = dplyr::first(accuracy[validity == 1]) - dplyr::first(accuracy[validity == 3]),
    NI_acc = dplyr::first(accuracy[validity == 3]) - dplyr::first(accuracy[validity == 2]),
    VN_rtn = dplyr::first(rxntime[validity == 1])  - dplyr::first(rxntime[validity == 3]),
    NI_rtn = dplyr::first(rxntime[validity == 3])  - dplyr::first(rxntime[validity == 2]),
    .groups = "drop"
  ) %>%
  # drop incomplete triplets
  dplyr::filter(!is.na(VN_acc) & !is.na(NI_acc) & !is.na(VN_rtn) & !is.na(NI_rtn)) %>%
  # round everything to 6 decimals for clean CSV
  dplyr::mutate(
    VN_acc = round(VN_acc, 6),
    NI_acc = round(NI_acc, 6),
    VN_rtn = round(VN_rtn, 6),
    NI_rtn = round(NI_rtn, 6)
  ) %>%
  dplyr::arrange(expt, universalID, target)

# Quick sanity check: expect 60 rows (10 × 3 × 2)
cat("VN/NI rows:", nrow(VN_NI_diffs), "\n")

# Export
write.csv(VN_NI_diffs, "data_VN_NI_accrt_bySubjExptTarget.csv", row.names = FALSE)










##### EXPLORATORY PLOTS: (1) Accuracy V–N vs N–I, (2) RT V–N vs N–I #####
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(rlang)  # for sym(), .data

# --- Color palette (exact shades) ---
col_map <- c(
  "aud-e1_T1" = "#fca5a5",  # lighter red
  "aud-e1_T2" = "#dc2626",  # darker red
  "aud-e2_T1" = "#fdba74",  # lighter orange
  "aud-e2_T2" = "#ea580c",  # darker orange
  "visual_T1" = "#93c5fd",  # lightest blue
  "visual_T2" = "#1d4ed8"   # darkest blue
)
light_gray <- "#d1d5db"

# Ensure types
VN_NI_diffs <- VN_NI_diffs %>%
  dplyr::mutate(
    target = as.factor(target),
    grp = paste(expt, paste0("T", target), sep = "_"),
    expt = factor(expt, levels = c("aud-e1","aud-e2","visual")),
    grp = factor(grp, levels = c("aud-e1_T1","aud-e1_T2","aud-e2_T1","aud-e2_T2","visual_T1","visual_T2"))
  )

# ---------- helper: within-experiment Subject links (T1 <-> T2) ----------
links_within_expt_acc <- VN_NI_diffs %>%
  dplyr::select(universalID, expt, target, VN_acc, NI_acc) %>%
  tidyr::pivot_wider(
    names_from = target,
    values_from = c(VN_acc, NI_acc),
    names_prefix = "t"
  ) %>%
  dplyr::filter(
    !is.na(VN_acc_t1), !is.na(VN_acc_t2),
    !is.na(NI_acc_t1), !is.na(NI_acc_t2)
  )

links_within_expt_rtn <- VN_NI_diffs %>%
  dplyr::select(universalID, expt, target, VN_rtn, NI_rtn) %>%
  tidyr::pivot_wider(
    names_from = target,
    values_from = c(VN_rtn, NI_rtn),
    names_prefix = "t"
  ) %>%
  dplyr::filter(
    !is.na(VN_rtn_t1), !is.na(VN_rtn_t2),
    !is.na(NI_rtn_t1), !is.na(NI_rtn_t2)
  )

# ---------- helper: across-experiment Subject links (aud-e1 <-> aud-e2) ----------
mk_across_links <- function(df, vn_col, ni_col) {
  wide <- df %>%
    dplyr::filter(expt %in% c("aud-e1","aud-e2")) %>%
    dplyr::select(universalID, expt, target, !!rlang::sym(vn_col), !!rlang::sym(ni_col)) %>%
    tidyr::pivot_wider(
      names_from = expt,
      values_from = c(!!rlang::sym(vn_col), !!rlang::sym(ni_col)),
      names_sep = "_"
    )
  wide %>%
    dplyr::filter(
      !is.na(.data[[paste0(vn_col, "_aud-e1")]]),
      !is.na(.data[[paste0(vn_col, "_aud-e2")]]),
      !is.na(.data[[paste0(ni_col, "_aud-e1")]]),
      !is.na(.data[[paste0(ni_col, "_aud-e2")]])
    )
}

links_across_expt_acc <- mk_across_links(VN_NI_diffs, "VN_acc", "NI_acc")
links_across_expt_rtn <- mk_across_links(VN_NI_diffs, "VN_rtn", "NI_rtn")

# --- Group means for overlay (3 expts × 2 targets = 6 points) ---
means_acc <- VN_NI_diffs %>%
  dplyr::group_by(expt, target) %>%
  dplyr::summarise(NI_acc = mean(NI_acc), VN_acc = mean(VN_acc), .groups = "drop") %>%
  dplyr::mutate(grp = paste(expt, paste0("T", target), sep = "_"))

means_rtn <- VN_NI_diffs %>%
  dplyr::group_by(expt, target) %>%
  dplyr::summarise(NI_rtn = mean(NI_rtn), VN_rtn = mean(VN_rtn), .groups = "drop") %>%
  dplyr::mutate(grp = paste(expt, paste0("T", target), sep = "_"))

# ---------- Plot 1: Accuracy ----------
p_acc <- ggplot() +
  # gray lines: within-experiment T1<->T2
  geom_segment(
    data = links_within_expt_acc,
    aes(x = NI_acc_t1, y = VN_acc_t1, xend = NI_acc_t2, yend = VN_acc_t2, group = interaction(universalID, expt)),
    color = light_gray, linewidth = 0.5, alpha = 0.7
  ) +
  # gray lines: across experiments (aud-e1 <-> aud-e2) for each target
  geom_segment(
    data = links_across_expt_acc,
    aes(
      x = .data[["NI_acc_aud-e1"]], y = .data[["VN_acc_aud-e1"]],
      xend = .data[["NI_acc_aud-e2"]], yend = .data[["VN_acc_aud-e2"]],
      group = interaction(universalID, target)
    ),
    color = light_gray, linewidth = 0.5, alpha = 0.7, linetype = "solid"
  ) +
  # identity line
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 0.5) +
  # points
  geom_point(
    data = VN_NI_diffs,
    aes(x = NI_acc, y = VN_acc, color = grp, shape = expt),
    size = 2.8, alpha = 0.95
  ) +
  geom_point(
    data = means_acc,
    aes(x = NI_acc, y = VN_acc, color = grp, shape = expt),
    size = 6, alpha = 1
  ) +
  scale_color_manual(values = col_map, name = "Expt × Target") +
  scale_shape_manual(values = c(16, 17, 15), name = "Experiment") +
  labs(
    title = "Accuracy: V−N (y) vs N−I (x)",
    x = "N − I (accuracy)",
    y = "V − N (accuracy)"
  ) +
  coord_equal() +  # <- uncomment for equal aspect ratio (true 45°)
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# ---------- Plot 2: Reaction Time ----------
p_rt <- ggplot() +
  # gray lines: within-experiment T1<->T2
  geom_segment(
    data = links_within_expt_rtn,
    aes(x = NI_rtn_t1, y = VN_rtn_t1, xend = NI_rtn_t2, yend = VN_rtn_t2, group = interaction(universalID, expt)),
    color = light_gray, linewidth = 0.5, alpha = 0.7
  ) +
  # gray lines: across experiments (aud-e1 <-> aud-e2) for each target
  geom_segment(
    data = links_across_expt_rtn,
    aes(
      x = .data[["NI_rtn_aud-e1"]], y = .data[["VN_rtn_aud-e1"]],
      xend = .data[["NI_rtn_aud-e2"]], yend = .data[["VN_rtn_aud-e2"]],
      group = interaction(universalID, target)
    ),
    color = light_gray, linewidth = 0.5, alpha = 0.7, linetype = "solid"
  ) +
  # identity line
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 0.5) +
  # points
  geom_point(
    data = VN_NI_diffs,
    aes(x = NI_rtn, y = VN_rtn, color = grp, shape = expt),
    size = 2.8, alpha = 0.95
  ) +
  geom_point(
    data = means_rtn,
    aes(x = NI_rtn, y = VN_rtn, color = grp, shape = expt),
    size = 6, alpha = 1
  ) + 
  scale_color_manual(values = col_map, name = "Expt × Target") +
  scale_shape_manual(values = c(16, 17, 15), name = "Experiment") +
  labs(
    title = "Reaction Time: V−N (y) vs N−I (x)",
    x = "N − I (RT)",
    y = "V − N (RT)"
  ) +
  coord_equal() +  # <- optional equal aspect ratio
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# View in RStudio
print(p_acc)
print(p_rt)






##### FOUR MORE PLOTS: Target-specific (T1 then T2), Accuracy then RT #####
library(dplyr)
library(tidyr)
library(ggplot2)
library(rlang)

# Helper: across-experiment links for a single target (aud-e1 <-> aud-e2)
mk_across_links_one_target <- function(df, target_val, vn_col, ni_col) {
  df %>%
    dplyr::filter(target == target_val, expt %in% c("aud-e1","aud-e2")) %>%
    dplyr::select(universalID, expt, target, !!sym(vn_col), !!sym(ni_col)) %>%
    tidyr::pivot_wider(
      names_from = expt,
      values_from = c(!!sym(vn_col), !!sym(ni_col)),
      names_sep = "_"
    ) %>%
    dplyr::filter(
      !is.na(.data[[paste0(vn_col, "_aud-e1")]]),
      !is.na(.data[[paste0(vn_col, "_aud-e2")]]),
      !is.na(.data[[paste0(ni_col, "_aud-e1")]]),
      !is.na(.data[[paste0(ni_col, "_aud-e2")]])
    )
}

make_target_plot <- function(df, target_val, y_col, x_col, title_text, x_lab, y_lab, col_map) {
  # subset data to the target
  dft <- df %>% dplyr::filter(target == target_val)
  
  # choose the color keys present for this target (expt × target)
  col_keys <- paste0(levels(dft$expt), "_T", target_val)
  col_keys <- col_keys[col_keys %in% names(col_map)]
  
  # across-experiment links (aud-e1 <-> aud-e2) for this target
  links <- mk_across_links_one_target(
    df = df, target_val = target_val,
    vn_col = y_col, ni_col = x_col
  )
  
  # --- means for this target (3 expts × this one target = up to 3 points) ---
  means_df <- dft %>%
    dplyr::group_by(expt, target, grp) %>%
    dplyr::summarise(
      mean_x = mean(.data[[x_col]]),
      mean_y = mean(.data[[y_col]]),
      .groups = "drop"
    )
  
  ggplot() +
    # across-exp gray segments per participant (aud-e1 <-> aud-e2)
    geom_segment(
      data = links,
      aes(
        x = .data[[paste0(x_col, "_aud-e1")]], y = .data[[paste0(y_col, "_aud-e1")]],
        xend = .data[[paste0(x_col, "_aud-e2")]], yend = .data[[paste0(y_col, "_aud-e2")]],
        group = universalID
      ),
      color = "#d1d5db", linewidth = 0.5, alpha = 0.8
    ) +
    # identity line y = x
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 0.5) +
    # points (all subjects)
    geom_point(
      data = dft,
      aes(x = .data[[x_col]], y = .data[[y_col]], color = grp, shape = expt),
      size = 2.8, alpha = 0.95
    ) +
    # BIG mean points (per expt for this target)
    geom_point(
      data = means_df,
      aes(x = mean_x, y = mean_y, color = grp, shape = expt),
      size = 6, alpha = 1
    ) +
    scale_color_manual(values = col_map[col_keys], limits = col_keys, name = "Expt × Target") +
    scale_shape_manual(values = c(16, 17, 15), name = "Experiment") +
    labs(title = title_text, x = x_lab, y = y_lab) +
    coord_equal() +  # square axes so y=x is truly 45°
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
}

# Ensure factor levels on VN_NI_diffs (matching earlier block)
VN_NI_diffs <- VN_NI_diffs %>%
  dplyr::mutate(
    target = as.factor(target),
    expt   = factor(expt, levels = c("aud-e1","aud-e2","visual")),
    grp    = factor(paste(expt, paste0("T", target), sep = "_"),
                    levels = c("aud-e1_T1","aud-e1_T2","aud-e2_T1","aud-e2_T2","visual_T1","visual_T2"))
  )

# COLOR MAP (reuse your existing one)
col_map <- c(
  "aud-e1_T1" = "#fca5a5",  # lighter red
  "aud-e1_T2" = "#dc2626",  # darker red
  "aud-e2_T1" = "#fdba74",  # lighter orange
  "aud-e2_T2" = "#ea580c",  # darker orange
  "visual_T1" = "#93c5fd",  # lightest blue
  "visual_T2" = "#1d4ed8"   # darkest blue
)

# ---- Build the four plots ----
p_T1_acc <- make_target_plot(
  df = VN_NI_diffs, target_val = "1",
  y_col = "VN_acc", x_col = "NI_acc",
  title_text = "Target 1 — Accuracy: V−N (y) vs N−I (x)",
  x_lab = "N − I (accuracy)", y_lab = "V − N (accuracy)",
  col_map = col_map
)

p_T1_rt <- make_target_plot(
  df = VN_NI_diffs, target_val = "1",
  y_col = "VN_rtn", x_col = "NI_rtn",
  title_text = "Target 1 — Reaction Time: V−N (y) vs N−I (x)",
  x_lab = "N − I (RT)", y_lab = "V − N (RT)",
  col_map = col_map
)

p_T2_acc <- make_target_plot(
  df = VN_NI_diffs, target_val = "2",
  y_col = "VN_acc", x_col = "NI_acc",
  title_text = "Target 2 — Accuracy: V−N (y) vs N−I (x)",
  x_lab = "N − I (accuracy)", y_lab = "V − N (accuracy)",
  col_map = col_map
)

p_T2_rt <- make_target_plot(
  df = VN_NI_diffs, target_val = "2",
  y_col = "VN_rtn", x_col = "NI_rtn",
  title_text = "Target 2 — Reaction Time: V−N (y) vs N−I (x)",
  x_lab = "N − I (RT)", y_lab = "V − N (RT)",
  col_map = col_map
)

# View them in RStudio
print(p_T1_acc)
print(p_T1_rt)
print(p_T2_acc)
print(p_T2_rt)


## Minimal raw V vs N and N vs I plots (Accuracy + RT), T1 and T2
## Uses: g_ave_all (universalID, expt, target, validity, accuracy, rxntime)
## Colors: col_map already defined

library(dplyr)
library(tidyr)
library(ggplot2)

# 0) ensure 1 row per (ID, expt, target, validity)
g_ave_unique <- g_ave_all %>%
  dplyr::group_by(universalID, expt, target, validity) %>%
  dplyr::summarise(
    accuracy = mean(accuracy, na.rm = TRUE),
    rxntime  = mean(rxntime,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    expt   = factor(expt, levels = c("aud-e1","aud-e2","visual")),
    target = factor(target)
  )

# OPTIONAL: one gray you already used elsewhere
light_gray <- "#d1d5db"

make_panel <- function(targ = "1", dv = c("accuracy","rxntime"), pair = c("VN","NI"),
                       title_text, x_lab, y_lab) {
  dv <- match.arg(dv); pair <- match.arg(pair)
  
  if (pair == "VN") {
    vw <- g_ave_unique %>% dplyr::filter(target == targ, validity %in% c("1","3")) %>%
      tidyr::pivot_wider(names_from = validity, values_from = c(accuracy, rxntime))
    x <- if (dv == "accuracy") "accuracy_3" else "rxntime_3"  # N on x
    y <- if (dv == "accuracy") "accuracy_1" else "rxntime_1"  # V on y
  } else { # NI
    vw <- g_ave_unique %>% dplyr::filter(target == targ, validity %in% c("2","3")) %>%
      tidyr::pivot_wider(names_from = validity, values_from = c(accuracy, rxntime))
    x <- if (dv == "accuracy") "accuracy_2" else "rxntime_2"  # I on x
    y <- if (dv == "accuracy") "accuracy_3" else "rxntime_3"  # N on y
  }
  
  # color = expt×target, shape = expt
  vw <- vw %>%
    dplyr::mutate(grp = factor(paste0(expt, "_T", targ),
                               levels = c("aud-e1_T1","aud-e1_T2","aud-e2_T1","aud-e2_T2","visual_T1","visual_T2")))
  
  # —— NEW (minimal): build cross-experiment links aud-e1 ↔ aud-e2 for the SAME subject
  left  <- vw %>% dplyr::filter(expt == "aud-e1") %>% dplyr::select(universalID, x1 = .data[[x]], y1 = .data[[y]])
  right <- vw %>% dplyr::filter(expt == "aud-e2") %>% dplyr::select(universalID, x2 = .data[[x]], y2 = .data[[y]])
  links <- dplyr::inner_join(left, right, by = "universalID")  # only subjects present in both e1 & e2
  
  # big mean points per expt for this target
  means <- vw %>%
    dplyr::group_by(expt, grp) %>%
    dplyr::summarise(mean_x = mean(.data[[x]], na.rm = TRUE),
                     mean_y = mean(.data[[y]], na.rm = TRUE),
                     .groups = "drop")
  
  ggplot() +
    # —— NEW: gray subject lines (aud-e1 -> aud-e2)
    geom_segment(
      data = links,
      aes(x = x1, y = y1, xend = x2, yend = y2, group = universalID),
      color = light_gray, linewidth = 0.5, alpha = 0.8
    ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_point(data = vw,
               aes(x = .data[[x]], y = .data[[y]], color = grp, shape = expt),
               size = 2.8, alpha = 0.95) +
    geom_point(data = means,
               aes(x = mean_x, y = mean_y, color = grp, shape = expt),
               size = 6) +
    scale_color_manual(values = col_map, name = "Expt × Target", drop = FALSE) +
    scale_shape_manual(values = c(16, 17, 15), name = "Experiment") +
    labs(title = title_text, x = x_lab, y = y_lab) +
    coord_equal() +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
}


## --- Accuracy panels (T1 | T2 on top = V vs N; bottom = N vs I) ---
p_acc_T1_VN <- make_panel("1", "accuracy", "VN",
                          "T1 — Accuracy: V (y) vs N (x)", "Neutral (accuracy)", "Valid (accuracy)")
p_acc_T2_VN <- make_panel("2", "accuracy", "VN",
                          "T2 — Accuracy: V (y) vs N (x)", "Neutral (accuracy)", "Valid (accuracy)")
p_acc_T1_NI <- make_panel("1", "accuracy", "NI",
                          "T1 — Accuracy: N (y) vs I (x)", "Invalid (accuracy)", "Neutral (accuracy)")
p_acc_T2_NI <- make_panel("2", "accuracy", "NI",
                          "T2 — Accuracy: N (y) vs I (x)", "Invalid (accuracy)", "Neutral (accuracy)")

## --- Reaction time panels (same layout) ---
p_rt_T1_VN <- make_panel("1", "rxntime", "VN",
                         "T1 — RT: V (y) vs N (x)", "Neutral (RT)", "Valid (RT)")
p_rt_T2_VN <- make_panel("2", "rxntime", "VN",
                         "T2 — RT: V (y) vs N (x)", "Neutral (RT)", "Valid (RT)")
p_rt_T1_NI <- make_panel("1", "rxntime", "NI",
                         "T1 — RT: N (y) vs I (x)", "Invalid (RT)", "Neutral (RT)")
p_rt_T2_NI <- make_panel("2", "rxntime", "NI",
                         "T2 — RT: N (y) vs I (x)", "Invalid (RT)", "Neutral (RT)")

## print panels individually if you like
# print(p_acc_T1_VN); print(p_acc_T2_VN); print(p_acc_T1_NI); print(p_acc_T2_NI)
# print(p_rt_T1_VN);  print(p_rt_T2_VN);  print(p_rt_T1_NI);  print(p_rt_T2_NI)

## Optional: assemble 2×2 figures if gridExtra is available
if (requireNamespace("gridExtra", quietly = TRUE)) {
  gridExtra::grid.arrange(p_acc_T1_VN, p_acc_T2_VN, p_acc_T1_NI, p_acc_T2_NI, ncol = 2)
  gridExtra::grid.arrange(p_rt_T1_VN,  p_rt_T2_VN,  p_rt_T1_NI,  p_rt_T2_NI,  ncol = 2)
}
## ---- 2×2 grids with base grid (no extra packages) ----
## Accuracy grid: (T1 V~N | T2 V~N) / (T1 N~I | T2 N~I)
grid_2x2 <- function(p11, p12, p21, p22, top_title = NULL) {
  # base grid is part of R; no installs needed
  grid::grid.newpage()
  lay <- grid::grid.layout(nrow = 2, ncol = 2)
  grid::pushViewport(grid::viewport(layout = lay))
  if (!is.null(top_title)) {
    grid::grid.text(top_title, x = 0.5, y = 1, just = c("center","top"),
                    gp = grid::gpar(fontface = "bold", cex = 1.2), vjust = 2)
  }
  print(p11, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p12, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p21, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p22, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
}

# Accuracy 2×2
grid_2x2(
  p_acc_T1_VN, p_acc_T2_VN,
  p_acc_T1_NI, p_acc_T2_NI,
  top_title = "Accuracy — raw V vs N (top) and N vs I (bottom)"
)

# Reaction time 2×2
grid_2x2(
  p_rt_T1_VN, p_rt_T2_VN,
  p_rt_T1_NI, p_rt_T2_NI,
  top_title = "Reaction Time — raw V vs N (top) and N vs I (bottom)"
)




### Ok now export this V N I and lets go do it in matlab
write.csv(g_ave_all, "g_ave_all.csv", row.names = FALSE)







##### 2×2 BAR PLOTS: (Accuracy T1 | Accuracy T2) / (RT T1 | RT T2) #####

# deps
library(dplyr)
library(ggplot2)
library(forcats)

# safety: SEM
sem <- function(x) sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))

# 0) Gather per-subject means (already grouped per subject × axis × target × validity)
e1_subj <- data_e1_grouped %>%
  dplyr::select(universalID, expt, axis, target, validity, accuracy, rxntime) %>%
  dplyr::mutate(expt = "aud-e1")

e2_subj <- data_e2_grouped %>%
  dplyr::select(universalID, expt, axis, target, validity, accuracy, rxntime) %>%
  dplyr::mutate(expt = "aud-e2")

subj_long <- dplyr::bind_rows(e1_subj, e2_subj) %>%
  dplyr::mutate(
    validity_lab = dplyr::recode(as.character(validity),
                                 "1" = "Valid", "2" = "Invalid", "3" = "Neutral"),
    axis_num  = as.integer(as.character(axis)),
    axis_label = ifelse(expt == "aud-e1",
                        paste0("E1-A", axis_num),
                        paste0("E2-A", axis_num)),
    validity_lab = factor(validity_lab, levels = c("Valid","Neutral","Invalid"))
  )

# 1) helper to build one barplot for a given DV ("accuracy" or "rxntime") and target (1/2)
make_barplot <- function(df, dv = c("accuracy","rxntime"), targ = 1) {
  dv <- match.arg(dv)
  y_lab <- if (dv == "accuracy") "Mean accuracy" else "Mean RT"
  ttl   <- paste0(ifelse(dv == "accuracy", "Accuracy", "Reaction Time"),
                  " by Validity across Axes — Target ", targ)
  
  df_targ <- df %>% dplyr::filter(target == targ)
  
  # summary across subjects per axis × validity
  axis_summary <- df_targ %>%
    dplyr::group_by(expt, axis_label, validity_lab) %>%
    dplyr::summarise(
      mean_y = mean(.data[[dv]], na.rm = TRUE),
      sem_y  = sem(.data[[dv]]),
      n_subj = dplyr::n_distinct(universalID),
      .groups = "drop"
    )
  
  # order x-axis: E1-A1..E1-A5 then E2-A1..E2-A2 (whatever exists)
  e1_axes <- sort(unique(axis_summary$axis_label[axis_summary$expt == "aud-e1"]))
  e2_axes <- sort(unique(axis_summary$axis_label[axis_summary$expt == "aud-e2"]))
  axis_levels <- c(e1_axes, e2_axes)
  
  axis_summary <- axis_summary %>%
    dplyr::mutate(axis_label = factor(axis_label, levels = axis_levels))
  
  # colors (Neutral gray by convention)
  validity_cols <- c("Valid" = "#1b9e77", "Neutral" = "#9ca3af", "Invalid" = "#d95f02")
  pd <- position_dodge(width = 0.8)
  
  ggplot(axis_summary, aes(x = axis_label, y = mean_y, fill = validity_lab)) +
    geom_col(position = pd, width = 0.7) +
    geom_errorbar(aes(ymin = mean_y - sem_y, ymax = mean_y + sem_y),
                  position = pd, width = 0.25, linewidth = 0.5) +
    scale_fill_manual(values = validity_cols, name = "Validity") +
    labs(title = ttl, x = "Axis (E1 has 5; E2 has 2)", y = y_lab) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# 2) Build the four panels
p_acc_T1 <- make_barplot(subj_long, dv = "accuracy", targ = 1)
p_acc_T2 <- make_barplot(subj_long, dv = "accuracy", targ = 2)
p_rt_T1  <- make_barplot(subj_long, dv = "rxntime",  targ = 1)
p_rt_T2  <- make_barplot(subj_long, dv = "rxntime",  targ = 2)

# 3) Arrange into a 2×2 grid
if (requireNamespace("gridExtra", quietly = TRUE)) {
  gridExtra::grid.arrange(
    p_acc_T1, p_acc_T2,
    p_rt_T1,  p_rt_T2,
    ncol = 2,
    top = grid::textGrob("Validity × Axis (Exp 1 & 2): Accuracy (top) and Reaction Time (bottom)",
                         gp = grid::gpar(fontface = "bold", cex = 1.2))
  )
} else {
  # base grid fallback (no extra packages required)
  grid::grid.newpage()
  lay <- grid::grid.layout(nrow = 2, ncol = 2)
  grid::pushViewport(grid::viewport(layout = lay))
  grid::grid.text("Validity × Axis (Exp 1 & 2): Accuracy (top) and Reaction Time (bottom)",
                  x = 0.5, y = 1, just = c("center","top"),
                  gp = grid::gpar(fontface = "bold", cex = 1.2), vjust = 2)
  print(p_acc_T1, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_acc_T2, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p_rt_T1,  vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p_rt_T2,  vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
}

# Optional saves:
# ggsave("bargrid_acc_rt_T1T2.png", width = 14, height = 10, dpi = 300)
# ggsave("bar_acc_T1.png", p_acc_T1, width = 9, height = 5, dpi = 300)
# ggsave("bar_acc_T2.png", p_acc_T2, width = 9, height = 5, dpi = 300)
# ggsave("bar_rt_T1.png",  p_rt_T1,  width = 9, height = 5, dpi = 300)
# ggsave("bar_rt_T2.png",  p_rt_T2,  width = 9, height = 5, dpi = 300)



##### 2×2 BAR PLOTS (Validity-collapsed): per-axis averages #####

library(dplyr)
library(ggplot2)
library(forcats)

sem <- function(x) sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))

# 0) Bring together E1/E2 subject-level grouped data
e1_subj <- data_e1_grouped %>%
  dplyr::select(universalID, expt, axis, target, validity, accuracy, rxntime) %>%
  dplyr::mutate(expt = "aud-e1")

e2_subj <- data_e2_grouped %>%
  dplyr::select(universalID, expt, axis, target, validity, accuracy, rxntime) %>%
  dplyr::mutate(expt = "aud-e2")

subj_long <- dplyr::bind_rows(e1_subj, e2_subj) %>%
  dplyr::mutate(
    axis_num   = as.integer(as.character(axis)),
    axis_label = ifelse(expt == "aud-e1",
                        paste0("E1-A", axis_num),
                        paste0("E2-A", axis_num))
  )

# 1) helper: build barplot for a given DV ("accuracy" or "rxntime") and target (1/2)
#    Two-stage averaging:
#    (a) within subject: average across validity (collapse 1/2/3)
#    (b) across subjects: mean ± SEM per axis
make_axis_avg_barplot <- function(df, dv = c("accuracy","rxntime"), targ = 1) {
  dv <- match.arg(dv)
  y_lab <- if (dv == "accuracy") "Mean accuracy (collapsed over V/N/I)"
  else                   "Mean RT (collapsed over V/N/I)"
  ttl   <- paste0(ifelse(dv == "accuracy", "Accuracy", "Reaction Time"),
                  " — Per-Axis Average (collapsed V/N/I), Target ", targ)
  
  # (a) collapse validity within subject
  subj_axis <- df %>%
    dplyr::filter(target == targ) %>%
    dplyr::group_by(universalID, expt, axis_label) %>%
    dplyr::summarise(
      subj_avg = mean(.data[[dv]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # (b) summarise across subjects
  axis_summary <- subj_axis %>%
    dplyr::group_by(expt, axis_label) %>%
    dplyr::summarise(
      mean_y = mean(subj_avg, na.rm = TRUE),
      sem_y  = sem(subj_avg),
      n_subj = dplyr::n_distinct(universalID),
      .groups = "drop"
    )
  
  # x order: E1 axes then E2 axes
  e1_axes <- sort(unique(axis_summary$axis_label[axis_summary$expt == "aud-e1"]))
  e2_axes <- sort(unique(axis_summary$axis_label[axis_summary$expt == "aud-e2"]))
  axis_levels <- c(e1_axes, e2_axes)
  
  axis_summary <- axis_summary %>%
    dplyr::mutate(
      axis_label = factor(axis_label, levels = axis_levels),
      expt = factor(expt, levels = c("aud-e1","aud-e2"))
    )
  
  # color by experiment (since validity is collapsed)
  expt_cols <- c("aud-e1" = "#ef4444",  # red-ish for E1
                 "aud-e2" = "#f59e0b")  # orange-ish for E2
  
  ggplot(axis_summary, aes(x = axis_label, y = mean_y, fill = expt)) +
    geom_col(width = 0.7) +
    geom_errorbar(aes(ymin = mean_y - sem_y, ymax = mean_y + sem_y),
                  width = 0.25, linewidth = 0.5) +
    scale_fill_manual(values = expt_cols, name = "Experiment",
                      labels = c("aud-e1" = "Aud Exp 1", "aud-e2" = "Aud Exp 2")) +
    labs(title = ttl, x = "Axis", y = y_lab) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# 2) Build four panels: Accuracy T1/T2, RT T1/T2
p_acc_T1_axis <- make_axis_avg_barplot(subj_long, dv = "accuracy", targ = 1)
p_acc_T2_axis <- make_axis_avg_barplot(subj_long, dv = "accuracy", targ = 2)
p_rt_T1_axis  <- make_axis_avg_barplot(subj_long, dv = "rxntime",  targ = 1)
p_rt_T2_axis  <- make_axis_avg_barplot(subj_long, dv = "rxntime",  targ = 2)

# 3) Arrange into a 2×2 grid
if (requireNamespace("gridExtra", quietly = TRUE)) {
  gridExtra::grid.arrange(
    p_acc_T1_axis, p_acc_T2_axis,
    p_rt_T1_axis,  p_rt_T2_axis,
    ncol = 2,
    top = grid::textGrob("Per-Axis Averages (collapsed over Valid/Neutral/Invalid): Accuracy (top) & RT (bottom)",
                         gp = grid::gpar(fontface = "bold", cex = 1.2))
  )
} else {
  # base grid fallback
  grid::grid.newpage()
  lay <- grid::grid.layout(nrow = 2, ncol = 2)
  grid::pushViewport(grid::viewport(layout = lay))
  grid::grid.text("Per-Axis Averages (collapsed over Valid/Neutral/Invalid): Accuracy (top) & RT (bottom)",
                  x = 0.5, y = 1, just = c("center","top"),
                  gp = grid::gpar(fontface = "bold", cex = 1.2), vjust = 2)
  print(p_acc_T1_axis, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_acc_T2_axis, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p_rt_T1_axis,  vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p_rt_T2_axis,  vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
}

# Optional saves:
# ggsave("bargrid_axis_avgs_acc_rt_T1T2.png", width = 14, height = 10, dpi = 300)


library(lme4)
library(car)

# Make sure log RT is present
data_e1$log_rxntime <- log(data_e1$rxntime)
data_e2$log_rxntime <- log(data_e2$rxntime)

datasets_axis <- list(
  e1 = data_e1,
  e2 = data_e2
)

models_axis <- list()
anova_axis <- list()

for (ds_name in names(datasets_axis)) {
  
  dat <- datasets_axis[[ds_name]]
  
  # ----- OVERALL (targets combined) -----
  # Accuracy GLMM
  models_axis[[paste0("acc_", ds_name, "_overall")]] <-
    glmer(accuracy ~ validity * axis * target + (1|universalID),
          data = dat, family = binomial)
  anova_axis[[paste0("acc_", ds_name, "_overall")]] <-
    Anova(models_axis[[paste0("acc_", ds_name, "_overall")]])
  
  # RT LMM
  models_axis[[paste0("rt_", ds_name, "_overall")]] <-
    lmer(log_rxntime ~ validity * axis * target + (1|universalID),
         data = dat)
  anova_axis[[paste0("rt_", ds_name, "_overall")]] <-
    Anova(models_axis[[paste0("rt_", ds_name, "_overall")]])
  
  
  # ----- TARGET 1 -----
  dat_t1 <- dat[dat$target == 1, ]
  
  # Accuracy GLMM
  models_axis[[paste0("acc_", ds_name, "_t1")]] <-
    glmer(accuracy ~ validity * axis + (1|universalID),
          data = dat_t1, family = binomial)
  anova_axis[[paste0("acc_", ds_name, "_t1")]] <-
    Anova(models_axis[[paste0("acc_", ds_name, "_t1")]])
  
  # RT LMM
  models_axis[[paste0("rt_", ds_name, "_t1")]] <-
    lmer(log_rxntime ~ validity * axis + (1|universalID),
         data = dat_t1)
  anova_axis[[paste0("rt_", ds_name, "_t1")]] <-
    Anova(models_axis[[paste0("rt_", ds_name, "_t1")]])
  
  
  # ----- TARGET 2 -----
  dat_t2 <- dat[dat$target == 2, ]
  
  # Accuracy GLMM
  models_axis[[paste0("acc_", ds_name, "_t2")]] <-
    glmer(accuracy ~ validity * axis + (1|universalID),
          data = dat_t2, family = binomial)
  anova_axis[[paste0("acc_", ds_name, "_t2")]] <-
    Anova(models_axis[[paste0("acc_", ds_name, "_t2")]])
  
  # RT LMM
  models_axis[[paste0("rt_", ds_name, "_t2")]] <-
    lmer(log_rxntime ~ validity * axis + (1|universalID),
         data = dat_t2)
  anova_axis[[paste0("rt_", ds_name, "_t2")]] <-
    Anova(models_axis[[paste0("rt_", ds_name, "_t2")]])
}

# View summaries
lapply(models_axis, summary)

# View ANOVA tables
lapply(anova_axis, print)

















### Testing combined data T2 no effect?

### Combined-data T2 equivalence tests on the **accuracy (probability) scale** ###

# Ensure model exists (Target 2, combined experiments)
# Robust getter for acc_both_t2
if (!exists("acc_both_t2") || !inherits(acc_both_t2, "merMod")) {
  acc_both_t2 <- glmer(
    accuracy ~ validity * expt + (1 | universalID),
    data   = subset(data_both, target == 2),
    family = binomial
  )
}


library(emmeans)

## ---------- BY-EXPERIMENT ----------
# 1) EMMs on response scale, then lock the grid to response scale for testing
emm_val_by_expt_t2 <- emmeans(acc_both_t2, ~ validity | expt, type = "response")
emm_val_by_expt_t2_resp <- regrid(emm_val_by_expt_t2, transform = "response")

# 2) Order-proof named contrasts (map your labels to factor names shown by 'levels(emm_val_by_expt_t2_resp)$validity')
#    If your levels are "1","2","3" (Valid, Invalid, Neutral), you can use these names:
validity_contrasts_named <- list(
  "V - I" = c("1" = 1, "2" = -1, "3" = 0),
  "V - N" = c("1" = 1, "2" =  0, "3" = -1),
  "N - I" = c("1" = 0, "2" = -1, "3" = 1)
)

con_val_by_expt_t2_resp <- contrast(
  emm_val_by_expt_t2_resp,
  method = validity_contrasts_named,
  by = "expt",
  adjust = "none"
)

# 3) TOST on the **accuracy** scale with ±3 percentage points
delta_pp <- 0.03   # change to your a priori bound
eq_by_expt <- summary(
  test(con_val_by_expt_t2_resp, side = "equivalence", delta = delta_pp),
  infer = c(TRUE, TRUE), level = 0.90
)
eq_by_expt


## ---------- OVERALL (collapse across experiment) ----------
# 1) Overall EMMs (proportional weighting), then lock to response scale
emm_val_overall_t2 <- emmeans(
  acc_both_t2, ~ validity,
  type = "response", weights = "proportional"
)
emm_val_overall_t2_resp <- regrid(emm_val_overall_t2, transform = "response")

# 2) Same named contrasts
con_val_overall_t2_resp <- contrast(
  emm_val_overall_t2_resp,
  method = validity_contrasts_named,
  adjust = "none"
)

# 3) TOST on accuracy scale
eq_overall <- summary(
  test(con_val_overall_t2_resp, side = "equivalence", delta = delta_pp),
  infer = c(TRUE, TRUE), level = 0.90
)
eq_overall








### Accidentally did on log-odds ratio

# # --- Ensure the model exists (rebuild if not found) ---
# if (!exists("acc_both_t2")) {
#   acc_both_t2 <- tryCatch(
#     models[["acc_both_t2"]],
#     error = function(e) {
#       glmer(accuracy ~ validity * expt + (1|universalID),
#             data = subset(data_both, target == 2),
#             family = binomial)
#     }
#   )
# } else {
#   acc_both_t2 <- models[["acc_both_t2"]]
# }
# 
# library(emmeans)
# 
# # Emmeans for validity within each experiment, on response (probability/accuracy) scale
# emm_val_by_expt_t2 <- emmeans(acc_both_t2, ~ validity | expt, type = "response")
# 
# # Validity contrasts (factor levels: 1=Valid, 2=Invalid, 3=Neutral)
# validity_contrasts <- list(
#   "V - I" = c( 1, -1,  0),
#   "V - N" = c( 1,  0, -1),
#   "N - I" = c( 0, -1,  1)
# )
# 
# # Contrasts per experiment (response-scale)
# con_val_by_expt_t2 <- contrast(emm_val_by_expt_t2,
#                                method = validity_contrasts,
#                                by = "expt",
#                                adjust = "none",
#                                type = "response")
# 
# # ---- Equivalence (TOST) in emmeans: ±3 percentage points on accuracy ----
# delta_pp <- 0.03   # CHANGE to your preferred bound
# 
# # 90% CI is standard for TOST
# eq_by_expt <- summary(
#   test(con_val_by_expt_t2, side = "equivalence", delta = delta_pp),
#   infer = c(TRUE, TRUE), level = 0.90
# )
# eq_by_expt   # prints estimates, 90% CI, and p-values for equivalence
# 
# # ---- Optional: overall validity effects at T2, collapsing across experiment ----
# emm_val_overall_t2 <- emmeans(acc_both_t2, ~ validity, type = "response", weights = "proportional")
# con_val_overall_t2 <- contrast(emm_val_overall_t2, method = validity_contrasts, adjust = "none")
# 
# eq_overall <- summary(
#   test(con_val_overall_t2, side = "equivalence", delta = delta_pp),
#   infer = c(TRUE, TRUE), level = 0.90
# )
# eq_overall



## ======= T1 & T2 validity equivalence on accuracy (response scale) =======

library(lme4)
library(emmeans)
library(dplyr)
library(tidyr)
library(purrr)

# --- Parameters ---
delta_pp <- 0.025   # your SESOI in accuracy points (change if needed)

# --- Ensure models exist (combined across experiments, target-specific) ---
get_or_fit_acc_model_for_target <- function(target_num) {
  name <- paste0("acc_both_t", target_num)
  if (exists(name) && inherits(get(name), "merMod")) {
    return(get(name))
  }
  # try from your 'models' list if present
  if (exists("models") && !is.null(models[[name]])) {
    return(models[[name]])
  }
  # else fit anew
  glmer(accuracy ~ validity * expt + (1|universalID),
        data = subset(data_both, target == target_num),
        family = binomial)
}

acc_both_t1 <- get_or_fit_acc_model_for_target(1)
acc_both_t2 <- get_or_fit_acc_model_for_target(2)

# Order-proof named contrasts (uses your validity coding: "1"=V, "2"=I, "3"=N)
validity_contrasts_named <- list(
  "V - I" = c("1" = 1, "2" = -1, "3" = 0),
  "V - N" = c("1" = 1, "2" =  0, "3" = -1),
  "N - I" = c("1" = 0, "2" = -1, "3" = 1)
)

# --- Helper to run emmeans+TOST for a given model ---
run_validity_equiv_for_model <- function(mod, target_label) {
  # by-experiment
  emm_byexp <- emmeans(mod, ~ validity | expt, type = "response")
  emm_byexp_resp <- regrid(emm_byexp, transform = "response")
  con_byexp <- contrast(emm_byexp_resp, method = validity_contrasts_named, by = "expt", adjust = "none")
  eq_byexp <- summary(test(con_byexp, side = "equivalence", delta = delta_pp),
                      infer = c(TRUE, TRUE), level = 0.90)
  
  # overall (collapse across expt)
  emm_overall <- emmeans(mod, ~ validity, type = "response", weights = "proportional")
  emm_overall_resp <- regrid(emm_overall, transform = "response")
  con_overall <- contrast(emm_overall_resp, method = validity_contrasts_named, adjust = "none")
  eq_overall <- summary(test(con_overall, side = "equivalence", delta = delta_pp),
                        infer = c(TRUE, TRUE), level = 0.90)
  
  # return a list + tidy frames for easy comparison
  list(
    eq_byexp = eq_byexp,
    eq_overall = eq_overall,
    emm_byexp = summary(emm_byexp_resp),
    emm_overall = summary(emm_overall_resp),
    tidy_byexp = eq_byexp |>
      as.data.frame() |>
      mutate(target = target_label),
    tidy_overall = eq_overall |>
      as.data.frame() |>
      mutate(target = target_label)
  )
}

# --- Run for T1 and T2 ---
res_t1 <- run_validity_equiv_for_model(acc_both_t1, "T1")
res_t2 <- run_validity_equiv_for_model(acc_both_t2, "T2")

# Print the same tables you saw for T2, now also for T1
cat("\n=== BY-EXPERIMENT (T1) ===\n"); print(res_t1$eq_byexp)
cat("\n=== OVERALL (T1) ===\n");      print(res_t1$eq_overall)

cat("\n=== BY-EXPERIMENT (T2) ===\n"); print(res_t2$eq_byexp)
cat("\n=== OVERALL (T2) ===\n");      print(res_t2$eq_overall)

# --- Side-by-side tidy comparison tables ---
byexp_compare <- bind_rows(res_t1$tidy_byexp, res_t2$tidy_byexp) |>
  relocate(target, expt, contrast)

overall_compare <- bind_rows(res_t1$tidy_overall, res_t2$tidy_overall) |>
  relocate(target, contrast)

cat("\n=== BY-EXPERIMENT COMPARISON (T1 vs T2) ===\n")
print(byexp_compare)

cat("\n=== OVERALL COMPARISON (T1 vs T2) ===\n")
print(overall_compare)

# OPTIONAL: quick glance at the underlying EMMs (accuracy means) by target
cat("\n=== EMMs by Experiment (T1) ===\n"); print(res_t1$emm_byexp)
cat("\n=== EMMs by Experiment (T2) ===\n"); print(res_t2$emm_byexp)




# Define which datasets to loop over
datasets <- list(
  "both" = data_both,
  "aud-e1" = data_e1,
  "aud-e2" = data_e2
)

# Loop and compute raw mean differences for T1
for (ds_name in names(datasets)) {
  dat <- datasets[[ds_name]]
  
  mean_valid   <- mean(dat$accuracy[dat$target == 1 & dat$validity == "1"], na.rm = TRUE)
  mean_invalid <- mean(dat$accuracy[dat$target == 1 & dat$validity == "2"], na.rm = TRUE)
  diff <- mean_valid - mean_invalid
  
  cat("\n", ds_name, ":\n", sep = "")
  cat("  T1 Valid mean:   ", round(mean_valid,   4), "\n", sep = "")
  cat("  T1 Invalid mean: ", round(mean_invalid, 4), "\n", sep = "")
  cat("  Difference (V - I): ", round(diff, 4), "\n", sep = "")
}





##### BAYESFACTOR PACKAGE
#------------------------------------------------------------
# CREATE NON-OVERLAPPING SUBJECT IDS FOR EXPERIMENT 2
#------------------------------------------------------------

data_both_g_ave_no_overlap <- data_both_g_ave

# Convert universalID to numeric so we can add 100
data_both_g_ave_no_overlap$universalID <- as.numeric(as.character(data_both_g_ave_no_overlap$universalID))

# Add 100 to expt 2 subjects
data_both_g_ave_no_overlap$universalID <- ifelse(
  data_both_g_ave_no_overlap$expt == "aud-e2",
  data_both_g_ave_no_overlap$universalID + 100,
  data_both_g_ave_no_overlap$universalID
)

# Convert back to factor for BayesFactor
data_both_g_ave_no_overlap$universalID <- factor(data_both_g_ave_no_overlap$universalID)

##### BAYESFACTOR PACKAGE
library(BayesFactor)

#------------------------------------------------------------
# SUBJECT-MEAN DATA ONLY
#------------------------------------------------------------

datasets_means <- list(
  both = data_both_g_ave,
  e1   = data_e1_g_ave,
  e2   = data_e2_g_ave,
  nooverlap = data_both_g_ave_no_overlap
)

# Ensure all predictors are factors (REQUIRED for anovaBF)
for (nm in names(datasets_means)) {
  datasets_means[[nm]]$universalID <- factor(datasets_means[[nm]]$universalID)
  datasets_means[[nm]]$target      <- factor(datasets_means[[nm]]$target)
  datasets_means[[nm]]$validity    <- factor(datasets_means[[nm]]$validity)
  datasets_means[[nm]]$expt        <- factor(datasets_means[[nm]]$expt)
}

targets  <- c("both", "t1", "t2")
outcomes <- c("accuracy")   # accuracy only
levels   <- c("mean")       # SUBJECT MEANS ONLY

#------------------------------------------------------------
# Helper: Filter dataset for T1/T2
#------------------------------------------------------------

get_data <- function(base_data, target_choice) {
  if (target_choice == "both") return(base_data)
  if (target_choice == "t1")   return(subset(base_data, target == 1))
  if (target_choice == "t2")   return(subset(base_data, target == 2))
}

#------------------------------------------------------------
# Helper: Build formula automatically
#------------------------------------------------------------

make_formula <- function(outcome, dataset_name, target_choice) {
  
  has_expt <- dataset_name %in% c("both", "nooverlap")
  
  if (target_choice == "both") {
    if (has_expt)
      f <- paste0(outcome, " ~ validity * target * expt + universalID")
    else
      f <- paste0(outcome, " ~ validity * target + universalID")
    
  } else {  # t1 or t2
    if (has_expt)
      f <- paste0(outcome, " ~ validity * expt + universalID")
    else
      f <- paste0(outcome, " ~ validity + universalID")
  }
  
  as.formula(f)
}

#------------------------------------------------------------
# RUN SUBJECT-MEAN MODELS WITH anovaBF()
#------------------------------------------------------------

results <- list()

for (d in names(datasets_means)) {
  for (t in targets) {
    for (o in outcomes) {
      
      name <- paste(o, d, t, "mean", sep = "_")
      
      dat <- get_data(datasets_means[[d]], t)
      fml <- make_formula(o, d, t)
      
      bf <- anovaBF(
        formula     = fml,
        data        = dat,
        whichRandom = "universalID",
        whichModels = "all",
        progress    = FALSE
      )
      
      results[[name]] <- sort(bf, decreasing = TRUE)
      
      cat("\n\n---", name, "---\n")
      print(results[[name]])
    }
  }
}







#----- Let's at least try lmBF idk if im doing it right though------

results_lm <- list()

for (d in names(datasets_means)) {
  for (t in targets) {
    for (o in outcomes) {
      
      name <- paste(o, d, t, "mean", sep = "_")
      
      dat <- get_data(datasets_means[[d]], t)
      fml <- make_formula(o, d, t)
      
      bf <- lmBF(
        formula     = fml,
        data        = dat,
        whichRandom = "universalID",
        progress    = FALSE
      )
      
      results_lm[[name]] <- sort(bf, decreasing = TRUE)
      
      cat("\n\n---", name, "---\n")
      print(results_lm[[name]])
    }
  }
}

accuracy_both_t1_mean_lm_h1 <- lmBF(accuracy ~ validity + expt + universalID,
                                 data = subset(data_both_g_ave, target == 1),
                                 whichRandom = "universalID",
                                 progress = TRUE)
accuracy_both_t2_mean_lm_h1 <- lmBF(accuracy ~ validity + expt + universalID,
                                 data = subset(data_both_g_ave, target == 2),
                                 whichRandom = "universalID",
                                 progress = TRUE)

accuracy_both_t1_mean_lm_h0 <- lmBF(accuracy ~ expt + universalID,
                                 data = subset(data_both_g_ave, target == 1),
                                 whichRandom = "universalID",
                                 progress = TRUE)
accuracy_both_t2_mean_lm_h0 <- lmBF(accuracy ~ expt + universalID,
                                 data = subset(data_both_g_ave, target == 2),
                                 whichRandom = "universalID",
                                 progress = TRUE)

accuracy_both_t1_mean_lm_hx <- lmBF(accuracy ~ validity * expt + universalID,
                                    data = subset(data_both_g_ave, target == 1),
                                    whichRandom = "universalID",
                                    progress = TRUE)
accuracy_both_t2_mean_lm_hx <- lmBF(accuracy ~ validity * expt + universalID,
                                    data = subset(data_both_g_ave, target == 2),
                                    whichRandom = "universalID",
                                    progress = TRUE)


#### New interaction test #$##
accuracy_both_mean_lm_hx_targ <- lmBF(accuracy ~ validity + expt + target + validity:expt + universalID,
                                    data = data_both_g_ave,
                                    whichRandom = "universalID",
                                    progress = TRUE)
accuracy_both_mean_lm_h0_targ <- lmBF(accuracy ~ validity + expt + target + universalID,
                                 data = data_both_g_ave,
                                 whichRandom = "universalID",
                                 progress = TRUE)


#### T1 Valid vs Invalid ####
accuracy_both_t1_mean_lm_h1_vi <- lmBF(accuracy ~ validity + expt + universalID,
                                    data = subset(data_both_g_ave, target == 1 & validity %in% c(1,3)),
                                    whichRandom = "universalID",
                                    progress = TRUE)

accuracy_both_t1_mean_lm_h0_vi <- lmBF(accuracy ~ expt + universalID,
                                    data = subset(data_both_g_ave, target == 1 & validity %in% c(1,3)),
                                    whichRandom = "universalID",
                                    progress = TRUE)

accuracy_both_t1_mean_lm_hx_vi <- lmBF(accuracy ~ validity * expt + universalID,
                                    data = subset(data_both_g_ave, target == 1 & validity %in% c(1,3)),
                                    whichRandom = "universalID",
                                    progress = TRUE)


data_both_g_ave$universalID <- factor(data_both_g_ave$universalID)
data_both_g_ave$target      <- factor(data_both_g_ave$target)
data_both_g_ave$validity    <- factor(data_both_g_ave$validity)
data_both_g_ave$expt        <- factor(data_both_g_ave$expt)
#### AOV ###

accuracy_both_t1_mean_aov_h1 <- anovaBF(accuracy ~ validity + expt + universalID,
                                    data = subset(data_both_g_ave, target == 1),
                                    whichRandom = "universalID",
                                    progress = TRUE)
accuracy_both_t2_mean_aov_h1 <- anovaBF(accuracy ~ validity + expt + universalID,
                                    data = subset(data_both_g_ave, target == 2),
                                    whichRandom = "universalID",
                                    progress = TRUE)

accuracy_both_t1_mean_aov_h0 <- anovaBF(accuracy ~ expt + universalID,
                                    data = subset(data_both_g_ave, target == 1),
                                    whichRandom = "universalID",
                                    progress = TRUE)
accuracy_both_t2_mean_aov_h0 <- anovaBF(accuracy ~ expt + universalID,
                                    data = subset(data_both_g_ave, target == 2),
                                    whichRandom = "universalID",
                                    progress = TRUE)

accuracy_both_t1_mean_aov_hx <- anovaBF(accuracy ~ validity * expt + universalID,
                                    data = subset(data_both_g_ave, target == 1),
                                    whichRandom = "universalID",
                                    progress = TRUE)
accuracy_both_t2_mean_aov_hx <- anovaBF(accuracy ~ validity * expt + universalID,
                                    data = subset(data_both_g_ave, target == 2),
                                    whichRandom = "universalID",
                                    progress = TRUE)


make_formula_reduced <- function(outcome, dataset_name, target) {
  
  # Build the full formula first
  full <- make_formula(outcome, dataset_name, target)
  
  # Expand the formula to explicit terms
  tt <- terms(full)
  term_labels <- attr(tt, "term.labels")
  
  # Keep only terms that DO NOT include "validity"
  kept_terms <- term_labels[!grepl("validity", term_labels)]
  
  # If everything was removed, fall back to intercept + random
  if (length(kept_terms) == 0) {
    rhs <- "1"
  } else {
    rhs <- paste(kept_terms, collapse = " + ")
  }
  
  # Rebuild formula
  reformulate(rhs, response = outcome)
}

library(BayesFactor)
library(dplyr)
library(tibble)
library(purrr)

results_lm_validity <- list()
summary_rows <- list()

for (d in names(datasets_means)) {
  for (t in targets) {
    for (o in outcomes) {
      
      name <- paste(o, d, t, "mean", sep = "_")
      
      # ---- DATA ----
      dat <- as.data.frame(get_data(datasets_means[[d]], t))
      
      # ---- FORMULAS ----
      fml_full    <- make_formula(o, d, t)
      fml_reduced <- make_formula_reduced(o, d, t)
      
      # ---- BF(full vs intercept) ----
      bf_full <- lmBF(
        formula = fml_full,
        data = dat,
        whichRandom = "universalID",
        progress = FALSE
      )
      
      # ---- BF(reduced vs intercept) ----
      bf_reduced <- lmBF(
        formula = fml_reduced,
        data = dat,
        whichRandom = "universalID",
        progress = FALSE
      )
      
      # ---- EXTRACT NUMERIC BF VALUES ----
      BF_full    <- extractBF(bf_full)$bf
      BF_reduced <- extractBF(bf_reduced)$bf
      
      # ---- TARGET EFFECT: FULL / REDUCED ----
      BF10 <- BF_full / BF_reduced
      BF01 <- 1 / BF10
      
      # ---- STORE ----
      results_lm_validity[[name]] <- list(
        formula_full = fml_full,
        formula_reduced = fml_reduced,
        full_BF = BF_full,
        reduced_BF = BF_reduced,
        BF10 = BF10,
        BF01 = BF01
      )
      
      summary_rows[[length(summary_rows) + 1]] <- tibble(
        dataset = d,
        target = t,
        outcome = o,
        BF10 = BF10,
        BF01 = BF01,
        formula_full = paste(deparse(fml_full), collapse = " "),
        formula_reduced = paste(deparse(fml_reduced), collapse = " ")
      )
      
      cat("\n---", name, "---\n")
      print(results_lm_validity[[name]])
    }
  }
}

# ---- FINAL SUMMARY TABLE ----
lmBF_validity_summary <- bind_rows(summary_rows)

cat("\n\n============================\n")
cat("SUMMARY TABLE WITH FORMULAS\n")
cat("============================\n\n")
print(lmBF_validity_summary, width = Inf)




###############################################################################
##### EXPERIMENT 3 + ALL 3 AUDITORY EXPERIMENTS ###############################
###############################################################################

##### IMPORT E3 #####

e3_filename <- "data_e3/analysis/RStudio_input_alls_expt3_ta-auditory_20260818.csv"

# Choose E3 RT measure:
# "rt" or "rtResponseAllowed"
e3_rt_variable <- "rtResponseAllowed"

data_e3 <- read.csv(e3_filename)

# completed trials only
data_e3 <- subset(data_e3, abortedTrial == 0 & !is.na(accuracy))

# match E1/E2 subject coding
data_e3$universalID <- factor(data_e3$subjectNumber)

# chosen RT becomes canonical rxntime
data_e3$rxntime <- data_e3[[e3_rt_variable]]

# queried target's axis
data_e3$axis <- ifelse(
  data_e3$target == 1,
  data_e3$targetAxisT1,
  data_e3$targetAxisT2
)

data_e3$expt = "aud-e3"
data_e3$axis <- factor(data_e3$axis)
data_e3$target <- factor(data_e3$target)
data_e3$validity <- factor(data_e3$validity)


##### SAME RT OUTLIER EXCLUSION AS E1/E2 #####

if (exclude == 1) {
  data_e3 <- data_e3 %>%
    group_by(universalID) %>%
    filter(abs(log(rxntime) - mean(log(rxntime), na.rm = TRUE)) <=
             6 * sd(log(rxntime), na.rm = TRUE)) %>%
    ungroup()
}


##### GROUP E3 THE SAME WAY AS E1/E2 #####

data_e3_grouped <- data_e3 %>%
  group_by(universalID, axis, target, validity, expt) %>%
  summarize(accuracy = mean(accuracy), rxntime = mean(rxntime))

data_e3_g_ave <- data_e3_grouped %>%
  group_by(universalID, target, validity, expt) %>%
  summarize(accuracy = mean(accuracy), rxntime = mean(rxntime))


##### ALL 3 DATASETS #####

# bind_rows because E3 contains extra columns
data_all3 <- bind_rows(data_e1, data_e2, data_e3)

data_all3_g_ave <- bind_rows(
  data_e1_g_ave,
  data_e2_g_ave,
  data_e3_g_ave
)

data_all3$universalID <- factor(data_all3$universalID)
data_all3$target <- factor(data_all3$target)
data_all3$validity <- factor(data_all3$validity)
data_all3$expt <- factor(data_all3$expt)

data_all3_g_ave$universalID <- factor(data_all3_g_ave$universalID)
data_all3_g_ave$target <- factor(data_all3_g_ave$target)
data_all3_g_ave$validity <- factor(data_all3_g_ave$validity)
data_all3_g_ave$expt <- factor(data_all3_g_ave$expt)


##### EXPORT SUBJECT MEANS #####

write.csv(data_e3_g_ave,
          "R_data_e3_subj_ave_means.csv",
          row.names = FALSE)

write.csv(data_all3_g_ave,
          "R_data_all3_subj_ave_means.csv",
          row.names = FALSE)


##### SIMPLE GROUP MEANS / SEMS #####

data_e3_g_ave_means <- data_e3_g_ave %>%
  group_by(target, validity) %>%
  summarize(
    mean_accuracy = mean(accuracy),
    sem_accuracy = sem(accuracy),
    mean_rxntime = mean(rxntime),
    sem_rxntime = sem(rxntime)
  )

data_all3_g_ave_means <- data_all3_g_ave %>%
  group_by(target, validity) %>%
  summarize(
    mean_accuracy = mean(accuracy),
    sem_accuracy = sem(accuracy),
    mean_rxntime = mean(rxntime),
    sem_rxntime = sem(rxntime)
  )

write.csv(data_e3_g_ave_means,
          "R_data_e3_g_ave_means.csv",
          row.names = FALSE)

write.csv(data_all3_g_ave_means,
          "R_data_all3_g_ave_means.csv",
          row.names = FALSE)



###############################################################################
##### E3 RT DISTRIBUTION: SAME FOUR DISTRIBUTIONS #############################
###############################################################################

# Reuse existing analyze_rt_distribution() exactly.

aic_bic_results_old <- aic_bic_results
best_fits_old <- best_fits
plots_list_old <- plots_list

aic_bic_results <- data.frame()
best_fits <- data.frame()
plots_list <- list()

e3_subjects <- unique(data_e3$universalID)

for (subject_id in e3_subjects) {
  
  data_subset <- subset(
    data_e3,
    universalID == subject_id
  )
  
  plots_list[[paste(subject_id, "aud-e3", sep = "_")]] <-
    analyze_rt_distribution(
      data_subset,
      subject_id,
      "aud-e3"
    )
}

aic_bic_results_e3 <- aic_bic_results
best_fits_e3 <- best_fits
plots_list_e3 <- plots_list

avg_aic_bic_e3 <- aic_bic_results_e3 %>%
  group_by(Distribution) %>%
  summarise(
    Average_AIC = mean(AIC),
    Average_BIC = mean(BIC)
  )

print(best_fits_e3)
print(avg_aic_bic_e3)

# inspect any subject:
example_pair_e3 <- names(plots_list_e3)[1]
print(plots_list_e3[[example_pair_e3]]$qq_plot)
print(plots_list_e3[[example_pair_e3]]$density_plot)

# restore original E1/E2 objects
aic_bic_results <- aic_bic_results_old
best_fits <- best_fits_old
plots_list <- plots_list_old

# aggregate e1, e2, e3
avg_aic_bic_e1_e2_e3 <- bind_rows(
  aic_bic_results,
  aic_bic_results_e3
) %>%
  group_by(expt, Distribution) %>%
  summarise(
    Average_AIC = mean(AIC),
    Average_BIC = mean(BIC),
    .groups = "drop"
  )

avg_aic_bic_e1_e2_e3

avg_aic_bic_e1_e2_e3 %>%
  arrange(Distribution, expt)

avg_aic_bic_all3 <- bind_rows(aic_bic_results, aic_bic_results_e3) %>%
  group_by(Distribution) %>%
  summarise(
    Average_AIC = mean(AIC),
    Average_BIC = mean(BIC),
    .groups = "drop"
  )

avg_aic_bic_all3



##### OVERALL E3 RT DISTRIBUTION #####

rt_data_e3 <- data_e3$rxntime

fit_gaussian_e3 <- fitdist(rt_data_e3, "norm")
fit_gamma_e3 <- fitdist(rt_data_e3, "gamma")
fit_invgauss_e3 <- fitdist(rt_data_e3, "invgauss")
fit_lognormal_e3 <- fitdist(rt_data_e3, "lnorm")

dist_comparison_e3 <- data.frame(
  Distribution = c(
    "Gaussian",
    "Gamma",
    "Inverse Gaussian",
    "Lognormal"
  ),
  AIC = c(
    AIC(fit_gaussian_e3),
    AIC(fit_gamma_e3),
    AIC(fit_invgauss_e3),
    AIC(fit_lognormal_e3)
  ),
  BIC = c(
    BIC(fit_gaussian_e3),
    BIC(fit_gamma_e3),
    BIC(fit_invgauss_e3),
    BIC(fit_lognormal_e3)
  )
)

print(dist_comparison_e3)

qq_e3 <- data.frame(
  sample_quantiles = sort(rt_data_e3),
  Gaussian = qnorm(
    ppoints(length(rt_data_e3)),
    mean = fit_gaussian_e3$estimate[1],
    sd = fit_gaussian_e3$estimate[2]
  ),
  Gamma = qgamma(
    ppoints(length(rt_data_e3)),
    shape = fit_gamma_e3$estimate[1],
    rate = fit_gamma_e3$estimate[2]
  ),
  Inverse_Gaussian = qinvgauss(
    ppoints(length(rt_data_e3)),
    mean = fit_invgauss_e3$estimate[1],
    shape = fit_invgauss_e3$estimate[2]
  ),
  Lognormal = qlnorm(
    ppoints(length(rt_data_e3)),
    meanlog = fit_lognormal_e3$estimate[1],
    sdlog = fit_lognormal_e3$estimate[2]
  )
) %>%
  pivot_longer(
    cols = -sample_quantiles,
    names_to = "Distribution",
    values_to = "Theoretical_Quantiles"
  )

ggplot(
  qq_e3,
  aes(
    x = Theoretical_Quantiles,
    y = sample_quantiles,
    color = Distribution
  )
) +
  geom_point(alpha = 0.6) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  theme_minimal() +
  labs(
    title = paste(
      "E3 RT Q-Q Plot —",
      e3_rt_variable
    ),
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

ggplot(
  data.frame(rt_data_e3),
  aes(x = rt_data_e3)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 50,
    fill = "gray",
    alpha = 0.5
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = fit_gaussian_e3$estimate[1],
      sd = fit_gaussian_e3$estimate[2]
    ),
    color = "blue",
    linetype = "dashed"
  ) +
  stat_function(
    fun = dgamma,
    args = list(
      shape = fit_gamma_e3$estimate[1],
      rate = fit_gamma_e3$estimate[2]
    ),
    color = "red"
  ) +
  stat_function(
    fun = dinvgauss,
    args = list(
      mean = fit_invgauss_e3$estimate[1],
      shape = fit_invgauss_e3$estimate[2]
    ),
    color = "green"
  ) +
  stat_function(
    fun = dlnorm,
    args = list(
      meanlog = fit_lognormal_e3$estimate[1],
      sdlog = fit_lognormal_e3$estimate[2]
    ),
    color = "purple"
  ) +
  theme_minimal() +
  labs(
    title = paste(
      "E3 RT Distribution Fit —",
      e3_rt_variable
    ),
    x = "Reaction Time",
    y = "Density",
    subtitle =
      "Blue = Gaussian, Red = Gamma, Green = Inverse Gaussian, Purple = Lognormal"
  )



###############################################################################
##### LOG RT ##################################################################
###############################################################################

data_e3$log_rxntime <- log(data_e3$rxntime)
data_all3$log_rxntime <- log(data_all3$rxntime)



###############################################################################
##### E3 + ALL3 BGLMMs AND LOGNORMAL LMMS #####################################
###############################################################################

datasets_e3 <- list(
  "all3" = data_all3,
  "e3" = data_e3
)

models_e3 <- list()
anova_results_e3 <- list()

for (ds_name in names(datasets_e3)) {
  
  data <- datasets_e3[[ds_name]]
  
  if (ds_name == "all3") {
    
    models_e3[[paste0("acc_", ds_name)]] <-
      glmer(
        accuracy ~ validity * target * expt + (1|universalID),
        data = data,
        family = binomial
      )
    
    models_e3[[paste0("rxn_", ds_name)]] <-
      lmer(
        log_rxntime ~ validity * target * expt + (1|universalID),
        data = data
      )
    
  } else {
    
    models_e3[[paste0("acc_", ds_name)]] <-
      glmer(
        accuracy ~ validity * target + (1|universalID),
        data = data,
        family = binomial
      )
    
    models_e3[[paste0("rxn_", ds_name)]] <-
      lmer(
        log_rxntime ~ validity * target + (1|universalID),
        data = data
      )
  }
  
  anova_results_e3[[paste0("aov_acc_", ds_name)]] <-
    Anova(models_e3[[paste0("acc_", ds_name)]])
  
  anova_results_e3[[paste0("aov_rxn_", ds_name)]] <-
    Anova(models_e3[[paste0("rxn_", ds_name)]])
  
  for (target in 1:2) {
    
    target_data <- data[data$target == target, ]
    
    model_name_acc <- paste0(
      "acc_",
      ds_name,
      "_t",
      target
    )
    
    model_name_rxn <- paste0(
      "rxn_",
      ds_name,
      "_t",
      target
    )
    
    formula_acc <- ifelse(
      ds_name == "all3",
      "accuracy ~ validity * expt + (1|universalID)",
      "accuracy ~ validity + (1|universalID)"
    )
    
    formula_rxn <- ifelse(
      ds_name == "all3",
      "log_rxntime ~ validity * expt + (1|universalID)",
      "log_rxntime ~ validity + (1|universalID)"
    )
    
    models_e3[[model_name_acc]] <-
      glmer(
        as.formula(formula_acc),
        data = target_data,
        family = binomial
      )
    
    models_e3[[model_name_rxn]] <-
      lmer(
        as.formula(formula_rxn),
        data = target_data
      )
    
    anova_results_e3[[paste0("aov_", model_name_acc)]] <-
      Anova(models_e3[[model_name_acc]])
    
    anova_results_e3[[paste0("aov_", model_name_rxn)]] <-
      Anova(models_e3[[model_name_rxn]])
  }
}

lapply(models_e3, summary)
lapply(anova_results_e3, print)



###############################################################################
##### PAIRWISE COMPARISONS ####################################################
###############################################################################

# reuse existing validity_pairs:
#
# vi = c(1,2)
# vn = c(1,3)
# ni = c(2,3)

models_e3_paired <- list()
anova_results_e3_paired <- list()

for (ds_name in names(datasets_e3)) {
  
  data <- datasets_e3[[ds_name]]
  
  for (target in 1:2) {
    
    for (pair_name in names(validity_pairs)) {
      
      pair_values <- validity_pairs[[pair_name]]
      
      target_data <- data[
        data$target == target &
          data$validity %in% pair_values,
      ]
      
      model_name_acc <- paste0(
        "acc_",
        ds_name,
        "_t",
        target,
        "_",
        pair_name
      )
      
      model_name_rxn <- paste0(
        "rxn_",
        ds_name,
        "_t",
        target,
        "_",
        pair_name
      )
      
      formula_acc <- ifelse(
        ds_name == "all3",
        "accuracy ~ validity * expt + (1|universalID)",
        "accuracy ~ validity + (1|universalID)"
      )
      
      formula_rxn <- ifelse(
        ds_name == "all3",
        "log_rxntime ~ validity * expt + (1|universalID)",
        "log_rxntime ~ validity + (1|universalID)"
      )
      
      models_e3_paired[[model_name_acc]] <-
        glmer(
          as.formula(formula_acc),
          data = target_data,
          family = binomial
        )
      
      models_e3_paired[[model_name_rxn]] <-
        lmer(
          as.formula(formula_rxn),
          data = target_data
        )
      
      anova_results_e3_paired[[paste0("aov_", model_name_acc)]] <-
        Anova(models_e3_paired[[model_name_acc]])
      
      anova_results_e3_paired[[paste0("aov_", model_name_rxn)]] <-
        Anova(models_e3_paired[[model_name_rxn]])
    }
  }
}

lapply(models_e3_paired, summary)
lapply(anova_results_e3_paired, print)



###############################################################################
##### GAMMA(LOG) RT MODELS ####################################################
###############################################################################

gamma_models_e3 <- list()
gamma_anovas_e3 <- list()

for (ds_name in names(datasets_e3)) {
  
  data <- datasets_e3[[ds_name]]
  
  formula_rxn <- if (
    ds_name == "all3"
  ) {
    "rxntime ~ validity * target * expt + (1|universalID)"
  } else {
    "rxntime ~ validity * target + (1|universalID)"
  }
  
  gamma_models_e3[[paste0("rxn_gamma_", ds_name)]] <-
    glmer(
      as.formula(formula_rxn),
      data = data,
      family = Gamma(link = "log")
    )
  
  gamma_anovas_e3[[paste0("aov_rxn_gamma_", ds_name)]] <-
    Anova(gamma_models_e3[[paste0("rxn_gamma_", ds_name)]])
  
  for (target in 1:2) {
    
    target_data <- data[data$target == target, ]
    
    formula_rxn_t <- if (
      ds_name == "all3"
    ) {
      "rxntime ~ validity * expt + (1|universalID)"
    } else {
      "rxntime ~ validity + (1|universalID)"
    }
    
    model_name <- paste0(
      "rxn_gamma_",
      ds_name,
      "_t",
      target
    )
    
    gamma_models_e3[[model_name]] <-
      glmer(
        as.formula(formula_rxn_t),
        data = target_data,
        family = Gamma(link = "log")
      )
    
    gamma_anovas_e3[[paste0("aov_", model_name)]] <-
      Anova(gamma_models_e3[[model_name]])
  }
}

lapply(gamma_models_e3, summary)
lapply(gamma_anovas_e3, print)



##### PAIRWISE GAMMA MODELS #####

gamma_models_e3_paired <- list()
gamma_anovas_e3_paired <- list()

for (ds_name in names(datasets_e3)) {
  
  data <- datasets_e3[[ds_name]]
  
  for (target in 1:2) {
    
    for (pair_name in names(validity_pairs)) {
      
      pair_values <- validity_pairs[[pair_name]]
      
      target_data <- data[
        data$target == target &
          data$validity %in% pair_values,
      ]
      
      model_name <- paste0(
        "rxn_gamma_",
        ds_name,
        "_t",
        target,
        "_",
        pair_name
      )
      
      formula_rxn <- ifelse(
        ds_name == "all3",
        "rxntime ~ validity * expt + (1|universalID)",
        "rxntime ~ validity + (1|universalID)"
      )
      
      gamma_models_e3_paired[[model_name]] <-
        glmer(
          as.formula(formula_rxn),
          data = target_data,
          family = Gamma(link = "log")
        )
      
      gamma_anovas_e3_paired[[paste0("aov_", model_name)]] <-
        Anova(gamma_models_e3_paired[[model_name]])
    }
  }
}

lapply(gamma_models_e3_paired, summary)
lapply(gamma_anovas_e3_paired, print)



##### AIC/BIC LMM VS GAMMA #####

aic_bic_comparison_e3 <- data.frame()

lmm_names <- names(models_e3)[
  grepl("^rxn_", names(models_e3))
]

for (lmm_name in lmm_names) {
  
  gamma_name <- sub(
    "^rxn_",
    "rxn_gamma_",
    lmm_name
  )
  
  if (gamma_name %in% names(gamma_models_e3)) {
    
    aic_bic_comparison_e3 <- rbind(
      aic_bic_comparison_e3,
      data.frame(
        Model = lmm_name,
        AIC_LMM = AIC(models_e3[[lmm_name]]),
        BIC_LMM = BIC(models_e3[[lmm_name]]),
        AIC_Gamma = AIC(gamma_models_e3[[gamma_name]]),
        BIC_Gamma = BIC(gamma_models_e3[[gamma_name]])
      )
    )
  }
}

print(aic_bic_comparison_e3)

aic_bic_long_e3 <- pivot_longer(
  aic_bic_comparison_e3,
  cols = c(
    AIC_LMM,
    AIC_Gamma,
    BIC_LMM,
    BIC_Gamma
  ),
  names_to = c(
    "Metric",
    "ModelType"
  ),
  names_sep = "_",
  values_to = "Value"
)

ggplot(
  aic_bic_long_e3,
  aes(
    x = Model,
    y = Value,
    fill = ModelType
  )
) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  facet_wrap(
    ~ Metric,
    scales = "free_y"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 8
    )
  ) +
  labs(
    title = "AIC/BIC: LMM (log RT) vs Gamma GLMM — E3 / All3",
    x = "Model",
    y = "Score",
    fill = "Model Type"
  )



###############################################################################
##### SUBJECT-MEAN VI / VN / NI ###############################################
###############################################################################

VI_VN_NI_all3 <- data_all3_g_ave %>%
  group_by(
    universalID,
    expt,
    target
  ) %>%
  summarise(
    VI_acc =
      first(accuracy[validity == 1]) -
      first(accuracy[validity == 2]),
    
    VN_acc =
      first(accuracy[validity == 1]) -
      first(accuracy[validity == 3]),
    
    NI_acc =
      first(accuracy[validity == 3]) -
      first(accuracy[validity == 2]),
    
    VI_rtn =
      first(rxntime[validity == 1]) -
      first(rxntime[validity == 2]),
    
    VN_rtn =
      first(rxntime[validity == 1]) -
      first(rxntime[validity == 3]),
    
    NI_rtn =
      first(rxntime[validity == 3]) -
      first(rxntime[validity == 2]),
    
    .groups = "drop"
  )

VI_VN_NI_e3 <- subset(
  VI_VN_NI_all3,
  expt == "aud-e3"
)

write.csv(
  VI_VN_NI_e3,
  "data_e3_VI_VN_NI_accrt_bySubjTarget.csv",
  row.names = FALSE
)

write.csv(
  VI_VN_NI_all3,
  "data_all3_VI_VN_NI_accrt_bySubjExptTarget.csv",
  row.names = FALSE
)



###############################################################################
##### VN vs NI PLOTS — ALL 3 ##################################################
###############################################################################

VN_NI_diffs_all3 <- VI_VN_NI_all3 %>%
  mutate(
    target = factor(target),
    grp = paste(
      expt,
      paste0("T", target),
      sep = "_"
    )
  )

col_map_all3 <- c(
  "aud-e1_T1" = "#fca5a5",
  "aud-e1_T2" = "#dc2626",
  "aud-e2_T1" = "#fdba74",
  "aud-e2_T2" = "#ea580c",
  "aud-e3_T1" = "#86efac",
  "aud-e3_T2" = "#16a34a"
)

means_acc_all3 <- VN_NI_diffs_all3 %>%
  group_by(expt, target, grp) %>%
  summarise(
    NI_acc = mean(NI_acc),
    VN_acc = mean(VN_acc),
    .groups = "drop"
  )

means_rtn_all3 <- VN_NI_diffs_all3 %>%
  group_by(expt, target, grp) %>%
  summarise(
    NI_rtn = mean(NI_rtn),
    VN_rtn = mean(VN_rtn),
    .groups = "drop"
  )


p_acc_all3 <- ggplot(
  VN_NI_diffs_all3,
  aes(
    x = NI_acc,
    y = VN_acc,
    color = grp,
    shape = expt
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  geom_point(
    size = 2.8,
    alpha = 0.95
  ) +
  geom_point(
    data = means_acc_all3,
    size = 6
  ) +
  scale_color_manual(
    values = col_map_all3
  ) +
  scale_shape_manual(
    values = c(16,17,15)
  ) +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Accuracy: V-N (y) vs N-I (x)",
    x = "N - I (accuracy)",
    y = "V - N (accuracy)"
  )


p_rt_all3 <- ggplot(
  VN_NI_diffs_all3,
  aes(
    x = NI_rtn,
    y = VN_rtn,
    color = grp,
    shape = expt
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  geom_point(
    size = 2.8,
    alpha = 0.95
  ) +
  geom_point(
    data = means_rtn_all3,
    size = 6
  ) +
  scale_color_manual(
    values = col_map_all3
  ) +
  scale_shape_manual(
    values = c(16,17,15)
  ) +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Reaction Time: V-N (y) vs N-I (x)",
    x = "N - I (RT)",
    y = "V - N (RT)"
  )

print(p_acc_all3)
print(p_rt_all3)



###############################################################################
##### AXIS PLOTS — E1 + E2 + E3 ###############################################
###############################################################################

# Same structure as existing E1/E2 plot.

e1_subj <- data_e1_grouped %>%
  dplyr::select(universalID, expt, axis, target, validity, accuracy, rxntime) %>%
  dplyr::mutate(expt = "aud-e1")

e2_subj <- data_e2_grouped %>%
  dplyr::select(universalID, expt, axis, target, validity, accuracy, rxntime) %>%
  dplyr::mutate(expt = "aud-e2")

e3_subj <- data_e3_grouped %>%
  dplyr::select(universalID, expt, axis, target, validity, accuracy, rxntime) %>%
  dplyr::mutate(expt = "aud-e3")


subj_long_all3 <- bind_rows(
  e1_subj,
  e2_subj,
  e3_subj
) %>%
  mutate(
    validity_lab = dplyr::recode(
      as.character(validity),
      "1" = "Valid",
      "2" = "Invalid",
      "3" = "Neutral"
    ),
    
    axis_num =
      as.integer(
        as.character(axis)
      ),
    
    axis_label = case_when(
      expt == "aud-e1" ~
        paste0("E1-A", axis_num),
      
      expt == "aud-e2" ~
        paste0("E2-A", axis_num),
      
      expt == "aud-e3" ~
        paste0("E3-A", axis_num)
    ),
    
    validity_lab = factor(
      validity_lab,
      levels = c(
        "Valid",
        "Neutral",
        "Invalid"
      )
    )
  )


make_barplot_all3 <- function(
    df,
    dv = c("accuracy","rxntime"),
    targ = 1
) {
  
  dv <- match.arg(dv)
  
  y_lab <- if (
    dv == "accuracy"
  ) {
    "Mean accuracy"
  } else {
    "Mean RT"
  }
  
  ttl <- paste0(
    ifelse(
      dv == "accuracy",
      "Accuracy",
      "Reaction Time"
    ),
    " by Validity across Axes — Target ",
    targ
  )
  
  axis_summary <- df %>%
    filter(target == targ) %>%
    group_by(
      expt,
      axis_label,
      validity_lab
    ) %>%
    summarise(
      mean_y = mean(.data[[dv]], na.rm = TRUE),
      sem_y = sem(.data[[dv]]),
      .groups = "drop"
    )
  
  axis_levels <- c(
    sort(unique(axis_summary$axis_label[
      axis_summary$expt == "aud-e1"
    ])),
    sort(unique(axis_summary$axis_label[
      axis_summary$expt == "aud-e2"
    ])),
    sort(unique(axis_summary$axis_label[
      axis_summary$expt == "aud-e3"
    ]))
  )
  
  axis_summary$axis_label <- factor(
    axis_summary$axis_label,
    levels = axis_levels
  )
  
  validity_cols <- c(
    "Valid" = "#1b9e77",
    "Neutral" = "#9ca3af",
    "Invalid" = "#d95f02"
  )
  
  pd <- position_dodge(width = 0.8)
  
  ggplot(
    axis_summary,
    aes(
      x = axis_label,
      y = mean_y,
      fill = validity_lab
    )
  ) +
    geom_col(
      position = pd,
      width = 0.7
    ) +
    geom_errorbar(
      aes(
        ymin = mean_y - sem_y,
        ymax = mean_y + sem_y
      ),
      position = pd,
      width = 0.25
    ) +
    scale_fill_manual(
      values = validity_cols
    ) +
    labs(
      title = ttl,
      x = "Axis",
      y = y_lab
    ) +
    theme_minimal() +
    theme(
      axis.text.x =
        element_text(
          angle = 45,
          hjust = 1
        )
    )
}


p_acc_T1_all3_axis <- make_barplot_all3(
  subj_long_all3,
  "accuracy",
  1
)

p_acc_T2_all3_axis <- make_barplot_all3(
  subj_long_all3,
  "accuracy",
  2
)

p_rt_T1_all3_axis <- make_barplot_all3(
  subj_long_all3,
  "rxntime",
  1
)

p_rt_T2_all3_axis <- make_barplot_all3(
  subj_long_all3,
  "rxntime",
  2
)

if (requireNamespace("gridExtra", quietly = TRUE)) {
  gridExtra::grid.arrange(
    p_acc_T1_all3_axis,
    p_acc_T2_all3_axis,
    p_rt_T1_all3_axis,
    p_rt_T2_all3_axis,
    ncol = 2
  )
}



###############################################################################
##### EMMEANS / EQUIVALENCE — ALL 3 ###########################################
###############################################################################

# Same target-specific models used above

acc_all3_t1 <- models_e3[["acc_all3_t1"]]
acc_all3_t2 <- models_e3[["acc_all3_t2"]]

# reuse existing:
# validity_contrasts_named
# run_validity_equiv_for_model()
# delta_pp already = .025 in canonical generalized block

res_all3_t1 <- run_validity_equiv_for_model(
  acc_all3_t1,
  "T1"
)

res_all3_t2 <- run_validity_equiv_for_model(
  acc_all3_t2,
  "T2"
)

cat("\n=== BY-EXPERIMENT (T1, ALL 3) ===\n")
print(res_all3_t1$eq_byexp)

cat("\n=== OVERALL (T1, ALL 3) ===\n")
print(res_all3_t1$eq_overall)

cat("\n=== BY-EXPERIMENT (T2, ALL 3) ===\n")
print(res_all3_t2$eq_byexp)

cat("\n=== OVERALL (T2, ALL 3) ===\n")
print(res_all3_t2$eq_overall)

byexp_compare_all3 <- bind_rows(
  res_all3_t1$tidy_byexp,
  res_all3_t2$tidy_byexp
) %>%
  relocate(
    target,
    expt,
    contrast
  )

overall_compare_all3 <- bind_rows(
  res_all3_t1$tidy_overall,
  res_all3_t2$tidy_overall
) %>%
  relocate(
    target,
    contrast
  )

print(byexp_compare_all3)
print(overall_compare_all3)



###############################################################################
##### BAYESFACTOR — E3 + ALL3 #################################################
###############################################################################

library(BayesFactor)

# Same no-overlap idea as existing script
data_all3_g_ave_no_overlap <- data_all3_g_ave

data_all3_g_ave_no_overlap$universalID <- factor(
  paste(
    data_all3_g_ave_no_overlap$expt,
    data_all3_g_ave_no_overlap$universalID,
    sep = "_"
  )
)

datasets_means_e3 <- list(
  all3 = data_all3_g_ave,
  e3 = data_e3_g_ave,
  nooverlap = data_all3_g_ave_no_overlap
)

for (nm in names(datasets_means_e3)) {
  datasets_means_e3[[nm]]$universalID <-
    factor(datasets_means_e3[[nm]]$universalID)
  
  datasets_means_e3[[nm]]$target <-
    factor(datasets_means_e3[[nm]]$target)
  
  datasets_means_e3[[nm]]$validity <-
    factor(datasets_means_e3[[nm]]$validity)
  
  datasets_means_e3[[nm]]$expt <-
    factor(datasets_means_e3[[nm]]$expt)
}


make_formula_e3 <- function(
    outcome,
    dataset_name,
    target_choice
) {
  
  has_expt <- dataset_name %in%
    c("all3","nooverlap")
  
  if (target_choice == "both") {
    
    if (has_expt) {
      f <- paste0(
        outcome,
        " ~ validity * target * expt + universalID"
      )
    } else {
      f <- paste0(
        outcome,
        " ~ validity * target + universalID"
      )
    }
    
  } else {
    
    if (has_expt) {
      f <- paste0(
        outcome,
        " ~ validity * expt + universalID"
      )
    } else {
      f <- paste0(
        outcome,
        " ~ validity + universalID"
      )
    }
  }
  
  as.formula(f)
}


get_data_e3 <- function(
    base_data,
    target_choice
) {
  
  if (target_choice == "both") {
    return(base_data)
  }
  
  if (target_choice == "t1") {
    return(
      subset(
        base_data,
        target == 1
      )
    )
  }
  
  if (target_choice == "t2") {
    return(
      subset(
        base_data,
        target == 2
      )
    )
  }
}


targets_e3 <- c(
  "both",
  "t1",
  "t2"
)

outcomes_e3 <- c(
  "accuracy"
)


##### anovaBF #####

results_bf_e3 <- list()

for (d in names(datasets_means_e3)) {
  
  for (t in targets_e3) {
    
    for (o in outcomes_e3) {
      
      name <- paste(
        o,
        d,
        t,
        "mean",
        sep = "_"
      )
      
      dat <- get_data_e3(
        datasets_means_e3[[d]],
        t
      )
      
      fml <- make_formula_e3(
        o,
        d,
        t
      )
      
      bf <- anovaBF(
        formula = fml,
        data = dat,
        whichRandom = "universalID",
        whichModels = "all",
        progress = FALSE
      )
      
      results_bf_e3[[name]] <-
        sort(
          bf,
          decreasing = TRUE
        )
      
      cat(
        "\n\n---",
        name,
        "---\n"
      )
      
      print(
        results_bf_e3[[name]]
      )
    }
  }
}



##### lmBF #####

results_lm_e3 <- list()

for (d in names(datasets_means_e3)) {
  
  for (t in targets_e3) {
    
    for (o in outcomes_e3) {
      
      name <- paste(
        o,
        d,
        t,
        "mean",
        sep = "_"
      )
      
      dat <- get_data_e3(
        datasets_means_e3[[d]],
        t
      )
      
      fml <- make_formula_e3(
        o,
        d,
        t
      )
      
      bf <- lmBF(
        formula = fml,
        data = dat,
        whichRandom = "universalID",
        progress = FALSE
      )
      
      results_lm_e3[[name]] <-
        sort(
          bf,
          decreasing = TRUE
        )
      
      cat(
        "\n\n---",
        name,
        "---\n"
      )
      
      print(
        results_lm_e3[[name]]
      )
    }
  }
}



##### VALIDITY FULL VS REDUCED #####

make_formula_reduced_e3 <- function(
    outcome,
    dataset_name,
    target
) {
  
  full <- make_formula_e3(
    outcome,
    dataset_name,
    target
  )
  
  tt <- terms(full)
  term_labels <- attr(
    tt,
    "term.labels"
  )
  
  kept_terms <- term_labels[
    !grepl(
      "validity",
      term_labels
    )
  ]
  
  rhs <- if (
    length(kept_terms) == 0
  ) {
    "1"
  } else {
    paste(
      kept_terms,
      collapse = " + "
    )
  }
  
  reformulate(
    rhs,
    response = outcome
  )
}


results_lm_validity_e3 <- list()
summary_rows_e3 <- list()

for (d in names(datasets_means_e3)) {
  
  for (t in targets_e3) {
    
    for (o in outcomes_e3) {
      
      name <- paste(
        o,
        d,
        t,
        "mean",
        sep = "_"
      )
      
      dat <- as.data.frame(
        get_data_e3(
          datasets_means_e3[[d]],
          t
        )
      )
      
      fml_full <- make_formula_e3(
        o,
        d,
        t
      )
      
      fml_reduced <-
        make_formula_reduced_e3(
          o,
          d,
          t
        )
      
      bf_full <- lmBF(
        formula = fml_full,
        data = dat,
        whichRandom = "universalID",
        progress = FALSE
      )
      
      bf_reduced <- lmBF(
        formula = fml_reduced,
        data = dat,
        whichRandom = "universalID",
        progress = FALSE
      )
      
      BF_full <- extractBF(bf_full)$bf
      BF_reduced <- extractBF(bf_reduced)$bf
      
      BF10 <- BF_full / BF_reduced
      BF01 <- 1 / BF10
      
      results_lm_validity_e3[[name]] <- list(
        formula_full = fml_full,
        formula_reduced = fml_reduced,
        full_BF = BF_full,
        reduced_BF = BF_reduced,
        BF10 = BF10,
        BF01 = BF01
      )
      
      summary_rows_e3[[
        length(summary_rows_e3) + 1
      ]] <- tibble(
        dataset = d,
        target = t,
        outcome = o,
        BF10 = BF10,
        BF01 = BF01,
        formula_full =
          paste(
            deparse(fml_full),
            collapse = " "
          ),
        formula_reduced =
          paste(
            deparse(fml_reduced),
            collapse = " "
          )
      )
      
      cat(
        "\n---",
        name,
        "---\n"
      )
      
      print(
        results_lm_validity_e3[[name]]
      )
    }
  }
}

lmBF_validity_summary_e3 <-
  bind_rows(summary_rows_e3)

print(
  lmBF_validity_summary_e3,
  width = Inf
)


## Test expt:validity interaction
accuracy_all3_mean_lm_hx_targ <- lmBF(
  accuracy ~ validity + expt + target + validity:expt + universalID,
  data = data_all3_g_ave,
  whichRandom = "universalID",
  progress = TRUE
)

accuracy_all3_mean_lm_h0_targ <- lmBF(
  accuracy ~ validity + expt + target + universalID,
  data = data_all3_g_ave,
  whichRandom = "universalID",
  progress = TRUE
)

# BF10 for validity × experiment interaction
accuracy_all3_mean_lm_hx_targ / accuracy_all3_mean_lm_h0_targ

# BF01
accuracy_all3_mean_lm_h0_targ / accuracy_all3_mean_lm_hx_targ

## Test the interaction with the validity:target that we now have too
# H1: includes validity × experiment
bf_vxexp_h1 <- lmBF(
  accuracy ~ validity + target + expt +
    validity:target +
    validity:expt +
    universalID,
  data = data_all3_g_ave,
  whichRandom = "universalID",
  progress = TRUE
)

# H0: identical, except no validity × experiment
bf_vxexp_h0 <- lmBF(
  accuracy ~ validity + target + expt +
    validity:target +
    universalID,
  data = data_all3_g_ave,
  whichRandom = "universalID",
  progress = TRUE
)

# evidence for validity × experiment
bf_vxexp_h1 / bf_vxexp_h0

# evidence against validity × experiment
bf_vxexp_h0 / bf_vxexp_h1

## T1 valid vs neutral
#### ALL3 T1 Valid vs Neutral ####

accuracy_all3_t1_mean_lm_h1_vn <- lmBF(
  accuracy ~ validity + expt + universalID,
  data = subset(data_all3_g_ave, target == 1 & validity %in% c(1,3)),
  whichRandom = "universalID",
  progress = TRUE
)

accuracy_all3_t1_mean_lm_h0_vn <- lmBF(
  accuracy ~ expt + universalID,
  data = subset(data_all3_g_ave, target == 1 & validity %in% c(1,3)),
  whichRandom = "universalID",
  progress = TRUE
)

# BF10: evidence for a Valid vs Neutral difference
accuracy_all3_t1_mean_lm_h1_vn / accuracy_all3_t1_mean_lm_h0_vn

# BF01: evidence for no Valid vs Neutral difference
accuracy_all3_t1_mean_lm_h0_vn / accuracy_all3_t1_mean_lm_h1_vn

###############################################################################
##### DONE ####################################################################
###############################################################################

cat("\n========================================\n")
cat("E3 / ALL3 ANALYSES COMPLETE\n")
cat("E3 RT variable:", e3_rt_variable, "\n")
cat("========================================\n")

##### E3 / ALL3 EXTENSION: CROSS-EXPERIMENT CONDITION AVERAGES + VI DIFFERENCE SCORES #####


################################################################################
##### 1. CONDITION-AVERAGE COMPARISONS
##### E3 vs Visual, E3 vs E1, E3 vs E2, E1/E2/E3/Visual, All3 vs Visual
################################################################################

# Make sure all 3 auditory experiments are together
data_all3_g_ave <- dplyr::bind_rows(
  data_e1_g_ave,
  data_e2_g_ave,
  data_e3_g_ave
)

# For the pooled All3 vs Visual comparison, average across auditory experiments
# within subject × target × validity so overlapping E1/E2 subjects count once.
data_all3_pooled_g_ave <- data_all3_g_ave %>%
  dplyr::group_by(universalID, target, validity) %>%
  dplyr::summarise(
    accuracy = mean(accuracy, na.rm = TRUE),
    rxntime  = mean(rxntime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(expt = "aud-all3")

# Combine individual auditory experiments, pooled All3, and Visual
dataAV3_condition_avgs <- dplyr::bind_rows(
  dplyr::select(data_all3_g_ave,
                universalID, expt, target, validity, accuracy, rxntime),
  dplyr::select(data_all3_pooled_g_ave,
                universalID, expt, target, validity, accuracy, rxntime),
  dplyr::select(datavis_g_ave,
                universalID, expt, target, validity, accuracy, rxntime)
)

# Same validity subsets as original analysis
validity_pairs_e3 <- list(
  vi  = c("1", "2"),
  vn  = c("1", "3"),
  ni  = c("2", "3"),
  all = c("1", "2", "3")
)

dvs_e3 <- c("rxntime", "accuracy")
targets_e3 <- c("all", "1", "2")

# New experiment comparisons
expt_sets_e3 <- list(
  a3v    = c("aud-e3", "visual"),
  a13    = c("aud-e1", "aud-e3"),
  a23    = c("aud-e2", "aud-e3"),
  a123v  = c("aud-e1", "aud-e2", "aud-e3", "visual"),
  all3v  = c("aud-all3", "visual")
)

all_condavg_models_e3 <- list()

# Same model loop as original
for (dv in dvs_e3) {
  for (targ in targets_e3) {
    for (exp_name in names(expt_sets_e3)) {
      for (val_name in names(validity_pairs_e3)) {
        
        df <- dataAV3_condition_avgs %>%
          dplyr::filter(
            expt %in% expt_sets_e3[[exp_name]],
            validity %in% validity_pairs_e3[[val_name]]
          ) %>%
          { if (targ != "all") dplyr::filter(., target == targ) else . }
        
        if (nrow(df) < 10) next
        
        formula <- if (targ == "all" && val_name == "all") {
          as.formula(paste0(
            dv, " ~ expt * target * validity + (1|universalID)"
          ))
        } else if (val_name == "all") {
          as.formula(paste0(
            dv, " ~ expt * validity + (1|universalID)"
          ))
        } else if (targ == "all") {
          as.formula(paste0(
            dv, " ~ expt * target + (1|universalID)"
          ))
        } else {
          as.formula(paste0(
            dv, " ~ expt + (1|universalID)"
          ))
        }
        
        model <- tryCatch({
          
          fit <- lme4::lmer(formula, data = df)
          
          if (lme4::isSingular(fit, tol = 1e-4)) {
            message(
              "Singular fit for: ",
              dv, " / ", exp_name, " / ", val_name, " / t", targ
            )
          }
          
          car::Anova(fit)
          
        }, error = function(e) {
          
          message(
            "Model error for: ",
            dv, " / ", exp_name, " / ", val_name, " / t", targ
          )
          
          return(NULL)
        })
        
        model_name <- paste(
          dv,
          exp_name,
          val_name,
          if (targ != "all") paste0("t", targ) else "all",
          sep = "_"
        )
        
        all_condavg_models_e3[[model_name]] <- model
      }
    }
  }
}

# View all new condition-average ANOVA tables
all_condavg_models_e3



################################################################################
##### 2. V-I DIFFERENCE-SCORE COMPARISONS
##### E3 vs Visual, E3 vs E1, E3 vs E2, E1/E2/E3/Visual, All3 vs Visual
################################################################################

# Individual auditory experiment V-I difference scores
dataaud_all3_diff <- data_all3_g_ave %>%
  dplyr::filter(validity %in% c(1, 2)) %>%
  dplyr::group_by(universalID, target, expt) %>%
  dplyr::summarise(
    accuracy_diff = accuracy[validity == 1] - accuracy[validity == 2],
    rxntime_diff  = rxntime[validity == 1]  - rxntime[validity == 2],
    .groups = "drop"
  )

# Visual V-I difference scores
datavis_diff_e3 <- datavis_g_ave %>%
  dplyr::filter(validity %in% c(1, 2)) %>%
  dplyr::group_by(universalID, target, expt) %>%
  dplyr::summarise(
    accuracy_diff = accuracy[validity == 1] - accuracy[validity == 2],
    rxntime_diff  = rxntime[validity == 1]  - rxntime[validity == 2],
    .groups = "drop"
  )

# Pooled All3 auditory difference scores:
# average E1/E2/E3 difference scores within subject × target
data_all3_pooled_diff <- dataaud_all3_diff %>%
  dplyr::group_by(universalID, target) %>%
  dplyr::summarise(
    accuracy_diff = mean(accuracy_diff, na.rm = TRUE),
    rxntime_diff  = mean(rxntime_diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(expt = "aud-all3")

# Combine everything
dataAV3_diff <- dplyr::bind_rows(
  dataaud_all3_diff,
  data_all3_pooled_diff,
  datavis_diff_e3
)


# Same structure as original fit_diff_lmms_by_target()
fit_diff_lmms_e3 <- function(dataAV) {
  
  comparisons <- list(
    "aud_e3_vs_visual" = c("aud-e3", "visual"),
    "aud_e1_vs_e3" = c("aud-e1", "aud-e3"),
    "aud_e2_vs_e3" = c("aud-e2", "aud-e3"),
    "aud_e1_vs_e2_vs_e3_vs_visual" =
      c("aud-e1", "aud-e2", "aud-e3", "visual"),
    "all3_vs_visual" = c("aud-all3", "visual")
  )
  
  models <- list()
  anovas <- list()
  
  for (target_label in c("all", "1", "2")) {
    
    if (target_label == "all") {
      target_data <- dataAV
    } else {
      target_data <- dplyr::filter(dataAV, target == target_label)
    }
    
    for (label in names(comparisons)) {
      
      expt_levels <- comparisons[[label]]
      
      data_subset <- dplyr::filter(
        target_data,
        expt %in% expt_levels
      )
      
      data_subset$expt <- factor(
        data_subset$expt,
        levels = expt_levels
      )
      
      n_obs <- nrow(data_subset)
      n_ids <- length(unique(data_subset$universalID))
      
      suffix <- ifelse(
        target_label == "all",
        "",
        paste0("_t", target_label)
      )
      
      
      # Reaction time
      if (n_obs > n_ids) {
        
        model_rxn <- lme4::lmer(
          rxntime_diff ~ expt + (1|universalID),
          data = data_subset
        )
        
      } else {
        
        model_rxn <- lm(
          rxntime_diff ~ expt,
          data = data_subset
        )
      }
      
      models[[paste0("rxn_", label, suffix)]] <- model_rxn
      anovas[[paste0("rxn_", label, suffix)]] <- car::Anova(model_rxn)
      
      
      # Accuracy
      if (n_obs > n_ids) {
        
        model_acc <- lme4::lmer(
          accuracy_diff ~ expt + (1|universalID),
          data = data_subset
        )
        
      } else {
        
        model_acc <- lm(
          accuracy_diff ~ expt,
          data = data_subset
        )
      }
      
      models[[paste0("acc_", label, suffix)]] <- model_acc
      anovas[[paste0("acc_", label, suffix)]] <- car::Anova(model_acc)
    }
  }
  
  return(list(
    models = models,
    anovas = anovas
  ))
}


diff_lmm_results_e3 <- fit_diff_lmms_e3(dataAV3_diff)

# Model summaries
lapply(diff_lmm_results_e3$models, summary)

# ANOVA tables
lapply(diff_lmm_results_e3$anovas, print)


# Same clean summary format as original
anova_summary_e3 <- extract_anova_summary(
  diff_lmm_results_e3$anovas
)

anova_summary_e3

##### EXTENDED ANOVA SUMMARY: E3 / ALL3 DIFFERENCE-SCORE MODELS #####

anova_summary_extended_e3 <- extract_anova_extended_summary(
  diff_lmm_results_e3$models,
  diff_lmm_results_e3$anovas
)

print(anova_summary_extended_e3)


##### SUBJECT-AVERAGE RT HISTOGRAMS BY VALIDITY: E1 / E2 / E3 #####

library(dplyr)
library(ggplot2)

# -------------------------------------------------------------------------
# Collapse to exactly ONE observation per subject x validity x experiment
# -------------------------------------------------------------------------

rt_hist_data <- bind_rows(
  data_e1_g_ave,
  data_e2_g_ave,
  data_e3_g_ave
) %>%
  group_by(universalID, validity, expt) %>%
  summarize(
    rxntime = mean(rxntime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    validity = factor(
      validity,
      levels = c(1, 3, 2),
      labels = c("Valid", "Neutral", "Invalid")
    )
  )


# -------------------------------------------------------------------------
# Plotting function
# -------------------------------------------------------------------------

plot_rt_hist <- function(dat, expt_name, plot_title) {
  
  ggplot(
    filter(dat, expt == expt_name),
    aes(x = rxntime, fill = validity)
  ) +
    geom_histogram(
      binwidth = 0.10,
      position = "identity",
      alpha = 0.50,
      color = "black"
    ) +
    labs(
      title = plot_title,
      x = "Subject-average reaction time (s)",
      y = "Count",
      fill = "Validity"
    ) +
    theme_classic(base_size = 18) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
}


# -------------------------------------------------------------------------
# THREE SEPARATE FIGURES
# -------------------------------------------------------------------------

p_rt_e1 <- plot_rt_hist(
  rt_hist_data,
  "aud-e1",
  "Experiment 1"
)

p_rt_e2 <- plot_rt_hist(
  rt_hist_data,
  "aud-e2",
  "Experiment 2"
)

p_rt_e3 <- plot_rt_hist(
  rt_hist_data,
  "aud-e3",
  "Experiment 3"
)


# Display them
print(p_rt_e1)
print(p_rt_e2)
print(p_rt_e3)

##### SUBJECT-AVERAGE RT HISTOGRAM GRID: VALIDITY x EXPERIMENT #####

library(dplyr)
library(ggplot2)

# -------------------------------------------------------------------------
# One observation per subject x validity x experiment
# -------------------------------------------------------------------------

rt_hist_data <- bind_rows(
  data_e1_g_ave,
  data_e2_g_ave,
  data_e3_g_ave
) %>%
  group_by(universalID, validity, expt) %>%
  summarize(
    rxntime = mean(rxntime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    validity = factor(
      validity,
      levels = c(1, 3, 2),
      labels = c("Valid", "Neutral", "Invalid")
    ),
    expt = factor(
      expt,
      levels = c("aud-e1", "aud-e2", "aud-e3"),
      labels = c("Exp 1", "Exp 2", "Exp 3")
    )
  )


# -------------------------------------------------------------------------
# 3 columns = experiments
# 3 rows    = validity
# -------------------------------------------------------------------------

p_rt_grid <- ggplot(
  rt_hist_data,
  aes(x = rxntime)
) +
  geom_histogram(
    binwidth = 0.10,
    boundary = 0,
    color = "black",
    fill = "gray70"
  ) +
  facet_grid(
    validity ~ expt
  ) +
  labs(
    x = "Subject-average reaction time (s)",
    y = "Count"
  ) +
  theme_classic(base_size = 18) +
  theme(
    strip.text = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18)
  )

print(p_rt_grid)



##### E1 MIDDLE FREQUENCY (1386 Hz) VS ALL E2: TRIALWISE PERFORMANCE #####

library(dplyr)
library(lme4)
library(car)

# -------------------------------------------------------------------------
# Keep only the easiest / middle E1 central frequency (1386 Hz)
# Keep ALL E2 central frequencies
# -------------------------------------------------------------------------

data_e1_1386 <- data_e1 %>%
  filter(as.numeric(as.character(axis)) %in% c(2,3))

data_e1_1386_vs_e2 <- bind_rows(
  data_e1_1386,
  data_e2
)

# Make E1 the reference experiment
data_e1_1386_vs_e2$expt <- factor(
  data_e1_1386_vs_e2$expt,
  levels = c("aud-e1", "aud-e2")
)

data_e1_1386_vs_e2$validity <- factor(data_e1_1386_vs_e2$validity)
data_e1_1386_vs_e2$target   <- factor(data_e1_1386_vs_e2$target)


# =========================================================================
# ACCURACY: BINOMIAL GLMM
# =========================================================================

model_acc_e1_1386_vs_e2 <- glmer(
  accuracy ~ expt + validity * target + (1 | universalID),
  data = data_e1_1386_vs_e2,
  family = binomial
)

summary(model_acc_e1_1386_vs_e2)

Anova(
  model_acc_e1_1386_vs_e2,
  type = 2
)


# =========================================================================
# REACTION TIME: LOG-RT LMM
# =========================================================================

data_e1_1386_vs_e2$log_rxntime <- log(data_e1_1386_vs_e2$rxntime)

model_rt_e1_1386_vs_e2 <- lmer(
  log_rxntime ~ expt + validity * target + (1 | universalID),
  data = data_e1_1386_vs_e2
)

summary(model_rt_e1_1386_vs_e2)

Anova(
  model_rt_e1_1386_vs_e2,
  type = 2
)

##### OVERALL ACCURACY BY AXIS: E1 / E2 / E3 #####

library(dplyr)
library(ggplot2)

acc_by_axis <- bind_rows(
  data_e1,
  data_e2,
  data_e3
) %>%
  mutate(
    axis_num = as.numeric(as.character(axis))
  ) %>%
  group_by(universalID, expt, axis_num) %>%
  summarize(
    subj_accuracy = mean(accuracy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(expt, axis_num) %>%
  summarize(
    mean_accuracy = mean(subj_accuracy, na.rm = TRUE),
    sem_accuracy = sd(subj_accuracy, na.rm = TRUE) /
      sqrt(sum(!is.na(subj_accuracy))),
    .groups = "drop"
  ) %>%
  mutate(
    axis_label = paste0(
      case_when(
        expt == "aud-e1" ~ "E1",
        expt == "aud-e2" ~ "E2",
        expt == "aud-e3" ~ "E3"
      ),
      "-A", axis_num
    )
  )


ggplot(
  acc_by_axis,
  aes(x = axis_label, y = mean_accuracy, fill = expt)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_accuracy - sem_accuracy,
      ymax = mean_accuracy + sem_accuracy
    ),
    width = 0.20,
    linewidth = 0.6
  ) +
  labs(
    x = "Central frequency / axis",
    y = "Overall accuracy",
    fill = "Experiment"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




##### SUBJECT RT RANGE / MEAN / MEDIAN — E1, E2, E3 #####

library(dplyr)
library(tidyr)
library(ggplot2)
#library(patchwork)


# -------------------------------------------------------------------------
# Combine trialwise data
# -------------------------------------------------------------------------

rt_subject_summary <- bind_rows(
  data_e1,
  data_e2,
  data_e3
) %>%
  group_by(universalID, expt) %>%
  summarize(
    rt_range  = max(rxntime, na.rm = TRUE) - min(rxntime, na.rm = TRUE),
    rt_mean   = mean(rxntime, na.rm = TRUE),
    rt_median = median(rxntime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    expt = factor(
      expt,
      levels = c("aud-e1", "aud-e2", "aud-e3"),
      labels = c("Exp 1", "Exp 2", "Exp 3")
    )
  )


# -------------------------------------------------------------------------
# 1. SUBJECT RT RANGES
# -------------------------------------------------------------------------

p_rt_range <- ggplot(
  rt_subject_summary,
  aes(x = expt, y = rt_range)
) +
  geom_boxplot(
    width = 0.60,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.10,
    size = 2.5,
    alpha = 0.75
  ) +
  labs(
    title = "Subject RT ranges",
    x = NULL,
    y = "RT range (s)"
  ) +
  theme_classic(base_size = 18)


# -------------------------------------------------------------------------
# 2. SUBJECT MEAN RTs
# -------------------------------------------------------------------------

p_rt_mean <- ggplot(
  rt_subject_summary,
  aes(x = expt, y = rt_mean)
) +
  geom_boxplot(
    width = 0.60,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.10,
    size = 2.5,
    alpha = 0.75
  ) +
  labs(
    title = "Subject mean RTs",
    x = NULL,
    y = "Mean RT (s)"
  ) +
  theme_classic(base_size = 18)


# -------------------------------------------------------------------------
# 3. SUBJECT MEDIAN RTs
# -------------------------------------------------------------------------

p_rt_median <- ggplot(
  rt_subject_summary,
  aes(x = expt, y = rt_median)
) +
  geom_boxplot(
    width = 0.60,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.10,
    size = 2.5,
    alpha = 0.75
  ) +
  labs(
    title = "Subject median RTs",
    x = NULL,
    y = "Median RT (s)"
  ) +
  theme_classic(base_size = 18)


##### COMBINE INTO ONE SHAREABLE FIGURE #####

library(gridExtra)
library(grid)

gridExtra::grid.arrange(
  p_rt_range,
  p_rt_mean,
  p_rt_median,
  ncol = 3,
  top = grid::textGrob(
    "Reaction-time distributions across subjects",
    gp = grid::gpar(
      fontface = "bold",
      fontsize = 20
    )
  )
)

##### SUBJECT RT RANGE / MEAN / MEDIAN — ALL 3 EXPERIMENTS COMBINED #####

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)


# -------------------------------------------------------------------------
# Combine all trialwise data, then summarize ONCE per subject
# across E1 + E2 + E3
# -------------------------------------------------------------------------

rt_subject_summary_all <- bind_rows(
  data_e1,
  data_e2,
  data_e3
) %>%
  group_by(universalID) %>%
  summarize(
    rt_range  = max(rxntime, na.rm = TRUE) - min(rxntime, na.rm = TRUE),
    rt_mean   = mean(rxntime, na.rm = TRUE),
    rt_median = median(rxntime, na.rm = TRUE),
    .groups = "drop"
  )


# -------------------------------------------------------------------------
# 1. SUBJECT RT RANGES
# -------------------------------------------------------------------------

p_rt_range_all <- ggplot(
  rt_subject_summary_all,
  aes(x = "", y = rt_range)
) +
  geom_boxplot(
    width = 0.45,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.08,
    size = 2.5,
    alpha = 0.75
  ) +
  labs(
    title = "Subject RT ranges",
    x = NULL,
    y = "RT range (s)"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# -------------------------------------------------------------------------
# 2. SUBJECT MEAN RTs
# -------------------------------------------------------------------------

p_rt_mean_all <- ggplot(
  rt_subject_summary_all,
  aes(x = "", y = rt_mean)
) +
  geom_boxplot(
    width = 0.45,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.08,
    size = 2.5,
    alpha = 0.75
  ) +
  labs(
    title = "Subject mean RTs",
    x = NULL,
    y = "Mean RT (s)"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# -------------------------------------------------------------------------
# 3. SUBJECT MEDIAN RTs
# -------------------------------------------------------------------------

p_rt_median_all <- ggplot(
  rt_subject_summary_all,
  aes(x = "", y = rt_median)
) +
  geom_boxplot(
    width = 0.45,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.08,
    size = 2.5,
    alpha = 0.75
  ) +
  labs(
    title = "Subject median RTs",
    x = NULL,
    y = "Median RT (s)"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# -------------------------------------------------------------------------
# COMBINE INTO ONE FIGURE
# -------------------------------------------------------------------------

gridExtra::grid.arrange(
  p_rt_range_all,
  p_rt_mean_all,
  p_rt_median_all,
  ncol = 3,
  top = grid::textGrob(
    "Reaction-time distributions across subjects — E1–E3 combined",
    gp = grid::gpar(
      fontface = "bold",
      fontsize = 20
    )
  )
)

##### =====================================================================
##### ROBUSTNESS CHECK: LMMs ON SUBJECT-CONDITION AVERAGES
##### E1 + E2 + E3
#####
##### One row per subject × experiment × target × validity
##### Accuracy: subject-condition mean accuracy
##### RT:       log(subject-condition mean RT)
##### =====================================================================

library(dplyr)
library(lme4)
library(car)


# -------------------------------------------------------------------------
# Combine subject-condition averages from all 3 auditory experiments
# -------------------------------------------------------------------------

data_all_subjave <- bind_rows(
  data_e1_g_ave,
  data_e2_g_ave,
  data_e3_g_ave
) %>%
  mutate(
    universalID = factor(universalID),
    
    expt = factor(
      expt,
      levels = c("aud-e1", "aud-e2", "aud-e3")
    ),
    
    target   = factor(target),
    validity = factor(validity),
    
    log_rxntime = log(rxntime)
  )


##### =====================================================================
##### COMBINED E1 + E2 + E3
##### =====================================================================


# -------------------------------------------------------------------------
# ACCURACY
# -------------------------------------------------------------------------

model_acc_subjave_all <- lmer(
  accuracy ~ expt * validity * target +
    (1 | universalID),
  data = data_all_subjave
)

summary(model_acc_subjave_all)

car::Anova(
  model_acc_subjave_all,
  type = 2
)

# -------------------------------------------------------------------------
# ACCURACY — T1 ONLY
# -------------------------------------------------------------------------

model_acc_subjave_t1 <- lmer(
  accuracy ~ expt * validity +
    (1 | universalID),
  data = data_all_subjave %>%
    dplyr::filter(target == 1)
)

summary(model_acc_subjave_t1)

car::Anova(
  model_acc_subjave_t1,
  type = 2
)


# -------------------------------------------------------------------------
# ACCURACY — T2 ONLY
# -------------------------------------------------------------------------

model_acc_subjave_t2 <- lmer(
  accuracy ~ expt * validity +
    (1 | universalID),
  data = data_all_subjave %>%
    dplyr::filter(target == 2)
)

summary(model_acc_subjave_t2)

car::Anova(
  model_acc_subjave_t2,
  type = 2
)

# -------------------------------------------------------------------------
# ACCURACY — VALID VS INVALID ONLY
# -------------------------------------------------------------------------

model_acc_subjave_vi <- lmer(
  accuracy ~ expt * validity * target +
    (1 | universalID),
  data = data_all_subjave %>%
    dplyr::filter(validity %in% c(1, 2))
)

summary(model_acc_subjave_vi)

car::Anova(
  model_acc_subjave_vi,
  type = 2
)

# -------------------------------------------------------------------------
# ACCURACY — T1, VALID VS INVALID ONLY
# -------------------------------------------------------------------------

model_acc_subjave_t1_vi <- lmer(
  accuracy ~ expt * validity +
    (1 | universalID),
  data = data_all_subjave %>%
    dplyr::filter(
      target == 1,
      validity %in% c(1, 2)
    )
)

summary(model_acc_subjave_t1_vi)

car::Anova(
  model_acc_subjave_t1_vi,
  type = 2
)


# -------------------------------------------------------------------------
# ACCURACY — T2, VALID VS INVALID ONLY
# -------------------------------------------------------------------------

model_acc_subjave_t2_vi <- lmer(
  accuracy ~ expt * validity +
    (1 | universalID),
  data = data_all_subjave %>%
    dplyr::filter(
      target == 2,
      validity %in% c(1, 2)
    )
)

summary(model_acc_subjave_t2_vi)

car::Anova(
  model_acc_subjave_t2_vi,
  type = 2
)

# -------------------------------------------------------------------------
# REACTION TIME
# -------------------------------------------------------------------------

model_rt_subjave_all <- lmer(
  log_rxntime ~ expt * validity * target +
    (1 | universalID),
  data = data_all_subjave
)

summary(model_rt_subjave_all)

car::Anova(
  model_rt_subjave_all,
  type = 2
)



##### =====================================================================
##### INDIVIDUAL EXPERIMENTS
##### =====================================================================


##### ---------------------------------------------------------------------
##### EXPERIMENT 1
##### ---------------------------------------------------------------------

data_e1_subjave <- data_all_subjave %>%
  filter(expt == "aud-e1") %>%
  droplevels()


model_acc_subjave_e1 <- lmer(
  accuracy ~ validity * target +
    (1 | universalID),
  data = data_e1_subjave
)

summary(model_acc_subjave_e1)
car::Anova(model_acc_subjave_e1, type = 2)


model_rt_subjave_e1 <- lmer(
  log_rxntime ~ validity * target +
    (1 | universalID),
  data = data_e1_subjave
)

summary(model_rt_subjave_e1)
car::Anova(model_rt_subjave_e1, type = 2)



##### ---------------------------------------------------------------------
##### EXPERIMENT 2
##### ---------------------------------------------------------------------

data_e2_subjave <- data_all_subjave %>%
  filter(expt == "aud-e2") %>%
  droplevels()


model_acc_subjave_e2 <- lmer(
  accuracy ~ validity * target +
    (1 | universalID),
  data = data_e2_subjave
)

summary(model_acc_subjave_e2)
car::Anova(model_acc_subjave_e2, type = 2)


model_rt_subjave_e2 <- lmer(
  log_rxntime ~ validity * target +
    (1 | universalID),
  data = data_e2_subjave
)

summary(model_rt_subjave_e2)
car::Anova(model_rt_subjave_e2, type = 2)



##### ---------------------------------------------------------------------
##### EXPERIMENT 3
##### ---------------------------------------------------------------------

data_e3_subjave <- data_all_subjave %>%
  filter(expt == "aud-e3") %>%
  droplevels()


model_acc_subjave_e3 <- lmer(
  accuracy ~ validity * target +
    (1 | universalID),
  data = data_e3_subjave
)

summary(model_acc_subjave_e3)
car::Anova(model_acc_subjave_e3, type = 2)


model_rt_subjave_e3 <- lmer(
  log_rxntime ~ validity * target +
    (1 | universalID),
  data = data_e3_subjave
)

summary(model_rt_subjave_e3)
car::Anova(model_rt_subjave_e3, type = 2)

##### =====================================================================
##### COMBINED E1 + E2 + E3 SUBJECT-AVERAGE LMM
##### INCLUDING EXPERIMENT
##### =====================================================================

# ACCURACY
model_acc_subjave_combined_expt <- lmer(
  accuracy ~ expt * validity * target +
    (1 | universalID),
  data = data_all_subjave
)

summary(model_acc_subjave_combined_expt)

car::Anova(
  model_acc_subjave_combined_expt,
  type = 2
)


# REACTION TIME
model_rt_subjave_combined_expt <- lmer(
  log_rxntime ~ expt * validity * target +
    (1 | universalID),
  data = data_all_subjave
)

summary(model_rt_subjave_combined_expt)

car::Anova(
  model_rt_subjave_combined_expt,
  type = 2
)




##### =====================================================================
##### CONDITION-AVERAGE MODELS: ALL AUDITORY E1+E2+E3 POOLED AS "aud"
##### VERSUS VISUAL AS "vis"
##### =====================================================================

dvs_av <- c("rxntime", "accuracy")
targets_av <- c("all", "1", "2")

# Only pooled Auditory vs Visual
expt_sets_av <- list(
  av = c("aud", "vis")
)

all_condavg_models_av <- list()


# -------------------------------------------------------------------------
# Recode E1/E2/E3 -> aud, visual -> vis
# -------------------------------------------------------------------------

dataAV3_condition_avgs_av <- dataAV3_condition_avgs %>%
  dplyr::mutate(
    expt_av = dplyr::case_when(
      expt %in% c("aud-e1", "aud-e2", "aud-e3") ~ "aud",
      expt == "visual" ~ "vis",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(expt_av)) %>%
  dplyr::mutate(
    expt_av = factor(
      expt_av,
      levels = c("aud", "vis")
    )
  )


# -------------------------------------------------------------------------
# Same model loop as original
# -------------------------------------------------------------------------

for (dv in dvs_av) {
  for (targ in targets_av) {
    for (exp_name in names(expt_sets_av)) {
      for (val_name in names(validity_pairs_e3)) {
        
        df <- dataAV3_condition_avgs_av %>%
          dplyr::filter(
            expt_av %in% expt_sets_av[[exp_name]],
            validity %in% validity_pairs_e3[[val_name]]
          ) %>%
          { if (targ != "all") dplyr::filter(., target == targ) else . }
        
        if (nrow(df) < 10) next
        
        
        formula <- if (targ == "all" && val_name == "all") {
          
          as.formula(paste0(
            dv, " ~ expt_av * target * validity + (1|universalID)"
          ))
          
        } else if (val_name == "all") {
          
          as.formula(paste0(
            dv, " ~ expt_av * validity + (1|universalID)"
          ))
          
        } else if (targ == "all") {
          
          as.formula(paste0(
            dv, " ~ expt_av * target + (1|universalID)"
          ))
          
        } else {
          
          as.formula(paste0(
            dv, " ~ expt_av + (1|universalID)"
          ))
          
        }
        
        
        model <- tryCatch({
          
          fit <- lme4::lmer(
            formula,
            data = df
          )
          
          if (lme4::isSingular(fit, tol = 1e-4)) {
            message(
              "Singular fit for: ",
              dv, " / ", exp_name, " / ", val_name, " / t", targ
            )
          }
          
          car::Anova(fit)
          
        }, error = function(e) {
          
          message(
            "Model error for: ",
            dv, " / ", exp_name, " / ", val_name, " / t", targ
          )
          
          return(NULL)
        })
        
        
        model_name <- paste(
          dv,
          exp_name,
          val_name,
          if (targ != "all") paste0("t", targ) else "all",
          sep = "_"
        )
        
        
        all_condavg_models_av[[model_name]] <- model
        
      }
    }
  }
}


# -------------------------------------------------------------------------
# View all pooled Auditory-vs-Visual condition-average ANOVA tables
# -------------------------------------------------------------------------

all_condavg_models_av