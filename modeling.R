#load packages
library(pastecs)
library(psych)
library(dplyr)
library(VGAM)
library(AER)
library( plm )
library(censReg)
library(stargazer)
library(ggplot2)
library(corrplot)
library(heatmaply)

getwd()

setwd("~/courses/SocialImpact")

#Load data files

#outtrjo is data from the trust game
outtrjo <- read.csv("outtrjo.csv")

#destroy is data from the contest game
destroy <- read.csv("destroy.csv")

memchar <- read.csv("memchar.csv")

#surv_ques is data from survey questions
surv_ques <- read.csv("surv_ques.csv")

outtrjo <- subset(outtrjo, outtrjo$SESSION < 99 & outtrjo$TRT > 1)

# extra modeling by Kang
# Check treatment group coding
table(outtrjo$TRT)

# From the code:
# TRT = 2: Unbiased Monitor
# TRT = 3: Biased Monitor  
# TRT = 4: No Monitor
# TRT = 1: Old baseline (already excluded)

# Create new continuous variable based on your mapping
outtrjo$monitor_coef <- NA
outtrjo$monitor_coef[outtrjo$TRT == 2] <- 0    # Unbiased
outtrjo$monitor_coef[outtrjo$TRT == 3] <- 0.5  # Biased
outtrjo$monitor_coef[outtrjo$TRT == 4] <- 1    # No monitor

# For trustor model (MOVE1, trust level)
trust_data <- subset(outtrjo, select = c("MOVE1", "monitor_coef", "PERIOD", "P1A", "ID1"))

# For trustee model (return percentage)
# First get return percentage data from outtrjo2
outtrjo2 <- subset(outtrjo, MOVE1 > 0)  # Only consider cases with positive sending
outtrjo2$MOVE1x3 <- outtrjo2$MOVE1 * 3
outtrjo2$P2_100 <- round(outtrjo2$MOVE2 / outtrjo2$MOVE1x3 * 100, digits = 0)

trustee_data <- subset(outtrjo2, select = c("P2_100", "monitor_coef", "PERIOD", "P2A", "ID2", "MOVE1"))

# Model 1a: Trustor model (MOVE1 ~ monitor coefficient)
lm_trust_simple <- lm(MOVE1 ~ monitor_coef, data = trust_data)
summary(lm_trust_simple)

# Model 1b: Trustee model (return percentage ~ monitor coefficient)
lm_trustee_simple <- lm(P2_100 ~ monitor_coef, data = trustee_data)
summary(lm_trustee_simple)

# Model 2a: Trustor model, adding period and beliefs
lm_trust_full <- lm(MOVE1 ~ monitor_coef + PERIOD + P1A, data = trust_data)
summary(lm_trust_full)

# Model 2b: Trustee model, adding period, beliefs and amount received
lm_trustee_full <- lm(P2_100 ~ monitor_coef + PERIOD + P2A + MOVE1, data = trustee_data)
summary(lm_trustee_full)

# Create prediction data frame (covering [-1, 1] range)
pred_range <- data.frame(monitor_coef = seq(-1, 1, by = 0.1))

# Use models for prediction
# 1. Simple model predictions
pred_trust_simple <- predict(lm_trust_simple, newdata = pred_range, interval = "confidence")
pred_trustee_simple <- predict(lm_trustee_simple, newdata = pred_range, interval = "confidence")

# 2. Full model predictions (need to set values for other variables)
pred_range_full <- data.frame(
  monitor_coef = seq(-1, 1, by = 0.1),
  PERIOD = median(trustee_data$PERIOD, na.rm = TRUE),  # Set as median
  P2A = median(trustee_data$P2A, na.rm = TRUE),        # Set as median
  MOVE1 = median(trustee_data$MOVE1, na.rm = TRUE)     # Set as median
)

pred_trustee_full <- predict(lm_trustee_full, newdata = pred_range_full, interval = "confidence")

# Figure 1: Trustor model prediction (simple model)
ggplot() +
  geom_point(data = trust_data, aes(x = monitor_coef, y = MOVE1), 
             alpha = 0.5, color = "blue") +
  geom_line(data = cbind(pred_range, pred_trust_simple), 
            aes(x = monitor_coef, y = fit), color = "red", size = 1) +
  geom_ribbon(data = cbind(pred_range, pred_trust_simple),
              aes(x = monitor_coef, ymin = lwr, ymax = upr),
              alpha = 0.2, fill = "red") +
  labs(title = "Trust Level vs Monitor Coefficient",
       x = "Monitor Coefficient (0=Unbiased, 0.5=Biased, 1=No Monitor)",
       y = "Trustor Amount Sent (MOVE1)") +
  scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1),
                     labels = c("trust as Monitor\n(-1)", "Monitor for trust\n(-0.5)", "Unbiased\n(0)", 
                                "Monitor for trustee\n(0.5)", "No Monitor\n or trustee as Monitor\n(1)")) +
  theme_minimal()

# Figure 2: Trustee model prediction (full model)
ggplot() +
  geom_point(data = trustee_data, aes(x = monitor_coef, y = P2_100), 
             alpha = 0.5, color = "darkgreen") +
  geom_line(data = cbind(pred_range_full, pred_trustee_full), 
            aes(x = monitor_coef, y = fit), color = "darkred", size = 1) +
  geom_ribbon(data = cbind(pred_range_full, pred_trustee_full),
              aes(x = monitor_coef, ymin = lwr, ymax = upr),
              alpha = 0.2, fill = "darkred") +
  labs(title = "Return Percentage vs Monitor Coefficient",
       x = "Monitor Coefficient (0=Unbiased, 0.5=Biased, 1=No Monitor)",
       y = "Trustee Return Percentage (%)") +
  scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1),
                     labels = c("trust as Monitor\n(-1)", "Monitor for trust\n(-0.5)", "Unbiased\n(0)", 
                                "Monitor for trustee\n(0.5)", "No Monitor\n or trustee as Monitor\n(1)")) +
  theme_minimal() +
  geom_hline(yintercept = 33.3, linetype = "dashed", color = "gray", 
             alpha = 0.7) +  # Break-even point
  annotate("text", x = 0, y = 35, label = "Break-even Point (33.3%)", 
           color = "gray", size = 3)


# First ensure we use the correct dataset
# outtrjo2 contains only cases with MOVE1>0 (i.e., cases with positive sending)
# This is the dataset already created in the article

# Check cases with zero return
trustee_data <- outtrjo2  # Use the filtered data

# 1. Overall statistics
total_decisions <- nrow(trustee_data)
zero_returns <- sum(trustee_data$MOVE2 == 0)
zero_return_rate <- zero_returns / total_decisions

cat("=== Overall Statistics for Zero Returns ===\n")
cat(sprintf("Total Decisions: %d\n", total_decisions))
cat(sprintf("Zero Return Count: %d\n", zero_returns))
cat(sprintf("Zero Return Rate: %.2f%%\n", zero_return_rate * 100))

# 2. Statistics by treatment group
zero_by_treatment <- trustee_data %>%
  group_by(TRT) %>%
  summarise(
    total = n(),
    zero_returns = sum(MOVE2 == 0),
    zero_rate = zero_returns / total * 100,
    .groups = 'drop'
  )

# Add treatment group labels
zero_by_treatment$treatment_label <- factor(zero_by_treatment$TRT,
                                            levels = c(2, 3, 4),
                                            labels = c("Unbiased Monitor", "Biased Monitor", "No Monitor")
)

print(zero_by_treatment)

# Figure 2: Zero return rate by treatment group
p2 <- ggplot(zero_by_treatment, aes(x = treatment_label, y = zero_rate)) +
  geom_bar(stat = "identity", aes(fill = treatment_label), 
           width = 0.7, alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", zero_rate, total)), 
            vjust = -0.3, size = 4, fontface = "bold") +
  labs(title = "Zero Return Rate (by Treatment Group)",
       x = "Treatment Group",
       y = "Zero Return Rate (%)") +
  scale_fill_manual(values = c("Unbiased Monitor" = "#2196F3", 
                               "Biased Monitor" = "#FF9800", 
                               "No Monitor" = "#9E9E9E")) +
  ylim(0, max(zero_by_treatment$zero_rate) * 1.2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.position = "none",
        panel.grid.major.x = element_blank())
p2





# Create a comprehensive dataset with all relevant variables
correlation_data <- outtrjo %>%
  # Select variables for correlation analysis
  select(ID1, monitor_coef, MOVE1, MOVE2, P1A, P2A, TRT, PERIOD) %>%
  # Filter only cases where trustor sent something (for trustee variables to be meaningful)
  filter(MOVE1 > 0) %>%
  # Calculate additional derived variables
  mutate(
    # Return percentage (only when MOVE1 > 0)
    return_percentage = ifelse(MOVE1 > 0, (MOVE2 / (MOVE1 * 3)) * 100, NA),
    
    # Binary variable: whether return was made
    return_made = as.numeric(MOVE2 > 0),
    
    # Trustor's belief accuracy (difference between predicted and actual return)
    trustor_belief_accuracy = ifelse(MOVE1 > 0, P1A - (MOVE2/3), NA),
    
    # Trustee's belief accuracy
    trustee_belief_accuracy = ifelse(!is.na(P2A), P2A - MOVE1, NA),
    
    # Treatment group dummies
    unbiased_dummy = as.numeric(TRT == 2),
    biased_dummy = as.numeric(TRT == 3),
    no_monitor_dummy = as.numeric(TRT == 4)
  ) %>%
  # Remove rows with missing values for correlation analysis
  na.omit()

# Also create a dataset with destroy game data (if available)
# First, let's merge destroy data with outtrjo data
if(exists("destroy")) {
  # Summarize destroy game behavior by participant
  destroy_summary <- destroy %>%
    group_by(id) %>%
    summarise(
      avg_tickets_destroyed = mean(ticket, na.rm = TRUE),
      total_tickets_destroyed = sum(ticket, na.rm = TRUE),
      total_wins = sum(group_won, na.rm = TRUE),
      avg_private_earn = mean(private_earn, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(ID1 = id)
  
  # Merge with correlation_data
  correlation_data <- correlation_data %>%
    left_join(destroy_summary, by = "ID1")
  
  # Create a combined dataset with destroy variables
  correlation_data_complete <- correlation_data %>%
    select(monitor_coef, MOVE1, MOVE2, return_percentage, 
           P1A, P2A, return_made, trustor_belief_accuracy, trustee_belief_accuracy,
           avg_tickets_destroyed, total_wins, avg_private_earn) %>%
    na.omit()
} else {
  # If destroy data not loaded, use the basic dataset
  correlation_data_complete <- correlation_data %>%
    select(monitor_coef, MOVE1, MOVE2, return_percentage, 
           P1A, P2A, return_made, trustor_belief_accuracy, trustee_belief_accuracy) %>%
    na.omit()
}

# ===========================================
# 1. BASIC CORRELATION MATRIX
# ===========================================

# Calculate correlation matrix
cor_matrix <- cor(correlation_data_complete, use = "complete.obs")

# Print correlation matrix
cat("=== Correlation Matrix ===\n")
print(round(cor_matrix, 3))

# ===========================================
# 2. VISUALIZATION : GGPLOT HEATMAP
# ===========================================

# Reshape correlation matrix for ggplot
cor_matrix_melted <- as.data.frame(as.table(cor_matrix))
names(cor_matrix_melted) <- c("Var1", "Var2", "Correlation")

# Remove self-correlations and duplicates
cor_matrix_melted <- cor_matrix_melted %>%
  filter(Var1 != Var2) %>%
  mutate(
    Var1 = factor(Var1),
    Var2 = factor(Var2),
    # Create label with correlation value
    Correlation_label = sprintf("%.3f", Correlation),
    # Color based on correlation strength and direction
    Correlation_category = cut(Correlation,
                               breaks = c(-1, -0.7, -0.3, 0, 0.3, 0.7, 1),
                               labels = c("Strong Negative", "Moderate Negative", "Weak Negative",
                                          "Weak Positive", "Moderate Positive", "Strong Positive"))
  )

# Create ggplot heatmap
p3 <- ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = Correlation_label), color = "black", size = 3.5) +
  scale_fill_gradient2(low = "#D73027", mid = "white", high = "#4477AA",
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation") +
  labs(title = "Correlation Heatmap: All Variables",
       subtitle = "Monitor coefficient correlations with other variables",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "right"
  ) +
  coord_fixed()

print(p3)



# =====================================================
# MOVE1 & trustee_belief_accuracy regression analysis
# =====================================================

# calculate trustee_belief_accuracy
correlation_data$trustee_belief_accuracy <- correlation_data$P2A - correlation_data$MOVE1
reg_data <- correlation_data %>%
  select(MOVE1, trustee_belief_accuracy, monitor_coef, PERIOD, P2A) %>%
  na.omit()
model1 <- lm(trustee_belief_accuracy ~ MOVE1, data = reg_data)

print(summary(model1))
model2 <- lm(trustee_belief_accuracy ~ poly(MOVE1, 2), data = reg_data)

print(summary(model2))

pred_range_m1 <- data.frame(MOVE1 = seq(0, 100, by = 1))
pred_simple <- predict(model1, newdata = pred_range_m1, interval = "confidence")
pred_quad <- predict(model2, newdata = pred_range_m1, interval = "confidence")
p_reg1 <- ggplot(reg_data, aes(x = MOVE1, y = trustee_belief_accuracy)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 2) +
  geom_line(data = cbind(pred_range_m1, pred_simple), 
            aes(x = MOVE1, y = fit), color = "red", size = 1.2) +
  labs(title = "Trustee Belief Accuracy vs Amount Sent",
       x = "Amount Sent by Trustor (MOVE1)",
       y = "Trustee Belief Accuracy (P2A - MOVE1)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11))
print(p_reg1)
