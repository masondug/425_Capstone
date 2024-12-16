df$zero_inflated <- ifelse(df$Goals == 0, 1, 0)

# Prepare data
X_binary <- model.matrix(zero_inflated ~ . - 1, data = df %>% 
                           ungroup() %>% 
                           select(-Goals, -shooting_team, -goalieNameForShot, -cluster, 
                                  -game_id, -goalie_team, -season, -gameDate))  # Create matrix
row_indices <- attr(X_binary, "assign") 
excluded_rows <- setdiff(seq_len(nrow(df)), row_indices)
df_clean <- df[rownames(X_binary), ]
y_binary <- df_clean$zero_inflated

# Fit lasso logistic regression
cv_binary <- cv.glmnet(X_binary, y_binary, family = "binomial", alpha = 1)
best_lambda_binary <- cv_binary$lambda.min

# Get coefficients at the best lambda
selected_coefs <- coef(cv_binary, s = best_lambda_binary)

# Convert sparse matrix to a standard vector
selected_coefs_vector <- as.vector(selected_coefs)

# Extract names of predictors with non-zero coefficients
selected_binary <- rownames(selected_coefs)[selected_coefs_vector != 0]
selected_binary <- selected_binary[selected_binary != "(Intercept)"]  # Exclude intercept

# Check if home_or_awayHOME or home_or_awayAWAY exists in selected_binary
if (any(c("home_or_awayAWAY", "home_or_awayHOME") %in% selected_binary)) {
  # Remove home_or_awayAWAY and home_or_awayHOME
  selected_binary <- setdiff(selected_binary, c("home_or_awayAWAY", "home_or_awayHOME"))
  
  # Add 'home_or_away' as a single categorical predictor
  selected_binary <- c(selected_binary, "home_or_away")
}

df_nonzero <- df[df$Goals > 0, ]

# Prepare data
X_count <- model.matrix(zero_inflated ~ . - 1, data = df_nonzero %>% 
                          ungroup() %>% 
                          select(-Goals, -shooting_team, -goalieNameForShot, -cluster, 
                                 -game_id, -goalie_team, -season, -gameDate))

row_indices <- attr(X_binary, "assign") 
excluded_rows <- setdiff(seq_len(nrow(df_nonzero)), row_indices)
df_clean <- df[rownames(X_count), ]
y_count <- df_clean$Goals

# Fit lasso Poisson regression
cv_count <- cv.glmnet(X_count, y_count, family = "poisson", alpha = 1)
best_lambda_count <- cv_count$lambda.min

# Extract selected predictors
selected_coefs <- coef(cv_binary, s = best_lambda_count)

# Convert sparse matrix to a standard vector
selected_coefs_vector <- as.vector(selected_coefs)

selected_count <- rownames(selected_coefs)[selected_coefs_vector != 0]
selected_count <- selected_count[selected_count != "(Intercept)"]

if (any(c("home_or_awayAWAY", "home_or_awayHOME") %in% selected_count)) {
  # Remove home_or_awayAWAY and home_or_awayHOME
  selected_count <- setdiff(selected_count, c("home_or_awayAWAY", "home_or_awayHOME"))
  
  # Add 'home_or_away' as a single categorical predictor
  selected_count <- c(selected_count, "home_or_away")
}

selected_predictors <- unique(c(selected_binary, selected_count))

count_formula <- paste("Goals ~ -1 +", paste(selected_predictors, collapse = " + "), "+ (1|cluster:goalieNameForShot)")

# Create the formula for the zero-inflation component
zi_formula <- paste("~ -1 +", paste(selected_binary, collapse = " + "))

zip_model_lasso <- glmmTMB(
  as.formula(count_formula),   # Count component
  ziformula = as.formula(zi_formula),  # Zero-inflation component
  data = df,
  family = poisson()
)

zi_summary <- summary(zip_model_lasso)$coefficients$zi

selected_binary_new <- setdiff(rownames(zi_summary)[zi_summary[,'Pr(>|z|)'] < .25], NA)

if (any(c("home_or_awayAWAY", "home_or_awayHOME") %in% selected_binary_new)) {
  # Remove home_or_awayAWAY and home_or_awayHOME
  selected_binary_new <- setdiff(selected_binary_new, c("home_or_awayAWAY", "home_or_awayHOME"))
}

zi_formula_new <- paste("~ -1 +", paste(selected_binary_new, collapse = " + "))

count_summary <- summary(zip_model_lasso)$coefficients$cond
selected_count_new <- setdiff(rownames(count_summary)[count_summary[,'Pr(>|z|)'] < .25], '(Intercept)')
selected_count_new <- setdiff(selected_count_new, NA)

if (any(c("home_or_awayAWAY", "home_or_awayHOME") %in% selected_count_new)) {
  # Remove home_or_awayAWAY and home_or_awayHOME
  selected_count_new <- setdiff(selected_count_new, c("home_or_awayAWAY", "home_or_awayHOME"))
}

selected_predictors_new <- unique(c(selected_binary_new, selected_count_new))

count_formula_new <- paste("Goals ~ -1 +", paste(selected_predictors_new, collapse = " + "), "+ (1|cluster:goalieNameForShot)")

zip_model_lasso <- glmmTMB(
  as.formula(count_formula_new),
  ziformula = as.formula(zi_formula_new),
  data = df,
  family = poisson()
)

#write_rds(zip_model_lasso, 'zip_model_lasso.rds')
