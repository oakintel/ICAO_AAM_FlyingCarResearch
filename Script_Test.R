

library(dplyr)
library(tidyr)
library(lavaan)
library(ggplot2)
library(scatterplot3d)
library(plotly)

# Read the data
data <- read.csv("raw_data/UpdatedSurveyApr29.csv", skip = 3, header = FALSE, na.strings = c("", " "), stringsAsFactors = FALSE)

# View the modified data
head(data)
summary(data)

# Read the data
colnames(data) <- c("Familiarity", "WillingnessToTry", "Interest", "FlyingExperience", "AirshowExperience","PerceivedReliability", "EnvironmentalConcern", "WillingnessToPay", "ImportanceOfTimeSavings","ImportanceOfTrafficAvoidance", "ImportanceOfConvenience", "ImportanceOfEnvBenefits","ComfortWithFlying", "MainConcern", "DemonstrationInterest", "AAMUseCases", "OhioTesting","AgeGroup", "IncomeLevel", "AreaType")

# View the modified data
head(data)
summary(data)
#view(data)
#write.csv(data, "modified_data.csv", row.names = FALSE)

# Convert WillingnessToTry, Interest, PerceivedReliability, EnvironmentalConcern, and ComfortWithFlying
factor_vars <- c("WillingnessToTry", "Interest", "PerceivedReliability", "EnvironmentalConcern", "ComfortWithFlying")
level_mapping <- c("Extremely unlikely" = -2, "Somewhat unlikely" = -1, "Neutral" = 0, "Somewhat likely" = 1, "Extremely likely" = 2,
                   "Extremely uninterested" = -2, "Somewhat uninterested" = -1, "Somewhat interested" = 1, "Extremely interested" = 2,
                   "Extremely unreliable" = -2, "Somewhat unreliable" = -1, "Somewhat reliable" = 1, "Extremely reliable" = 2,
                   "Not at all important" = -2, "Slightly important" = -1, "Moderately important" = 0, "Very important" = 1, "Extremely important" = 2,
                   "Extremely uncomfortable" = -2, "Somewhat uncomfortable" = -1, "Neither comfortable nor uncomfortable" = 0, "Somewhat comfortable" = 1, "Extremely comfortable" = 2)

for (var in factor_vars) {
  encoded_var <- paste0("enc_", var)
  data[[encoded_var]] <- factor(data[[var]], levels = names(level_mapping), labels = level_mapping)
}

# Convert FlyingExperience to binary (1 if any experience, 0 if none)
data$enc_FlyingExperience <- ifelse(data$FlyingExperience == "None of the above", 0, 1)

# Convert AirshowExperience, DemonstrationInterest, and OhioTesting
binary_vars <- c("Familiarity", "AirshowExperience", "DemonstrationInterest", "OhioTesting")
for (var in binary_vars) {
  encoded_var <- paste0("enc_", var)
  data[[encoded_var]] <- factor(data[[var]], levels = c("No", "Maybe", "Yes"), labels = c(-1, 0, 1))
}

# Convert WillingnessToPay
data$enc_WillingnessToPay <- factor(data$WillingnessToPay, levels = c("No more than a regular taxi/ride-share", "Up to 25% more", "Up to 50% more", "Up to 100% more (double)", "More than double"),
                                    labels = c(0, 0.25, 0.5, 1, 2))

# Convert IncomeLevel
data$enc_IncomeLevel <- factor(data$IncomeLevel, levels = c("Less than $25,000", "$25,000 - $50,000", "$50,001 - $75,000", "$75,001 - $100,000", "$100,001 - $150,000", "Over $150,000"),
                               labels = c(1, 2, 3, 4, 5, 6))

# Convert AreaType
data$enc_AreaType <- factor(data$AreaType, levels = c("Rural", "Suburban", "Urban"), labels = c(1, 2, 3))

# Save the modified dataset to a new CSV file
write.csv(data, "modified_data_encoded.csv", row.names = FALSE)
data <- read.csv("modified_data_encoded.csv")
summary(data)



# Load the modified dataset with encoded values
data <- read.csv("modified_data_encoded.csv", stringsAsFactors = FALSE)

data_no_na <- na.omit(data)

model <- lm(enc_Interest ~ enc_WillingnessToPay + enc_WillingnessToTry, data = data_no_na)

# Extract the coefficients and R-squared
coefficients <- coef(model)
r_squared <- summary(model)$r.squared

# Print the coefficients and R-squared
print(coefficients)
print(r_squared)

# Create the plot
ggplot(data = data_no_na, aes(x = enc_WillingnessToPay, y = enc_Interest)) +
  geom_point(color = "blue") +  # Add points
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Add regression line
  labs(x = "Willingness to Pay More", y = "Interest in AAM") +  # Set labels
  theme_bw()  # Apply a theme (optional)

# Create the plot
ggplot(data = data_no_na, aes(x =enc_WillingnessToTry , y = enc_Interest)) +
  geom_point(color = "blue") +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add regression line
  labs(x = "Willingness to Try", y = "Interest") +  # Set labels
  theme_bw()  # Apply a theme (optional)

# Create the plot
ggplot(data = data_no_na, aes(x = enc_WillingnessToPay, y = enc_WillingnessToTry)) +
  geom_point(color = "blue") +  # Add points
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Add regression line
  labs(x = "Willingness to Pay", y = "Willingness to Try") +  # Set labels
  theme_bw()  # Apply a theme (optional)



library(plotly)

# Adding a plane is more complex and requires additional calculations 
# and manipulation of 3D mesh objects.

# View the model summary
summary(model)
summary(model)$sigma #Standard Error of the Model
plot(model$fitted.values, model$residuals, xlab= "Fitted Values", ylab = "Residuals") #if there is a pattern, what's up??
hist(model$residuals)
qqnorm(model$residuals, col="red")
qqline(model$residuals)
anova(model)
summary(model)$r.squared

library(plotly)

plot_ly(data_no_na, x = ~enc_WillingnessToPay, y = ~enc_WillingnessToTry, z = ~enc_Interest,
        type = "scatter3d", mode = "markers",
        marker = list(size = ~enc_Interest * 3,  # Scale size based on interest
                      color = ~enc_Interest, colorscale = "Viridis", showscale = TRUE)) %>%  # Set color gradient
  layout(scene = list(xaxis = list(title = "Willingness to Pay More"),
                      yaxis = list(title = "Willingness to Try AAM"),
                      zaxis = list(title = "Interest in AAM")))


# Fit a linear regression model with all three variables
model_3d <- lm(enc_Interest ~ enc_WillingnessToPay + enc_WillingnessToTry, data = data_no_na)

#Analyzing the Regression Correlation for Public Acceptance
# Load the modified dataset with encoded values
data <- read.csv("modified_data_encoded.csv", stringsAsFactors = FALSE)

data_no_na <- na.omit(data)

model <- lm(enc_Interest ~ enc_WillingnessToTry, data = data_no_na)

# View the model summary
summary(model)
summary(model)$sigma #Standard Error of the Model
plot(model$fitted.values, model$residuals, xlab= "Fitted Values", ylab = "Residuals") #if there is a pattern, what's up??
hist(model$residuals)
qqnorm(model$residuals, col="red")
qqline(model$residuals)

# Extract the coefficients and R-squared
coefficients <- coef(model)
r_squared <- summary(model)$r.squared

# Print the coefficients and R-squared
print(coefficients)
print(r_squared)

# Calculate counts and percentages

data_long <- data %>%
  pivot_longer(cols = starts_with("ImportanceOf"),
               names_to = "category",
               values_to = "rank") 

top_ranked_summary <- data_long %>%
  filter(rank == 1) %>%
  na.omit() %>%
  group_by(category) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(desc(percentage)) %>%
  mutate(category = recode(category, 
                           ImportanceOfConvenience = "Convenience",
                           ImportanceOfEnvBenefits = "Environmental Benefits",
                           ImportanceOfTimeSavings = "Time Savings",
                           ImportanceOfTrafficAvoidance = "Traffic Avoidance"))  # Rename categories

# Create bar chart with renamed x-axis labels
ggplot(top_ranked_summary, aes(x = reorder(category, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Factor", y = "Percentage of Respondents", 
       title = "Distribution of Top-Ranked Factors") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
summary(data)

# Load the data (assuming your data is in 'modified_data_encoded.csv')
data <- read.csv("modified_data_encoded.csv", stringsAsFactors = FALSE)

# Standardize relevant variables
data_scaled <- data %>%
  mutate(across(c(enc_Familiarity, enc_PerceivedReliability, enc_ComfortWithFlying, 
                  enc_WillingnessToTry, enc_WillingnessToPay, enc_Interest), scale))

# Select variables for PCA
demand_vars <- data_scaled %>%
  select(enc_WillingnessToTry, enc_WillingnessToPay, enc_Interest)

# Perform PCA
pca_result <- prcomp(demand_vars, scale = TRUE)

# Extract first principal component
demand_scores <- pca_result$x[, 1]

# Add Demand Score to the data
data_scaled$DemandScore <- demand_scores

# ... (further analysis and visualization)

# Standardize relevant variables (excluding importance rankings)
data_scaled <- data %>%
  mutate(across(c(enc_Familiarity, enc_PerceivedReliability, enc_ComfortWithFlying, enc_WillingnessToTry, enc_WillingnessToPay, enc_Interest), scale))

# Remove rows with NAs before calculating scores
data_scaled <- na.omit(data_scaled)

# Calculate Demand Score using PCA
demand_vars <- data_scaled %>%
  select(enc_WillingnessToTry, enc_WillingnessToPay, enc_Interest)
pca_demand <- prcomp(demand_vars, scale = TRUE)
data_scaled$DemandScore <- pca_demand$x[, 1]

# Calculate Acceptance Score using PCA
acceptance_vars <- data_scaled %>%
  select(enc_Familiarity, enc_PerceivedReliability, enc_EnvironmentalConcern, enc_ComfortWithFlying)
pca_acceptance <- prcomp(acceptance_vars, scale = TRUE)
data_scaled$AcceptanceScore <- pca_acceptance$x[, 1]

# Standardize Demand and Acceptance Scores
data_scaled <- data_scaled %>%
  mutate(StandardizedDemandScore = scale(DemandScore),
         StandardizedAcceptanceScore = scale(AcceptanceScore))

# Plot Demand vs Acceptance with color-coding by age group
ggplot(data_scaled, aes(x = StandardizedDemandScore, y = StandardizedAcceptanceScore)) +
  geom_point(aes(color = AgeGroup), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Standardized Demand Score", y = "Standardized Acceptance Score",
       title = "Demand vs Acceptance for AAM") +
  scale_color_viridis_d() +
  theme_minimal()

# Calculate correlation between Demand and Acceptance
correlation <- cor(data_scaled$StandardizedDemandScore, data_scaled$StandardizedAcceptanceScore)
print(paste("Correlation between Demand and Acceptance:", correlation))

# Regression model to predict Acceptance based on Demand and other factors
model <- lm(StandardizedAcceptanceScore ~ StandardizedDemandScore + enc_IncomeLevel + enc_AreaType,
            data = data_scaled)
summary(model)



