library("ggplot2")
library(class)
library(caret)
EPI_data <- read.csv("epi2024results_DA_F24_lab03.csv")
attach(EPI_data)
fix(EPI_data) 

subsaharan_data <- subset(EPI_data, region=="Sub-Saharan Africa")

saharan_GHN <- subsaharan_data$GHN


soviet_data <- subset(EPI_data, region=="Former Soviet States")
soviet_GHN <- soviet_data$GHN


hist(saharan_GHN,probability = TRUE, col = "red")
lines(density(saharan_GHN))
hist(soviet_GHN,probability = TRUE, col = "blue")
lines(density(soviet_GHN))


ggplot(data = data.frame(sample = saharan_GHN), aes(sample = saharan_GHN)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  theme_minimal() +
  ggtitle("QQ plot of Sub-Saharan GHN")

ggplot(data = data.frame(sample = soviet_GHN), aes(sample = soviet_GHN)) +
  stat_qq() +
  stat_qq_line(col = "blue") +
  theme_minimal() +
  ggtitle("QQ plot of Former Soviet GHN")




lin.mod.epinew <- lm(subsaharan_data$EPI ~ subsaharan_data$ECO+subsaharan_data$BDH + subsaharan_data$PAR+subsaharan_data$SPI + subsaharan_data$TBN, data = subsaharan_data)
summary(lin.mod.epinew)
plot(subsaharan_data$BDH, subsaharan_data$EPI,
     xlab = "Subsaharan ECO+BDH", ylab = "EPI New")
abline(lin.mod.epinew)

lin.mod.epinew <- lm(subsaharan_data$EPI ~ subsaharan_data$ECO+subsaharan_data$BDH + subsaharan_data$PAR+subsaharan_data$SPI + subsaharan_data$TBN, data = subsaharan_data)
summary(lin.mod.epinew)
plot(subsaharan_data$BDH, subsaharan_data$EPI,
     xlab = "Former Soviet ECO+BDH", ylab = "EPI New")
abline(lin.mod.epinew)

variables <- c("EPI", "ECO", "BDH", "PAR", "SPI")
regions1 <- c("Sub-Saharan Africa", "Former Soviet States", "Middle East")

# Filter the data
data1 <- EPI_data[EPI_data$region %in% regions1, c("region", variables)]
data1 <- na.omit(data1)
# Split the data into features and labels
features1 <- scale(data1[, variables])
labels1 <- data1$region

# Train kNN model (k=5 for this example, you might want to tune this)
set.seed(123)
knn_model1 <- knn(train = features1, test = features1, cl = labels1, k = 5)

# Create contingency matrix
cm1 <- table(Predicted = knn_model1, Actual = labels1)
print("Contingency Matrix for Model 1:")
print(cm1)

# Calculate accuracy
accuracy1 <- sum(diag(cm1)) / sum(cm1)
print(paste("Accuracy for Model 1:", round(accuracy1, 4)))

# 3.2 Second kNN model
# Choose 3 different regions
regions2 <- c("Global West", "Latin America & Caribbean", "Greater Middle East")

# Filter the data
data2 <- EPI_data[EPI_data$region %in% regions2, c("region", variables)]
data2 <- na.omit(data2)
# Split the data into features and labels
features2 <- scale(data2[, variables])
labels2 <- data2$region
fix(data2)
# Train kNN model
set.seed(123)
knn_model2 <- knn(train = features2, test = features2, cl = labels2, k = 5)

# Create contingency matrix
cm2 <- table(Predicted = knn_model2, Actual = labels2)
print("Contingency Matrix for Model 2:")
print(cm2)

# Calculate accuracy
accuracy2 <- sum(diag(cm2)) / sum(cm2)
print(paste("Accuracy for Model 2:", round(accuracy2, 4)))



variables <- c("EPI", "ECO", "BDH", "PAR", "SPI")

# Define two groups of regions
regions1 <- c("Sub-Saharan Africa", "Former Soviet States", "Middle East")
regions2 <- c("Global West", "Latin America & Caribbean", "Greater Middle East")

# Function to prepare data
prepare_data <- function(regions) {
  data <- EPI_data[EPI_data$region %in% regions, variables]
  data <- na.omit(data)
  return(scale(data))
}

# Prepare data for both region groups
data1 <- prepare_data(regions1)
data2 <- prepare_data(regions2)

# 1. Fit k-means models for k=3 (since we have 3 regions in each group)
set.seed(123)
kmeans1 <- kmeans(data1, centers = 3)
kmeans2 <- kmeans(data2, centers = 3)

# 1.1 Compare performance using WCSS
wcss1 <- kmeans1$tot.withinss
wcss2 <- kmeans2$tot.withinss

print(paste("WCSS for model 1:", wcss1))
print(paste("WCSS for model 2:", wcss2))

# 1.2 Fit k-means models for multiple k values and plot WCSS
k_values <- 1:10
wcss_values1 <- numeric(length(k_values))
wcss_values2 <- numeric(length(k_values))

for (i in k_values) {
  kmeans1 <- kmeans(data1, centers = i)
  kmeans2 <- kmeans(data2, centers = i)
  wcss_values1[i] <- kmeans1$tot.withinss
  wcss_values2[i] <- kmeans2$tot.withinss
}

# Create a data frame for plotting
plot_data <- data.frame(
  k = rep(k_values, 2),
  WCSS = c(wcss_values1, wcss_values2),
  Model = rep(c("Model 1", "Model 2"), each = length(k_values))
)

# Plot WCSS across k values
ggplot(plot_data, aes(x = k, y = WCSS, color = Model)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "WCSS vs. Number of Clusters (k)",
       x = "Number of Clusters (k)",
       y = "Within Cluster Sum of Squares (WCSS)")

# Print the plot
print(last_plot())


