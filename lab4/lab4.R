##########################################
### Principal Component Analysis (PCA) ###
##########################################

library(ggfortify)
library(e1071)
library(class)
library(psych)

# PCA with iris dataset
wine <- read.csv("wine.data")
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)

wine <- wine[,-c(4,5,10)]

pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

principal_components <- princomp(wine[2:10],  cor = TRUE, score = TRUE)

summary(principal_components)
plot(principal_components)


set.seed(123)
train_index <- sample(1:nrow(wine), 0.7 * nrow(wine))
train_data <- wine[train_index, ]
test_data <- wine[-train_index, ]


full_model <- lm(as.numeric(wine$Type) ~ ., data = wine[, -1])
full_predictions <- predict(full_model, wine[, -1])
full_mse <- mean((as.numeric(wine$Type) - full_predictions)^2)

pca<- predict(principal_components, wine[, -1])

# Create data frames with first 3 PCs
pca_df <- data.frame(
  Type = wine$Type,
  PC1 = pca[, 1],
  PC2 = pca[, 2],
  PC3 = pca[, 3]
)

pca_model <- lm(as.numeric(Type) ~ PC1 + PC2 + PC3, data = pca_df)
pca_predictions <- predict(pca_model, pca_df)
pca_mse <- mean((as.numeric(wine$Type) - pca_predictions)^2)

par(mfrow=c(1,2))
plot(as.numeric(wine$Type), full_predictions, 
     main="Full Model\nActual vs Predicted",
     xlab="Actual", ylab="Predicted")
abline(0,1, col="red")

plot(as.numeric(wine$Type), pca_predictions, 
     main="PCA Model\nActual vs Predicted",
     xlab="Actual", ylab="Predicted")
abline(0,1, col="red")

cat(pca_mse)
cat(full_mse)
