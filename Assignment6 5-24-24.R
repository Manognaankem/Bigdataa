# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Load the data
all <- read.csv(file.choose())

# Inspect the data to ensure it is loaded correctly
print(head(all))
print(colnames(all))

# Standardize the data using all_of()
scaled_data <- all %>%
  dplyr::select(all_of(c("V1", "V2", "X"))) %>%
  scale()

# Perform k-means clustering with 3 clusters
set.seed(42)
kmeans_result <- kmeans(scaled_data, centers = 3)
all$Cluster <- as.factor(kmeans_result$cluster)

# Plot 1: V1 vs V2
plot1 <- ggplot(all, aes(x = V1, y = V2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "V1 vs V2", x = "V1", y = "V2") +
  theme_minimal()

# Plot 2: V1 vs X
plot2 <- ggplot(all, aes(x = V1, y = X, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "V1 vs X", x = "V1", y = "X") +
  theme_minimal()

# Plot 3: V2 vs X
plot3 <- ggplot(all, aes(x = V2, y = X, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "V2 vs X", x = "V2", y = "X") +
  theme_minimal()

# Display the plots side by side
grid.arrange(plot1, plot2, plot3, ncol = 3)

# Explicitly print each plot (if needed)
print(plot1)
print(plot2)
print(plot3)

