Demographics <- read.csv("C:\Users\MANOGNA\OneDrive\Desktop\Demographics.r")
>   View(Demographics)

# Load the data
demographic_data <- read.csv("demographic.csv")

# Subset the Data for France
france_data <- subset(Demographics, Country.Name == "France")

# Display the Data
print(france_data)



# Filter the data for countries with birth rate > 20 and High income
filtered_data <- subset(demographic_data, Birth.rate > 20 & Income.Group == "High income")

# Display the filtered data
print(filtered_data)


# Find the row index with the highest number of internet users
max_internet_users_index <- which.max(demographic_data$Internet.users)

# Display the data of the country with the highest internet users
print(demographic_data[max_internet_users_index, ])


# Calculate the average birth rate
average_birthrate <- mean(Demographics$Birth.rate, na.rm = TRUE)

# Display the average birth rate
print(average_birthrate)


# Calculate the standard deviation of the birthrate
birthrate_sd <- sd(Demographics$Birth.rate)

# Print the standard deviation of the birthrate
print(birthrate_sd)

# Load the data
demographic_data <- read.csv("~/Desktop/Income.csv")

# Calculate the mean and standard deviation of the birth rate
mean_birth_rate <- mean(Demographics$Birth.rate)
sd_birth_rate <- sd(Demographics$Birth.rate)

# Calculate the threshold (mean + 2 * SD)
threshold <- mean_birth_rate + 2 * sd_birth_rate

# Filter the data for countries with birth rate greater than the threshold
high_birthrate_countries <- subset(Demographics, Birth.rate > threshold)

# Display the filtered data
print(high_birthrate_countries)

# Load the data
demographic_data <- read.csv("~/Desktop/Income.csv")

# Calculate the interquartile range (IQR) of the birth rate
birthrate_iqr <- IQR(Demographics$Birth.rate)

# Print the interquartile range (IQR) of the birth rate
print(birthrate_iqr)


