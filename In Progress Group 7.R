# Group number: Group 7
# Names - Manogna, Mehboob, Priya

# Target/Label column: delay
# Other variables:
#   - year: Year of the flight
#   - month: Month of the flight
#   - day: Day of the flight
#   - dep_time: Actual departure time
#   - sched_dep_time: Scheduled departure time
#   - delay: Delay in minutes
#   - dep_delay: Departure delay in minutes
#   - arr_time: Actual arrival time
#   - sched_arr_time: Scheduled arrival time
#   - arr_delay: Arrival delay in minutes
#   - carrier: Airline carrier code
#   - flight: Flight number
#   - tailnum: Tail number of the aircraft
#   - origin: Origin airport code
#   - dest: Destination airport code
#   - air_time: Duration of the flight in minutes
#   - distance: Distance of the flight
#   - hour: Hour of the departure time
#   - minute: Minute of the departure time
#   - time_hour: Timestamp of the departure time
#   - temp: Temperature at the time of departure
#   - dewp: Dew point at the time of departure
#   - humid: Humidity at the time of departure
#   - wind_dir: Wind direction at the time of departure
#   - wind_speed: Wind speed at the time of departure
#   - wind_gust: Wind gust at the time of departure
#   - precip: Precipitation at the time of departure
#   - pressure: Pressure at the time of departure
#   - visib: Visibility at the time of departure

install.packages("rmarkdown")

# Load the rmarkdown package
library(rmarkdown)

# Render the R Markdown document to a Word document
render("/mnt/data/BigData.Rmd", output_format = "word_document")

# Loading the dataset from file
df <- read.csv("C:/Users/mariy/Downloads/flights.csv")
summary(df)
# Display the first few rows of the dataset
head(df)
# Display the structure of the dataset
str(df)

# Install ggplot2 package
install.packages("ggplot2")

# Load ggplot2 package
library(ggplot2)

# Check for missing values
missing_values <- colSums(is.na(df))
print(missing_values)

library(ggplot2)

# Define custom color palette for the bars
custom_palette <- c("#4CAF50", "#FFC107", "#FF5722", "#2196F3")

# Create the plot with adjusted number of bins
ggplot(df, aes(x = cut(dep_delay, breaks = seq(-60, 240, by = 75)), fill = cut(dep_delay, breaks = seq(-60, 240, by = 75)))) +
  geom_bar() +  # Create bars
  labs(
    x = "Arrival Delay (minutes)",  # X-axis label
    y = "Number of Flights",  # Y-axis label
    title = "Distribution of Arrival Delays",  # Plot title
    caption = "Data Source: [Your Source]"  # Caption mentioning data source
  ) +
  scale_fill_manual(values = custom_palette) +  # Set bar colors using custom palette
  theme_minimal() +  # Minimal theme for clean appearance
  theme(
    plot.title = element_text(size = 16, face = "bold"),  # Increase title size and make it bold
    axis.title = element_text(size = 14),  # Increase axis label size
    axis.text = element_text(size = 12),  # Increase axis tick label size
    legend.position = "none"  # Remove legend
  )

# Load the ggplot2 library for creating visualizations
library(ggplot2)

# Load your dataset (if not already loaded)
df <- read.csv("C:/Users/mariy/Downloads/flights.csv")

# Create a bar chart to visualize the distribution of flight delays (0 or 1)
ggplot(df, aes(x = delay)) +  # Map "delay" variable to x-axis
  geom_bar(stat = "count") +  # Create bar geom with "count" statistic (number of flights in each group)
  labs(title = "Distribution of Flight Delays (0 or 1)",  # Set chart title and labels
       x = "Delay",  # Label for x-axis (delay)
       y = "Frequency")  # Label for y-axis (number of flights)


# Load necessary libraries
library(ggplot2)
library(dplyr) # Assuming you might want to use it for any data manipulation

# Load the dataset from a CSV file
flights <- read.csv("C:/Users/mariy/Downloads/flights.csv")

# Check the names of the columns and the first few rows to ensure correct loading
print(names(flights))
head(flights)

# Create a bar chart to visualize the distribution of flights by origin airport
ggplot(flights, aes(x = origin)) +
  geom_bar(stat = "count", fill = "lightblue") +  # Set bar color
  labs(title = "Flights by Origin Airport",       # Set chart title
       x = "Origin Airport",                      # Label for x-axis
       y = "Frequency") +                        # Label for y-axis
  theme_minimal()                                # Apply a cleaner theme


