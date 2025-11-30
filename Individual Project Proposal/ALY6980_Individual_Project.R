# Import CSV file
df <- read.csv("C:/Users/seanm/OneDrive/Desktop/ALY6980 - Individual Project/Technographic Data .csv", stringsAsFactors = FALSE)

# View the first few rows of the dataset
head(df)

#1. Technology Adoption Over Time

library(ggplot2)
library(dplyr)

# Convert dates
df$First.Seen.At <- as.Date(df$First.Seen.At)
df$Last.Seen.At <- as.Date(df$Last.Seen.At)

# Count technologies by date
tech_trend <- df %>%
  group_by(First.Seen.At) %>%
  summarise(count = n())

# Plot
ggplot(tech_trend, aes(x = First.Seen.At, y = count)) +
  geom_line(color = "blue") +
  labs(title = "Technology Adoption Over Time", x = "Date", y = "Number of New Technologies Detected")

#2. Most Popular Technologies
top_tech <- df %>%
  count(Technology.Name, sort = TRUE) %>%
  top_n(10)

ggplot(top_tech, aes(x = reorder(Technology.Name, n), y = n, fill = Technology.Name)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Most Used Technologies", x = "Technology", y = "Count") +
  theme_minimal()

#3. Bar Chart for Website Domains
library(ggplot2)
library(dplyr)

# Count the number of occurrences for each website domain
domain_counts <- df %>%
  count(Website.Domain, sort = TRUE) %>%
  top_n(10)  # Get the top 10 domains for better visualization

# Plot the bar chart
ggplot(domain_counts, aes(x = reorder(Website.Domain, n), y = n, fill = Website.Domain)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Website Domains", x = "Website Domain", y = "Count") +
  theme_minimal()


#4. Firewall vs. Non-Firewall Technologies
library(ggplot2)

firewall_counts <- table(df$Behind.Firewall)

ggplot(as.data.frame(firewall_counts), aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Firewall vs. Non-Firewall Technologies")

#5. Bar Chart for Tickers
# Count the number of occurrences for each ticker
ticker_counts <- df %>%
  count(Ticker, sort = TRUE) %>%
  top_n(10)  # Get the top 10 tickers for better visualization

# Plot the bar chart
ggplot(ticker_counts, aes(x = reorder(Ticker, n), y = n, fill = Ticker)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Tickers", x = "Ticker", y = "Count") +
  theme_minimal()



# Check the structure of the dataframe to see data types of each column
str(df)

# Count unique data types in each column
cell_types <- sapply(df, class)

# Create a table showing the count of each data type per column
unique_types_count <- as.data.frame(table(cell_types))

# View the unique data types
print(unique_types_count)

# Optionally, display the unique types for each column
unique_types_per_column <- sapply(df, function(col) length(unique(col)))
print(unique_types_per_column)
View(df)
