df1 <- get(load('Data/GDP.Rdata'))
df2 <- get(load('Data/QS_Rankings.Rdata'))
df3 <- get(load('Data/Times_Rankings.Rdata'))

# Make sure the Country column name is consistent
names(df1)[1] <- "Country"

# Clean up Country names in df2 for consistent comparison
df2$Country <- replace(df2$Country, df2$Country == "China (Mainland)", "China")
df2$Country <- replace(df2$Country, df2$Country == "Hong Kong SAR", "Hong Kong")
df2$Country <- replace(df2$Country, df2$Country == "Macau SAR", "Macao")

# Convert Overall column to numeric if it isn't already
df2$Overall <- as.numeric(df2$Overall)

# Create FemaleStudentRatio based on matching titles between df2 and df3
df2 <- df2 %>%
  left_join(df3 %>% select(Title, `Female Student Ratio (in %)`), by = "Title") %>%
  rename(FemaleStudentRatio = `Female Student Ratio (in %)`)

# Calculate the median of non-zero, non-NA values in FemaleStudentRatio
median_female_ratio <- median(df2$FemaleStudentRatio[df2$FemaleStudentRatio != 0], na.rm = TRUE)

# Replace 0 and NA values with the median value
df2$FemaleStudentRatio <- ifelse(
  is.na(df2$FemaleStudentRatio) | df2$FemaleStudentRatio == 0,
  median_female_ratio,
  df2$FemaleStudentRatio
)

# Add GDP per capita by matching on Country
df2 <- df2 %>%
  left_join(df1 %>% select(Country, `GDP per Capita (in $)`), by = "Country") %>%
  rename(GDPperCapita = `GDP per Capita (in $)`)

# Remove the Employment Outcomes column if it exists
df2$`Employment Outcomes` <- NULL

# Apply log transformation to GDPperCapita (log to base 10)
df2$GDPperCapita <- log10(df2$GDPperCapita)

# Normalize the log-transformed values to a score out of 100
# Check that max and min are different to avoid division by zero
if (max(df2$GDPperCapita, na.rm = TRUE) != min(df2$GDPperCapita, na.rm = TRUE)) {
  df2$GDPperCapita <- (df2$GDPperCapita - min(df2$GDPperCapita, na.rm = TRUE)) / 
    (max(df2$GDPperCapita, na.rm = TRUE) - min(df2$GDPperCapita, na.rm = TRUE)) * 100
} else {
  df2$GDPperCapita <- 100  # If all values are the same, assign 100
}

library(httr)
library(jsonlite)
#Getting the data

url = "https://www.topuniversities.com/rankings/endpoint?nid=3990755&page=0&items_per_page=1000&tab=indicators&region=&countries=&cities=&search=&star=&sort_by=rank&order_by=asc&program_type=&scholarship=&fee=&english_score=&academic_score=&mix_student=&loggedincache="
response = GET(url)           # Fetch the data from the URL
json_data = fromJSON(content(response, as = "text"))  # Parse the JSON content

Logo = json_data[["score_nodes"]][["logo"]]

rank <- 1:1000
link <- Logo

df2$ImageLink <- link
# Save the updated dataframe to CSV
write.csv(df2, file = 'global_data.csv', row.names = FALSE)
