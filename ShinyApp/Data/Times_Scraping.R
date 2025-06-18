library(httr)
library(jsonlite)
library(ggplot2)
#Getting the data

url = "https://www.timeshighereducation.com/sites/default/files/the_data_rankings/world_university_rankings_2024_0__91239a4509dc50911f1949984e3fb8c5.json"
response = GET(url)           # Fetch the data from the URL
json_data = fromJSON(content(response, as = "text"))  # Parse the JSON content

#Scraping the required data

Title = json_data[["data"]][["name"]]
Female_Ratio = sapply(strsplit(json_data[["data"]][["stats_female_male_ratio"]], ":"),
                      function(x) as.numeric(trimws(x[1])))

df=data.frame(Column1 = c(1:2671),
              Column2 = Title,
              Column3 = Female_Ratio)
colnames(df) = c("Rank", "Title", "Female Student Ratio (in %)")

#Saving the Data

save(df, file = "Times_Rankings.RData")
        
  
  






