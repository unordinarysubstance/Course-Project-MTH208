library(rvest)
library(dplyr)

# Read the HTML file

html =  read_html("https://www.worldometers.info/gdp/gdp-by-country/")


#Scraping and Cleaning the data

Countries = html %>% html_nodes('table tbody tr td:nth-child(2) a') %>% html_text()
GDP = gsub("[^0-9.]", "", html %>% html_nodes('table tbody tr td:nth-child(3)') %>% html_text())
Population = gsub("[^0-9.]", "", html %>% html_nodes('table tbody tr td:nth-child(6)') %>% html_text())
GDP_per_capita = gsub("[^0-9.]", "", html %>% html_nodes('table tbody tr td:nth-child(7)') %>% html_text())

#Creating the dataframe

GDP = (as.numeric(GDP))/1e+9
Population = as.numeric(Population)
GDP_per_capita = as.numeric(GDP_per_capita)

df = data.frame( Column1 = Countries,
                 Column2 = GDP,
                 Column3 = GDP_per_capita,
                 Column4 = Population
                  ) 
colnames(df) = c("Countries",
                 "GDP (in billion $)",
                 "GDP per Capita (in $)",
                 "Population")

#Saving the data

save(df, file = "GDP.RData")







