library(httr)

#Getting the data

url = "https://www.topuniversities.com/rankings/endpoint?nid=3990755&page=0&items_per_page=1000&tab=indicators&region=&countries=&cities=&search=&star=&sort_by=rank&order_by=asc&program_type=&scholarship=&fee=&english_score=&academic_score=&mix_student=&loggedincache="
response = GET(url)           # Fetch the data from the URL
json_data = fromJSON(content(response, as = "text"))  # Parse the JSON content

#Defining the fields

Overall_Score = json_data[["score_nodes"]][["overall_score"]]
Title = json_data[["score_nodes"]][["title"]]
Country = json_data[["score_nodes"]][["country"]]
Region = json_data[["score_nodes"]][["region"]]
Rank = json_data[["score_nodes"]][["rank"]]

Academic_Rep = numeric(1000)
Cit_per_Faculty = numeric(1000)
Fac_Stu_Ratio = numeric(1000)
Employer_Rep = numeric(1000)
Employment_Outcomes = numeric(1000)
Int_Stu_Ratio = numeric(1000)
Int_Research_Net = numeric(1000)
Int_Fac_Ratio = numeric(1000)
Sustainability = numeric(1000)

#Scraping the data

for (i in 1:1000)
{
   Academic_Rep[i] = json_data[["score_nodes"]][["scores"]][["Research & Discovery"]][[i]][["score"]][1]
   Cit_per_Faculty[i] = json_data[["score_nodes"]][["scores"]][["Research & Discovery"]][[i]][["score"]][2]
   Fac_Stu_Ratio[i] = json_data[["score_nodes"]][["scores"]][["Learning Experience"]][[i]][["score"]][1]
   Employer_Rep[i] = json_data[["score_nodes"]][["scores"]][["Employability"]][[i]][["score"]][1]
   Employment_Outcomes[i] = json_data[["score_nodes"]][["scores"]][["Employability"]][[i]][["indicator_name"]][2]
   Int_Stu_Ratio[i] = json_data[["score_nodes"]][["scores"]][["Global Engagement"]][[i]][["score"]][1]
   Int_Research_Net[i] = json_data[["score_nodes"]][["scores"]][["Global Engagement"]][[i]][["score"]][2]
   Int_Fac_Ratio[i] = json_data[["score_nodes"]][["scores"]][["Global Engagement"]][[i]][["score"]][3]
   Sustainability[i] = json_data[["score_nodes"]][["scores"]][["Sustainability"]][[i]][["score"]][1]
}

#Creating the dataframe

df = data.frame(
  Column1 = Rank,
  Column2 = Title,
  Column3 = Overall_Score,
  Column4 = Country,
  Column5 = Region,
  Column6 = Academic_Rep,
  Column7 = Cit_per_Faculty,
  Column8 = Fac_Stu_Ratio,
  Column9 = Employer_Rep,
  Column10 = Employment_Outcomes,
  Column11 = Int_Stu_Ratio,
  Column12 = Int_Research_Net,
  Column13 = Int_Fac_Ratio,
  Column14 = Sustainability
)

colnames(df) = c("Rank",
                 "Title", 
                 "Overall",
                 "Country",
                 "Region",
                 "Academic Reputation",
                 "Citations per Faculty",
                 "Faculty Student Ratio",
                 "Employer Reputation",
                 "Employment Outcomes",
                 "International Student Ratio",
                 "International Research Network",
                 "International Faculty Ratio",
                 "Sustainabilty")

#Saving the data

save(df, file = "QS_Ranking_Parameters.RData")






