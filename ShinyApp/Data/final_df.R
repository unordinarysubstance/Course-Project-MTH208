df1 <- get(load('Data/GDP.Rdata'))
df2 <- get(load('Data/QS_Rankings.Rdata'))
df3 <- get(load('Data/Times_Rankings.Rdata'))


FemaleStudentRatio = numeric(length = 1e3)
for(i in 1:length(df2$Title))
{
  for(j in 1:length(df3$Title))
  {
    if(df2$Title[i] == df3$Title[j])
    {
      FemaleStudentRatio[i] = df3$`Female Student Ratio (in %)`[j] 
    } 
  }
}


GDPperCapita = numeric(length = 1e3)
for(i in 1:length(df2$Country))
{
  for(j in 1:length(df1$Countries))
  {
    if(df2$Country[i] == df1$Countries[j])
    {
      
      GDPperCapita[i] = df1$`GDP per Capita (in $)`[j]
    }
  }
}

df2 <- df2 %>% mutate(GDPperCapita) 
df2 <- df2 %>% mutate(FemaleStudentRatio)
df2$`Employment Outcomes` <- NULL 

write.csv(df2 , file = 'global_data.csv' , row.names = FALSE)
