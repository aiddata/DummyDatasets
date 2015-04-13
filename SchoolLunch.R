#DGP - Hansen, Sullivan, Ledbetter
#Outcome: Cognitize Abilities
#Treatment: Free School Lunches

beta_df <- data.frame(matrix(ncol=14, nrow=4000))


colnames(beta_df)[1] <- "Baseline_Score"
colnames(beta_df)[2] <- "End_Year_Score"
colnames(beta_df)[3] <- "School"
colnames(beta_df)[4] <- "Gender"
colnames(beta_df)[5] <- "Guardians"
colnames(beta_df)[6] <- "Guardian_Relationship"
colnames(beta_df)[7] <- "Guardian_Relative"
colnames(beta_df)[8] <- "Guardian_Marital"
colnames(beta_df)[9] <- "Last_Year_School_Attendance"
colnames(beta_df)[10] <- "Urban_Rural"
colnames(beta_df)[11] <- "County"
colnames(beta_df)[12] <- "HH_Income"
colnames(beta_df)[13] <- "FreeSchoolLunch"
colnames(beta_df)[14] <- "Year"

set.seed(42710311602)

start_year = 2011
end_year = 2014
cur_year = start_year
yr_cnt = 0





while(cur_year <= end_year)
{
  print(cur_year)

  
  cnt = 0
  
  while (cnt <= 1000)
  {
    row_id = cnt + (yr_cnt * 1000)
    beta_df["Year"][row_id,] <- cur_year
    beta_df["Baseline_Score"][row_id,] <- abs(rnorm(1)*100)
    beta_df["School"][row_id,] <- sample(1:10)[1]
    beta_df["Gender"][row_id,] <- sample(1:2)[1]
    beta_df["Guardians"][row_id,] <- sample(1:3)[1]
    beta_df["Guardian_Relationship"][row_id,] <- sample(0:1)[1]
    beta_df["Guardian_Marital"][row_id,] <- sample(0:1)[1]
    beta_df["Guardian_Relative"][row_id,] <- sample(0:1)[1]
    beta_df["Last_Year_School_Attendance"][row_id,] <- sample(0:1)[1]
    beta_df["Urban_Rural"][row_id,] <- sample(0:1)[1]
    
    beta_df["County"][row_id,] <-  round(beta_df["School"][row_id,] / 2,0)
    beta_df["HH_Income"][row_id,] <- sample(10000:24000)[1]
    beta_df["Urban_Rural"][row_id,] <- sample(0:1)[1]
    beta_df["FreeSchoolLunch"][row_id,] <- 0
    if(cur_year == end_year)
    {
      beta_df["FreeSchoolLunch"][row_id,] <- sample(0:1)[1]
    }
    cnt = cnt + 1
  }
  cur_year = cur_year + 1
  yr_cnt = yr_cnt + 1
}

beta_df <- cbind(Student_ID = rownames(beta_df), beta_df)

beta_df["End_Year_Score"] <- 5.0 + 
                              (.80 * beta_df["Baseline_Score"]) +
                              (2.5 * beta_df["Guardians"]) +
                              (2.0 * beta_df["Last_Year_School_Attendance"]) +
                              (.001 * beta_df["HH_Income"]) +
                              (20.0 * beta_df["FreeSchoolLunch"]) +
                              runif(1,0,20)

noise <- runif(4000,5,50)

beta_df["End_Year_Score"] <- beta_df["End_Year_Score"] + noise

write.csv(beta_df,"Hansen_Sullivan_Ledbetter.csv")

summary(lm(End_Year_Score ~ Baseline_Score + Guardians + Last_Year_School_Attendance + HH_Income + FreeSchoolLunch, data=beta_df))

