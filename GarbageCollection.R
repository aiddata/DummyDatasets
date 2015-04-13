#Data Generation Process - Ryan B. and Adam P.
#Treatment Binary - if garbage-truck labor is unionized
#Tech_Change =  TrtBin + FE(state) +  Time_after_unionization + Time_after_unionizatin * TrtBin + e
#Also include controls - time record:  state pop; # of major cities;
#total state area by square mile; income per capita; gas prices.
#Ultimately predicting: Impact of unionization on tech level.

beta_df <- data.frame(matrix(ncol=10, nrow=40))

colnames(beta_df)[9] <- "Year"
colnames(beta_df)[1] <- "Unionized"
colnames(beta_df)[2] <- "State"
colnames(beta_df)[3] <- "State_Pop"
colnames(beta_df)[4] <- "Major_Cities"
colnames(beta_df)[5] <- "Total_Size"
colnames(beta_df)[6] <- "Income_PerCap"
colnames(beta_df)[7] <- "Gas_Prices"
colnames(beta_df)[8] <- "Years_Post_Union"
colnames(beta_df)[10] <- "Tech_Level"

set.seed(42710311602)

start_year = 1980
end_year = 1999
cur_year = start_year
row_cnt = 1





while(cur_year <= end_year)
{
  print(cur_year)
  cur_year = cur_year + 1
  
  beta_df["Year"][row_cnt,] <- cur_year
  beta_df["Year"][(row_cnt+1),] <- cur_year
  
  beta_df["State"][row_cnt,] <- 1
  beta_df["State"][(row_cnt+1),] <- 0

  #Assign treatment "dates" to each unit.
  if(row_cnt > 15)
    {
   beta_df[1][(row_cnt-2),] <- 1
   beta_df[1][(row_cnt),] <- 1
   beta_df["Years_Post_Union"][row_cnt,] <- beta_df["Years_Post_Union"][(row_cnt-2),] + 1
    }
  else
   {
     beta_df[1][row_cnt,] <- 0
     beta_df["Years_Post_Union"][row_cnt,] <- 0
   }
   
   if(row_cnt > 23)
   {
     beta_df[1][(row_cnt-1),] <- 1
     beta_df[1][(row_cnt+1),] <- 1
     beta_df["Years_Post_Union"][(row_cnt+1),] <- beta_df["Years_Post_Union"][(row_cnt-1),] + 1
   }
   else
   {
     beta_df[1][(row_cnt+1),] <- 0
     beta_df["Years_Post_Union"][(row_cnt+1),] <- 0
   }
   
  
  if(row_cnt == 1)
  {
    beta_df["State_Pop"][row_cnt,] <- abs(rnorm(1) * 7500000)
    beta_df["State_Pop"][(row_cnt+1),] <- abs(rnorm(1) * 7500000)
    
    beta_df["Income_PerCap"][row_cnt,] <- round(abs(rnorm(1) * 50000),2)
    beta_df["Income_PerCap"][(row_cnt+1),] <- round(abs(rnorm(1) * 50000),2)
    
    beta_df["Gas_Prices"][row_cnt,] <- 0.75
    beta_df["Gas_Prices"][(row_cnt+1),] <- 0.82
    
    beta_df["Tech_Level"][row_cnt,] <- 3
    beta_df["Tech_Level"][(row_cnt+1),] <- 5
  }
  
  else
  {
    beta_df["State_Pop"][row_cnt,] <- (rnorm(1) * 10000) + beta_df["State_Pop"][(row_cnt-2),]
    beta_df["State_Pop"][(row_cnt+1),] <- (rnorm(1) * 10000) + beta_df["State_Pop"][(row_cnt-1),]
    
    beta_df["Income_PerCap"][row_cnt,] <- round((abs(rnorm(1)) * 100) + beta_df["Income_PerCap"][(row_cnt-2),],2)
    beta_df["Income_PerCap"][(row_cnt+1),] <- round((abs(rnorm(1)) * 100) + beta_df["Income_PerCap"][(row_cnt-1),],2)
    
    beta_df["Gas_Prices"][row_cnt,] <- round((abs(rnorm(1) * 0.15)) + beta_df["Gas_Prices"][(row_cnt-2),],2)
    beta_df["Gas_Prices"][(row_cnt+1),] <- round((abs(rnorm(1) * 0.15)) + beta_df["Gas_Prices"][(row_cnt-1),],2)
    
    beta_df["Tech_Level"][row_cnt,] <- round((abs(rnorm(1) * 0.15)) + beta_df["Tech_Level"][(row_cnt-2),],2)
    beta_df["Tech_Level"][(row_cnt+1),] <- round((abs(rnorm(1) * 0.15)) + beta_df["Tech_Level"][(row_cnt-1),],2)
  }
  
  beta_df["Major_Cities"][row_cnt,] <- ceiling(beta_df["State_Pop"][row_cnt,] / 1000000)
  beta_df["Major_Cities"][(row_cnt+1),] <- ceiling(beta_df["State_Pop"][(row_cnt+1),] / 1000000)
  
  beta_df["Total_Size"][row_cnt,] <- 26400
  beta_df["Total_Size"][(row_cnt+1),] <- 42000
  
  row_cnt = row_cnt + 2
  
}

beta_df["Tech_Level"] <- 1.5 + (beta_df["Unionized"]  * 0.5) + 
                         (beta_df["Years_Post_Union"] * 0.05) +
                         (beta_df["Years_Post_Union"] * beta_df["Unionized"] * .05) + 
                         (beta_df["Gas_Prices"] * .25) +
                         (beta_df["Income_PerCap"] * .000025)

noise <- runif(40,0,0.5)

beta_df["Tech_Level"] <- beta_df["Tech_Level"] + noise

write.csv(beta_df,"RyanB_AdamP.csv")

summary(lm(Tech_Level ~ Unionized + Years_Post_Union + (Years_Post_Union * Unionized) + State_Pop + 
                Major_Cities + Total_Size + Income_PerCap + Gas_Prices, data=beta_df))

