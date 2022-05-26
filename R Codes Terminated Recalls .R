# Difference between two population proportions of terminated recalls in year 2021 and 2020 
# H0: p1 >= p2
# Ha: p1 < p2

library("readxl") #to read .xlsx file

#import data
recalls_df = read_excel("Documents/BUS2_194A_Statistical_Analysis/Group_Project/terminated_recalls.xlsx")
head(recalls_df) #gives us a snapshot of the Excel file 

#for visualization purpose, 0 means "no" (has not been terminated), 1 means "yes" 
recallsEDA_df = read_excel("Documents/BUS2_194A_Statistical_Analysis/Group_Project/recallsEDA.xlsx") 
# Clean the data for 2021
recallsEDA1_df<- subset(recallsEDA_df, select = `2021`)
recallsEDA1_df
recallsEDA1_df <- na.omit(recallsEDA1_df)
recallsEDA1_df
# Clean data for 2020 
recallsEDA2_df<- subset(recallsEDA_df, select = `2020`)
recallsEDA2_df
recallsEDA2_df <- na.omit(recallsEDA2_df)
recallsEDA2_df

#EDA
summary(recallsEDA1_df) #This gives the descriptive statistics of the data
summary(recallsEDA2_df)
sapply(recallsEDA1_df, sd) #This gives us the standard deviation using the "sd" function
sapply(recallsEDA2_df, sd) 
xtabs(~`2021` + `2020`, data = recalls_df) #Creates a two-way contingency table of categorical outcome and predictors
hist(recallsEDA1_df$`2021`) #histogram 
hist(recallsEDA2_df$`2020`)

#pie chart for 2021 
recallsEDApie_df = read_excel("Documents/BUS2_194A_Statistical_Analysis/Group_Project/recallsEDApie.xlsx") 
labels = c("No", "Yes")
piepercent<- round(100 * recallsEDApie_df$`2021` / sum(recallsEDApie_df$`2021`), 1)
pie(recallsEDApie_df$`2021`,labels = piepercent, main = "Percentage of terminated recalls 2021", col = rainbow(length(recallsEDApie_df$`2021`)))
legend("topright", c("No","Yes"), cex = 0.5, fill = rainbow(length(recallsEDApie_df$`2021`)))
#pie chart for 2020
piepercent2<- round(100 * recallsEDApie_df$`2020` / sum(recallsEDApie_df$`2020`), 1)
pie(recallsEDApie_df$`2020`,labels = piepercent2, main = "Percentage of terminated recalls 2020", col = rainbow(length(recallsEDApie_df$`2020`)))
legend("topright", c("No","Yes"), cex = 0.5, fill = rainbow(length(recallsEDApie_df$`2020`)))

#Hypothesis test
# Clean the data for 2021
recalls2021_df<- subset(recalls_df, select = `2021`)
recalls2021_df

recalls2021_df <- na.omit(recalls2021_df)
recalls2021_df

# Look at the number of Yes in the data
yes2021 <- subset(recalls2021_df, `2021`=="Yes")
yes2021

num_yes2021 <- nrow(yes2021)
num_yes2021

# Look at the number of No in the data
no_2021 <- subset(recalls2021_df, `2021`=="No")
no_2021

num_no2021 <- nrow(no_2021)
num_no2021

# Count the samples of Yes and No
samp_size1 <- num_yes2021 + num_no2021
samp_size1

# Clean data for 2020 
recalls2020_df<- subset(recalls_df, select = `2020`)
recalls2020_df

recalls2020_df <- na.omit(recalls2020_df)
recalls2020_df

# Look at the number of Yes in the data
yes2020 <- subset(recalls2020_df, `2020` =="Yes")
yes2020

num_yes2020 <- nrow(yes2020)
num_yes2020

# Look at the number of No in the data
no_2020 <- subset(recalls2020_df, `2020`=="No")
no_2020

num_no2020 <- nrow(no_2020)
num_no2020

# Count the samples of Yes and No
samp_size2 <- num_yes2020 + num_no2020
samp_size2

# Perform the hypothesis test using the "prop.test" function, which is for population proportion
test <- prop.test(c(num_yes2021, num_yes2020), c(samp_size1, samp_size2), alternative = "less", 
                  conf.level = 0.95, correct = FALSE)
list(test)
# We reject H0 as the p-value is smaller than alpha (0.05) and conclude that percentage of terminated recalls 
# is indeed lower in 2021 than in 2020
#Our hypothesis testing conclusion is consistent with the report's interval estimation results that showed the
#interval estimate of the difference between the population proportions 2021 and 2020 to be -1.0000000 to -0.2296296, with 2021 
#having lower percentage of terminated recalls
