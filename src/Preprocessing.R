#attach the dataset

library(readxl)
file_path <-"C:/Users/danuk/OneDrive/Desktop/15555-TakeHome2/worldwide disasters.csv"
df <- read.csv(file_path)
attach(df)
View(df)

# Replace NaN values with zeros for specific columns
df$Total.Deaths[is.na(df$Total.Deaths)] <- 0
df$No.Injured[is.na(df$No.Injured)] <- 0
df$No.Affected[is.na(df$No.Affected)] <- 0
df$No.Homeless[is.na(df$No.Homeless)] <- 0
df$Total.Affected[is.na(df$Total.Affected)] <- 0
df$Total.Damages[is.na(df$Total.Damages)] <- 0
df$Reconstruction.Costs[is.na(df$Reconstruction.Costs)] <- 0
df$Insured.Damages[is.na(df$Insured.Damages)] <- 0

# Define the breaks for the categories
breaks <- c(1970, 1981, 1991, 2001,2011,2021)

# Define the labels for the categories
labels <- c("1970-1980", "1981-1991","1992-2001", "2002-2011",'2012-2021')

# Create a new variable "YearCat" by categorizing "Year" into the specified ranges
df$YearCat <- cut(df$Year, breaks = breaks, labels = labels, include.lowest = TRUE)
View(df)
write.csv(df, "clean_data.csv", row.names = FALSE)
