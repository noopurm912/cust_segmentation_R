# Customer Segmentation using RFM analysis and K-means clustering
# ANLY 506-51- B-2021/Summer-Exploratory Data Analysis
# Course Final Project
#Submitted to: Dr. Doaa Taha
# Author : Noopur Mishra
##############################

# Clear the environment
rm(list = ls())

# Setting work directory
setwd("C:\\Users\\itsni\\OneDrive - Harrisburg University\\Semester 5\\Data Analysis\\Project2")

# Install packages and libraries
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)

# Read the CSV data file 
eCommData <- read.csv("C:\\Users\\itsni\\OneDrive - Harrisburg University\\Semester 5\\Data Analysis\\Project2\\e-commerce_data.csv")
dim(eCommData)

# Data pre-processing steps

# Check data set description  
glimpse(eCommData)

#############################

# Find the total null values in the data set
table(is.na(eCommData))

# Find the total null values in each column
colSums(is.na(eCommData))

# Remove the null values
eCommData <- na.omit(eCommData)

# Keep unique rows 
eCommData %>% distinct()

# Check summary of the data set to analyze numerical columns
summary(eCommData)

# Remove observations with negative values in Quantity column
dim(eCommData[eCommData$Quantity < 0, ])
eCommData <- eCommData[eCommData$Quantity > 0, ]

# Remove observations with 0 values in UnitPrice column
dim(eCommData[eCommData$UnitPrice == 0, ])
eCommData <- eCommData[eCommData$UnitPrice > 0, ]
dim(eCommData)

# Add a new column "TotalInvoicePrice" to calculate the the total invoice amount:  UnitePrice * Quantity
eCommData$TotalInvoicePrice <- eCommData$UnitPrice * eCommData$Quantity

# Convert the InvoiceDate column from character type to datetime type
eCommData$InvoiceDateTime = mdy_hm(eCommData$InvoiceDate)

# Extract date, month, year, weekday, and hour from invoice date
eCommData$InvoiceFullDate <- date(eCommData$InvoiceDateTime)
eCommData$InvoiceMonth <- as.factor(month(eCommData$InvoiceDateTime, label = TRUE, abbr = FALSE))
eCommData$InvoiceWeekDay <- as.factor(wday(eCommData$InvoiceDateTime, label = TRUE, abbr = FALSE))
eCommData$InvoiceYear <- as.factor(year(eCommData$InvoiceDateTime))
levels(eCommData$InvoiceYear) <- c(2010,2011)
eCommData$HourOfDay <- as.factor((hour(eCommData$InvoiceDateTime)))

# Delete the original InvoiceDate column
eCommData <- within(eCommData, rm(InvoiceDate))

# Change the Customer ID and Country column from character type to factor
eCommData$Country <- as.factor(eCommData$Country)
eCommData$CustomerID <- as.factor(eCommData$CustomerID)

# Write the data
write.csv(eCommData, 
          "C:\\Users\\itsni\\OneDrive - Harrisburg University\\Semester 5\\Data Analysis\\Project2\\cleanedEcommData.csv")

# Filter the transactions for the United Kingdom customers
eCommDataUK <- eCommData %>% filter(eCommData$Country == "United Kingdom")
dim(eCommDataUK)
glimpse(eCommData)

#write the UK data
write.csv(eCommDataUK, 
          "C:\\Users\\itsni\\OneDrive - Harrisburg University\\Semester 5\\Data Analysis\\Project2\\eCommDataUK.csv")

#############################################
# Data Visualization
install.packages("sqldf")
library(sqldf)
install.packages("ggplot2")
library(ggplot2)

Top5_Countries2010 <- sqldf("SELECT Country, sum(TotalInvoicePrice) as TotalInvoicePrice FROM eCommData
      WHERE InvoiceYear = 2010
      GROUP BY Country
      ORDER BY sum(TotalInvoicePrice) DESC
      LIMIT 5")
Top5_Countries2010
######################################
Top5_Countries2011 <- sqldf("SELECT Country, 
      sum(TotalInvoicePrice) as TotalInvoicePrice 
      FROM eCommData
      WHERE InvoiceYear = 2011
      GROUP BY Country
      ORDER BY sum(TotalInvoicePrice) DESC
      LIMIT 5")
Top5_Countries2011
##############################################
# Number of daily transaction for each country
DailyTransaction <- sqldf("SELECT Country, Count(InvoiceNo) as TransactionCount FROM eCommData
      GROUP BY Country
      ORDER BY TransactionCount DESC
                          LIMIT 10")
DailyTransaction

ggplot(DailyTransaction, 
       aes(x=Country, y=TransactionCount))+
  geom_point(stat = "identity", color="blue")+
  geom_text(label=DailyTransaction$TransactionCount, nudge_x = 0.35, check_overlap = TRUE)+
  ggtitle("Transaction count for each country")
  
par(mar=c(2,2,2,2))
#Top 10 customerIDs by TotalInvoicePrice

Top10_CustomerID <- sqldf("SELECT CustomerID, sum(TotalInvoicePrice) as TotalInvoicePrice 
      FROM eCommDataUK
      GROUP BY CustomerID
      ORDER BY sum(TotalInvoicePrice) DESC
      LIMIT 10")
Top10_CustomerID
barplot(Top10_CustomerID$TotalInvoicePrice,
        main = "Top 10 Customer ID by maximum revenue",
        xlab = "Customer ID",
        ylab = "Revenue",
        names.arg = Top10_CustomerID$CustomerID,
        col = "lightblue",)

####################################################
# Monthly distribution of the UK transaction

monthTransactionUK <- sqldf("SELECT InvoiceMonth, 
                count(InvoiceNo) as Number_Of_Transaction
                FROM eCommDataUK
                GROUP BY InvoiceMonth
                ORDER BY Number_Of_Transaction DESC
                ")
monthTransactionUK
ggplot(monthTransactionUK, 
       aes(x=InvoiceMonth, y=Number_Of_Transaction))+
  geom_point(color="blue")+
  geom_text(label=monthTransactionUK$Number_Of_Transaction, nudge_x = .35, check_overlap = TRUE)+
  ggtitle("Number of Transactions-Monthwise")

####################################################
# Day-wise distribution of the UK transactions

dayTransactionUK <- sqldf("SELECT InvoiceWeekDay, count(InvoiceNo) as Number_Of_Transaction
                FROM eCommDataUK
                GROUP BY InvoiceWeekDay
                ORDER By Number_Of_Transaction DESC
                ")
ggplot(dayTransactionUK, aes(x=InvoiceWeekDay, y=Number_Of_Transaction))+
  geom_point(color="blue")+
  geom_text(label=dayTransactionUK$Number_Of_Transaction, nudge_x = .35, check_overlap = TRUE)+
  ggtitle("Number of Transactions-Daywise")

#####################################################
# Hourly distribution of the UK transactions

hourTransactionUK <- sqldf("SELECT HourOfDay, count(InvoiceNo) as Number_Of_Transaction
                FROM eCommDataUK
                GROUP BY HourOfDay
                ORDER By Number_Of_Transaction DESC
                ")
hourTransactionUK$HourOfDay <- as.numeric(as.character(hourTransactionUK$HourOfDay))

ggplot(hourTransactionUK, aes(x=HourOfDay, y=Number_Of_Transaction))+
  geom_point(color="black")+
  geom_line(color="blue")+
  geom_text(label=hourTransactionUK$Number_Of_Transaction, nudge_x = .55, check_overlap = TRUE)+
  ggtitle("Number of Transactions-Hourwise")
########################################################################################


# Customer segmentation through RFM analysis
install.packages("rfm")
library(rfm)

# Check the recent transaction date
max(eCommDataUK$InvoiceFullDate)

analysis_date <- as.Date("2011-12-10")

rfm_result <- rfm_table_order(eCommDataUK, CustomerID, 
                              InvoiceFullDate, TotalInvoicePrice, 
                              analysis_date)

# check the class for rfm_result
class(rfm_result)

# write rfm_result in a CSV file 
write.csv(rfm_result$rfm, 
          "C:\\Users\\itsni\\OneDrive - Harrisburg University\\Semester 5\\Data Analysis\\Project2\\rfm_results.csv")

# Read the rfm dataset 
rfm_eCommDataUK <- read.csv("C:\\Users\\itsni\\OneDrive - Harrisburg University\\Semester 5\\Data Analysis\\Project2\\rfm_results.csv")
glimpse(rfm_eCommDataUK)

# Rename the recency_days,transaction_count, and amount columns to Recency, Frequency, and Monetary values
rfm_eCommDataUK <-
  rename(
    rfm_eCommDataUK,
    Recency_value= recency_days,
    Frequency_value= transaction_count,
    Monetary_value=amount,
    )

# Change calculation for rfm_score
rfm_eCommDataUK$rfm_score = rfm_eCommDataUK$recency_score+rfm_eCommDataUK$frequency_score+rfm_eCommDataUK$monetary_score

# Delete the date_most_recent column from the data set
rfm_eCommDataUK <- within(rfm_eCommDataUK, rm(date_most_recent))

glimpse(rfm_eCommDataUK)

# RFM score will be from 3 to 15
describe(rfm_eCommDataUK$rfm_score)

# Assign Segment to each customer
#Platinum(Loyal): 13-15, gold(potential loyal): 10-12, silver(Promising): 7-9, bronze(need attention):3-6
# cut() function will start from 3
rfm_eCommDataUK$Customer_Segment <- cut(rfm_eCommDataUK$rfm_score,
                                        breaks = c(2, 6, 9, 12, Inf),
                                    labels = c("Bronze(Need Attention)", 
                                              "Silver(Promising)", 
                                              "Gold(Potential loyal)", 
                                              "Platinum(Loyal)"))

write.csv(rfm_eCommDataUK, "C:\\Users\\itsni\\OneDrive - Harrisburg University\\Semester 5\\Data Analysis\\Project2\\rfmResultsCustomerSegments.csv")

# Plotting the RFM customer segment data set
install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)

# Check customers in each segments
table(rfm_eCommDataUK$Customer_Segment)

# Bar plot for RFM customer segmentation
png(file = "barchart_CS.png")
ggplot(rfm_eCommDataUK) +geom_bar(aes(x = Customer_Segment, fill = Customer_Segment)) +theme(axis.text.x=element_text(angle=45,hjust=1)) +labs(title = "Customer Segmentation using RFM analysis")
dev.off()

# scatter plot for RFM customer segmentation
p <- plot_ly(rfm_eCommDataUK, x=~Recency_value, y=~Monetary_value, 
             z=~Frequency_value, color=~Customer_Segment) %>% 
  add_markers(size=1.5)
print(p)

######################################################################
#Segmentation using K-means algorithm

# Create new columns Recency, Frequency, and Monetary from RFM values for normalization and scaling purpose

rfm_eCommDataUK$Recency = rfm_eCommDataUK$Recency_value
rfm_eCommDataUK$Frequency = rfm_eCommDataUK$Frequency_value
rfm_eCommDataUK$Monetary = rfm_eCommDataUK$Monetary_value

# Check the distribution in Recency, Frequency, and Monetory value columns

# Install package for describe function
install.packages("psych")
library(psych)
par(mar=c(3,3,3,3))

# Recency Distribution Plot
describe(rfm_eCommDataUK$Recency)
hist(rfm_eCommDataUK$Recency, 
     main = "Recency Distribution Plot", 
     col = rgb(1, 0, 0, 0.4), 
     freq = FALSE,
     xlab="Recency",
     ylab = "Density",
     border ="black")
lines(density(rfm_eCommDataUK$Recency), lwd = 2, col = 'black')

# Frequency Distribution Plot
describe(rfm_eCommDataUK$Frequency)
hist(rfm_eCommDataUK$Frequency, 
     main = "Frequency Distribution Plot", 
     col = rgb(1, 0, 0, 0.5), 
     freq = FALSE,
     xlab="Frequency",
     ylab = "Density",
     border ="black")
lines(density(rfm_eCommDataUK$Frequency), lwd = 2, col = 'black')

# Monetary Distribution Plot 
describe(rfm_eCommDataUK$Monetary)
hist(rfm_eCommDataUK$Monetary, 
     main = "Monetary Distribution Plot", 
     col = rgb(1, 0, 0, 0.5), 
     freq = FALSE,
     xlab="Monetary",
     ylab = "Density",
     border ="black")
lines(density(rfm_eCommDataUK$Monetary), lwd = 2, col = 'black')
##########################################################################

# Distribution shows that data is right skewed(positive skewed))
# Using log-transformation to remove the right-skewness in Recency, Frequency, and Monetary columns 
rfm_eCommDataUK[c("Recency","Frequency","Monetary")] <- lapply(rfm_eCommDataUK[c("Recency","Frequency","Monetary")], log)

# rfm_eCommDataUK[11:13] <- lapply(rfm_eCommDataUK[11:13], log)

# Scaling the data to get R, F, and M on same level
rfm_eCommDataUK[c("Recency","Frequency","Monetary")] <- as.data.frame(scale(rfm_eCommDataUK[c("Recency","Frequency","Monetary")]))
rfm_eCommDataUK[c("Recency","Frequency","Monetary")] <- round(rfm_eCommDataUK[c("Recency","Frequency","Monetary")], 3)
glimpse(rfm_eCommDataUK)

# Check distribution again for Recency, Frequency, Monetary
# Recency
hist(rfm_eCommDataUK$Recency, 
     main = "Recency Distribution Plot", 
     col = rgb(1, 0, 0, 0.5), 
     freq = FALSE,
     xlab="Recency",
     ylab = "Density",
     border ="black")
lines(density(rfm_eCommDataUK$Recency), lwd = 2, col = 'red')

#Frequency
hist(rfm_eCommDataUK$Frequency, 
     main = "Frequency Distribution Plot", 
     col = rgb(1, 0, 0, 0.5), 
     freq = FALSE,
     xlab="Frequency",
     ylab = "Density",
     border ="black")
lines(density(rfm_eCommDataUK$Frequency), lwd = 2, col = 'red')

# Monetary
hist(rfm_eCommDataUK$Monetary, 
     main = "Monetary Distribution Plot", 
     col = rgb(1, 0, 0, 0.5), 
     freq = FALSE,
     xlab="Monetary",
     ylab = "Density",
     border ="black")
lines(density(rfm_eCommDataUK$Monetary), lwd = 2, col = 'red')

####################################################################################
# Find optimal number of clusters for the data set: Elbow method and Silhouette Method

install.packages("factoextra")
library(factoextra)

# Elbow method to find optimal number of clusters(k) k = 3"
fviz_nbclust(rfm_eCommDataUK[c("Recency","Frequency","Monetary")], kmeans,method = "wss",nstart=50)

################################

# Silhouette Method (gave k=2 as usual)
fviz_nbclust(rfm_eCommDataUK[c("Recency","Frequency","Monetary")], kmeans, method = 'silhouette',nstart=50)


#################################################

# Fit k-means clustering model on normalized R, F, and M values
set.seed(1234)
kMeanResult <- kmeans(rfm_eCommDataUK[c("Recency","Frequency","Monetary")], centers = 3 ,nstart = 50 ,iter.max = 1000)
rfm_eCommDataUK$Cluster <- kMeanResult$cluster
class(rfm_eCommDataUK$Cluster)
rfm_eCommDataUK$Cluster <- as.factor(rfm_eCommDataUK$Cluster)
glimpse(rfm_eCommDataUK)
table(rfm_eCommDataUK$Cluster)

# Plot the clusters on scatter plot

install.packages("plotly")
library(plotly)
p <- plot_ly(rfm_eCommDataUK, x=~Recency, y=~Monetary, 
             z=~Frequency, color=~Cluster)  
p <- p %>% add_markers(size=1.5) 
p <- p %>% layout(title="K-means clusters for the UK transactions")
p

# Write the final data set 
write.csv(rfm_eCommDataUK, "C:\\Users\\itsni\\OneDrive - Harrisburg University\\Semester 5\\Data Analysis\\Project2\\final_data_rfm_kmeans.csv")

#################################################
# Check Data point in each cluster
rfm_cluster1 <- rfm_eCommDataUK %>% filter(rfm_eCommDataUK$Cluster == 1)
rfm_cluster2 <- rfm_eCommDataUK %>% filter(rfm_eCommDataUK$Cluster == 2)
rfm_cluster3 <- rfm_eCommDataUK %>% filter(rfm_eCommDataUK$Cluster == 3)
write.csv(rfm_cluster1, "C:\\Users\\itsni\\OneDrive - Harrisburg University\\Semester 5\\Data Analysis\\Project2\\rfm_cluster1.csv")
write.csv(rfm_cluster2, "C:\\Users\\itsni\\OneDrive - Harrisburg University\\Semester 5\\Data Analysis\\Project2\\rfm_cluster2.csv")
write.csv(rfm_cluster3, "C:\\Users\\itsni\\OneDrive - Harrisburg University\\Semester 5\\Data Analysis\\Project2\\rfm_cluster3.csv")
##################################################
