#IP6
#Christina Porter
#Version 1.0
#3/17/2021
#This project uses and analyses the exoplanet dataset from the Kepler Observatory.
#https://www.kaggle.com/nasa/kepler-exoplanet-search-results


#imports
library(datasets)
library(tidyverse)
library(psych)
library(reshape2)

#import dataset from csv
exoplanet <- read.csv("cumulative.csv")

#getting rid of rowid since it seems synonymous to the index
exoplanet <- subset(exoplanet, select=-c(rowid))

#renaming my columns so they seem more readable than the scientific version
names(exoplanet)[1]<-"Kepler_ID"
names(exoplanet)[2]<-"Object_Name"
names(exoplanet)[3]<-"Kepler_Name"
names(exoplanet)[4]<-"EArchive_Disposition"
names(exoplanet)[5]<-"Kepler_Disposition"
names(exoplanet)[6]<-"Disposition_Score"
names(exoplanet)[7]<-"Not_TL_Flag"
names(exoplanet)[8]<-"SE_Flag"
names(exoplanet)[9]<-"CO_Flag"
names(exoplanet)[10]<-"Contamination_Flag"
names(exoplanet)[11]<-"Orbital_Period"
names(exoplanet)[12]<-"Pos_Orbital_Error"
names(exoplanet)[13]<-"Neg_Orbital_Error"
names(exoplanet)[14]<-"Transit_Epoch"
names(exoplanet)[15]<-"Pos_TransitE_Error"
names(exoplanet)[16]<-"Neg_TransitE_Error"
names(exoplanet)[17]<-"Impact_Parameter"
names(exoplanet)[18]<-"Pos_Impact_Error"
names(exoplanet)[19]<-"Neg_Impact_Error"
names(exoplanet)[20]<-"Transit_Duration"
names(exoplanet)[21]<-"Pos_TransitD_Error"
names(exoplanet)[22]<-"Neg_TransitD_Error"
names(exoplanet)[23]<-"Transit_Depth"
names(exoplanet)[24]<-"Pos_Depth_Error"
names(exoplanet)[25]<-"Neg_Depth_Error"
names(exoplanet)[26]<-"Planetary_Radius"
names(exoplanet)[27]<-"Pos_Radius_Error"
names(exoplanet)[28]<-"Neg_Radius_Error"
names(exoplanet)[29]<-"Equil_Temp"
names(exoplanet)[30]<-"Pos_Temp_Error"
names(exoplanet)[31]<-"Neg_Temp_Error"
names(exoplanet)[32]<-"Insol_Flux"
names(exoplanet)[33]<-"Pos_Insol_Error"
names(exoplanet)[34]<-"Neg_Insol_Error"
names(exoplanet)[35]<-"Signal_to_Noise"
names(exoplanet)[36]<-"TCE_Planet_Number"
names(exoplanet)[37]<-"TCE_Deliv_Name"
names(exoplanet)[38]<-"Stellar_Effective_Temp"
names(exoplanet)[39]<-"Pos_StelTemp_Error"
names(exoplanet)[40]<-"Neg_StelTemp_Error"
names(exoplanet)[41]<-"Stellar_Surface_Gravity"
names(exoplanet)[42]<-"Pos_StelGrav_Error"
names(exoplanet)[43]<-"Neg_StelGrav_Error"
names(exoplanet)[44]<-"Stellar_Radius"
names(exoplanet)[45]<-"Pos_StelRad_Error"
names(exoplanet)[46]<-"Neg_StelRad_Error"
names(exoplanet)[47]<-"Right_Ascension"
names(exoplanet)[48]<-"Declination"
names(exoplanet)[49]<-"Band_Magnitude"

#calculating how many na and non-nda values are in each column
navalues = data.frame(exoplanet %>% summarise_all(~ sum(is.na(.))))
nonNaValues = data.frame(exoplanet %>% summarise_all(~ sum(!is.na(.))))

columnList <- c()
percentageList <- c()

#Calculating the proportion of na values in my dataset for each column.
for(i in 1:ncol(navalues)){
  name <- names(exoplanet)[i]
  percentage <- (navalues[1, i]/nonNaValues[1, i])*100
  columnList <- c(columnList, name)
  percentageList <- c(percentageList, percentage)
}

#placing my proportion columns in a dataframe
percentageCol <- data.frame()
percentageCol <- data.frame(columnList, percentageList)

#Gathering the class of each of my variables
dtypeList <- c(lapply(exoplanet, class))

#Manually setting the general data types
ord_dtype <- c("nominal", "nominal", "nominal", "nominal", "nominal", "interval", "interval", "interval", 
               "interval", "interval", "interval", "interval", "interval", "ratio", "ratio", "ratio", "ratio", "ratio", "ratio", 
               "interval", "interval", "interval", "ratio", "ratio", "ratio", "ratio", "ratio", "ratio", "interval", 
               "interval", "interval", "interval", "interval", "interval", "ratio", "nominal", "nominal", "interval", 
               "interval", "interval", "ratio", "ratio", "ratio", "ratio", "ratio", "ratio", "ratio", "ratio", "ratio")

#Adding the data types to my percentage dataframe
percentageCol$df_dtype <- dtypeList

#Adding the general data types to my percentage dataframe
percentageCol$var_dtype <- ord_dtype

#Renaming the columns
names(percentageCol)[1] <- "Category"
names(percentageCol)[2] <- "Missing Data %"
names(percentageCol)[3] <- "Data Type in CSV"
names(percentageCol)[4] <- "General Data Type"

#Reodering the columns to match the order they were in for IP5
columns <- c("Category", "General Data Type", "Data Type in CSV", "Missing Data %")
percentageCol <- percentageCol[,columns]

#Creating my summary stats
summaryStats <- describe(exoplanet)
data.frame(summaryStats)

#Placing all my nominal values in their own dataframe so I can find the proportions for them
nominal_data <- data.frame("Kepler_ID", "Object_Name", "Kepler_Name", "EArchive_Disposition", "Kepler_Disposition", "TCE_Planet_Number", "TCE_Deliv_Name")

#Finding the proportions for each unique value in my nominal columns,
#calculating the percentage that value represents, and setting the
#results to a dataframe
for(i in 1:ncol(nominal_data)){
  name <- nominal_data[1,i]
  df_name <- paste(name, "Proportions", sep="_", collapse = NULL)
  count <- as.data.frame(c(table(exoplanet[name])))
  variable <- as.data.frame(row.names(count))
  proportion <- as.data.frame(c(round(table(exoplanet[name])/nrow(exoplanet)*100, digits = 4)))
  assign(df_name, as.data.frame(c(variable, count, proportion)))
}

#renaming the columns for each dataframe
names(Kepler_Disposition_Proportions)[1] = "Kepler_Disposition"
names(Kepler_Disposition_Proportions)[2] = "count"
names(Kepler_Disposition_Proportions)[3] = "proportion"
names(Kepler_ID_Proportions)[1] = "Kepler_ID"
names(Kepler_ID_Proportions)[2] = "count"
names(Kepler_ID_Proportions)[3] = "proportion"
names(Object_Name_Proportions)[1] = "Object_Name"
names(Object_Name_Proportions)[2] = "count"
names(Object_Name_Proportions)[3] = "proportion"
names(Kepler_Name_Proportions)[1] = "Kepler_Name"
names(Kepler_Name_Proportions)[2] = "count"
names(Kepler_Name_Proportions)[3] = "proportion"
names(EArchive_Disposition_Proportions)[1] = "EArchive_Disposition"
names(EArchive_Disposition_Proportions)[2] = "count"
names(EArchive_Disposition_Proportions)[3] = "proportion"
names(TCE_Planet_Number_Proportions)[1] = "TCE_Planet_Number"
names(TCE_Planet_Number_Proportions)[2] = "count"
names(TCE_Planet_Number_Proportions)[3] = "proportion"
names(TCE_Deliv_Name_Proportions)[1] = "TCE_Deliv_Name"
names(TCE_Deliv_Name_Proportions)[2] = "count"
names(TCE_Deliv_Name_Proportions)[3] = "proportion"

#finding the correlation values for each value
corr_dataframe = subset(exoplanet, select=-c(Kepler_Disposition, Kepler_ID, Object_Name, Kepler_Name, EArchive_Disposition, TCE_Planet_Number, TCE_Deliv_Name))
corr_matrix <- round(cor(corr_dataframe),2)

#creating my heatmap
exoplanet_heatmap <- heatmap(corr_matrix, Colv = NA, Rowv = NA, scale = "column")

#Exploring the Dataset

#Bar chart of the EArchive Disposition
ggplot(exoplanet, mapping = aes(x=EArchive_Disposition)) +
  geom_bar(stat = "count", aes(fill = EArchive_Disposition))+
  ggtitle("EArchive Disposition Counts")

#Bar Chart of the Kepler Disposition
ggplot(exoplanet, mapping = aes(x=Kepler_Disposition)) +
  geom_bar(stat = "count", aes(fill = Kepler_Disposition))+
  ggtitle("Kepler Disposition Counts")

#Bar Chart of TCE Delivery Name
ggplot(exoplanet, mapping = aes(x=TCE_Deliv_Name)) +
  geom_bar(stat = "count", aes(fill = TCE_Deliv_Name))+
  ggtitle("TCE Delivery Name Counts")

#Bar Chart of the Planet Number
ggplot(exoplanet, mapping = aes(x=TCE_Planet_Number)) +
  geom_bar(stat = "count", aes(fill = TCE_Planet_Number))+
  ggtitle("TCE Planet Number Counts")

#Scatter Plot of the Planetary Radius and Orbital Period
ggplot(exoplanet, mapping = aes(x= Planetary_Radius, y= Orbital_Period))+
  geom_point()+
  ggtitle("Planetary Radius and Orbital Period")

#Scatter Plot of the Planetary Radius and Orbital Period in Log Scale
ggplot(exoplanet, mapping = aes(x= Planetary_Radius, y= Orbital_Period))+
  geom_point()+
  ggtitle("Planetary Radius and Orbital Period (Log)")+
  geom_smooth(method="lm")+
  scale_y_log10()+
  scale_x_log10()

#Scatter Plot of the Planetary Radius and Temperature
ggplot(exoplanet, mapping = aes(x= Planetary_Radius, y= Equil_Temp))+
  geom_point()+
  ggtitle("Planetary Radius and Temperature")

#Scatter Plot of the Planetary Radius and Temperature Log
ggplot(exoplanet, mapping = aes(x= Planetary_Radius, y= Equil_Temp))+
  geom_point()+
  ggtitle("Planetary Radius and Temperature (Log)")+
  geom_smooth(method="lm")+
  scale_y_log10()+
  scale_x_log10()

#Density Plot of the Disposition Score
ggplot(exoplanet, aes(x = Disposition_Score)) +
  geom_histogram(aes(y = ..density..), color = "grey30", fill = "white")+
  geom_density(alpha = .2, fill="antiquewhite3")+
  ggtitle("Planetary Radius Density")

#Density Plot of the Transit Epoch
ggplot(exoplanet, aes(x = Transit_Epoch)) +
  geom_histogram(aes(y = ..density..), color = "grey30", fill = "white")+
  geom_density(alpha = .2, fill="antiquewhite3")+
  ggtitle("Orbital Period Density")

#Density Plot of the Impact Parameter
ggplot(exoplanet, aes(x = Impact_Parameter)) +
  geom_histogram(aes(y = ..density..), color = "grey30", fill = "white")+
  geom_density(alpha = .2, fill="antiquewhite3")+
  ggtitle("Orbital Period Density")

#Density Plot of the Impact Parameter Log Scale
ggplot(exoplanet, aes(x = Impact_Parameter)) +
  geom_histogram(aes(y = ..density..), color = "grey30", fill = "white")+
  geom_density(alpha = .2, fill="antiquewhite3")+
  ggtitle("Orbital Period Density (Log)")+
  scale_y_log10()+
  scale_x_log10()

#Scatter Plot of Planetary Radius and the Transit Depth
ggplot(exoplanet, mapping = aes(x= Planetary_Radius, y= Transit_Depth))+
  geom_point()+
  ggtitle("Planetary Radius and Transit Depth")

#Scatter Plot of Planetary Radius and the Transit Depth Log Scale
ggplot(exoplanet, mapping = aes(x= Planetary_Radius, y= Transit_Depth))+
  geom_point()+
  ggtitle("Planetary Radius and Transit Depth (Log)")+
  geom_smooth(method="lm")+
  scale_y_log10()+
  scale_x_log10()

#Scatter Plot of Transit Depth and Transit Duration
ggplot(exoplanet, mapping = aes(x= Transit_Depth, y= Transit_Duration))+
  geom_point()+
  ggtitle("Transit Depth and Transit Duration")

#Scatter Plot of Transit Depth and Transit Duration Log Values
ggplot(exoplanet, mapping = aes(x= Transit_Depth, y= Transit_Duration))+
  geom_point()+
  ggtitle("Transit Depth and Transit Duration (Log)")+
  geom_smooth(method="lm")+
  scale_y_log10()+
  scale_x_log10()

#Sampling subsetting dataset
attach(exoplanet)
sample <- exoplanet[which(Kepler_Disposition=="CANDIDATE"),]

#Boxplots of the orbital period of each kepler disposition
ggplot(exoplanet, mapping = aes(x=Kepler_Disposition, y=Orbital_Period))+
  geom_boxplot()+
  ggtitle("Boxplot of the Orbital Period for Kepler Disposition")+
  scale_y_log10()

#Boxplots of the orbital period of each earchive disposition
ggplot(exoplanet, mapping = aes(x=EArchive_Disposition, y=Orbital_Period))+
  geom_boxplot()+
  ggtitle("Boxplot of the Orbital Period for Earchive Disposition")+
  scale_y_log10()
