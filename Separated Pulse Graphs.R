# Script to break the pulse graphs back out if we choose to do so #

# Load haven for .dta files & readxl for .xlsx files #
library("haven")
library("readxl")

# Load dplyr for bind_cols #
library("dplyr")

# Load lubridate for timeseries #
library("lubridate")

# Load ggplot, ggthemes, reshape2, & ggpubr for graphs #
library("ggplot2")
library("ggthemes")
library("reshape2")
library("ggpubr")

# Load stringr for string manipulation #
library("stringr")

# Clear memory #
rm(list = ls())

# Set working directory #
setwd("/Users/peterwilliams/Desktop/The Effects of Emergency Rental Assistance During the Pandemic")

# Read in data on ERA1 Payments #
ERA1_Payment_Timeseries <- read_excel("June-2022-ERA-Monthly-Data.xlsx", sheet = "ERA1 Summary", range = "A4:C19", col_names = FALSE)
ERA1_Payment_Timeseries <- ERA1_Payment_Timeseries[,c(1,3)]
colnames(ERA1_Payment_Timeseries) <- c("Date", "Unique Households Assisted (Thousands)")
# Note the fact that 01/2021 - 03/2021 payments are grouped under 03/2021 #
ERA1_Payment_Timeseries$Date <- seq(as.Date("2021-04-01"), as.Date("2022-07-01"), by = "1 month") -1

# Read in data on ERA2 Payments #
ERA2_Payment_Timeseries <- read_excel("June-2022-ERA-Monthly-Data.xlsx", sheet = "ERA2 Summary", range = "A4:C16", col_names = FALSE)
ERA2_Payment_Timeseries <- ERA2_Payment_Timeseries[,c(1,3)]
colnames(ERA2_Payment_Timeseries) <- c("Date", "Unique Households Assisted (Thousands)")
# Note the fact that 01/2021 - 03/2021 payments are grouped under 03/2021 #
ERA2_Payment_Timeseries$Date <- seq(as.Date("2021-07-01"), as.Date("2022-07-01"), by = "1 month") -1

# Calculate cumulative ERA payments #
ERA_Payments <- data.frame(matrix(nrow = 16, ncol = 2))
colnames(ERA_Payments) <- c("Date", "Cumulative Households Assisted, ERA1 & ERA2")
ERA_Payments$Date <- ERA1_Payment_Timeseries$Date
for(i in 1:nrow(ERA_Payments)) {
  ERA_Payments$`Cumulative Households Assisted, ERA1 & ERA2`[i] <- sum(ERA1_Payment_Timeseries$`Unique Households Assisted (Thousands)`[ERA1_Payment_Timeseries$Date <= ERA_Payments$Date[i]]) + sum(ERA2_Payment_Timeseries$`Unique Households Assisted (Thousands)`[ERA2_Payment_Timeseries$Date <= ERA_Payments$Date[i]])
}

# Convert Households Assisted to millions #
ERA_Payments$`Cumulative Households Assisted, ERA1 & ERA2` <- ERA_Payments$`Cumulative Households Assisted, ERA1 & ERA2` / 1000000

# Create ID column for graph legend #
ERA_Payments$ID <- colnames(ERA_Payments)[2]

# Read in data on households in rental arrears #
Behind_On_Rent_Filenames <- list.files(path = ".", pattern = "housing1b*")
Behind_On_Rent_Filenames <- Behind_On_Rent_Filenames[order(as.numeric(str_replace(str_sub(Behind_On_Rent_Filenames,15,16), pattern = "[.]", replacement = "")), decreasing = FALSE)]
Behind_On_Rent_Data <- data.frame(matrix(nrow = length(Behind_On_Rent_Filenames), ncol = 1))
colnames(Behind_On_Rent_Data) <- "Households in Rental Arrears"
for (i in 1:nrow(Behind_On_Rent_Data)) {
  Temporary_Data <- read_excel(Behind_On_Rent_Filenames[i], sheet = "US")
  # Select the row where the first column == 'Total' or 'Total*' #
  Row_Number <- as.logical(unlist(lapply(Temporary_Data[,1], grepl, pattern = "Total"))) == TRUE & as.numeric(unlist(lapply(Temporary_Data[,1], nchar))) <= 6  & is.na(Temporary_Data[,1]) == FALSE
  # Select the first row where the first column = 'NA' and then select the column where 'Last month's payment status' = 'No' #
  Column_Number <- Temporary_Data[is.na(Temporary_Data[,1]),][1,] == "No" & is.na(Temporary_Data[is.na(Temporary_Data[,1]),][1,]) == FALSE
  # Take 'Last month's payment status' = 'No'
  Behind_On_Rent_Data[i,1] <- as.numeric(Temporary_Data[Row_Number, Column_Number[1,]]) / 1000000
}

# Assign dates based on the Pulse Survey dates #
Behind_On_Rent_Data$Date <- c(seq(as.Date("2020-05-05"), as.Date("2020-07-21"), by = "1 week"),
                              seq(as.Date("2020-8-31"), as.Date("2020-12-21"), by = "2 weeks"),
                              seq(as.Date("2021-01-18"), as.Date("2021-03-29"), by = "2 weeks"),
                              seq(as.Date("2021-04-25"), as.Date("2021-07-05"), by = "2 weeks"),
                              seq(as.Date("2021-08-02"), as.Date("2021-10-11"), by = "2 weeks"),
                              as.Date("2021-12-13"), as.Date("2022-01-10"), as.Date("2022-02-07"),
                              seq(as.Date("2022-03-14"), as.Date("2022-05-09"), by = "4 weeks"),
                              as.Date("2022-06-13"), as.Date("2022-07-11"), as.Date("2022-08-08"),
                              as.Date("2022-09-26"), as.Date("2022-10-17"), as.Date("2022-11-14"), 
                              seq(as.Date("2022-12-19"), as.Date("2023-05-08"), by = "4 weeks"),
                              as.Date("2023-06-19"), as.Date("2023-07-10"), as.Date("2023-08-7"),
                              as.Date("2023-09-04"), as.Date("2023-10-02"), as.Date("2023-10-30"))

# Keep only the data through the end of March 2023 #
Behind_On_Rent_Data <- Behind_On_Rent_Data[c(1:56),]

# Assign an ID for the graph legend #
Behind_On_Rent_Data$ID <- "Households in Rental Arrears"

Colors <- c("Cumulative Households Assisted, ERA1 & ERA2" = "grey70", "Households in Rental Arrears" = "navyblue")

# Graph the % of Households in Rental Arrears from May 2020 to October 2023 based on Pulse Survey data #
Rent_Payment_Status_Timeseries_Graph <- 
  ggplot() +
  geom_ribbon(
    aes(x = c(as.Date("2020-04-30"), as.Date("2020-06-30")), ymax = c(14,14), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-07-31"), as.Date("2020-08-29")), ymax = c(14,14), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-11-30"), as.Date("2020-12-31")), ymax = c(13,13), ymin = c(0,0)),
    color = NA,
    alpha = 1,
    fill = "grey50"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-10-31"), as.Date("2020-12-31")), ymax = c(13.5,13.5), ymin= c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey70"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-09-01"), as.Date("2020-12-31")), ymax = c(14,14), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_line(
    ERA_Payments,
    mapping = aes(y = `Cumulative Households Assisted, ERA1 & ERA2` / 7 * 14, x = Date, color = ID),
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  geom_segment(
    aes(x = as.Date("2020-04-30"), xend = as.Date("2020-04-30"), y = 0, yend = 14),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-07-31"), xend = as.Date("2020-07-31"), y = 0, yend = 14),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-11-30"), xend = as.Date("2020-11-30"), y = 0, yend = 13),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-10-31"), xend = as.Date("2020-10-31"), y = 0, yend = 13.5),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-09-05"), xend = as.Date("2020-09-05"), y = 0, yend = 14),
    color = "black",
    linewidth = 0.5
  ) +
  annotate(
    "text", x = as.Date("2020-05-15"), y = 8, label= "Chicago DOH", angle = 270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-08-15"), y = 7, label="Chicago TRP", angle = 270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-12-15"), y = 7.75, label="Harris County", angle=270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-11-15"), y = 11.5, label="King County", angle=270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-09-18"), y = 13.5, label="LA", angle=270, size = 2, color="black"
  ) +
  geom_line(
    Behind_On_Rent_Data,
    mapping = aes(y = `Households in Rental Arrears`, x = `Date`, color = ID),
    linewidth = 0.75
  ) +
  labs(
    title = paste0("Rental Households Behind on Payments"),
    subtitle = "This data is estimated by the U.S. Census Bureau's Household Pulse Survey. Households are considered behind on rent if they reported 'No' for 'Last month's payment status.'",
    y = "Households (Millions)",
    x = ""
  ) +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(size = 10, hjust = 0.5),
    plot.subtitle = element_text(size = 5, hjust = 0.5),
    plot.margin = margin(0.1,0.2,0,0.2, "cm"),
    axis.text = element_text(size = 6, margin = 1),
    axis.title = element_text(size = 8, margin = 1),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 4.5),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.text = element_text(size = 4.5),
    legend.title = element_blank(),
    legend.spacing.x = unit(1, "cm"),
    legend.spacing.y = unit(0.2, "mm"),
    legend.position = "bottom",
    legend.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +  
  ylab(
    label = "Households (Millions)"
  ) +
  scale_y_continuous(
    limits = c(0,14),
    breaks = seq(0,14, by = 2),
    sec.axis = sec_axis(transform = ~./14 * 7, name = "Households Assisted (Millions)", breaks = seq(1, 7, by  = 1))
  ) +
  scale_x_date(
    limits = c(as.Date("2019-02-01"), as.Date("2023-04-01")),
    breaks = seq(as.Date("2019-02-01"), as.Date("2023-04-01"), by = "6 months") - 1
  ) + 
  scale_color_manual(
    values = Colors,
    breaks = c("Households in Rental Arrears", "Cumulative Households Assisted, ERA1 & ERA2")
  ) +
  guides(
    color = guide_legend(position = "bottom")
  )

# Part 4: Create timeseries with Census Bureau Pulse Survey data on households at risk of eviction #

# Read in data on households at risk of foreclosure #
Eviction_Risk_Filenames <- list.files(path = ".", pattern = "housing3b*")
Eviction_Risk_Filenames <- Eviction_Risk_Filenames[order(as.numeric(str_replace(str_sub(Eviction_Risk_Filenames,15,16), pattern = "[.]", replacement = "")), decreasing = FALSE)]
Eviction_Risk_Data <- data.frame(matrix(nrow = length(Eviction_Risk_Filenames), ncol = 1))
colnames(Eviction_Risk_Data) <- "Households in Rental Arrears at Risk of Eviction"
for (i in 1:nrow(Eviction_Risk_Data)) {
  Temporary_Data <- read_excel(Eviction_Risk_Filenames[i], sheet = "US")
  # Select the row where the first column == 'Total' or 'Total*' #
  Row_Number <- as.logical(unlist(lapply(Temporary_Data[,1], grepl, pattern = "Total"))) == TRUE & as.numeric(unlist(lapply(Temporary_Data[,1], nchar))) <= 6  & is.na(Temporary_Data[,1]) == FALSE
  # Select the first row where the first column = 'NA' and then select the column where 'Likelihood of leaving this home due to eviction in the next two months' = 'Very Likely' #
  Column_Number_1 <- Temporary_Data[is.na(Temporary_Data[,1]),][1,] == "Very likely" & is.na(Temporary_Data[is.na(Temporary_Data[,1]),][1,]) == FALSE
  # Select the first row where the first column = 'NA' and then select the column where 'Likelihood of leaving this home due to eviction in the next two months' = 'Somewhat Likely' #
  Column_Number_2 <- Temporary_Data[is.na(Temporary_Data[,1]),][1,] == "Somewhat likely" & is.na(Temporary_Data[is.na(Temporary_Data[,1]),][1,]) == FALSE
  # Take 'Likelihood of leaving this home due to eviction in the next two months' = ('Very likely' + 'Somewhat likely') / 'Total'
  Eviction_Risk_Data[i,1] <- (as.numeric(Temporary_Data[Row_Number, Column_Number_1[1,]]) + as.numeric(Temporary_Data[Row_Number, Column_Number_2[1,]])) / 1000000
}

# Assign dates based on the Pulse Survey dates #
Eviction_Risk_Data$Date <- c(seq(as.Date("2020-8-31"), as.Date("2020-12-21"), by = "2 weeks"),
                             seq(as.Date("2021-01-18"), as.Date("2021-03-29"), by = "2 weeks"),
                             seq(as.Date("2021-04-25"), as.Date("2021-07-05"), by = "2 weeks"),
                             seq(as.Date("2021-08-02"), as.Date("2021-10-11"), by = "2 weeks"),
                             as.Date("2021-12-13"), as.Date("2022-01-10"), as.Date("2022-02-07"),
                             seq(as.Date("2022-03-14"), as.Date("2022-05-09"), by = "4 weeks"),
                             as.Date("2022-06-13"), as.Date("2022-07-11"), as.Date("2022-08-08"),
                             as.Date("2022-09-26"), as.Date("2022-10-17"), as.Date("2022-11-14"), 
                             seq(as.Date("2022-12-19"), as.Date("2023-05-08"), by = "4 weeks"),
                             as.Date("2023-06-19"), as.Date("2023-07-10"), as.Date("2023-08-7"),
                             as.Date("2023-09-04"), as.Date("2023-10-02"), as.Date("2023-10-30"))

# Keep only the data through the end of March 2023 #
Eviction_Risk_Data <- Eviction_Risk_Data[c(1:44),]

# Read in data on proportion of households that are unconfident in their ability to pay the next month's rent #
Next_Rent_Risk_Filenames <- list.files(path = ".", pattern = "housing2b*")
Next_Rent_Risk_Filenames <- Next_Rent_Risk_Filenames[order(as.numeric(str_replace(str_sub(Next_Rent_Risk_Filenames,15,16), pattern = "[.]", replacement = "")), decreasing = FALSE)]
Next_Rent_Risk_Data <- data.frame(matrix(nrow = length(Next_Rent_Risk_Filenames), ncol = 1))
colnames(Next_Rent_Risk_Data) <- "Households Unconfident in Next Month's Rent Payment"
for (i in 1:nrow(Next_Rent_Risk_Data)) {
  Temporary_Data <- read_excel(Next_Rent_Risk_Filenames[i], sheet = "US")
  # Select the row where the first column == 'Total' or 'Total*' #
  Row_Number <- as.logical(unlist(lapply(Temporary_Data[,1], grepl, pattern = "Total"))) == TRUE & as.numeric(unlist(lapply(Temporary_Data[,1], nchar))) <= 6  & is.na(Temporary_Data[,1]) == FALSE
  # Select the first row where the first column = 'NA' and then select the column where 'Confidence to pay next month's rent = 'No confidence' #
  Column_Number <- Temporary_Data[is.na(Temporary_Data[,1]),][1,] %in% c("No confidence", "No at all confident") & is.na(Temporary_Data[is.na(Temporary_Data[,1]),][1,]) == FALSE
  # Take 'Confidence to pay next month's rent' = 'No confidence' / 'Total'
  Next_Rent_Risk_Data[i,1] <- as.numeric(Temporary_Data[Row_Number, Column_Number[1,]]) / 1000000
}

# Assign dates based on the Pulse Survey dates #
Next_Rent_Risk_Data$Date <- c(seq(as.Date("2020-05-05"), as.Date("2020-07-21"), by = "1 week"),
                              seq(as.Date("2020-8-31"), as.Date("2020-12-21"), by = "2 weeks"),
                              seq(as.Date("2021-01-18"), as.Date("2021-03-29"), by = "2 weeks"),
                              seq(as.Date("2021-04-25"), as.Date("2021-07-05"), by = "2 weeks"),
                              seq(as.Date("2021-08-02"), as.Date("2021-10-11"), by = "2 weeks"),
                              as.Date("2021-12-13"), as.Date("2022-01-10"), as.Date("2022-02-07"),
                              seq(as.Date("2022-03-14"), as.Date("2022-05-09"), by = "4 weeks"))

# Combine eviction risk and next rent risk data #
Eviction_and_Next_Rent_Risk_Data <- data.frame(matrix(nrow = length(unique(c(Next_Rent_Risk_Data$Date, Eviction_Risk_Data$Date))), ncol = 3))
colnames(Eviction_and_Next_Rent_Risk_Data) <- c("Date", "Households in Rental Arrears at Risk of Eviction", "Households Unconfident in Next Month's Rent Payment")
Eviction_and_Next_Rent_Risk_Data$Date <- unique(c(Next_Rent_Risk_Data$Date, Eviction_Risk_Data$Date))
for (i in 1:nrow(Eviction_and_Next_Rent_Risk_Data)) {
  ifelse (
    Eviction_and_Next_Rent_Risk_Data$Date[i] %in% Eviction_Risk_Data$Date,
    Eviction_and_Next_Rent_Risk_Data$`Households in Rental Arrears at Risk of Eviction` [i] <- Eviction_Risk_Data$`Households in Rental Arrears at Risk of Eviction`[Eviction_Risk_Data$Date == Eviction_and_Next_Rent_Risk_Data$Date[i]],
    Eviction_and_Next_Rent_Risk_Data$`Households in Rental Arrears at Risk of Eviction`[i] <- 0
  )
  
  ifelse (
    Eviction_and_Next_Rent_Risk_Data$Date[i] %in% Next_Rent_Risk_Data$Date,
    Eviction_and_Next_Rent_Risk_Data$`Households Unconfident in Next Month's Rent Payment`[i] <- Next_Rent_Risk_Data$`Households Unconfident in Next Month's Rent Payment`[Next_Rent_Risk_Data$Date ==  Eviction_and_Next_Rent_Risk_Data$Date[i]],
    Eviction_and_Next_Rent_Risk_Data$`Households Unconfident in Next Month's Rent Payment`[i] <- 0
  )
}

# Melt the data #
Eviction_and_Next_Rent_Risk_Data$Date <- as.character(Eviction_and_Next_Rent_Risk_Data$Date)
Eviction_and_Next_Rent_Risk_Data <- melt(Eviction_and_Next_Rent_Risk_Data)
colnames(Eviction_and_Next_Rent_Risk_Data) <- c("Date", "Variable", "Households")
Eviction_and_Next_Rent_Risk_Data$Date <- as.Date(Eviction_and_Next_Rent_Risk_Data$Date)
for (i in 1:nrow(Eviction_and_Next_Rent_Risk_Data)) {
  if (Eviction_and_Next_Rent_Risk_Data$`Households`[i] == 0) {
    Eviction_and_Next_Rent_Risk_Data$`Households`[i] <- NA
  }
}

# Specify colors #
Colors <- c("Households in Rental Arrears at Risk of Eviction" = "deeppink", "Households Unconfident in Next Month's Rent Payment" = "navyblue",
            "Cumulative Households Assisted, ERA1 & ERA2" = "grey70")

# Graph the households that are unconfident in their ability to pay next month's rent and the households at risk of eviction in the next two months #
Eviction_Risk_Timeseries_Graph <- 
  ggplot() +
  geom_ribbon(
    aes(x = c(as.Date("2020-04-30"), as.Date("2020-06-30")), ymax = c(11,11), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-07-31"), as.Date("2020-08-29")), ymax = c(11,11), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-11-30"), as.Date("2020-12-31")), ymax = c(10,10), ymin = c(0,0)),
    color = NA,
    alpha = 1,
    fill = "grey50"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-10-31"), as.Date("2020-12-31")), ymax = c(10.5,10.5), ymin= c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey70"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-09-01"), as.Date("2020-12-31")), ymax = c(11,11), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_line(
    ERA_Payments,
    mapping = aes(y = `Cumulative Households Assisted, ERA1 & ERA2` / 7 * 11, x = Date, color = ID),
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  geom_segment(
    aes(x = as.Date("2020-04-30"), xend = as.Date("2020-04-30"), y = 0, yend = 11),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-07-31"), xend = as.Date("2020-07-31"), y = 0, yend = 11),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-11-30"), xend = as.Date("2020-11-30"), y = 0, yend = 10),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-10-31"), xend = as.Date("2020-10-31"), y = 0, yend = 10.5),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-09-05"), xend = as.Date("2020-09-05"), y = 0, yend = 11),
    color = "black",
    linewidth = 0.5
  ) +
  annotate(
    "text", x = as.Date("2020-05-15"), y = 7, label= "Chicago DOH", angle = 270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-08-15"), y = 4.5, label="Chicago TRP", angle = 270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-12-15"), y = 3, label="Harris County", angle=270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-11-15"), y = 9, label="King County", angle=270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-09-22"), y = 10.5, label="LA", angle=270, size = 2, color="black"
  ) +
  geom_line(
    Eviction_and_Next_Rent_Risk_Data,
    mapping = aes(y = `Households`, x = `Date`, color = Variable),
    linewidth = 0.75
  ) +
  labs(
    title = paste0("Rental Households at Risk of Arrears & Rental Households Already in Arrears at Risk of Eviction"),
    subtitle = "This data is estimated by the U.S. Census Bureau's Household Pulse Survey. Households are considered at risk of eviction if they reported themselves as 'very likely' or 'somewhat likely' to experience eviction in the
next two months; households are considered unconfident in their ability to pay next month's rent if they reported themselves as having 'no confidence' in their ability to pay next month's rent.",
    y = "Households (Millions)",
    x = ""
  ) +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(size = 10, hjust = 0.5),
    plot.subtitle = element_text(size = 5, hjust = 0.5),
    plot.margin = margin(0.1,0.2,0,0.2, "cm"),
    axis.text = element_text(size = 6, margin = 1),
    axis.title = element_text(size = 8, margin = 1),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 4.5),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.text = element_text(size = 4.5),
    legend.title = element_blank(),
    legend.spacing.x = unit(1, "cm"),
    legend.spacing.y = unit(0.2, "mm"),
    legend.position = "bottom",
    legend.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +  
  ylab(
    label = "Households (Millions)"
  ) +
  scale_y_continuous(
    limits = c(0,11),
    breaks = seq(0,10, by = 2),
    sec.axis = sec_axis(transform = ~./11 * 7, name = "Households Assisted (Millions)", breaks = seq(1, 7, by = 1))
  ) +
  scale_color_manual(
    values = Colors,
    name = "",
    breaks = c("Households in Rental Arrears at Risk of Eviction", "Households Unconfident in Next Month's Rent Payment", "Cumulative Households Assisted, ERA1 & ERA2")
  ) + 
  scale_x_date(
    limits = c(as.Date("2019-02-01"), as.Date("2023-04-01")),
    breaks = seq(as.Date("2019-02-01"), as.Date("2023-04-01"), by = "6 months") - 1
  ) +
  guides(
    color = guide_legend(position = "bottom")
  )

# Save the graphs as a ggarrange object #
Graphs_Page_2 <- ggarrange(Rent_Payment_Status_Timeseries_Graph, Eviction_Risk_Timeseries_Graph, heights = c(1,1), nrow = 2)

# Export the graphs to a PDF #
pdf("Separated Pulse Graphs.pdf")
print(Graphs_Page_2)
dev.off()

