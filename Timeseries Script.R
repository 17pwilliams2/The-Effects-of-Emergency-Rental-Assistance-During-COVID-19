# Script to make graphs for Winnie #

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


# Part 1: Create timeseries to plot average income per unit among the bottom 50% of households from 01/2019 to 03/2023 #

# Read income data from Stata files produced by realtimeinequality.org #
Princ_Working_Age_Data <- read_dta("STATA Exports/decomposition-monthly-princ-working_age.dta")
Peinc_Working_Age_Data <- read_dta("STATA Exports/decomposition-monthly-peinc-working_age.dta")
Dispo_Working_Age_Data <- read_dta("STATA Exports/decomposition-monthly-dispo-working_age.dta")
Poinc_Working_Age_Data <- read_dta("STATA Exports/decomposition-monthly-poinc-working_age.dta")

# Bind rows #
Working_Age_Data <- bind_cols(Princ_Working_Age_Data[,c(1:4)], Poinc_Working_Age_Data[,-c(1:3,5)], Peinc_Working_Age_Data[,-c(1:3,5)], Dispo_Working_Age_Data[,-c(1:3,5)])

# Read in nipa deflator and add to working age data #
Nipa_Deflator <- read_dta("STATA Exports/nipa-simplified-monthly.dta")
Working_Age_Data$'Nipa Deflator' <- 0
for (i in 1:nrow(Working_Age_Data)) {
  Working_Age_Data$`Nipa Deflator`[i] <- Nipa_Deflator$nipa_deflator[Nipa_Deflator$year == Working_Age_Data$year[i] & Nipa_Deflator$month == Working_Age_Data$month[i]]
}

# Generate inverse probability weights #
Working_Age_Data <- Working_Age_Data[order(Working_Age_Data$year, Working_Age_Data$month, Working_Age_Data$p),]

# Keep only bottom 50% of households #
Working_Age_Data <- Working_Age_Data[Working_Age_Data$p <= 49000,]
Working_Age_Data <- Working_Age_Data[,-3]

# Make a dataframe of weighted averages #
Income
Income_Timeseries <- data.frame(matrix(nrow = 51, ncol = ncol(Working_Age_Data)))
colnames(Income_Timeseries) <- colnames(Working_Age_Data)
Income_Timeseries$year <- c(rep(2019,12), rep(2020,12), rep(2021,12), rep(2022,12), rep(2023,3))
Income_Timeseries$month <- c(rep(c(1:12),4),c(1:3))
for (i in 1:nrow(Income_Timeseries)) {
  for (j in 3:ncol(Income_Timeseries)) {
    Income_Timeseries[i,j] <- colMeans(Working_Age_Data[Working_Age_Data$year == Income_Timeseries$year[i] & Working_Age_Data$month == Income_Timeseries$month[i],j])
  }
}

# De-annualize and divide by the NIPA deflator #
for (i in 1:nrow(Income_Timeseries)) {
  for (j in 3:(ncol(Income_Timeseries) - 1)) {
    Income_Timeseries[i,j] <- Income_Timeseries[i,j] / 12 / Income_Timeseries$`Nipa Deflator`[i]
  }
}

# Create columns for graph #
Income_Timeseries$'Factor Income' <- Income_Timeseries$princ
Income_Timeseries$'Subsidized Factor Income' <- Income_Timeseries$princ + Income_Timeseries$covidsub
Income_Timeseries$'Subsidized Pretax National Income' <- Income_Timeseries$`Subsidized Factor Income` + Income_Timeseries$uiben + Income_Timeseries$penben - Income_Timeseries$contrib
Income_Timeseries$'Post-Tax Disposable Income' <- Income_Timeseries$`Subsidized Pretax National Income` + + Income_Timeseries$vet + Income_Timeseries$othcash - Income_Timeseries$taxes - Income_Timeseries$estatetax - Income_Timeseries$corptax - Income_Timeseries$othercontrib + Income_Timeseries$covidrelief
Income_Timeseries$'Date' <- seq(as.Date("2019-02-01"), as.Date("2023-04-01"), by = "1 month") - 1
Income_Timeseries <- Income_Timeseries[,c(27:31)]

# Melt the graph for legend purposes #
Income_Timeseries$Date <- as.character(Income_Timeseries$Date)
Income_Timeseries_Melted <- melt(Income_Timeseries)
Income_Timeseries_Melted$Date <- seq(as.Date("2019-02-01"), as.Date("2023-04-01"), by = "1 month") - 1
colnames(Income_Timeseries_Melted) <- c("Date", "Type of Income", "Monthly Income per adult (constant USD)")

# Remove extra variables for line graphs #
Income_Timeseries_Melted_Factor_and_Disposable <- Income_Timeseries_Melted[Income_Timeseries_Melted$`Type of Income` %in% c("Factor Income", "Post-Tax Disposable Income"),]

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

# Specify colors #
Colors <- c("Factor Income" = "deeppink", "Post-Tax Disposable Income" = "navyblue", "Cumulative Households Assisted, ERA1 & ERA2" = "grey70")

# Graph income of the bottom 50% of households from January 2019 to March 2023 #
Income_Timeseries_Graph <- 
  ggplot() +
  geom_ribbon(
    aes(x = c(as.Date("2020-04-30"), as.Date("2020-06-30")), ymax = c(4500,4500), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-07-31"), as.Date("2020-08-29")), ymax = c(4500,4500), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-11-30"), as.Date("2020-12-31")), ymax = c(4000,4000), ymin = c(0,0)),
    color = NA,
    alpha = 1,
    fill = "grey50"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-10-31"), as.Date("2020-12-31")), ymax = c(4250,4250), ymin= c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey70"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-09-05"), as.Date("2020-12-31")), ymax = c(4500,4500), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_line(
    ERA_Payments,
    mapping = aes(y = `Cumulative Households Assisted, ERA1 & ERA2` / 7 * 4500, x = Date, color = ID),
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  geom_segment(
    aes(x = as.Date("2020-04-30"), xend = as.Date("2020-04-30"), y = 0, yend = 4500),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-07-31"), xend = as.Date("2020-07-31"), y = 0, yend = 4500),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-11-30"), xend = as.Date("2020-11-30"), y = 0, yend = 4000),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-10-31"), xend = as.Date("2020-10-31"), y = 0, yend = 4250),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-09-05"), xend = as.Date("2020-09-05"), y = 0, yend = 4500),
    color = "black",
    linewidth = 0.5
  ) +
  geom_line(
    Income_Timeseries_Melted_Factor_and_Disposable,
    mapping = aes(y = `Monthly Income per adult (constant USD)`, x = `Date`, group = `Type of Income`, color = `Type of Income`),
    linewidth = 0.75
  ) +
  annotate(
    "text", x = as.Date("2020-05-31"), y = 4000, label= "Chicago\nDOH", angle = 270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-08-15"), y = 3837, label="Chicago TRP", angle = 270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-12-15"), y = 3312, label="Harris County", angle=270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-11-15"), y = 3625, label="King County", angle=270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-09-22"), y = 4325, label="LA", angle=270, size = 2, color="black"
  ) +
  labs(
    title = paste0("Disposable Income (Bottom 50% of Households)"),
    subtitle = "This data comes from realtimeinequality.org.",
    y = "Income / Month ($)",
    x = ""
  ) +
  theme_bw() +
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
    legend.spacing.x = unit(0.01, "cm"),
    legend.spacing.y = unit(0.2, "mm"),
    legend.position = "inside",
    legend.position.inside = c(0.83,0.23),
    legend.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +  
  ylab(
    label = "Income / Month ($)"
  ) +
  scale_x_date(
    breaks = seq(as.Date("2019-02-01"), as.Date("2023-04-01"), by = "6 months") - 1
  ) + 
  scale_color_manual(
    values = Colors,
    name = "",
    breaks = c("Factor Income", "Post-Tax Disposable Income", "Cumulative Households Assisted, ERA1 & ERA2")
  ) +
  guides(
    color = guide_legend(override.aes = list(linetype = c("solid", "solid", "dashed")), nrow = 1, position = "bottom")
  ) + 
  scale_y_continuous(
    limits = c(0,4500),
    breaks = seq(0, 4500, by = 500),
    sec.axis = sec_axis(transform = ~./ 4500 * 7, name = "Households Assisted (Millions)", breaks = seq(1, 7, by = 1))
  )

# Part 2: Create timeseries to plot unemployment from 01/2019 to 03/2023 #

# Read data from https://data.bls.gov/timeseries/LNS14000000 #
Unemployment_Rate <- read_excel("Unemployment Rate (BLS).xlsx", range = "A12:M22", col_names = TRUE)
Unemployment_Timeseries <- data.frame(matrix(nrow = 51, ncol = 2))
colnames(Unemployment_Timeseries) <- c("Date", "Unemployment Rate (%)")
Unemployment_Timeseries$Date <- seq(as.Date("2019-02-01"), as.Date("2023-04-01"), by = "1 month") - 1
Unemployment_Timeseries$`Unemployment Rate (%)` <- as.numeric(c(Unemployment_Rate[6,c(2:13)], Unemployment_Rate[7,c(2:13)], Unemployment_Rate[8,c(2:13)], Unemployment_Rate[9,c(2:13)], Unemployment_Rate[10,c(2:4)]))

# Assign an ID for the graph legend #
Unemployment_Timeseries$ID <- "Unemployment Rate (%)"

Colors <- c("Cumulative Households Assisted, ERA1 & ERA2" = "grey70", "Unemployment Rate (%)" = "navyblue")

# Graph unemployment from January 2019 to March 2023 #
Unemployment_Timeseries_Graph <- 
  ggplot() +
  geom_ribbon(
    aes(x = c(as.Date("2020-04-30"), as.Date("2020-06-30")), ymax = c(18,18), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-07-31"), as.Date("2020-08-29")), ymax = c(18,18), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-11-30"), as.Date("2020-12-31")), ymax = c(16,16), ymin = c(0,0)),
    color = NA,
    alpha = 1,
    fill = "grey50"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-10-31"), as.Date("2020-12-31")), ymax = c(17,17), ymin= c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey70"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-09-01"), as.Date("2020-12-31")), ymax = c(18,18), ymin = c(0,0)),
    color = NA,
    alpha = 0.6,
    fill = "grey90"
  ) +
  geom_line(
    ERA_Payments,
    mapping = aes(y = `Cumulative Households Assisted, ERA1 & ERA2` / 7 * 18, x = Date, color = ID),
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  geom_segment(
    aes(x = as.Date("2020-04-30"), xend = as.Date("2020-04-30"), y = 0, yend = 18),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-07-31"), xend = as.Date("2020-07-31"), y = 0, yend = 18),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-11-30"), xend = as.Date("2020-11-30"), y = 0, yend = 16),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-10-31"), xend = as.Date("2020-10-31"), y = 0, yend = 17),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-09-05"), xend = as.Date("2020-09-05"), y = 0, yend = 18),
    color = "black",
    linewidth = 0.5
  ) +
  geom_line(
    Unemployment_Timeseries,
    mapping = aes(y = `Unemployment Rate (%)`, x = `Date`, color = ID),
    linewidth = 0.75
  ) +
  annotate(
    "text", x = as.Date("2020-05-30"), y = 16.3, label= " Chicago \nDOH", angle = 270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-08-15"), y = 15.5, label="Chicago TRP", angle = 270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-12-15"), y = 13.5, label="Harris County", angle=270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-11-15"), y = 14.37, label="King County", angle=270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-09-22"), y = 17.12, label="LA", angle=270, size = 2, color="black"
  ) +
  labs(
    title = paste0("Unemployment Rate"),
    subtitle = "This data comes from U.S. Bureau of Labor Statistics.",
    y = "Unemployment (%)",
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
    label = "Unemployment (%)"
  ) +
  scale_y_continuous(
    limits = c(0,18),
    breaks = seq(0,18, by = 3),
    sec.axis = sec_axis(transform = ~./18 * 7, name = "Households Assisted (Millions)", breaks = seq(1, 7, by = 1))
  ) +
  scale_x_date(
    breaks = seq(as.Date("2019-02-01"), as.Date("2023-04-01"), by = "6 months") - 1
  ) +
  scale_color_manual(
    values = Colors,
    breaks = c("Unemployment Rate (%)", "Cumulative Households Assisted, ERA1 & ERA2")
  ) +
  guides(
    color = guide_legend(position = "bottom")
  )

# Part 3: Create timeseries with Census Bureau Pulse Survey data #

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

# Read in data on Households at risk of foreclosure #
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

# Assign ID for legend purposes #
Eviction_Risk_Data$ID <- "Households in Rental Arrears at Risk of Eviction"

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

# Assign ID for legend purposes #
Next_Rent_Risk_Data$ID <- "Households Unconfident in Next Month's Rent Payment"

# Specify colors #
Colors <- c("Households in Rental Arrears at Risk of Eviction" = "deeppink", "Households Unconfident in Next Month's Rent Payment" = "navyblue",
            "Households in Rental Arrears" = "darkgreen", "Cumulative Households Assisted, ERA1 & ERA2" = "grey70")

# Graph the Household Pulse Survey data #
Household_Pulse_Survey_Graph <- 
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
    aes(x = c(as.Date("2020-11-30"), as.Date("2020-12-31")), ymax = c(12.5,12.5), ymin = c(0,0)),
    color = NA,
    alpha = 1,
    fill = "grey50"
  ) +
  geom_ribbon(
    aes(x = c(as.Date("2020-10-31"), as.Date("2020-12-31")), ymax = c(13.25,13.25), ymin= c(0,0)),
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
    aes(x = as.Date("2020-11-30"), xend = as.Date("2020-11-30"), y = 0, yend = 12.5),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-10-31"), xend = as.Date("2020-10-31"), y = 0, yend = 13.25),
    color = "black",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = as.Date("2020-09-05"), xend = as.Date("2020-09-05"), y = 0, yend = 14),
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
    "text", x = as.Date("2020-12-15"), y = 2.75, label="Harris County", angle=270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-11-15"), y = 11.25, label="King County", angle=270, size = 2, color="black"
  ) +
  annotate(
    "text", x = as.Date("2020-09-22"), y = 13.5, label="LA", angle=270, size = 2, color="black"
  ) +
  geom_line(
    Behind_On_Rent_Data,
    mapping = aes(y = `Households in Rental Arrears`, x = `Date`, color = ID),
    linewidth = 0.75
  ) +
  geom_line(
    Eviction_Risk_Data,
    mapping = aes(y = `Households in Rental Arrears at Risk of Eviction`, x = `Date`, color = ID),
    linewidth = 0.75
  ) +
  geom_line(
    Next_Rent_Risk_Data,
    mapping = aes(y = `Households Unconfident in Next Month's Rent Payment`, x = `Date`, color = ID),
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
    limits = c(0,14),
    breaks = seq(0,14, by = 2),
    sec.axis = sec_axis(transform = ~./14 * 7, name = "Households Assisted (Millions)", breaks = seq(1, 7, by = 1))
  ) +
  scale_color_manual(
    values = Colors,
    name = "",
    breaks = c("Households in Rental Arrears at Risk of Eviction", "Households Unconfident in Next Month's Rent Payment", "Households in Rental Arrears", "Cumulative Households Assisted, ERA1 & ERA2")
  ) + 
  scale_x_date(
    limits = c(as.Date("2019-02-01"), as.Date("2023-04-01")),
    breaks = seq(as.Date("2019-02-01"), as.Date("2023-04-01"), by = "6 months") - 1
  ) +
  guides(
    color = guide_legend(position = "bottom")
  )

# Combine graphs #
Graphs_Page_1 <- ggarrange(Income_Timeseries_Graph, Unemployment_Timeseries_Graph, heights = c(1,1), nrow = 2)
# Return to code and combine all three time series into one dataframe if the authors like the combined graph #
Graphs_Page_2 <- ggarrange(Household_Pulse_Survey_Graph, heights = c(1,1), nrow = 2)

pdf("The Effects of ERA During the Pandemic Figure.pdf")
print(Graphs_Page_1)
print(Graphs_Page_2)
dev.off()


#### Make a note that the timing of the programs we study in these graphs comes from Appendix Table C.9 ####
