# Script to make detailed disposable income figure for the appendix #

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
Nipa_Deflator <- read_dta("nipa-simplified-monthly.dta")
Working_Age_Data$'Nipa Deflator' <- 0
for (i in 1:nrow(Working_Age_Data)) {
  Working_Age_Data$`Nipa Deflator`[i] <- Nipa_Deflator$nipa_deflator[Nipa_Deflator$year == Working_Age_Data$year[i] & Nipa_Deflator$month == Working_Age_Data$month[i]]
}

# Generate inverse probability weights #
Working_Age_Data <- Working_Age_Data[order(Working_Age_Data$year, Working_Age_Data$month, Working_Age_Data$p),]

# Keep only bottom 50% of households #
Working_Age_Data <- Working_Age_Data[Working_Age_Data$p <= 49000,]
Working_Age_Data <- Working_Age_Data[,-3]

# Make a dataframe of averages #
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

# Specify 'Lower' & 'Fill' variables for geom_ribbon #
for (i in 1:nrow(Income_Timeseries_Melted)) {
  if (Income_Timeseries_Melted$`Type of Income`[i] == "Factor Income") {
    Income_Timeseries_Melted$Lower[i] <- Income_Timeseries_Melted$`Monthly Income per adult (constant USD)`[Income_Timeseries_Melted$Date == Income_Timeseries_Melted$Date[i] & Income_Timeseries_Melted$`Type of Income` == "Factor Income"]
    Income_Timeseries_Melted$Fill[i] <- NA
  }
  if (Income_Timeseries_Melted$`Type of Income`[i] == "Subsidized Factor Income") {
    Income_Timeseries_Melted$Lower[i] <- Income_Timeseries_Melted$`Monthly Income per adult (constant USD)`[Income_Timeseries_Melted$Date == Income_Timeseries_Melted$Date[i] & Income_Timeseries_Melted$`Type of Income` == "Factor Income"]
    Income_Timeseries_Melted$Fill[i] <- "Paycheck Protection Program"
  }
  if (Income_Timeseries_Melted$`Type of Income`[i] == "Subsidized Pretax National Income") {
    Income_Timeseries_Melted$Lower[i] <- Income_Timeseries_Melted$`Monthly Income per adult (constant USD)`[Income_Timeseries_Melted$Date == Income_Timeseries_Melted$Date[i] & Income_Timeseries_Melted$`Type of Income` == "Subsidized Factor Income"]
    Income_Timeseries_Melted$Fill[i] <- "UI and Other Benefits, Net of Contributions"
  }
  if (Income_Timeseries_Melted$`Type of Income`[i] == "Post-Tax Disposable Income") {
    Income_Timeseries_Melted$Lower[i] <- Income_Timeseries_Melted$`Monthly Income per adult (constant USD)`[Income_Timeseries_Melted$Date == Income_Timeseries_Melted$Date[i] & Income_Timeseries_Melted$`Type of Income` == "Subsidized Pretax National Income"]
    Income_Timeseries_Melted$Fill[i] <- "Cash Transfers & COVID Stimulus, Net of Taxes"
  }
}
Income_Timeseries_Melted$Lower <- as.numeric(Income_Timeseries_Melted$Lower)

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

# Specify colors, shapes, & fill #
Colors <- c("Factor Income" = "black", "Post-Tax Disposable Income" = "black", "Cumulative Households Assisted, ERA1 & ERA2" = "grey70")
Shapes <- c("Factor Income" = 1, "Subsidized Factor Income" = 0, "Subsidized Pretax National Income" = 2, 
            "Post-Tax Disposable Income" = 5)
Fills <- c("Paycheck Protection Program" = "lightblue", "UI and Other Benefits, Net of Contributions" = "lightpink", "Cash Transfers & COVID Stimulus, Net of Taxes" = "lightgreen")


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
  geom_ribbon(
    Income_Timeseries_Melted,
    mapping = aes(ymax = `Monthly Income per adult (constant USD)`, ymin = Lower, x = Date, group = `Type of Income`, fill = Fill),
  ) +
  geom_line(
    Income_Timeseries_Melted_Factor_and_Disposable,
    mapping = aes(y = `Monthly Income per adult (constant USD)`, x = `Date`, group = `Type of Income`, color = `Type of Income`),
  ) +
  geom_point(
    Income_Timeseries_Melted_Factor_and_Disposable,
    mapping = aes(y = `Monthly Income per adult (constant USD)`, x = `Date`, group = `Type of Income`, color = `Type of Income`, shape = `Type of Income`, color = `Type of Income`, shape = `Type of Income`),
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
    plot.margin = margin(0.2,0.2,0.1,0.2, "cm"),
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
  scale_shape_manual(
    values = Shapes
  ) +
  scale_fill_manual(
    values = Fills,
    na.translate = F,
    breaks = c("Cash Transfers & COVID Stimulus, Net of Taxes", "UI and Other Benefits, Net of Contributions", "Paycheck Protection Program")
  ) +
  guides(
    color = guide_legend(override.aes = list(linetype = c("solid", "solid", "dashed"), shape = c(1,5,NA)), nrow = 1, position = "bottom"),
    shape = "none",
    fill = guide_legend(nrow = 3)
  ) + 
  scale_y_continuous(
    limits = c(0,4500),
    breaks = seq(0, 4500, by = 500),
    sec.axis = sec_axis(transform = ~./ 4500 * 7, name = "Households Assisted (Millions)", breaks = seq(1, 7, by = 1))
  )

# Save as a ggarrange object #
Plot <- ggarrange(Income_Timeseries_Graph, nrow = 2, heights = c(1,1))

# Export the graph #
pdf("Disposable Income During COVID-19, Appendix.pdf")
print(Plot)
dev.off()

