colony_data <- read.csv("pdamicornis.csv")

library(ggplot2)

colony_data$areadead = (colony_data$Perc_D * colony_data$D1 * colony_data$D2 /100)

colony_data$areaalive <- (colony_data$D1 * colony_data$D2 * ((100 - colony_data$Perc_D)/100))

colony_data$area <- (colony_data$D1 * colony_data$D2)


##### BOX PLOT SIZE ########

area_data <- data.frame(
  Type = rep(c("Area", "AreaAlive", "AreaDead"), each = nrow(colony_data)),
  Value = c(colony_data$area, colony_data$areaalive, colony_data$areadead)
)

area_data2 <- data.frame(
  Type = rep(c("Area", "AreaAlive"), each = nrow(colony_data)),
  Value = c(colony_data$area, colony_data$areaalive)
)

# Customized theme with black axes lines and larger font sizes
my_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 14)
  )

##########
library(tidyr)
library(dplyr)

# Reshape summary_data to have separate columns for mean and se
summary_data <- area_data2 %>%
  group_by(Type) %>%
  summarize(
    Mean = mean(Value),
    SE = sd(Value) / sqrt(n())
  )

summary_data_df <- as.data.frame(summary_data)

my_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )

ggplot() +
  geom_violin(data = area_data2, aes(x = Type, y = Value, fill = Type), alpha = 0.5) +
  geom_errorbar(data = summary_data_df, aes(x = Type, ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.25, color = "black", size = 1, position = position_dodge(width = 0.9)) +
  labs(
    x = "Area Type",
    y = expression(paste("Colony Area (cm"^2, " ± SE)")),
  ) +
  scale_fill_manual(values = c("grey", "white")) +
  my_theme
############

#Finding Values: 

areabreak <- c(1000,2000,3000,4000)

mean(area_data2$Value[area_data2$Type == "Area"])
mean(area_data2$Value[area_data2$Type == "AreaAlive"])
min(area_data2$Value[area_data2$Type == "Area"])
max(area_data2$Value[area_data2$Type == "Area"])


################################## Plot for % mortality and Size 
breaks <- c(0, 25, 50, 75, 100)

colony_data$arearanges <- cut(colony_data$Perc_D, breaks = breaks, labels = c("1000", "2000", "3000", "4000"), include.lowest = TRUE)

table(colony_data$arearanges)
# Create a factor variable representing the percentage mortality groups
colony_data$Perc_D_Group <- cut(colony_data$Perc_D, breaks = breaks, labels = c("0-25%", "25-50%", "50-75%", "75-100%"), include.lowest = TRUE)

# Customized theme with black axes lines and larger font sizes
my_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )

colony_data$perc <- (colony_data$Perc_D)


lm_model <- lm(Perc_D ~ area, data = colony_data)

# Plot with a single trend line
ggplot(colony_data, aes(x = area, y = Perc_D, fill = Perc_D_Group)) +
  geom_point(shape = 21, size = 3, colour = "black") +
  scale_fill_manual(values = c("0-25%" = "black", "25-50%" = "azure3", "50-75%" = "grey", "75-100%" = "azure2")) +
  labs(
    x = expression(paste("Colony Size (cm"^2, ")")),
    y = "% of Partial Mortality",
    fill = "% Mortality Groups"
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
  my_theme

# Perform linear regression
lm_model <- lm(area ~ Perc_D, data = colony_data)

# Get summary of the linear regression model
summary(lm_model)

#################### Frequency of Mortality in relation to Competition #######


breaks <- c(0, 25, 50, 75, 100)

# Create a factor variable representing the groups
colony_data$Perc_D_Group <- cut(colony_data$Perc_D, breaks = breaks, labels = c("0-25%", "25-50%", "50-75%", "75-100%"), include.lowest = TRUE)

# Count the frequency of each DWC category within each percentage mortality group
group_freq <- with(colony_data, table(Perc_D_Group, DWC))

# Convert the table to a dataframe for plotting
group_freq_df <- as.data.frame(group_freq)
names(group_freq_df) <- c("Perc_D_Group", "DWC", "Frequency")

my_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16),  # Adjust the legend title size
    legend.text = element_text(size = 12),# Adjust the legend text size
  )

# Create a grouped bar plot with customized aesthetics
ggplot(group_freq_df, aes(x = Perc_D_Group, y = Frequency, fill = DWC)) +
  geom_bar(stat = "identity") +
  labs(
    x = "% Mortality Groups",
    y = "Number of Colonies",
    fill = "Mortality Location"  # Change the legend name
  ) +
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "grey"), labels = c("Presence of Competition", "Absence of Competition")) +
  my_theme


chi_square_test <- chisq.test(group_freq_df$Frequency)

################### Mortality and competition occurance #####################

taxonomy_freq <- with(colony_data, table(Taxonomy, DWC))

# Convert the table to a dataframe for plotting
taxonomy_freq_df <- as.data.frame(taxonomy_freq)
names(taxonomy_freq_df) <- c("Taxonomy", "DWC", "Frequency")

# Create a custom theme
my_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
  )

# Create a grouped bar plot with customized aesthetics
ggplot(taxonomy_freq_df, aes(x = Taxonomy, y = Frequency, fill = DWC)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Taxonomy",
    y = "Number of Colonies",
    fill = "DWC"  
  ) +
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "grey"), labels = c("TRUE", "FALSE")) +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

##############
DWC_freq <- table(colony_data$DWC)

# Calculate percentages
DWC_percent <- round(prop.table(DWC_freq) * 100, 1)

# Create a pie chart with percentages as labels
pie(DWC_freq, 
    labels = c(
      paste("Presence of Competition (", DWC_percent[1], "%)"),
      paste("Absence of Competition (", DWC_percent[2], "%)")
    ), 
    main = "Frequency of Partial Mortality in Relation to Competition",
    col = c("grey", "black"))


# Fit an ANOVA model
results <- lm(Perc_D ~ DWC, data = colony_data)

anova(results)

########### Competition Taxonomy ###########

WLS_taxonomy_freq <- table(colony_data$WLS, colony_data$Taxonomy)

# Convert the table to a data frame
WLS_taxonomy_df <- as.data.frame(WLS_taxonomy_freq)

# Rename the columns for clarity
names(WLS_taxonomy_df) <- c("WLS", "Taxonomy", "Frequency")

wls_colors <- c("W" = "black", "L" = "grey", "S" = "azure4")

my_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
  )

############### Taxonomy by proportion #######

ggplot(WLS_taxonomy_df, aes(x = Taxonomy, fill = WLS, y = Frequency)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "Taxonomy",
    y = "Proportion",
    fill = "Competition Outcome"
  ) +
  scale_fill_manual(values = wls_colors, labels = c("Losing", "Standstill", "Winning")) +  # Set custom colors for WLS
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentages
  my_theme

chi_sq_result <- chisq.test(WLS_taxonomy_freq)

# Print the chi-square test result
print(chi_sq_result)

# Extract the p-value from the result
p_value <- chi_sq_result$p.value

######### Reef Zone distributions 

my_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
  )

ggplot(colony_data, aes(x = Location)) +
  geom_bar(fill = "grey", color = "black") +
  labs(
       x = "Reef Zone",
       y = "Number of Colonies") +
  my_theme

