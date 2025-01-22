#This is figure 4

library(tidyr)
library(ggplot2)
library(dplyr)
library(vegan)
library(devEMF)

#load df
envfam <- read.csv('C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/KMreef_lm1_envfam_stdevfrommean.csv', row.names = 1)

#separate out data by habitat
all.fore <- filter(envfam, Reef.Type == "Forereef")
all.back <- filter(envfam, Reef.Type == "Backreef")
all.pass <- filter(envfam, Reef.Type == "Pass")



######ORDER THE FIGURES ACCORDING TO NORTH ABUNDANCE ######################
#Make the summary figure
meta.all <- as.data.frame(envfam[,1:49])
all <- as.data.frame(envfam[,c(2,50:ncol(envfam))])
# Reshape the data from wide to long format
all_long <- all %>%
  pivot_longer(cols = -Reef.Side, names_to = "Variable", values_to = "Value")
# Summarize the data: Calculate the mean and standard deviation for each variable and category
all_summary <- all_long %>%
  group_by(Variable, Reef.Side) %>%
  summarize(
    mean_value = mean(Value),
    sd_value = sd(Value),  # Calculate the standard deviation
    .groups = 'drop'
  )
#Reorder the families by mean in the north side
all_summary1 <- all_summary %>%
  group_by(Variable) %>%
  mutate(north_mean = ifelse(Reef.Side == "North", mean_value, NA)) %>%  # Calculate mean for Coral
  fill(north_mean, .direction = "updown") %>%  # Fill NA values with Coral mean within each Variable
  ungroup() %>%
  mutate(Variable = reorder(Variable, north_mean))
# Create the diverging dot plot with error bars
emf(file = "C:/Users/jacqu/Desktop/Figure5a.emf", emfPlus = T, width=9, height=6)
ggplot(all_summary1, aes(x = mean_value, y = Variable, color = Reef.Side)) +
  geom_point(size = 3, alpha = 0.9, position = position_dodge(width = 0.5), show.legend = FALSE) +  # Fill the original points
  geom_errorbarh(aes(xmin = mean_value - sd_value, xmax = mean_value + sd_value), alpha = 0.5, height = 0.6, position = position_dodge(width = 0.5)) +  # Add horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Add a vertical line at x = 0 for reference
  labs(x = "Mean Value", y = "Variables") +
  scale_color_manual(values = c("North" = "#F35B6B", "West" = "#83A7FD","East" = "#E1DA3A")) +
  theme_minimal()
dev.off()

######################Make the forereef figure###############################
meta.fore <- as.data.frame(all.fore[,1:49])
fore <- as.data.frame(all.fore[,c(2,50:ncol(all.fore))])
# Reshape the data from wide to long format
fore_long <- fore %>%
  pivot_longer(cols = -Reef.Side, names_to = "Variable", values_to = "Value")
# Summarize the data: Calculate the mean and standard deviation for each variable and category
fore_summary <- fore_long %>%
  group_by(Variable, Reef.Side) %>%
  summarize(
    mean_value = mean(Value),
    sd_value = sd(Value),  # Calculate the standard deviation
    .groups = 'drop'
  )
# Reorder the families based on the order in pass_summary
fore_summary1 <- fore_summary %>%
  mutate(Variable = factor(Variable, levels = levels(all_summary1$Variable)))  # Match levels with pass_summary

# Create the diverging dot plot with error bars
emf(file = "C:/Users/jacqu/Desktop/Figure5b.emf", emfPlus = T, width=9, height=6)
ggplot(fore_summary1, aes(x = mean_value, y = Variable, color = Reef.Side)) +
  geom_tile(size = 2.2, alpha = 0.9, height = .15, width = .1, position = position_dodge(width = 0.5), show.legend = FALSE) +  # Fill the original points
  geom_errorbarh(aes(xmin = mean_value - sd_value, xmax = mean_value + sd_value), alpha = 0.5, height = 0.6, position = position_dodge(width = 0.5)) +  # Add horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Add a vertical line at x = 0 for reference
  labs(x = "Mean Value", y = "Variables") +
  scale_color_manual(values = c("North" = "#FFADB8", "West" = "#98C7FF","East" = "#F3EA4D")) +
  theme_minimal()
dev.off()

####################Make the backreef figure##################################
meta.back <- as.data.frame(all.back[,1:49])
back <- as.data.frame(all.back[,c(2,50:ncol(all.back))])
# Reshape the data from wide to long format
back_long <- back %>%
  pivot_longer(cols = -Reef.Side, names_to = "Variable", values_to = "Value")
# Summarize the data: Calculate the mean and standard deviation for each variable and category
back_summary <- back_long %>%
  group_by(Variable, Reef.Side) %>%
  summarize(
    mean_value = mean(Value),
    sd_value = sd(Value),  # Calculate the standard deviation
    .groups = 'drop'
  )
# Reorder the data to match the order in pass_summary
back_summary <- back_summary %>%
  mutate(Variable = factor(Variable, levels = levels(all_summary1$Variable)))  # Match levels with pass_summary

# Create the diverging dot plot with error bars
emf(file = "C:/Users/jacqu/Desktop/Figure5c.emf", emfPlus = T, width=9, height=6)
ggplot(back_summary, aes(x = mean_value, y = Variable, color = Reef.Side)) +
  geom_point(size = 2, shape = 8, stroke = 1.3, alpha = 0.9, position = position_dodge(width = 0.5), show.legend = FALSE) +
  geom_errorbarh(aes(xmin = mean_value - sd_value, xmax = mean_value + sd_value), alpha = 0.5, height = 0.6, position = position_dodge(width = 0.5)) +  # Add horizontal error bars
  labs(x = "Mean Value", y = "Variables") +
  scale_color_manual(values = c("North" = "#F35B6B", "West" = "#83A7FD","East" = "#E1DA3A")) +
  theme_minimal()
dev.off()


#######################Make the pass figure##############################################
meta.pass <- as.data.frame(all.pass[,1:49])
pass <- as.data.frame(all.pass[,c(2,50:ncol(all.pass))])
# Reshape the data from wide to long format
pass_long <- pass %>%
  pivot_longer(cols = -Reef.Side, names_to = "Variable", values_to = "Value")
# Summarize the data: Calculate the mean and standard deviation for each variable and category
pass_summary <- pass_long %>%
  group_by(Variable, Reef.Side) %>%
  summarize(
    mean_value = mean(Value),
    sd_value = sd(Value),  # Calculate the standard deviation
    .groups = 'drop'
  )
#Reorder the families by mean in the north side
pass_summary1 <- pass_summary %>%
 mutate(Variable = factor(Variable, levels = levels(all_summary1$Variable)))  # Match levels with pass_summary
# Create the diverging dot plot with error bars
emf(file = "C:/Users/jacqu/Desktop/Figure5d.emf", emfPlus = T, width=9, height=6)
ggplot(pass_summary1, aes(x = mean_value, y = Variable, color = Reef.Side)) +
   geom_point(size = 3, alpha = 0.9, position = position_dodge(width = 0.5), show.legend = FALSE) +  # Fill the original points
  geom_errorbarh(aes(xmin = mean_value - sd_value, xmax = mean_value + sd_value), alpha = 0.5, height = 0.6, position = position_dodge(width = 0.5)) +  # Add horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Add a vertical line at x = 0 for reference
  labs(x = "Mean Value", y = "Variables") +
  scale_color_manual(values = c("North" = "#B33A4E", "West" = "#356EC0","East" = "#BFB735")) +
  theme_minimal()
dev.off()
