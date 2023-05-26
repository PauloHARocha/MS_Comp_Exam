library(dplyr)
library(survey)
library(sjstats)
library(MASS)
library(ggplot2)
library(GGally)

X <- read.csv("data/adult_2015_2019_vars_interest_final.csv")

dim(X)
head(X)

# getting weights 
repw <- which(names(X) %in% paste0("RAKEDW", 1:80))

# Define the survey design with raked weights
design <- svydesign(
  id = ~1,
  data = X[-repw],
  repweights = X[repw],
  weights = ~RAKEDW0,
  combined.weights = TRUE,
  type = "other",
  scale = 1,
  rscales = 1
)

# Generate descriptive statistics

df_plot <- data.frame(svyby(~DISTRESS, ~ YEAR_NAME, design, svymean))

pd <- position_dodge(0.1) # move them .05 to the left and right

p1 <- ggplot(df_plot, aes(
  x = YEAR_NAME, y = DISTRESS
)) +
  geom_errorbar(aes(ymin = DISTRESS - 1.96*se, ymax = DISTRESS + 1.96*se), width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3, shape = 21, fill = "white") + # 21 is filled circle
  xlab("Year") +
  ylab("Distress Score") +
  # expand_limits(y = 0) + # Expand y range
  # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
  theme_bw() +
  theme(
    text = element_text(size = 40),
    # legend.justification = c(1, 0),
    # legend.position = c(1, 0)
  ) # Position legend in bottom right

ggsave("figures/DISTRESS_YEAR_NAME.png", width = 10, height = 10, units = "in")

df_plot <- data.frame(svyby(~DISTRESS_BI, ~YEAR_NAME, design, svymean))

# bar plot of DISTRESS_BI as barplot with confidence interval by YEAR_NAME using ggplot2 in the same plot
p2 <- ggplot(df_plot) +
  geom_bar(aes(x = YEAR_NAME, y = DISTRESS_BI), stat = "identity", fill = "gray", alpha = 0.7) +
  geom_errorbar(aes(x = YEAR_NAME, ymin = DISTRESS_BI - 1.96 * se, ymax = DISTRESS_BI + 1.96 * se), 
  width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
  xlab("Year") +
  ylab("Proportion in high Distress") +
  theme_bw() +
    theme(
      text = element_text(size = 40),
      # legend.justification = c(1, 0),
      # legend.position = c(1, 0)
    ) # Position legend in bottom right
ggsave("figures/DISTRESS_BI_YEAR_NAME.png", width = 10, height = 10, units = "in")


# plot histogram of DISTRESS using ggplot2
p3 <- ggplot(X, aes(x = DISTRESS)) +
   geom_histogram(color = "#e9ecef", alpha = 0.6, position = "identity", binwidth = 1) +
   xlab("Distress score") +
  ylab("Count") +
   theme_bw() +
   theme(
    text = element_text(size = 40))

# save plot
ggsave("figures/DISTRESS_hist.png", width = 10, height = 10, units = "in")


# plot boxplot of DISTRESS using ggplot2 remove x ticks
p4 <- ggplot(X, aes(y = DISTRESS)) +
  geom_boxplot() +
  ylab("Distress score") +
  theme_bw() +
  theme(
    text = element_text(size = 40),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) # remove x ticks

# save plot
ggsave("figures/DISTRESS_boxplot.png", width = 10, height = 10, units = "in")


plot_grid(p3,
    p4,
    p1,
    p2,
    labels = c("A", "B", "C", "D"),
    label_size = 20,
    # label_x = 0.2,
    ncol = 2,
    nrow = 2
)

# save plot
ggsave("figures/DISTRESS_panel.png", width = 25, height = 25, units = "in")




df_plot <- data.frame(svyby(~ DISTRESS, ~ YEAR_NAME + Sex, design, svymean))

# plot DISTRESS by YEAR_NAME and SRSEX as factor

pd <- position_dodge(0.1) # move them .05 to the left and right

table(X$Sex)/nrow(X)*100

p_sex <- ggplot(df_plot, aes(x = YEAR_NAME, y = DISTRESS, colour = as.factor(Sex),
group = as.factor(Sex))) +
  geom_errorbar(aes(ymin = DISTRESS - 1.96*se, ymax = DISTRESS + 1.96*se), width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3, shape = 21, fill = "white") + # 21 is filled circle
  xlab("Year") +
  ylab("Distress score") +
  scale_colour_hue(
    name = "Sex", # Legend label, use darker colors
    breaks = c("Male", "Female"),
    labels = c("Male (55.78%)", "Female (44.22%)"),
    l = 40
  )+ # Use darker colors, lightness=40
  # ggtitle("Distress score by year and sex") +
  # expand_limits(y = 0) + # Expand y range
  # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
  theme_bw() +
  theme(
    text = element_text(size = 40),
    legend.justification = c(1, 0),
    legend.position = c(0.95, 0.02)
  ) # Position legend in bottom right


# save plot
ggsave("figures/DISTRESS_SEX.png", width = 10, height = 10, units = "in")

# Urban/Rural

df_plot <- data.frame(svyby(~DISTRESS, ~ YEAR_NAME + UrbanRural, design, svymean))

pd <- position_dodge(0.1) # move them .05 to the left and right

table(X$UrbanRural) / nrow(X) * 100

p_location <- ggplot(df_plot, aes(
    x = YEAR_NAME, y = DISTRESS, colour = as.factor(UrbanRural),
    group = as.factor(UrbanRural)
  )) +
    geom_errorbar(aes(ymin = DISTRESS - 1.96*se, ymax = DISTRESS + 1.96*se), width = .1, position = pd) +
    geom_line(position = pd) +
    geom_point(position = pd, size = 3, shape = 21, fill = "white") + # 21 is filled circle
    xlab("Year") +
    ylab("Distress score") +
    scale_colour_hue(
      name = "Urban/Rural", # Legend label, use darker colors
      breaks = c("Urban", "Rural"),
      labels = c("Urban (85.14%)", "Rural (14.86%)"),
      l = 40
    ) + # Use darker colors, lightness=40
    # ggtitle("Distress score by year and Urban/Rural") +
    # expand_limits(y = 0) + # Expand y range
    # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
    theme_bw() +
    theme(
      text = element_text(size = 40),
      legend.justification = c(1, 0),
      legend.position = c(0.95, 0.02)
    ) # Position legend in bottom right

# save plot
ggsave("figures/DISTRESS_UrbanRural.png", width = 10, height = 10, units = "in")


# OVRWT
df_plot <- data.frame(svyby(~DISTRESS, ~ YEAR_NAME + Overweight, design, svymean))

pd <- position_dodge(0.1) # move them .05 to the left and right

table(X$Overweight) / nrow(X) * 100

ggplot(df_plot, aes(
  x = YEAR_NAME, y = DISTRESS, colour = as.factor(Overweight),
  group = as.factor(Overweight)
)) +
  geom_errorbar(aes(ymin = DISTRESS - 1.96*se, ymax = DISTRESS + 1.96*se), width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3, shape = 21, fill = "white") + # 21 is filled circle
  xlab("Year") +
  ylab("Distress") +
  scale_colour_hue(
    name = "Overweight/Obese", # Legend label, use darker colors
    breaks = c("Yes", "No"),
    labels = c("Yes (61%)", "No (39%)"),
    l = 40
  ) + # Use darker colors, lightness=40
  ggtitle("Distress by year and Overweight/Obese") +
  # expand_limits(y = 0) + # Expand y range
  # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
  theme_bw() +
  theme(
    text = element_text(size = 20),
    legend.justification = c(1, 0),
      legend.position = c(0.95, 0.02)
  ) # Position legend in bottom right

# save plot
ggsave("figures/DISTRESS_Overweight.png", width = 10, height = 10, units = "in")

# AK28 - Feel safe in neighborhood
df_plot <- data.frame(svyby(~DISTRESS, ~ YEAR_NAME + FeelSafe, design, svymean))

pd <- position_dodge(0.1) # move them .05 to the left and right

table(X$FeelSafe) / nrow(X) * 100

p_safe <- ggplot(df_plot, aes(
  x = YEAR_NAME, y = DISTRESS, colour = as.factor(FeelSafe),
  group = as.factor(FeelSafe)
)) +
  geom_errorbar(aes(ymin = DISTRESS - 1.96*se, ymax = DISTRESS + 1.96*se), width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3, shape = 21, fill = "white") + # 21 is filled circle
  xlab("Year") +
  ylab("Distress score") +
  scale_colour_hue(
    name = "Feel Safe", # Legend label, use darker colors
    breaks = c("All_time", "Most_time", "None", "Some_time"),
    labels = c("All time (51.30%)", "Most time (39.49%)", 
    "Some time (7.67%)", "Not at all (1.54%)"),
    l = 40
  ) + # Use darker colors, lightness=40
  # ggtitle("Distress score by year and Feel Safe in neighborhood") +
  # expand_limits(y = 0) + # Expand y range
  # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
  theme_bw() +
  theme(
    text = element_text(size = 35),
    legend.justification = c(1, 0),
    legend.position = c(0.35, 0.70)
  ) # Position legend in bottom right

# save plot
ggsave("figures/DISTRESS_FeelSafe.png", width = 10, height = 10, units = "in")


# RACECN_P1 - Race
df_plot <- data.frame(svyby(~DISTRESS, ~ YEAR_NAME + Race, design, svymean))

pd <- position_dodge(0.1) # move them .05 to the left and right

table(X$Race) / nrow(X) * 100

ggplot(df_plot, aes(
  x = YEAR_NAME, y = DISTRESS, colour = as.factor(Race),
  group = as.factor(Race)
)) +
  geom_errorbar(aes(ymin = DISTRESS - 1.96 * se, ymax = DISTRESS + 1.96 * se), width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3, shape = 21, fill = "white") + # 21 is filled circle
  xlab("Year") +
  ylab("Distress score") +
  scale_colour_hue(
    name = "Race", # Legend label, use darker colors
    breaks = c("AfricanAmerican", "Asian", "Multiple", "Native", "OtherSingle", "White"),
    labels = c(
      "African American (5.42%)", "Asian (9.68%)", "Multiple (3.24%)",
      "Native (2.39%)", "Other (8.03%)", "White (71.23%)"
    ),
    l = 40
  ) + # Use darker colors, lightness=40
  ggtitle("Distress score by year and Race") +
  # expand_limits(y = 0) + # Expand y range
  # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
  theme_bw() +
  theme(
    text = element_text(size = 20),
    legend.justification = c(1, 0),
    legend.position = c(0.35, 0.85)
  ) # Position legend in bottom right

# save plot
ggsave("figures/DISTRESS_Race.png", width = 10, height = 10, units = "in")

# AHEDC_P1 - Education
df_plot <- data.frame(svyby(~DISTRESS, ~ YEAR_NAME + Education, design, svymean))

table(X$Education, X$YEAR_NAME)

p_edu <- ggplot(df_plot, aes(
    x = YEAR_NAME, y = DISTRESS, colour = as.factor(Education),
    group = as.factor(Education)
)) +
    geom_errorbar(aes(ymin = DISTRESS - se, ymax = DISTRESS + se), width = .1, position = pd) +
    geom_line(position = pd) +
    geom_point(position = pd, size = 3, shape = 21, fill = "white") + # 21 is filled circle
    xlab("Year") +
    ylab("Distress score") +
    scale_colour_hue(
        name = "Education", # Legend label, use darker colors
        breaks = c("HighSchool", "Master/Phd", "SomeCollege/Bachelor"),
        labels = c("HighSchool (28.40%)", "Master/Phd (18.17%)", "SomeCollege/Bachelor (53.42%)"),
        l = 40
    ) + # Use darker colors, lightness=40
    # ggtitle("Distress by year and Education") +
    # expand_limits(y = 0) + # Expand y range
    # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
    theme_bw() +
    theme(
        text = element_text(size = 35),
        legend.justification = c(1, 0),
        legend.position = c(0.95, 0.02)
    ) # Position legend in bottom right

# save plot
ggsave("figures/DISTRESS_Education.png", width = 10, height = 10, units = "in")


# SRAGE_P1 - Age
df_plot <- data.frame(svyby(~DISTRESS, ~ YEAR_NAME + Age, design, svymean))

table(X$Age) / nrow(X) * 100

p_age <- ggplot(df_plot, aes(
    x = YEAR_NAME, y = DISTRESS, colour = as.factor(Age),
    group = as.factor(Age)
)) +
    geom_errorbar(aes(ymin = DISTRESS - se, ymax = DISTRESS + se), width = .1, position = pd) +
    geom_line(position = pd) +
    geom_point(position = pd, size = 3, shape = 21, fill = "white") + # 21 is filled circle
    xlab("Year") +
    ylab("Distress score") +
    scale_colour_hue(
        name = "Age", # Legend label, use darker colors
        breaks = c("18-29", "30-44", "45-64", "65-84", "85+"),
        labels = c("18-29 (12.13%)", "30-44 (15.76%)", "45-64 (34.82%)", 
        "65-84 (32.52%)", "85+ (4.77%)"),
        l = 40
    ) + # Use darker colors, lightness=40
    # ggtitle("Distress by year and Age") +
    # expand_limits(y = 0) + # Expand y range
    # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
    theme_bw() +
    theme(
        text = element_text(size = 40),
        legend.justification = c(1, 0),
        legend.position = c(0.35, 0.70)
    ) # Position legend in bottom right

# save plot
ggsave("figures/DISTRESS_Age.png", width = 10, height = 10, units = "in")

# AE15 - smoked 100 cigaretes
df_plot <- data.frame(svyby(~DISTRESS, ~ YEAR_NAME + Smoked100, design, svymean))

table(X$Smoked100) / nrow(X) * 100

ggplot(df_plot, aes(
    x = YEAR_NAME, y = DISTRESS, colour = as.factor(Smoked100),
    group = as.factor(Smoked100)
)) +
    geom_errorbar(aes(ymin = DISTRESS - se, ymax = DISTRESS + se), width = .1, position = pd) +
    geom_line(position = pd) +
    geom_point(position = pd, size = 3, shape = 21, fill = "white") + # 21 is filled circle
    xlab("Year") +
    ylab("Distress score") +
    scale_colour_hue(
        name = "Smoked 100 cigaretes", # Legend label, use darker colors
        breaks = c("Yes", "No"),
        labels = c("Yes (37.98%)", "No (62.02%)"),
        l = 40
    ) + # Use darker colors, lightness=40
    ggtitle("Distress by year and smoked 100 cigaretes") +
    # expand_limits(y = 0) + # Expand y range
    # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
    theme_bw() +
    theme(
        text = element_text(size = 20),
        legend.justification = c(1, 0),
        legend.position = c(0.9, 0.02)
    ) # Position legend in bottom right

# save plot
ggsave("figures/DISTRESS_Smoked100.png", width = 10, height = 10, units = "in")


# AH33NEW - Born in US
df_plot <- data.frame(svyby(~DISTRESS, ~ YEAR_NAME + BornUS, design, svymean))

table(X$BornUS) / nrow(X) * 100

ggplot(df_plot, aes(
    x = YEAR_NAME, y = DISTRESS, colour = as.factor(BornUS),
    group = as.factor(BornUS)
)) +
    geom_errorbar(aes(ymin = DISTRESS - se, ymax = DISTRESS + se), width = .1, position = pd) +
    geom_line(position = pd) +
    geom_point(position = pd, size = 3, shape = 21, fill = "white") + # 21 is filled circle
    xlab("Year") +
    ylab("Distress score") +
    scale_colour_hue(
        name = "Born in US", # Legend label, use darker colors
        breaks = c("Yes", "No"),
        labels = c("Yes (77.89%)", "No (22.11%)"),
        l = 40
    ) + # Use darker colors, lightness=40
    ggtitle("Distress by year and born in US") +
    # expand_limits(y = 0) + # Expand y range
    # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
    theme_bw() +
    theme(
        text = element_text(size = 20),
        legend.justification = c(1, 0),
        legend.position = c(0.9, 0.02)
    ) # Position legend in bottom right

# save plot
ggsave("figures/DISTRESS_BornUS.png", width = 10, height = 10, units = "in")


# UR_BG6 Rural and Urban - Claritas (By block group) (6 levels)
df_plot <- data.frame(svyby(~DISTRESS, ~ YEAR_NAME + UrbanRural6, design, svymean))


table(X$UrbanRural6) / nrow(X) * 100

p_location6 <- ggplot(df_plot, aes(
  x = YEAR_NAME, y = DISTRESS, colour = as.factor(UrbanRural6),
  group = as.factor(UrbanRural6)
)) +
  geom_errorbar(aes(ymin = DISTRESS - se, ymax = DISTRESS + se), width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3, shape = 21, fill = "white") + # 21 is filled circle
  xlab("Year") +
  ylab("Distress score") +
  scale_colour_hue(
    name = "Urban/Rural 6 levels", # Legend label, use darker colors
    breaks = c("2ndCity", "Mixed", "Rural", "Suburban", "Town", "Urban"),
    labels = c("2ndCity (4.32%)", "Mixed (10.47%)", "Rural (6.18%)", "Suburban (11.21%)", 
    "Town (7.40%)", "Urban (21.07%)"),
    l = 40
  ) + # Use darker colors, lightness=40
  # ggtitle("Distress by year and urban/rural 6 levels") +
  # expand_limits(y = 0) + # Expand y range
  # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
  theme_bw() +
  theme(
    text = element_text(size = 40),
    legend.justification = c(1, 0),
    legend.position = c(0.4, 0.60)
  ) # Position legend in bottom right

# save plot
ggsave("figures/DISTRESS_UrbanRural6.png", width = 10, height = 10, units = "in")


plot_grid(p_age,
  p_safe,
  p_edu,
  p_sex,
  p_location,
  p_location6,
  # labels = c("A", "B", "C", "D"),
  # label_size = 20,
  # label_x = 0.2,
  ncol = 3,
  nrow = 3
)

# save plot
ggsave("figures/Bivariate_panel.png", width = 45, height = 35, units = "in")