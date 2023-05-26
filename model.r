library(dplyr)
library(survey)
library(sjstats)
library(MASS)
library(ggplot2)
library(GGally)
library(cowplot)
library(xtable)
library(lawstat)
library(car)


setwd("/home/jovyan/work/MS_Comp_Exam/")

X <- read.csv("data/adult_2015_2019_vars_interest_final.csv")

dim(X)
head(X)

AJ_cols <- c("AJ29", "AJ30", "AJ31", "AJ32", "AJ33", "AJ34")

# get the number of rows where all columns from AJ_cols are equal to 4
sum(rowSums(X[AJ_cols] == 4) == 5)

# getting weights
repw <- which(names(X) %in% paste0("RAKEDW", 1:80))

X$UrbanRural6 <- factor(X$UrbanRural6)
X$UrbanRural6 <- relevel(X$UrbanRural6, ref = "Urban")

X$UrbanRural <- factor(X$UrbanRural)
X$UrbanRural <- relevel(X$UrbanRural, ref = "Urban")

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


# Next steps
# Fit logistic regression model with DISTRESS as the outcome and the covariates as predictors

table(X$DISTRESS_BI)

# fit logistic regression model with YEAR and UrbanRural with Urban as reference using survey package
fit_log_year <- svyglm(DISTRESS_BI ~ YEAR, design = design, family = binomial(link = "logit"))
summary(fit_log_year)

p1 <- ggcoef(fit_log_year, exponentiate = TRUE,
conf.int = TRUE, conf.level = 0.95,
exclude_intercept = TRUE) + 
theme_bw() +
theme(legend.position = "none", aspect.ratio = 1 / 8) +
scale_y_discrete(
    name = "Model 1",
    breaks = c("YEAR"),
    labels = c("   Year")
) +
xlim(0.5, 1.5)

fit_log_location <- svyglm(DISTRESS_BI ~ YEAR + UrbanRural, design = design, family = binomial(link = "logit"))
summary(fit_log_location)

p2 <- ggcoef(fit_log_location,
    exponentiate = TRUE,
    exclude_intercept = TRUE,
    conf.int = TRUE, conf.level = 0.95
) + theme_bw() +
    theme(legend.position = "none", aspect.ratio = 1 / 6) +
        scale_y_discrete(
            name = "Model 2",
            breaks = c("YEAR", "UrbanRuralRural"),
            labels = c("Year", "Rural")
        ) +
        xlim(0.5, 1.5)

fit_log_location6 <- svyglm(DISTRESS_BI ~ YEAR + UrbanRural6, design = design, family = binomial(link = "logit"))
summary(fit_log_location6)

p3 <- ggcoef(fit_log_location6,
    exponentiate = TRUE,
    exclude_intercept = TRUE,
    conf.int = TRUE, conf.level = 0.95
) + theme_bw() +
    theme(legend.position = "none", aspect.ratio = 1 / 6) +
        scale_y_discrete(
            name = "Model 3",
            breaks = c("YEAR", "UrbanRural62ndCity", "UrbanRural6Mixed", "UrbanRural6Rural", 
            "UrbanRural6Suburban", "UrbanRural6Town"),
            labels = c("Year", "2nd City", "Mixed", "Rural", "Suburban", "Town")
        ) +
        xlim(0.5, 1.5)


fit_log_full_location <- svyglm(DISTRESS_BI ~ YEAR + UrbanRural + Sex + Age + Education +
Race + BornUS + FeelSafe + Overweight + Smoked100, 
design = design, family = binomial(link = "logit"))
summary(fit_log_full_location)

vif(fit_log_full_location6)

png(filename = "figures/logistic_location_fitted_residuals.png")
plot(fit_log_full_location, which=1)
dev.off()


ggcoef(fit_log_full_location,
    exponentiate = TRUE,
    exclude_intercept = TRUE,
    conf.int = TRUE, conf.level = 0.95
) + 
xlab("Odds Ratio (OR)") +
theme_bw() +
    theme(legend.position = "none", aspect.ratio = 2 / 3) +
        scale_y_discrete(
            name = "",
            breaks = c("YEAR", "UrbanRuralRural", "SexMale", "Age30-44", "Age45-64",
"Age65-84", "Age85+", "EducationMaster/Phd", "EducationSomeCollege/Bachelor",
"RaceAsian", "RaceMultiple", "RaceNative", "RaceOtherSingle",
"RaceWhite", "BornUSYes", "FeelSafeMost_time", "FeelSafeNone",
"FeelSafeSome_time", "OverweightYes", "Smoked100Yes"),
            labels = c(
                "Year", "Rural", "Male", "Age 30-44", "Age 45-64",
                "Age 65-84", "Age 85+", "Master/Phd", "SomeCollege/Bachelor",
                "Race Asian", "Race Multiple", "Race Native", "Race Other Single",
                "Race White", "Born in US", "Feel Safe Most time", "Feel Safe None",
                "Feel Safe Some time", "Overweight/Obese", "Smoked 100"
            )
        )


ggsave("figures/logitic_coef_plot_year_location_full.png", width = 6, height = 4, units = "in")

# dataframe to latex using xtable
library(xtable)


# name of each row
table_or <- data.frame(exp(coef(fit_log_full_location)))
rownames(table_or) <- c(
    "I", "Year", "Rural", "Male", "Age 30-44", "Age 45-64",
    "Age 65-84", "Age 85+", "Master/Phd", "SomeCollege/Bachelor",
    "Race Asian", "Race Multiple", "Race Native", "Race Other Single",
    "Race White", "Born in US", "Feel Safe Most time", "Feel Safe None",
    "Feel Safe Some time", "Overweight/Obese", "Smoked 100"
)

xtable(table_or)



fit_log_full_location6 <- svyglm(DISTRESS_BI ~ YEAR + UrbanRural6 + Sex + Age + Education +
Race + BornUS + FeelSafe + Overweight + Smoked100, 
design = design, family = binomial(link = "logit"))
summary(fit_log_full_location6)

png(filename = "figures/logistic_location6_fitted_residuals.png")
plot(fit_log_full_location6, which = 1)
dev.off()


ggcoef(fit_log_full_location6,
    exponentiate = TRUE,
    exclude_intercept = TRUE,
    conf.int = TRUE, conf.level = 0.95
) + 
xlab("Odds Ratio (OR)") +
theme_bw() +
    theme(legend.position = "none", aspect.ratio = 4 / 5) +
        scale_y_discrete(
            name = "",
            breaks = c(
                "YEAR", "UrbanRural6Town", "UrbanRural6Suburban", "UrbanRural6Rural",
                "UrbanRural6Mixed", "UrbanRural62ndCity",
                "SexMale", "Age30-44", "Age45-64",
                "Age65-84", "Age85+", "EducationMaster/Phd", "EducationSomeCollege/Bachelor",
                "RaceAsian", "RaceMultiple", "RaceNative", "RaceOtherSingle",
                "RaceWhite", "BornUSYes", "FeelSafeMost_time", "FeelSafeNone",
                "FeelSafeSome_time", "OverweightYes", "Smoked100Yes"
            ),
            labels = c(
                "Year", "Town", "Suburban","Rural", "Mixed", "2ndCity",
                "Male", "Age 30-44", "Age 45-64",
                "Age 65-84", "Age 85+", "Master/Phd", "SomeCollege/Bachelor",
                "Race Asian", "Race Multiple", "Race Native", "Race Other Single",
                "Race White", "Born in US", "Feel Safe Most time", "Feel Safe None",
                "Feel Safe Some time", "Overweight/Obese", "Smoked 100"
            )
        )

ggsave("figures/logitic_coef_plot_year_location6_full.png", width = 6, height = 4, units = "in")




# name of each row
table_or <- data.frame(exp(coef(fit_log_full_location6)))
rownames(table_or) <- c(
    "I","Year", "Town", "Suburban", "Rural", "Mixed", "2ndCity",
    "Male", "Age 30-44", "Age 45-64",
    "Age 65-84", "Age 85+", "Master/Phd", "SomeCollege/Bachelor",
    "Race Asian", "Race Multiple", "Race Native", "Race Other Single",
    "Race White", "Born in US", "Feel Safe Most time", "Feel Safe None",
    "Feel Safe Some time", "Overweight/Obese", "Smoked 100"
)

xtable(table_or)

# plot_grid(p1,
#     p2,
#     p3,
#     # labels = c("Fig A", "Fig B"),
#     # label_x = 0.2,
#     nrow = 3
# )



# Fit the negative binomial model with DISTRESS as the outcome and 2 level location
fit_nb_location <- svyglm.nb(DISTRESS ~ YEAR + UrbanRural + Sex + Age + Education +
    Race + BornUS + FeelSafe + Overweight + Smoked100, design = design)

summary(fit_nb_location)


nb_res <- residuals(fit_nb_location, type = "pearson")
nb_fit <- predict(fit_nb_location)

png(filename = "figures/negative_binomial_location_fitted_residuals.png")

plot(nb_fit, nb_res,
    xlab = "Predicted values",
    ylab = "Residuals",
    # ylim = c(-2,2),
    main = "Residuals vs Fitted"
)
abline(0, 0, lty = "dashed")

lines(smooth.spline(nb_fit, nb_res), col = "red", type = "l")

dev.off()


fit_nb_location

fit_nb_location_irr <- data.frame(
    term = c(
         "Year", "Rural", "Male", "Age 30-44", "Age 45-64",
        "Age 65-84", "Age 85+", "Master/Phd", "SomeCollege/Bachelor",
        "Race Asian", "Race Multiple", "Race Native", "Race Other Single", "Race White",
        "Born in US", "Feel Safe Most time", "Feel Safe None", "Feel Safe Some time",
        "Overweight/Obese", "Smoked 100"
    ),
    irr = c(
        1.0354, 1.0225, 0.9123, 0.8020, 0.6866, 0.5235, 0.5266, 0.8364, 0.9072,
        0.9487, 1.1391, 1.1027, 0.9941, 0.9782, 1.0414, 1.2419, 2.0327, 1.5803, 1.0046, 1.2514
    ),
    conf.low = c(
        1.0268, 0.9855, 0.8914, 0.7755, 0.6648, 0.5032, 0.4895, 0.8067, 0.8829,
        0.8918, 1.0636, 1.0116, 0.9371, 0.9307, 1.0113, 1.2112, 1.8858, 1.5187, 0.9806, 1.2211
    ),
    conf.high = c(
        1.0440, 1.0608, 0.9337, 0.8294, 0.7091, 0.5447, 0.5665, 0.8671, 0.9321,
        1.0092, 1.2198, 1.2020, 1.0546, 1.0280, 1.0724, 1.2734, 2.1909, 1.6444, 1.0292, 1.2825
    )
)



pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(fit_nb_location_irr, aes(
    y = term, x = irr
)) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0, position = pd) +
    geom_line(position = pd) +
    geom_point(position = pd, size = 2, shape = 21, fill = "black") + # 21 is filled circle
    geom_vline(xintercept = 1, size=1, linetype = "dotted", color = "gray")+
    xlab("Odds Ratio (OR)") +
    # expand_limits(y = 0) + # Expand y range
    # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
    theme_bw() +
    theme(
        legend.position = "none", 
        aspect.ratio = 4 / 5,
        # text = element_text(size = 20),
        # legend.justification = c(1, 0),
        # legend.position = c(1, 0)
    ) # Position legend in bottom right

ggsave("figures/negative_binomial_coef_plot_year_location.png", width = 6, height = 4, units = "in")


# show first two columns of fit_nb_location_irr without rownames with latex xtable
print(xtable(fit_nb_location_irr[,1:2]), include.rownames = FALSE)


# Fit the negative binomial model with DISTRESS as the outcome and 6 level location
fit_nb_location6 <- svyglm.nb(DISTRESS ~ YEAR + UrbanRural6 + Sex + Age + Education +
    Race + BornUS + FeelSafe + Overweight + Smoked100, design = design)

summary(fit_nb_location6)

nb_res <- residuals(fit_nb_location6, type = "pearson")
nb_fit <- predict(fit_nb_location6)

png(filename = "figures/negative_binomial_location6_fitted_residuals.png")

plot(nb_fit, nb_res,
    xlab = "Predicted values",
    ylab = "Residuals",
    # ylim = c(-2,2),
    main = "Residuals vs Fitted"
)
abline(0, 0, lty = "dashed")

lines(smooth.spline(nb_fit, nb_res), col = "red", type = "l")

dev.off()

fit_nb_location6


fit_nb_location6_irr <- data.frame(
    term = c(
        "Year", "2nd City", "Mixed", "Rural",
        "Suburban", "Town", "Male", "Age 30-44", "Age 45-64", "Age 65-84",
        "Age 85+", "Master/Phd", "SomeCollege/Bachelor", "Race Asian",
        "Race Multiple", "Race Native", "Race OtherSingle", "Race White", "Born in US", "Feel Safe Most time",
        "Feel Safe None", "Feel Safe Some time", "Overweight/Obese", "Smoked 100"
    ),
    irr = c(
         1.0233, 1.0350, 1.0410, 1.0228, 0.9486, 0.9635, 0.9025, 0.7749, 0.6169, 0.4638,
        0.4732, 0.8921, 0.9460, 1.0000, 1.1237, 1.1757, 1.0326, 1.0320, 1.0631, 1.2415, 1.9730,
        1.5026, 1.0131, 1.2418
    ),
    conf.low = c(
         1.0055, 0.9759, 1.0001, 0.9659, 0.9126, 0.9164, 0.8770, 0.7439, 0.5928, 0.4409,
        0.4314, 0.8537, 0.9139, 0.9340, 1.0373, 1.0572, 0.9608, 0.9756, 1.0248, 1.2035, 1.7666,
        1.4303, 0.9838, 1.2045
    ),
    conf.high = c(
         1.0414, 1.0978, 1.0835, 1.0831, 0.9861, 1.0130, 0.9287, 0.8073, 0.6420, 0.4879,
        0.5192, 0.9322, 0.9791, 1.0707, 1.2173, 1.3075, 1.1097, 1.0918, 1.1028, 1.2806, 2.2035,
        1.5786, 1.0432, 1.2803
    )
)



pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(fit_nb_location6_irr, aes(
    y = term, x = irr
)) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0, position = pd) +
    geom_line(position = pd) +
    geom_point(position = pd, size = 2, shape = 21, fill = "black") + # 21 is filled circle
    geom_vline(xintercept = 1, size = 1, linetype = "dotted", color = "gray") +
    xlab("Odds Ratio (OR)") +
    # expand_limits(y = 0) + # Expand y range
    # scale_y_continuous(breaks = 0:20 * 4) + # Set tick every 4
    theme_bw() +
    theme(
        legend.position = "none",
        aspect.ratio = 4 / 5,
        # text = element_text(size = 20),
        # legend.justification = c(1, 0),
        # legend.position = c(1, 0)
    ) # Position legend in bottom right

ggsave("figures/negative_binomial_coef_plot_year_location6.png", width = 6, height = 4, units = "in")


# show first two columns of fit_nb_location_irr without rownames with latex xtable
print(xtable(fit_nb_location6_irr[, 1:2]), include.rownames = FALSE)

