 Hi Paulo,

The datasets are for 2015-2019. The databases to be using are included in their respective year folders, name is adult.sas7bdat.

In the CHIS folder, you will need to look at variables of interest excel file to ensure that you know which variables to use
For the analysis, I also included the pdf files for the construction of variables for reference.

In summary, you will be analyzing data for 2015 -2019, and the files to look at are:
1) The sas data files which you can import on R

2) The variables_of_interest.xlsx  file to understand which variables you will analyze

3) The 2018_contrusted_variables pdf file describes the survey in detail

4) Any formats that you may need are included in the individual folders for the respective years.

Note: these datasets are survey so you will need to use weight for the analysis

Aim of study: To evaluate trends in depression rates (AJ32 and AF66)  in CHIS over the years, unadjusted and adjusted analysis.
For the most recent years, consider an analysis in which you look at all mental health variables (5 listed here), not just depression and characterize potential clusters of mental health cohorts based on risk factors? Can you determine which factors are most important in predicting mental health issues? Demographics, health behaviors?
 
AJ32 - FEEL DEPRESSED PAST 30 DAYS
AF66 - FEEL DEPRESSED WORST MONTH
AF79 - COMPLETED RECOMMENDED MENTAL HEALTH TREATMENT
AF86 - EVER THOUGHT TO COMMIT SUICIDE
AF80 - MAIN REASON QUIT MENTAL HEALTH TREATMENT
 
STATISTICAL ANALYSIS PLAN:

In this analysis we are interested in understanding mental health outcomes captured in the two outcomes described for years 2015-2019.
You can study each outcome individually but also could combine if you desire.
 
- First, Univariate analysis
- Second, Bivariate analysis over time in terms of demographic characteristics. You don’t have to run all bivariate, for pick at least two sections.
Section A variables: DEMOGRAPHIC INFORMATION, PART I
Section  B variables: General Health Condition
Section C variables: Health Behaviors
Section K variables: EMPLOYMENT, INCOME, POVERTY STATUS & FOOD SECURITY
Section M variables: HOUSING & COMMUNITY INVOLVEMENT
Note that this is univariate analysis. Note that the variable age could be categorized if you are interested! 

- Third: Prediction model that takes into account time and location (we don’ have zipcode but region variable).
 
- Fourth: Any additional analysis that help us understand the most important factors to predicting mental health outcomes in California
As per this dataset.
 
SECTIONS TO INCLUDE IN YOUR REPORT:
 
Introduction. State your topic of interest, your study question.
Discuss the dataset, including the type of study that generated the data.
Design of Analysis. Describe the study performed by: identifying an outcome variable and the explanatory variables. What analysis did you perform to answer the question (e.g., univariate, bivariate, multivariable, cluster analysis?).
What hypothesis test performed (including null and alternative hypotheses, appropriate choice of test, and decision rule for rejecting or failing to reject the null hypothesis) and the additional statistical analysis performed.
Methods. Describe data cleaning steps, steps taken to address missing data or miscoded values, creation of new variables as necessary, formatting, subsetting, and data transformations performed or state why these steps were not deemed necessary
Describe SAS/R procedures used to perform relevant statistical analyses and why these procedures were chosen.
Please note that the detailed section of your Statistical Analysis approach should be included within this Section of Methods.
 
Results. Describe your results, starting from univariate, bivariate, multivariable. State the numeric results of the hypothesis test performed, including relevant test statistics, p-values, and confidence intervals
State the results of your one additional statistical analysis.
Discussion. Provide a thoughtful discussion about the significance of your results for each analysis as it relates to your overall study question, whether or not the predictor variable was related to your outcome variable, and how the
two were related (positively correlated or negatively correlated, if appropriate) State the conclusions drawn from your hypothesis testing and how these results help you answer your study question.
Finally, discuss the results of your additional statistical analysis and how it informs your conclusions regarding the study population as well as future research steps. Can you describe any limitations with your study?
 
Conclusion. Reflect on your study as a whole. Did your analysis allow you to draw informed conclusions regarding your study question? What additional data would you collect if you could, or how would you record that data to be more effective in the analysis phase? Discuss various scenarios you can imagine that explain the results you observed. Use this opportunity to reflect on how important it is to consider the analysis of a study before conducting it.
 
Please provide up to 10 figures/tables and code generated to create analysis. 