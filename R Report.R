library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)

# Load necessary libraries
library(dplyr)

# Read the data
COVID_data <- data.frame(
  Location = c("United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "Brazil", "Brazil", "United States", "United States", "Brazil", "Brazil", "United States", "United States", "Brazil", "United States", "Brazil", "Brazil", "Brazil", "United Kingdom", "Germany", "United States", "Brazil", "United Kingdom", "Brazil", "United States", "United Kingdom", "United Kingdom", "United Kingdom", "United Kingdom", "United States", "Brazil", "United States", "Brazil", "United Kingdom", "Germany", "United States", "Brazil", "United States", "Brazil", "United States", "Germany", "United Kingdom", "Germany", "United Kingdom", "Brazil", "Germany", "Brazil", "United States", "Germany", "Brazil", "Germany", "United Kingdom", "United Kingdom", "Germany", "Brazil", "Germany", "Germany", "United States", "Germany", "United Kingdom", "United Kingdom", "Germany", "Brazil", "Germany", "United Kingdom", "United Kingdom", "Germany", "Germany", "United Kingdom", "United Kingdom", "Germany", "Germany", "Brazil", "Brazil", "United Kingdom", "Germany", "United Kingdom", "United Kingdom", "Germany", "United Kingdom", "Germany", "United Kingdom", "Germany", "Germany", "Germany", "Germany", "Brazil"),
  MonthYear = c("12/1/2020", "1/1/2021", "11/1/2020", "8/1/2021", "9/1/2021", "11/1/2021", "10/1/2021", "2/1/2021", "3/1/2021", "6/1/2021", "10/1/2020", "7/1/2020", "4/1/2021", "5/1/2021", "4/1/2021", "3/1/2021", "1/1/2021", "8/1/2020", "7/1/2021", "2/1/2021", "12/1/2020", "1/1/2021", "11/1/2021", "7/1/2021", "7/1/2020", "10/1/2021", "8/1/2020", "9/1/2020", "11/1/2021", "7/1/2021", "9/1/2021", "8/1/2021", "5/1/2021", "9/1/2020", "4/1/2020", "6/1/2020", "12/1/2020", "8/1/2021", "6/1/2020", "11/1/2020", "10/1/2020", "5/1/2021", "11/1/2021", "9/1/2021", "1/1/2021", "5/1/2020", "6/1/2021", "3/1/2021", "10/1/2021", "10/1/2021", "2/1/2021", "11/1/2021", "5/1/2021", "9/1/2021", "9/1/2020", "3/1/2020", "6/1/2021", "3/1/2021", "4/1/2020", "4/1/2021", "3/1/2020", "8/1/2021", "3/1/2021", "4/1/2020", "5/1/2020", "5/1/2021", "3/1/2021", "9/1/2020", "6/1/2021", "7/1/2021", "3/1/2020", "8/1/2020", "8/1/2020", "6/1/2020", "5/1/2020", "7/1/2020", "7/1/2020", "6/1/2020", "6/1/2020"),
  Year = c(2020L, 2021L, 2020L, 2021L, 2021L, 2021L, 2021L, 2021L, 2021L, 2021L, 2020L, 2020L, 2021L, 2021L, 2021L, 2021L, 2021L, 2020L, 2021L, 2021L, 2020L, 2021L, 2021L, 2021L, 2020L, 2021L, 2020L, 2020L, 2021L, 2021L, 2021L, 2021L, 2021L, 2020L, 2020L, 2020L, 2020L, 2021L, 2020L, 2020L, 2020L, 2021L, 2021L, 2021L, 2021L, 2020L, 2021L, 2021L, 2021L, 2021L, 2021L, 2021L, 2021L, 2020L, 2020L, 2021L, 2021L, 2020L, 2021L, 2020L, 2021L, 2021L, 2020L, 2020L, 2021L, 2021L, 2020L, 2021L, 2021L, 2020L, 2020L, 2020L, 2020L, 2020L, 2020L, 2020L, 2020L, 2020L),
  NewCases = c(6429031L, 6147690L, 4504713L, 4289498L, 4149395L, 2547548L, 2504090L, 2401691L, 2197488L, 2011587L, 1930191L, 1925246L, 1910264L, 1884761L, 1813466L, 1528758L, 1459475L, 1360714
               # Perform t-test for United States vs. Brazil
               t_test_us_brazil <- t.test(NewCases ~ Location, data = COVID_data, subset = Location %in% c("United States", "Brazil"))
               
               # Print the results
               print(t_test_us_brazil)
               
               # Perform ANOVA test for all locations
               anova_location <- aov(NewCases ~ Location, data = COVID_data)
               
               # Print the results
               print(summary(anova_location))
               
               #Objective:
              #** To investigate whether there is a significant difference in the number of new COVID-19 cases between Brazil and the United States.
               
               Hypotheses:
                 
                 Null Hypothesis (H0): There is no significant difference in the mean number of new COVID-19 cases between Brazil and the United States.
               Alternative Hypothesis (H1): There is a significant difference in the mean number of new COVID-19 cases between Brazil and the United States.
               Analysis:
                 
                 An analysis of variance (ANOVA) was conducted to determine if there is a significant difference in the mean number of new COVID-19 cases across the locations (Brazil and the United States). The ANOVA results indicated a significant difference in the mean number of new cases between the two countries (F(1, 52480) = 591.3, p < 0.001).
               A Welch's two-sample t-test was performed to further examine the difference in mean new COVID-19 cases between Brazil and the United States. The results of the t-test showed a significant difference in mean new cases between Brazil (M = 34,075.08, SD = unknown) and the United States (M = 71,483.25, SD = unknown), t(881.25) = -13.802, p < 0.001, 95% CI [-42,727.83, -32,088.50].
Managerial Implications:

The findings suggest that there is a significant difference in the number of new COVID-19 cases between Brazil and the United States.
Healthcare authorities and policymakers should consider the factors contributing to the higher number of cases in the United States compared to Brazil. This may include differences in testing, healthcare infrastructure, public health measures, and population density.
Resources and interventions aimed at controlling the spread of COVID-19 should be tailored to address the specific challenges faced by each country.
Continuous monitoring and analysis of COVID-19 data are essential for informed decision-making and effective management of the pandemic.
Conclusion:
Based on the results of the ANOVA and t-test, we reject the null hypothesis and conclude that there is a significant difference in the mean number of new COVID-19 cases between Brazil and the United States. Further investigation and targeted interventions are needed to address the disparities observed in the COVID-19 situation between these two countries.*#
               