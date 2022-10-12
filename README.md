# HCCR

# Health and Human Services Risk engine in R

### Background

Every year the department of Health and Human Services publishes the coefficients and parameters for their regression model used to calculate estimated health risk. This Health risk score is a estimate of a person's health care costs for the upcoming year, with the average being scored as 1.00. An estimate for costs can be derived then by multiplying the expected nationwide average costs by the index. This number is then used to estimate capitation to a medical practice without penalizing it for caring for sick patients. For instance a practice that is 1.5 times the national average would not be seen as excessive or wasteful if the average risk score for its patients was close to 1.5.

The program estimates costs by scoring a regression model that is re-fit periodically. The coefficients for this model are published periodically in spreadsheet format, and converting this spreadsheet to code is the problem this package aims to solve. The model takes into account, age, sex, relevant diagnoses, prescription drugs history.

### Need

There is a need for a program to compute a risk score similar to the Medicare HCC but for a commercial population. Although there are programs in SAS to implement the algorithm each year, there are people who prefer a completely open source solution. I also wanted to create this in a format that can handle bulk data with a relatively simple functional interface. It should be intuitive to load and save the model.

## Purpose and Design

The code in this package turns a HHS spreadsheet into code in another language. Short term, the target language is R, but SQL is also planned. This code can then be used to run the scoring algorithm on your own datasets.

# Design Goals

-   Make it easy to store multiple years factors and models into a single table. This way you can compare models on the same dataset easily and run multiple models at once, such as Medicare (CMS-HCC to be coming soon) and Commercial.

# Dependencies

## Programming Environment

To run the code builder, you need a version of R that includes the following packages

-   **`dplyr`**

-   **`stringr`**

-   **`data.table`**

-   **`rlang`**

The actual R code that this code produces requires only the package `data.table` to run.

### Input Data

The DIY spreadsheet from (web link) as well as the instructions (here) outlining how it is to be used. The program will need parameters telling where the following tables are:

The code assumes that we are evaluating risk for a patient over a specific claims year to estimate costs in the following year. (double check this)

### Output

The output is a program in a target language, which is capable of scoring data. The next sections describe how your claims and eligibility data should be laid out.

# Other Input Data Sets

The program ingests a spreadsheet, and outputs a file of R code (and SQL in a future version). In order to run the code that HCCR output against your own data, you will need to have data sets that meet the requirements below:

### Demographics data set.

Must have the following format. The variables must be named exactly as below, including case. While this is an annoying restriction, I plan to remove it at some point before release. On the principle that you should use a data set that contains only the information you need (and no more), I advise against including other geographical data, since that could potentially identify individual patients.

| Variable      | Description                                                                                                                                                                                    |
|---------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `pat_id`      | **Completely anonymized patient ID**                                                                                                                                                           |
| `pat_age`     | Must be an integer. This is done, again, For greater anonymity, round to nearest year and do not include birthdate.                                                                            |
| `months_elig` | Number of months of eligibility prior to end of reporting period. Cap this at 12. Eligibility files tend to be huge, so it's advisable to aggregate this on your own before starting the code. |

### Diagnostic data set

# Planned Improvements

-   Get the Medicare HCC included in this framework

-   Have an option to produce SQL code. These queries would then run on your tables.

-   Possibly SparkSQL if you need to do this on a really large scale.

-   R Shiny user interface

-   Create a container with the required applications including R server

-   Figure out what you want to do with metal level

# Instructions for running

Really could do this as an R-Shiny user interface app!

Start up R. Make sure the Excel file `CY20xx DIY Tables mm.dd.yyyy.xslx` is already in the current directory. Load the following programs, in this order into your R workspace

1.  `Model_Inputs.R`
2.  `AgeSexFactors.R`
3.  `ApplyHCC.R`

After running these three files, a function will be created in your R Workspace called `HHRisk`, which takes two arguments; a demographic and a ICD10 code table. Load your demographic and ICD10 diagnostic code by patient datasets into R. Typically these should be less than 2G each. You could also spin up a large virtual workstation if you want to do this all in memory.

Run the function `HHSRisk(DM,DT)` where DM This will output a table consisting of `pat_id, model year, Risk` score that you would presumably upload back to SQL as well as a table `pat_id, VARIABLE, coefficient`
