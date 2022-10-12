# HCCR

# Background

# HCCR Risk engine in R

Based on HHS HCC spreadsheet. ingest the spreadsheet and turn into R code on a completely automated basis. That R code can then be run against a diagnostic and demographic data set, whose structure will be outlined in the following setting.

# Requirements

## Programming Environment

To run the code builder, you need a version of R that includes the following packages

-   **`dplyr`**

-   **`stringr`**

-   **`data.table`**

-   **`rlang`**

The actual R code that this code produces requires only the package `data.table` to run.

### Government Data

The DIY spreadsheet from (web link) as well as the instructions (here) outlining how it is to be used. The program will need parameters telling where the following tables are:

The code assumes that we are evaluating risk for a patient over a specific claims year to estimate costs in the following year. (double check this)

# Other Input Data Sets

The program ingests a spreadsheet, and outputs a file of R code (and SQL in a future version). In order to run the code that HCCR output against your own data, you will need to have data sets that meet the requirements below:

### Demographics data set. 

Must have the following format. The variables must be named exactly as below, including case. While this is an annoying restriction, I plan to remove it at some point before release. On the principle that you should use a data set that contains only the information you need (and no more), I advise against including other geographical data, since that could potentially identify individual patients.

| Variable      | Description                                                                                                                                                                                    |
|--------------|----------------------------------------------------------|
| `pat_id`      | **Completely anonymized patient ID**                                                                                                                                                           |
| `pat_age`     | Must be an integer. This is done, again, For greater anonymity, round to nearest year and do not include birthdate.                                                                            |
| `months_elig` | Number of months of eligibility prior to end of reporting period. Cap this at 12. Eligibility files tend to be huge, so it's advisable to aggregate this on your own before starting the code. |

### Diagnostic data set

# Planned Improvements

-   Get the Medicare HCC included in this framework

-   Have an option to produce SQL code. These queries would then run on your tables.

-   
