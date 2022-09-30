# program: setup.R
# load project dependencies

require(lobstr)
require(rlang)
require(tidyverse)
require(data.table)
require(readxl)
require(readr)
require(lubridate)

# date  utility and helper functions

fn = file.path('CY2022 DIY tables 06.30.2022.xlsx')
