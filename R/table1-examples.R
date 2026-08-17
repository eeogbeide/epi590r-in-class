install.packages("gtsummary", dependencies = TRUE)
library(tidyverse)
library(gtsummary)
library(dplyr)
# Load and clean data
nlsy_cols <- c(
  "glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
  "id", "nsibs", "samp", "race_eth", "sex", "region",
  "income", "res_1980", "res_2002", "age_bir"
)
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
  na = c("-1", "-2", "-3", "-4", "-5", "-998"),
  skip = 1, col_names = nlsy_cols
) |>
  mutate(
    region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
    sex_cat = factor(sex, labels = c("Male", "Female")),
    race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
    eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
    glasses_cat = factor(glasses, labels = c("No", "Yes"))
  )


# simple table
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat, region_cat,
    eyesight_cat, glasses, age_bir
  )
)

# add labels for the variables and for the "missing" category
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat, region_cat,
    eyesight_cat, glasses, age_bir
  ),
  label = list(
    race_eth_cat ~ "Race/ethnicity",
    region_cat ~ "Region",
    eyesight_cat ~ "Eyesight",
    glasses ~ "Wears glasses",
    age_bir ~ "Age at first birth"
  ),
  missing_text = "Missing"
)

# add p-values, a total column, bold labels, and remove the footnote
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat,
    eyesight_cat, glasses, age_bir
  ),
  label = list(
    race_eth_cat ~ "Race/ethnicity",
    eyesight_cat ~ "Eyesight",
    glasses ~ "Wears glasses",
    age_bir ~ "Age at first birth"
  ),
  missing_text = "Missing"
) |>
  # change the test used to compare sex_cat groups
  add_p(test = list(
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"
  )) |>
  # add a total column with the number of observations
  add_overall(col_label = "**Total** N = {N}") |>
  bold_labels() |>
  # remove the default footnotes
  # (in gtsummary < 2.0 this was modify_footnote(update = everything() ~ NA),
  #  which is now deprecated -- you'll still see it suggested in a lot of places)
  remove_footnote_header() |>
  # replace the column headers and make them bold
  modify_header(label = "**Variable**", p.value = "**P**") |>
  # add a caption
  modify_caption("**Participant characteristics**")


###you try 3. simple table that includes categorical region, race/ethnicity,
#income, and the sleep variables. make them nicely labeled
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    region_cat, race_eth_cat, income,
    sleep_wkdy, sleep_wknd,
  ),  label = list(
    region_cat ~ "Region",
    race_eth_cat ~ "Race/ethnicity",
    income ~ "Income",
    sleep_wkdy ~ "Sleep on Weekdays",
    sleep_wknd ~ "Sleep on Weekends"
  ))
  missing_text = "Missing"


#4. stratify by table and sex, add a total column combining both sexes, and add pvalues
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    region_cat, race_eth_cat, income,
    starts_with("sleep"),
    #I changed the sleep variables to "start with" instead of labeling each. same thing.
  ),  label = list(
    region_cat ~ "Region",
    race_eth_cat ~ "Race/ethnicity",
    income ~ "Income",
    sleep_wkdy ~ "Sleep on Weekdays",
    sleep_wknd ~ "Sleep on Weekends"
  )) |>
  add_p(test = list(
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"
  )) |>
    modify_header(
      label = "**Variable**",
      p.value = "**P**") |>
  add_overall(col_label = "**Total** N = {N}")



#5. For the income variable, show the 10th and 90th percentiles of income with 3
#digits, and for the sleep variables, show the min and the max with 1 digit.
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    region_cat, race_eth_cat, income,
    starts_with("sleep")
  ),  label = list(
    region_cat ~ "Region",
    race_eth_cat ~ "Race/ethnicity",
    income ~ "Income",
    sleep_wkdy ~ "Sleep on Weekdays",
    sleep_wknd ~ "Sleep on Weekends"
  ),
  statistic = list(
    income ~ "{p10}, {p90}",
    starts_with("sleep") ~ "{min},{max}"
  ),
  digits = list(
    income ~ 3,
    starts_with("sleep") ~ 1
  )
) |>
  add_p(test = list(
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"
  )) |>
  modify_header(
    label = "**Variable**",
    p.value = "**P**") |>
  add_overall(col_label = "**Total** N = {N}")




#Add a footnote to the race/ethnicity variable with a link to the page
#describing how NLSY classified participants:
#https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data

tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    region_cat, race_eth_cat, income,
    starts_with("sleep")
  ),  label = list(
    region_cat ~ "Region",
    race_eth_cat ~ "Race/ethnicity",
    income ~ "Income",
    sleep_wkdy ~ "Sleep on Weekdays",
    sleep_wknd ~ "Sleep on Weekends"
  ),
  statistic = list(
    income ~ "{p10}, {p90}",
    starts_with("sleep") ~ "{min},{max}"
  ),
  digits = list(
    income ~ 3,
    starts_with("sleep") ~ 1
  )
) |>
  add_p(test = list(
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"
  )) |>
  modify_header(
    label = "**Variable**",
    p.value = "**P**") |>
  add_overall(col_label = "**Total** N = {N}")|>
  # add a caption
  modify_caption("**Participant characteristics**")|>
 modify_footnote_body(footnote = "https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data
",
columns = "label",
rows = variable == "race_eth_cat" & row_type == "label"
)


