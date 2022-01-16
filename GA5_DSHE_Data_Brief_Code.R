
# load the packages needed for analysis 
pacman::p_load(tidyverse, dplyr, readr, ggplot2, haven, janitor, skimr, gtsummary, flextable)

#load the data in 
chs_data <- read_sas('chs2019_public.sas7bdat')
chs_data

## generic base cleaning (removing missingness + connecting numbers to meanings)
chsdata_refined <- chs_data %>%
  select(agegroup, birthsex, newrace, fruitveg19, imputed_neighpovgroup4_1418) %>%
  dplyr::filter(!is.na(fruitveg19) & !is.na(imputed_neighpovgroup4_1418)) %>% 
  dplyr::filter(!is.na(birthsex) & !is.na(newrace) & !is.na(agegroup)) %>%
  dplyr::mutate(number_of_servings = dplyr::case_when(fruitveg19 == 1 ~ "0 servings",
                                                     fruitveg19 == 2 ~ "1-4 servings",
                                                     fruitveg19 == 3 ~ "5+ servings")) %>%
  dplyr::mutate(neighborhood_poverty_level = dplyr::case_when(imputed_neighpovgroup4_1418 == 1 ~ "Low Poverty",
                                                              imputed_neighpovgroup4_1418 == 2 ~ "Medium Poverty",
                                                              imputed_neighpovgroup4_1418 == 3 ~ "High Poverty",
                                                              imputed_neighpovgroup4_1418 == 4 ~ "Very High Poverty")) %>%
  dplyr::mutate(birth_sex = dplyr::case_when(birthsex == 1 ~ "Male",
                                             birthsex == 2 ~ "Female")) %>%
  dplyr::mutate(race_category = dplyr::case_when(newrace == 1 ~ "White Non-Hispanic",
                                                 newrace == 2 ~ "Black Non-Hispanic", 
                                                 newrace == 3 ~ "Hispanic",
                                                 newrace == 4 ~ "Asian/PI Non-Hispanic",
                                                 newrace == 5 ~ "Other Non-Hispanic")) %>%
  dplyr::mutate(age_group = dplyr::case_when(agegroup == 1 ~ "18-24 yrs",
                                             agegroup == 2 ~ "25-44 yrs", 
                                             agegroup == 3 ~ "45-64 yrs",
                                             agegroup == 4 ~ "65+ yrs")) %>%
  select(number_of_servings, neighborhood_poverty_level, birth_sex, race_category, age_group)

# set order to the levels of the neighborhood poverty level variable so that it doesn't default to alphabetical 
  chsdata_refined$neighborhood_poverty_level <- factor(chsdata_refined$neighborhood_poverty_level,
                                                          levels = c("Low Poverty","Medium Poverty","High Poverty","Very High Poverty"),
                                                          exclude = NA, ordered = TRUE, nmax = NA)
  
#this table generates a table summarizing the age groups, binary sex, racial categories, and relevant variables being evaluated 
# namely, neighborhood poverty level and daily fruit/veggie servings. Important to have the summary statistics to see 
# population representation in the dataset
  chsdata_refined %>% gtsummary::tbl_summary()


#this produces a proportion table grouped by the daily fruit/veggie servings 
chsdata_proportion <- chsdata_refined %>%
  select(number_of_servings, neighborhood_poverty_level, race_category) %>%
  count(number_of_servings, neighborhood_poverty_level, race_category) %>%
  group_by(number_of_servings) %>%
  mutate(prop = prop.table(n)) 


#NEIGHBORHOODS
#this produces a split bar chart showing the proportion of residents that consume either 0, 1-4, or 5+ servings 
#of fruits/vegetables per day across the varying neighborhood poverty levels

ggplot(chsdata_proportion, aes(fill= neighborhood_poverty_level, y=prop, x=number_of_servings)) + 
  geom_col(position="stack") + 
  facet_wrap(~neighborhood_poverty_level, ncol = 4) + 
  ggtitle("Daily Consumption of Fruits And/or Vegetables per Day Is Lower \namong Adults 18+ in Higher Poverty Neighborhoods in NYC") +
  xlab("Number of Servings of Fruit/Veggies Per Day") + 
  ylab("Proportion of Respondents") + 
  labs(fill = "Neighborhood Poverty Level")


##this is the tabular representation of the data within the neighborhoods bar chart, columns sum to 100
#across the number of fruit/veggie servings
chsdata_refined %>% 
  tabyl(number_of_servings, neighborhood_poverty_level) %>%  
  adorn_totals(where = 'row') %>%            
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>%                 
  adorn_ns(position = "front") %>%           
  adorn_title(row_name = "Daily Servings of Fruit and Vegetables", 
              col_name = "Neighborhood Poverty Level", 
              placement = "combined") %>%   # combine the labels, necessary to create the pretty table using flextable
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit()          # format so the frequencies and percentages are in one row for each cell



#this produces a split bar chart showing the proportion of residents that consume either 0, 1-4, or 5+ servings 
#of fruits/vegetables per day across racial/ethnic groups
##RACE AND SERVINGS VISUALIZATION
ggplot(chsdata_proportion, aes(fill=race_category, y=prop, x=number_of_servings)) + 
  geom_col(position="stack") + 
  facet_wrap(~race_category, ncol=5) + 
  xlab("Number of Servings of Fruit/Veggies Per Day") + 
  ylab("Proportion of Respondents") + 
  labs(fill = "Racial/Ethnic Group") + 
  ggtitle("Daily Consumption of Fruits And/or Vegetables per Day Is Higher Among \nWhite Non-Hispanic Adults Age 18+ Compared to Other Racial/Ethnic Groups in NYC")


