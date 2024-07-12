counties_selected <- counties %>%
  select(state, county, population, private_work, public_work, self_employed)

counties_selected %>%
  # Add a verb to sort in descending order of public_work
  arrange(desc(public_work))
counties_selected <- counties %>%
  select(state, county, population)

counties_selected %>%
  # Filter for counties with a population above 1000000
  filter(population>1000000)
counties_selected <- counties %>%
  select(state, county, population)

counties_selected %>%
  # Filter for counties with a population above 1000000
  filter(state == "California",
         population > 1000000)
counties_selected <- counties %>%
  select(state, county, population, private_work, public_work, self_employed)

counties_selected %>%
  # Filter for Texas and more than 10000 people
  filter(state == "Texas",
         population > 10000) %>%
  # Sort in descending order of private_work
  arrange(desc(private_work))
counties_selected <- counties %>%
  select(state, county, population, public_work)

counties_selected %>%
  # Add a new column public_workers with the number of people employed in public work
  mutate(public_workers=public_work*population/100)
counties_selected <- counties %>%
  # Select the columns state, county, population, men, and women
  select(state, county, population, men, women)

counties_selected %>%
  # Calculate proportion_women as the fraction of the population made up of women
  mutate(proportion_women = women / population)
