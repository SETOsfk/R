# Glimpse the counties table
glimpse(counties)

counties %>%
  # Select state, county, population, and industry-related columns
  select(state,county,population,professional,service,office,construction,production)%>%
  # Arrange service in descending order 
  arrange(desc(service))
counties %>%
  # Select the state, county, population, and those ending with "work"
  select(state,county,population,ends_with("work"))%>%
  # Filter for counties that have at least 50% of people engaged in public work
  filter(public_work>=50)
counties %>%
  # Select state, county, and poverty as poverty_rate
  select(state,county,poverty_rate=poverty)
counties %>%
  # Keep the state, county, and populations columns, and add a density column
  transmute(state,county,population,density=population/land_area)%>%#hem seçiyor hem mutate ediyor.
  # Filter for counties with a population greater than one million 
  filter(population>1000000)%>%
  # Sort density in ascending order 
  arrange(density)
# Change the name of the unemployment column
counties %>%
  rename(unemployment_rate = unemployment)

# Keep the state and county columns, and the columns containing poverty
counties %>%
  select(state, county, contains("poverty"))

# Calculate the fraction_women column without dropping the other columns
counties %>%
  mutate(fraction_women = women / population)

# Keep only the state, county, and employment_rate columns
counties %>%
  transmute(state, county, employment_rate = employed / population)