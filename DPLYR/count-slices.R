# Find number of counties per state, weighted by citizens, sorted in descending order
counties_selected %>%
  count(state, wt=citizens,sort=TRUE)
counties_selected %>%
  # Add population_walk containing the total number of people who walk to work 
  mutate(population_walk = population * walk / 100) %>%
  # Count weighted by the new column, sort in descending order
  count(state, wt = population_walk, sort = TRUE)
counties_selected %>%
  # Summarize to find minimum population, maximum unemployment, and average income
  summarize(min_population=min(population),max_unemployment=max(unemployment),average_income=mean(income))
counties_selected %>%
  # Group by state 
  group_by(state)%>%
  # Find the total area and population
  summarize(total_area=sum(land_area),total_population=sum(population))
counties_selected %>%
  group_by(state) %>%
  summarize(total_area = sum(land_area),
            total_population = sum(population)) %>%
  # Add a density column
  mutate(density= total_population/total_area)%>%
  # Sort by density in descending order
  arrange(desc(density))
counties_selected %>%
  # Group by region
  group_by(region)%>%
  # Find the greatest number of citizens who walk to work
  slice_max(walk,n=1)#n=1 her regiondan 1 veri gösterir.