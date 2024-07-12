babynames %>%
  # Filter for the year 1990
  filter(year==1990)%>%
  # Sort the number column in descending order 
  arrange(desc(number))
babynames %>%
  # Find the most common name in each year
  group_by(year)%>%slice_max(number,n=1)#n=2 desek en iyi 2 yi gösterir 
selected_names <- babynames %>%
  # Filter for the names Steven, Thomas, and Matthew 
  filter(name %in% c("Steven","Thomas","Matthew"))
# Calculate the fraction of people born each year with the same name
babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total) %>%
  # Find the year each name is most common
  group_by(name) %>%
  slice_max(fraction, n = 1)

babynames %>%
  # Add columns name_total and name_max for each name
  group_by(name) %>%
  mutate(name_total = sum(number),
         name_max = max(number)) %>%
  # Ungroup the table 
  ungroup()%>%
  # Add the fraction_max column containing the number by the name maximum 
  mutate(fraction_max=number/name_max)
names_filtered <- names_normalized %>%
  # Filter for the names Steven, Thomas, and Matthew
  filter(name %in% c("Steven", "Thomas", "Matthew"))

# Visualize these names over time
ggplot(names_filtered, aes(x = year, y = fraction_max, color = name)) +
  geom_line()