install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)
library(babynames)

# Find out the most common names in a particular year (1990)
babynames %>%
  # Change name of n to number
  rename(number = n) %>%
  # Filter for the year 1990
  filter(year == 1990) %>%
  # Sort the number column in descending order 
  arrange(desc(number))


# Find the most common names in every year
babynames %>%
  # Change name of n to number
  rename(number = n) %>%
  # Group by year
  group_by(year) %>%
  # Find the most common name
  top_n(1, number)
# It looks like John was the most common name in 1880, and Mary
# was the most common name for a while after that.


# Visualizing names with ggplot2
selected_names <- babynames %>%
  # Filter for the names Steven, Thomas, and Matthew
  filter(name %in% c("Steven", "Thomas", "Matthew"))
# Plot the names using a different color for each name
ggplot(selected_names, aes(x = year, y = n, color = name)) +
  geom_line()


# Calculate the fraction of people born each year with the same name
babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(n)) %>%
  ungroup() %>%
  mutate(fraction = n / year_total) %>%
  # Find year each name is most common
  group_by(name) %>%
  top_n(1, fraction)
# Notice that tht results are grouped by year, then name, 
# so the first few entries are names that were most popular
# in the 1880's that start with the letter H


# Add the total and maximum for each name
# Normalize by a different, but also interesting metric: you'll
# divide each name by the maximum for that name. This means every name
# will peak at 1.
#
# Add columns name_total and name_max for each name
babynames %>%
  group_by(name) %>%
  mutate(
    # name_total, with the total number of babies born with that
    # name in the entire datset.
    name_total = sum(n),
    # name_max, with the highest number of babies born in any year.
    name_max = max(n)
  ) %>%
  # Ungroup the table
  ungroup() %>%
  # Add the fraction_max column containing the number by the name maximum
  mutate(fraction_max = n/name_max)
# This tells you, for example, that the name Minnie was at 53.3% of its peak
# in the year 1880.


# Visualizing the normalized change in popularity
# You picked a few names and calculated each of them as a 
# fraction of their peak.
# This is a type of "normalizing" a name, where you're focused on the
# relative change within each name rather than the overall popularity
# of the name.
# Visualize the normality popularity of each name.
# Your work from the previous exercise, names_normalized, 
# has been provided for your.
names_normalized <- babynames %>%
  group_by(name) %>%
  mutate(
    name_total = sum(n),
    name_max = max(n)
  ) %>%
  ungroup() %>%
  mutate(fraction_max = n/name_max)

# Visualize the normalized change in popularity
# Filter for the names Steven, Thomas, and Matthew.
names_filtered <- names_normalized %>%
  filter(name %in% c("Steven", "Thomas", "Matthew"))

# Visualize these names over time
ggplot(names_filtered, aes(x = year, y = fraction_max, color = name)) +
  geom_line()



# Using ratios to describe the frequency of a name
#
# Changes in popularity of a name
babynames_fraction <- babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(n)) %>%
  ungroup() %>%
  mutate(fraction = n / year_total)

babynames_fraction %>%
  filter(name == "Samir") %>%
  arrange(year)

# Samir over time
babynames_fraction %>%
  filter(name == "Samir") %>%
  arrange(year) %>%
  mutate(difference = fraction - lag(fraction))

# Biggest jump in popularity
babynames_fraction %>%
  filter(name == "Samir") %>%
  arrange(year) %>%
  mutate(difference = fraction - lag(fraction)) %>%
  arrange(desc(difference))

# Changes within every name
# You'll need to do this as a grouped mutate. This ensures
# you won't include differences between one name and a different one.
babynames_fraction %>%
  arrange(name, year) %>%
  mutate(difference = fraction - lag(fraction)) %>%
  group_by(name) %>%
  arrange(desc(difference))


# Using ratios to describe the frequency of a name
# You'll start with the babynames_fraction data already, so that you
# can consider the popularity of each name within each year.
#
babynames_fraction %>%
  # Arrange the data in order of name, then year
  arrange(name, year) %>%
  # Group the data by name
  group_by(name) %>%
  # Add a ration column that contains the ration between each year
  mutate(ratio = fraction / lag(fraction))
# Notice that the first observation for each name is
# missing a ratio, since there is no previous year.


# Biggest jumps in a name
babynames_ratios_filtered <- babynames_fraction %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(ratio = fraction / lag(fraction)) %>%
  filter(fraction >= 0.00001)

# Look further into the names that experienced the biggest jumps
# in popularity in consecutive years.
#
babynames_ratios_filtered %>%
  # Extract the largest ratio from each name
  top_n(1, ratio) %>%
  # Sort the ratio column in descending order
  arrange(desc(ratio)) %>%
  # Filter for fractions greater than or equal to 0.001
  filter(fraction >= 0.001)
# Some of these can be interpreted: for example, 
# Grover Cleveland was a president elected in 1884.


