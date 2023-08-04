# Load the packages
library(tidyverse)

# Import the datesets
parts <- readRDS("Data/parts.rds")
part_categories <- readRDS("Data/part_categories.rds")
inventory_parts <- readRDS("Data/inventory_parts.rds")
inventories <- readRDS("Data/inventories.rds")
sets <- readRDS("Data/sets.rds")
colors <- readRDS("Data/colors.rds")
themes <- readRDS("Data/themes.rds")


## 'inner_join' **********************************************
# only keeps observations that appear in both tables

# Add the correct verb, table, and joining column
parts %>% 
  inner_join(part_categories, by = c("part_cat_id" = "id"))

# Use the suffix argument to replace .x and .y suffixes
parts %>% 
  inner_join(part_categories, by = c("part_cat_id" = "id"),
             suffix = c("_part", "_category"))

# Combine the parts and inventory_parts tables by part_num
parts %>%
  inner_join(inventory_parts, by = "part_num")

# Joining 3 or more tables ****************
sets %>%
  # Add inventories using an inner join 
  inner_join(inventories, by = "set_num") %>%
  # Add inventory_parts using an inner join 
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>% 
  # Add an inner join for the colors table
  inner_join(colors, by = c("color_id" = "id"),
             suffix = c("_set", "_color")) %>% 
  # Count the number of colors and sort
  count(name_color, sort = TRUE)

## 'left_join' **********************************************
# keeps all the observations from 1 of the tables
# the first or left table, including all the matching observations from the
# second or right table

# create a custom table for 'left_join' practice
inventory_parts_joined <- inventories %>% 
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>% 
  select(-id, -version) %>% 
  arrange(desc(quantity))

# isolate two lego sets for left_join from the custom table
millennium_falcon <- inventory_parts_joined %>%
  filter(set_num == "7965-1")

star_destroyer <- inventory_parts_joined %>%
  filter(set_num == "75190-1")

# Combine the star_destroyer and millennium_falcon tables
millennium_falcon %>%
  left_join(star_destroyer, by = c("part_num", "color_id"),
            suffix = c("_falcon", "_star_destroyer"))

# Aggregate Millennium Falcon for the total quantity in each part
# Sum the quantity column by color_id in the Millennium Falcon dataset.
millennium_falcon_colors <- millennium_falcon %>%
  group_by(color_id) %>%
  summarize(total_quantity = sum(quantity))

# Do the same for the Star Destroyer
star_destroyer_colors <- star_destroyer %>%
  group_by(color_id) %>%
  summarize(total_quantity = sum(quantity))

# Left join the Millennium Falcon colors to the Star Destroyer colors
millennium_falcon_colors %>%
  left_join(star_destroyer_colors, by = "color_id", 
            suffix = c("_falcon", "_star_destroyer"))

# isolate version 1 data from inventories
inventory_version_1 <- inventories %>%
  filter(version == 1)

# Join versions to sets
sets %>%
  left_join(inventory_version_1, by = "set_num") %>%
  # Filter for where version is na
  filter(is.na(version))


## 'right_join' **********************************************
# keeps all the observations from 1 of the tables
# the second or right table, including all the matching observations from the
# first or left table

parts %>%
  # Count the part_cat_id
  count(part_cat_id) %>%
  # Right join part_categories
  right_join(part_categories, by = c("part_cat_id" = "id")) %>% 
  # Filter for NA
  filter(is.na(n))

# Use replace_na to replace missing values in the n column
parts %>%
  count(part_cat_id) %>%
  right_join(part_categories, by = c("part_cat_id" = "id")) %>%
  replace_na(list(n = 0))


## joining tables to themselves ******************************************

themes %>% 
  # Inner join the themes table
  inner_join(themes, by = c("id" = "parent_id"),
             suffix = c("_parent", "_child")) %>%
  # Filter for the "Harry Potter" parent name 
  filter(name_parent == "Harry Potter")


# Join themes to itself again to find the grandchild relationships
themes %>% 
  inner_join(themes, by = c("id" = "parent_id"), suffix = c("_parent", "_child")) %>%
  inner_join(themes, by = c("id_child" = "parent_id"), suffix = c("_parent", "_grandchild")) 


themes %>% 
  # Left join the themes table to its own children
  left_join(themes, by = c("id" = "parent_id"),
            suffix = c("_parent", "_child")) %>%
  # Filter for themes that have no child themes
  filter(is.na(name_child))


## 'full_join' **********************************************
# Keep all observations in both tables, even if they do not match

# Customize a table
inventory_parts_joined <- inventories %>%
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  arrange(desc(quantity)) %>%
  select(-id, -version)

# Customize a table to compare Batman with Star Wars
inventory_sets_themes <- inventory_parts_joined %>%
  inner_join(sets, by = "set_num") %>%
  inner_join(themes, by = c("theme_id" = "id"), suffix = c("_set", "_theme"))

batman <- inventory_sets_themes %>%
  filter(name_theme == "Batman")

star_wars <- inventory_sets_themes %>%
  filter(name_theme == "Star Wars")

# Count the part number and color id, weight by quantity
batman_parts <- batman %>%
  count(part_num, color_id, wt = quantity)

star_wars_parts <- star_wars %>%
  count(part_num, color_id, wt = quantity)

# Full joining Batman and Star Wars LEGO parts
parts_joined <- batman_parts %>%
  # Combine the star_wars_parts table 
  full_join(star_wars_parts, by = c("part_num", "color_id"),
            suffix = c("_batman", "_star_wars")) %>%
  # Replace NAs with 0s in the n_batman and n_star_wars columns 
  replace_na(list(n_batman = 0,
                  n_star_wars = 0))

# Comparing Batman and Star Wars LEGO parts
parts_joined %>%
  # Sort the number of star wars pieces in descending order 
  arrange(desc(n_star_wars)) %>%
  # Join the colors table to the parts_joined table
  inner_join(colors, by = c("color_id" = "id")) %>%
  # Join the parts table to the previous join 
  inner_join(parts, by = "part_num",
             suffix = c("_color", "_part"))


## 'semi_join' ************ 'anti_join' *************************
# these are filtering joins - keep or remove observations from the 1st table
# 'semi_join' - what observations in table 1 are also in table 2?
# 'anti_join' - what observations in table 1 are not in table 2?

# Make a custom datasets
batmobile <- inventory_parts_joined %>%
  filter(set_num == "7784-1") %>%
  select(-set_num)

batwing <- inventory_parts_joined %>%
  filter(set_num == "70916-1") %>%
  select(-set_num)

# Filter the batwing set for parts that are also in the batmobile set
batwing %>%
  semi_join(batmobile, by = "part_num")

# Filter the batwing set for parts that aren't in the batmobile set
batwing %>%
  anti_join(batmobile, by = "part_num")

# Use inventory_parts to find colors included in at least one set
colors %>%
  semi_join(inventory_parts, by = c("id" = "color_id"))

# Use filter() to extract version 1 
version_1_inventories <- inventories %>%
  filter(version == 1)

# Use anti_join() to find which set is missing a version 1
sets %>%
  anti_join(version_1_inventories, by = "set_num")


## Visualizing differences ***************************************

# Make a custom dataset for visualising the differences
inventory_parts_themes <- inventories %>%
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  arrange(desc(quantity)) %>%
  select(-id, -version) %>%
  inner_join(sets, by = "set_num") %>%
  inner_join(themes, by = c("theme_id" = "id"), suffix = c("_set", "_theme"))

# Filter and aggregate the Batman set data; add a fraction column
batman_colors <- inventory_parts_themes %>%
  # Filter the inventory_parts_themes table for the Batman theme
  filter(name_theme == "Batman") %>%
  group_by(color_id) %>%
  summarize(total = sum(quantity)) %>%
  # Add a fraction column of the total divided by the sum of the total 
  mutate(fraction = total / sum(total))

# Filter and aggregate the Star Wars set data; add a fraction column
star_wars_colors <- inventory_parts_themes %>%
  # Filter the inventory_parts_themes table for the Star Wars theme
  filter(name_theme == "Star Wars") %>%
  group_by(color_id) %>%
  summarize(total = sum(quantity)) %>%
  # Add a fraction column of the total divided by the sum of the total 
  mutate(fraction = total / sum(total))

# Combining sets
colors_joined <- batman_colors %>%
  # Join the Batman and Star Wars colors
  full_join(star_wars_colors, by = "color_id", 
            suffix = c("_batman", "_star_wars")) %>%
  # Replace NAs in the total_batman and total_star_wars columns
  replace_na(list(total_batman = 0,
                  total_star_wars = 0)) %>%
  inner_join(colors, by = c("color_id" = "id")) %>% 
  # Create the difference and total columns
  mutate(difference = fraction_batman - fraction_star_wars,
         total = total_batman + total_star_wars) %>%
  # Filter for totals greater than 200
  filter(total > 200)

# Visualizing the difference: Batman and Star Wars
color_palette <- setNames(colors_joined$rgb, colors_joined$name)

colors_joined %>% 
  na.omit() %>% 
  mutate(name = fct_reorder(name, difference)) %>% 
  ggplot(aes(name, difference, fill = name)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(y = "Difference: Batman - Star Wars",
       x = "Colour Name",
       title = "Light Bluish Gray and Black coloured lego pieces dominate the sets") +
  theme(plot.title = element_text(hjust = 0.5))



