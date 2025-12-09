
# 0. PACKAGES AND HELPER FUNCTIONS

# loading all the packages I need for cleaning and analysis.
library(tidyverse)  # dplyr, ggplot2, stringr, etc.
library(janitor)    # clean_names()
library(here)
library(ggplot2)
library(purrr)
library(stringr)
library(ggfx)
library(scales)     # for percent() axis labels
library(ggridges)
library(forcats)
library(dplyr)



# writing a helper function to classify materials into broader medium groups.
#based on my knowledge of functions created in HTML for website project in IT course
# I will reuse this for both Met and Tate.
add_medium_group <- function(df) {
  df %>%
    mutate(
      medium_lower = str_to_lower(medium),
      medium_group = case_when(
        # 1. Oil painting (kept separate)
        str_detect(medium_lower, "oil") ~ "Oil painting",
        
        # 2. Water-based painting: watercolor, gouache, acrylic
        str_detect(medium_lower, "watercolor|watercolour|gouache|acrylic") ~
          "Water-based painting",
        
        # 3. Drawing / works on paper (direct mark-making, but not prints)
        str_detect(
          medium_lower,
          "pencil|graphite|charcoal|chalk|crayon|pen and ink|pen & ink|ink on paper|ink, graphite|wash on paper"
        ) &
          !str_detect(
            medium_lower,
            "print|etch|engraving|lithograph|woodcut|screenprint|linocut"
          ) ~
          "Drawing / Works on paper",
        
        # 4. Printmaking (matrix-based, reproducible)
        str_detect(
          medium_lower,
          "print|etch|engraving|lithograph|woodcut|screenprint|linocut|monotype"
        ) ~
          "Printmaking",
        
        # 5. Photography
        str_detect(
          medium_lower,
          "photograph|gelatin silver|albumen|cyanotype|c%-print|cibachrome|chromogenic"
        ) ~
          "Photography",
        
        # 6. Digital / media / film / video
        str_detect(
          medium_lower,
          "digital|inkjet|video|projection|film|screen|media art|computer"
        ) ~
          "Digital/Media",
        
        # 7. Sculpture
        str_detect(
          medium_lower,
          "sculpture|bronze|marble|stone|plaster|wood|resin|cast|metal"
        ) ~
          "Sculpture",
        
        # 8. Everything else
        TRUE ~ "Other"
      )
    )
}


# 1. MET DATA: LOAD AND BASIC CLEANING


# loading the Met dataset (I assume the file is called "MetObjects.csv" and is in my working folder).
met <- read_csv("MetObjects.csv") %>%
  clean_names()  # standardizing column names like "Object ID" -> "object_id"

# taking only the columns I need for this project.
met_small <- met %>%
  select(
    object_id,
    title,
    object_name,
    object_begin_date,
    object_end_date,
    classification,
    medium,
    department
  ) %>%
  rename(
    year = object_begin_date  # rename object_begin_date to "year" for clarity
  )

# build a text field that I will search for landscape/nature words.
met_small <- met_small %>%
  mutate(
    subject_text   = paste(title, object_name, classification, medium, sep = " "),
    subject_lower  = str_to_lower(subject_text)
  )


# 2. WORD LISTS: LANDSCAPE, URBAN, WILD, BAD


# define my landscape-related words (broad environment vocabulary).
landscape_words <- c(
  # Core landscape terms
  "landscape", "view", "vista", "scenery", "scene",
  
  # Water forms
  "river", "stream", "brook", "pond", "lake", "waterfall", "cascade",
  "sea", "ocean", "seascape", "shore", "coast", "harbor", "harbour",
  "bay", "lagoon", "canal", "marsh", "wetland",
  
  # Landforms
  "mountain", "mount", "peak", "hill", "ridge", "cliff", "valley",
  "canyon", "desert", "dune", "plateau", "meadow", "field", "plain",
  "savannah",
  
  # Forest / vegetation
  "forest", "woodland", "woods", "grove", "jungle", "rainforest",
  "tree", "trees", "orchard", "garden", "botanical", "flora",
  
  # Rural / agrarian
  "countryside", "pastoral", "farm", "village", "hamlet",
  
  # Sky & celestial linked to landscapes
  "sunset", "sunrise", "dawn", "dusk", "sky", "horizon", "cloud", "moon",
  
  # Mythological / allegorical nature scenes
  "nymph", "eden", "paradise",
  
  # Wilderness & ecology
  "wilderness", "nature", "outdoors", "environment", "terrain",
  
  # Ports and coastal infrastructure
  "port", "landing", "wharf", "delta", "fjord",
  
  # Misc natural environments
  "glacier", "tundra", "prairie", "moor"
)

# define urban/industrial environment words.
urban_words <- c(
  "city", "town", "village", "hamlet", "street", "avenue", "boulevard",
  "square", "plaza", "alley", "lane",
  "bridge", "harbor", "harbour", "port", "dock", "wharf", "pier",
  "station", "terminal", "depot", "ship", "shipyard", "boat", "steamboat",
  "factory", "industrial", "plant", "mill", "tunnel",
  "railway", "railroad", "train", "tram", "subway", "metro",
  "road", "highway", "freeway", "expressway",
  "suburb", "skyline", "district", "block", "neighborhood", "farmhouse"
)

# define wild-nature words (used later for wild vs urban classification).
wild_words <- c(
  "forest", "woodland", "woods", "grove", "jungle", "rainforest",
  "mountain", "mount", "peak", "hill", "ridge", "valley", "canyon",
  "river", "stream", "brook", "lake", "pond", "waterfall", "cascade",
  "sea", "ocean", "seascape", "shore", "coast", "bay", "lagoon",
  "marsh", "wetland", "delta", "fjord",
  "meadow", "field", "plain", "savannah", "prairie", "moor", "tundra",
  "desert", "plateau", "cliff",
  "grove", "orchard", "garden", "botanical", "flora",
  "countryside", "pastoral", "farm", "rural", "wilderness", "nature",
  "glacier", "outdoors", "terrain"
)

# define an umbrella list: any environment-related word (wild or urban).
environment_words <- c(landscape_words, urban_words)

# define bad words for object types I want to exclude (non-representational / functional).
bad_words <- c(
  # Decorative objects
  "medal", "coin", "cookie", "seal", "token", "bottle", "beaker",
  "bowl", "vase", "pitcher", "jug", "urn", "flask", "jar", "cup",
  "plate", "dish", "coaster", "tray", "glass", "decanter",
  
  # People / figures I don't want for this dataset
  "dancer", "javanese", "portrait", "bust", "figure", "saint",
  "angel", "warrior", "soldier",
  
  # Weapons and armor
  "weapon", "helmet", "armor", "armour", "shield", "sword", "dagger",
  "axe", "knife", "spear", "blade", "quiver",
  
  # Tools and mechanical objects
  "tool", "instrument", "machine", "key", "lock", "gear", "spring",
  
  # Furniture / interior
  "chair", "seat", "bench", "cabinet", "stool", "drawer", "desk",
  "mirror", "window", "grill", "screen", "door", "table", "case",
  "settee", "plant stand", "armchair", "square planter", "bedstead",
  "vases", "furniture",
  
  # Jewelry / wearable
  "buckle", "pendant", "bracelet", "ring", "earring", "necklace",
  "brooch",
  
  # Textiles rarely about landscape
  "carpet", "rug", "tapestry fragment", "lace", "shawl", "sash",
  "scarf",
  
  # Boxes and containers
  "box", "casket", "container", "wallet", "pouch",
  
  # Misc
  "mask", "pipe", "brazier", "lamp", "lantern", "stamp",
  "banknote", "coinage", "reliquary",
  
  # Extra from earlier debugging
  "plaque", "portfolio", "loggia"
)

# 2.5. BUILD REGEX PATTERNS WITH WORD BOUNDARIES (SAFER MATCHING)

# creating regex patterns that enforce word boundaries so that
# - "port" does not match "important"
# - "mill" does not match "million"
# etc.

env_pattern <- paste0("\\b(", paste(environment_words, collapse = "|"), ")\\b")
bad_pattern <- paste0("\\b(", paste(bad_words, collapse = "|"), ")\\b")


# 3. MET: BUILD LANDSCAPE-RELATED SUBSET


# filter Met artworks to:
#  - have a valid year >= 1820
#  - contain at least one environment-related word
#  - not contain any "bad" object-type words
met_land <- met_small %>%
  filter(!is.na(year)) %>%
  filter(year >= 1820) %>%
  filter(str_detect(subject_lower, env_pattern)) %>%
  filter(!str_detect(subject_lower, bad_pattern))

# simplify to the core columns I will keep for analysis.
met_land_simple <- met_land %>%
  transmute(
    object_id,
    title,
    year,
    classification,
    medium,
    subject_lower,
    museum = "Met"
  )


# now that I have met_land_simple, I want to classify each artwork
# into "wild", "urban", "mixed", or "other" based on the presence of wild_words and urban_words.

met_land_simple <- met_land_simple %>%
  mutate(
    # flags for whether any wild / urban words appear in the subject text
    is_wild_flag  = str_detect(subject_lower, paste(wild_words,  collapse = "|")),
    is_urban_flag = str_detect(subject_lower, paste(urban_words, collapse = "|")),
    
    # final depiction type label using the two flags
    depiction_type = case_when(
      is_wild_flag & !is_urban_flag ~ "wild",
      !is_wild_flag & is_urban_flag ~ "urban",
      is_wild_flag & is_urban_flag  ~ "mixed",
      TRUE                          ~ "other"
    )
  )

# adding the broader medium_group categories using the helper function I defined earlier.
met_land_simple <- met_land_simple %>%
  add_medium_group()

# adding the broader medium_group categories using the helper function I defined earlier.
met_land_simple <- met_land_simple %>%
  add_medium_group()

# creating decade and ecological phase labels for each Met artwork.
met_land_simple <- met_land_simple %>%
  mutate(
    decade = floor(year / 10) * 10,
    phase = case_when(
      year >= 1820 & year < 1870 ~ "Steam & Coal",
      year >= 1870 & year < 1920 ~ "Oil & Empire",
      year >= 1920 & year < 1960 ~ "Motor & Machine Vision",
      year >= 1960 & year < 1990 ~ "Plastic & Suburban Nature",
      year >= 1990              ~ "Carbon & Crisis",
      TRUE ~ NA_character_
    )
  ) %>%
  # I drop anything that somehow falls outside my defined phases.
  filter(!is.na(phase))

# For logistic regression I need a binary Y (urban vs wild).
# So here I keep only "wild" and "urban" artworks and create is_urban (0/1).
met_final <- met_land_simple %>%
  filter(depiction_type %in% c("wild", "urban")) %>%
  mutate(
    is_urban = if_else(depiction_type == "urban", 1L, 0L)
  )


# 5. TATE DATA: LOAD, CLEAN, AND BUILD SUBJECT TEXT


# now I repeat a parallel process for the Tate dataset.
# NOTE: tweaking the filename and column names based on the actual Tate CSV I have.

tate_raw <- read_csv("artwork_data.csv") %>%  
  clean_names()

# inspecting the structure to confirm what the year + category columns are.
glimpse(tate_raw)

# I select the core columns I actually need.
tate_small <- tate_raw %>%
  select(
    id,
    title,
    medium,
    year,
    # Tate does NOT have a classification column, so I create one:
    classification = artist_role       # OR "Unknown" if I prefer
  ) %>%
  mutate(
    # Ensure year is numeric (it already should be, but to be safe)
    year = as.numeric(year)
  ) %>%
  filter(!is.na(year))        # keep only artworks with dates

# Build subject text for keyword filtering
tate_small <- tate_small %>%
  mutate(
    subject_text  = paste(title, classification, medium, sep = " "),
    subject_lower = str_to_lower(subject_text)
  )

# Apply the same filtering rules as Met
tate_land <- tate_small %>%
  filter(year >= 1820) %>%
  filter(str_detect(subject_lower, env_pattern)) %>%
  filter(!str_detect(subject_lower, bad_pattern))

# 6. TATE: BUILD TATE_LAND_SIMPLE (PARALLEL TO MET_LAND_SIMPLE)

# now that I have tate_land filtered to landscape-related works,
# I want a simplified table that looks as similar to met_land_simple as possible.

tate_land_simple <- tate_land %>%
  transmute(
    # Tate has its own id column that I can treat like object_id
    object_id      = id,
    title,
    year,
    classification,
    medium,
    subject_lower,
    museum         = "Tate"
  )


# 7. TATE: WILD / URBAN / MIXED CLASSIFICATION, MEDIUM GROUP, DECADE, PHASE

# I now classify each Tate artwork as "wild", "urban", "mixed", or "other",
# using the SAME wild_words and urban_words as for the Met.
tate_land_simple <- tate_land_simple %>%
  mutate(
    # flags for presence of wild vs urban keywords
    is_wild_flag  = str_detect(subject_lower, paste(wild_words,  collapse = "|")),
    is_urban_flag = str_detect(subject_lower, paste(urban_words, collapse = "|")),
    
    depiction_type = case_when(
      is_wild_flag & !is_urban_flag ~ "wild",
      !is_wild_flag & is_urban_flag ~ "urban",
      is_wild_flag & is_urban_flag  ~ "mixed",
      TRUE                          ~ "other"
    )
  )

glimpse(tate_land_simple)

# I add the same broad medium_group categories to Tate that I used for the Met.
tate_land_simple <- tate_land_simple %>%
  add_medium_group()

# I compute decade and ecological phase for each Tate artwork.
tate_land_simple <- tate_land_simple %>%
  mutate(
    decade = floor(year / 10) * 10,
    phase = case_when(
      year >= 1820 & year < 1870 ~ "Steam & Coal",
      year >= 1870 & year < 1920 ~ "Oil & Empire",
      year >= 1920 & year < 1960 ~ "Motor & Machine Vision",
      year >= 1960 & year < 1990 ~ "Plastic & Suburban Nature",
      year >= 1990              ~ "Carbon & Crisis",
      TRUE ~ NA_character_
    )
  ) %>%
  # I drop anything outside my defined historical phases.
  filter(!is.na(phase))

# For logistic regression, I again want a clean binary Y: urban vs wild.
# So I keep only wild/urban and create is_urban (1 = urban, 0 = wild) for Tate.
tate_final <- tate_land_simple %>%
  filter(depiction_type %in% c("wild", "urban")) %>%
  mutate(
    is_urban = if_else(depiction_type == "urban", 1L, 0L)
  )


# 8. COMBINE MET + TATE: FULL DATASET (KEEPING wild / urban / mixed / other)

# Here I keep ALL depiction types. This is my full analysis universe.
all_art_full <- bind_rows(met_land_simple, tate_land_simple) %>%
  mutate(
    medium_group = factor(medium_group),
    phase = factor(
      phase,
      levels = c(
        "Steam & Coal",
        "Oil & Empire",
        "Motor & Machine Vision",
        "Plastic & Suburban Nature",
        "Carbon & Crisis"
      )
    ),
    museum = factor(museum),
    depiction_type = factor(
      depiction_type,
      levels = c("wild", "urban", "mixed", "other")
    )
  )

# 8B. MODELING DATASET: ONLY wild vs urban (binary Y = is_urban)

# For t-test + logistic regression I need a clean binary outcome,
# so here I keep only wild + urban and create is_urban.
all_art_model <- all_art_full %>%
  filter(depiction_type %in% c("wild", "urban")) %>%
  mutate(
    is_urban = if_else(depiction_type == "urban", 1L, 0L),
    # drop extra factor levels so chi-square etc. don't see empty columns
    depiction_type = droplevels(depiction_type)
  )

## checking the overall count of depiction types (wild, urban, mixed, other)
all_art_full %>% 
  count(depiction_type)

## checking the same across mediums
all_art_full %>%
  count(medium_group, depiction_type)

tab_full <- table(all_art_full$medium_group, all_art_full$depiction_type)
chisq.test(tab_full)$expected

all_art_full %>%
  filter(!is.na(year)) %>%
  group_by(museum) %>%
  summarise(
    min_year = min(year),
    max_year = max(year),
    .groups = "drop"
  )



### --------------------------------------------------------------
## INFERENTIAL TESTS START HERE
### --------------------------------------------------------------


# 9A. T-TEST: ARE URBAN WORKS MORE RECENT THAN WILD WORKS?

t_test_result <- t.test(year ~ depiction_type, data = all_art_model)
t_test_result


# 9B. CHI-SQUARE TEST: IS depiction_type RELATED TO medium_group?

tab_full <- table(all_art_full$medium_group, all_art_full$depiction_type)

chisq_result_full <- chisq.test(tab_full)
chisq_result_full

# row-wise proportions
prop.table(tab_full, margin = 1)


# Row-wise proportions (interpretation aid)
prop.table(tab_medium_depict, margin = 1)

# 9C. PROPORTION OF URBAN VS WILD BY DECADE

decade_summary <- all_art_model %>%
  group_by(decade) %>%
  summarise(
    n = n(),
    n_urban = sum(is_urban),
    prop_urban = n_urban / n
  ) %>%
  ungroup()

decade_summary


# 9D. LOGISTIC REGRESSION MODEL 1: YEAR ONLY

model1 <- glm(is_urban ~ year,
              data = all_art_model,
              family = "binomial")

summary(model1)

# Odds ratios (easier interpretation)
exp(coef(model1))


# 9E. MODEL 2: MEDIUM GROUP ONLY

model2 <- glm(is_urban ~ medium_group,
              data = all_art_model,
              family = "binomial")

summary(model2)
exp(coef(model2))

# 9F. FULL ECOLOGICAL MODEL

model3 <- glm(
  is_urban ~ year + medium_group + phase,
  data = all_art_model,
  family = "binomial"
)

summary(model3)
exp(coef(model3))


# 9G. MODEL 4: FULL ECOLOGICAL MODEL + MUSEUM IDENTITY

model4 <- glm(
  is_urban ~ year + medium_group + phase + museum,
  data = all_art_model,
  family = "binomial"
)

summary(model4)
exp(coef(model4))      # odds ratios for interpretation


AIC(model1, model2, model3, model4)


### --------------------------------------------------------------
### INFERENTIAL TESTS END HERE
### --------------------------------------------------------------



### --------------------------------------------------------------
### DATA VISUALIZATION EXPERIMENTATIONS START HERE ###
### --------------------------------------------------------------



### ## 1 ---> Windows dataset (without mixed)

# For safety, I quickly check what columns I actually have
glimpse(all_art_model)


# ia). Environment word lists (same logic as before)

landscape_words <- c(
  "landscape", "view", "vista", "scenery", "scene",
  "river", "stream", "brook", "pond", "lake", "waterfall", "cascade",
  "sea", "ocean", "seascape", "shore", "coast", "harbor", "harbour",
  "bay", "lagoon", "canal", "marsh", "wetland",
  "mountain", "mount", "peak", "hill", "ridge", "cliff", "valley",
  "canyon", "desert", "dune", "plateau", "meadow", "field", "plain",
  "savannah",
  "forest", "woodland", "woods", "grove", "jungle", "rainforest",
  "tree", "trees", "orchard", "garden", "botanical", "flora",
  "countryside", "pastoral", "farm", "village", "hamlet",
  "sunset", "sunrise", "dawn", "dusk", "sky", "horizon", "cloud", "moon",
  "nymph", "eden", "paradise",
  "wilderness", "nature", "outdoors", "environment", "terrain",
  "port", "landing", "wharf", "delta", "fjord",
  "glacier", "tundra", "prairie", "moor"
)

urban_words <- c(
  "city", "town", "village", "hamlet", "street", "avenue", "boulevard",
  "square", "plaza", "alley", "lane", "ship", "shipyard", "boat", "steamboat",
  "bridge", "harbor", "harbour", "port", "dock", "wharf", "pier",
  "station", "terminal", "depot", "tunnel",
  "factory", "industrial", "plant", "mill",
  "railway", "railroad", "train", "tram", "subway", "metro",
  "road", "highway", "freeway", "expressway",
  "suburb", "skyline", "district", "block", "neighborhood", "farmhouse"
)

wild_words <- c(
  "forest", "woodland", "woods", "grove", "jungle", "rainforest",
  "mountain", "mount", "peak", "hill", "ridge", "valley", "canyon",
  "river", "stream", "brook", "lake", "pond", "waterfall", "cascade",
  "sea", "ocean", "seascape", "shore", "coast", "bay", "lagoon",
  "marsh", "wetland", "delta", "fjord",
  "meadow", "field", "plain", "savannah", "prairie", "moor", "tundra",
  "desert", "plateau", "cliff",
  "grove", "orchard", "garden", "botanical", "flora",
  "countryside", "pastoral", "farm", "rural", "wilderness", "nature",
  "glacier", "outdoors", "terrain"
)

environment_words <- c(landscape_words, urban_words)





## 1 ---> landscape ribbons (with mixed and phase divsions)

# i). Starting from all_art_full, keep only what I need
ribbon_data <- all_art_full %>%
  filter(
    depiction_type %in% c("wild", "mixed", "urban"),
    !is.na(year),
    year >= 1820, year <= 2020
  ) %>%
  mutate(
    decade = floor(year / 10) * 10,
    phase = factor(
      phase,
      levels = c(
        "Steam & Coal",
        "Oil & Empire",
        "Motor & Machine Vision",
        "Plastic & Suburban Nature",
        "Carbon & Crisis"
      )
    ),
    museum = factor(museum, levels = c("Met", "Tate")),
    depiction_type = factor(
      depiction_type,
      levels = c("wild", "mixed", "urban")
    )
  )

# ii). Count artworks per decade / museum / phase / depiction type
ribbon_summary <- ribbon_data %>%
  group_by(museum, phase, decade, depiction_type) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  # 3. Within each museum–phase–decade, convert to proportions
  mutate(
    decade_total = sum(n),
    prop         = n / decade_total
  ) %>%
  ungroup()

# iii) Plot the stacked Landscape Ribbons

# My color palette – consistent with the other plots
ribbon_palette <- c(
  wild  = "#4F9D69",  # green
  mixed = "#A57DC7",  # violet
  urban = "#F28E61"   # orange
)

p_ribbon <- ggplot(
  ribbon_summary,
  aes(x = decade, y = prop, fill = depiction_type)
) +
  # Stacked areas = layered landscape ribbons
  geom_area(
    position = "stack",
    color    = NA,
    alpha    = 0.9
  ) +
  facet_grid(
    phase ~ museum,
    switch = "y"  # phase labels on the left
  ) +
  scale_fill_manual(
    name   = "Landscape framing",
    values = ribbon_palette,
    labels = c(
      wild  = "Wild / rural",
      mixed = "Mixed",
      urban = "Urban / industrial"
    )
  ) +
  scale_x_continuous(
    breaks       = seq(1820, 2020, by = 20),
    minor_breaks = seq(1820, 2020, by = 10),
    expand       = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Layers of Landscape: Wild, Mixed, and Urban Over Time",
    subtitle = "Each panel shows the share of wild / mixed / urban landscapes per decade\nwithin an ecological phase, for Met and Tate collections.",
    x = "Decade",
    y = "Share of landscapes in that decade"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    # make it feel more like soft terrain, less like default ggplot
    panel.grid.major.x = element_line(color = "grey90", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.3),
    strip.placement    = "outside",
    strip.text.y.left  = element_text(size = 16, face = "bold", margin = margin(r = 10)),
    strip.text.x       = element_text(size = 18, face = "bold", margin = margin(b = 6)),
    legend.position    = "bottom",
    legend.title       = element_text(size = 16, face = "bold"),
    legend.text        = element_text(size = 14),
    plot.title         = element_text(size = 26, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 16, hjust = 0.5, margin = margin(b = 10)),
    panel.spacing.y    = unit(1.2, "lines"),
    panel.spacing.x    = unit(1.5, "lines")
  )

p_ribbon

# iv) saving landscape ribbons plot

ggsave(
"landscape_ribbons_met_tate.png",
p_ribbon,
width  = 42,  # inches – adjust to my poster size
height = 28,
units  = "in",
dpi    = 300
)




# 2 -----> Ribbons Version 2

## --------------------------------------------------------
## i). Build decade-level proportions for wild/mixed/urban

ribbons_df <- all_art_full %>%
  # I only want works that are clearly wild / mixed / urban,
  # within my study period, and with valid years.
  filter(
    depiction_type %in% c("wild", "mixed", "urban"),
    !is.na(year),
    year >= 1820, year <= 2020
  ) %>%
  mutate(
    decade = floor(year / 10) * 10,
    phase = factor(
      phase,
      levels = c(
        "Steam & Coal",
        "Oil & Empire",
        "Motor & Machine Vision",
        "Plastic & Suburban Nature",
        "Carbon & Crisis"
      )
    ),
    depiction_type = factor(
      depiction_type,
      levels = c("wild", "mixed", "urban")
    ),
    museum = factor(museum, levels = c("Met", "Tate"))
  ) %>%
  # For each museum–phase–decade–type, I count artworks…
  group_by(museum, phase, decade, depiction_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  # …then turn counts into within-decade proportions.
  group_by(museum, phase, decade) %>%
  mutate(
    total_decade = sum(n),
    prop = n / total_decade
  ) %>%
  ungroup()

## Quick sanity check in the console (optional for me):
## ribbons_df %>%
##   group_by(museum, phase, decade) %>%
##   summarise(sum_prop = sum(prop))

## --------------------------------------------------------
## ii). Aesthetic choices (colours, labels)

cols_land <- c(
  wild  = "#4C8C6B",  # muted forest green
  mixed = "#B98FD3",  # soft violet
  urban = "#F29B63"   # warm clay/orange
)

## --------------------------------------------------------
## iii). Final ribbon plot 

p_ribbons <- ggplot(
  ribbons_df,
  aes(x = decade, y = prop, fill = depiction_type)
) +
  # stacked ribbons for wild/mixed/urban
  geom_area(alpha = 0.95, colour = NA) +
  
  # phase columns, museum rows – compact grid
  facet_grid(museum ~ phase) +
  
  # x-axis: decades as ticks
  scale_x_continuous(
    breaks = seq(1820, 2020, by = 20),
    minor_breaks = seq(1820, 2020, by = 10),
    limits = c(1820, 2020),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  # y-axis: proportions as percentages, no extra padding
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  scale_fill_manual(
    name   = "Landscape framing",
    values = cols_land,
    labels = c(
      wild  = "Wild / rural",
      mixed = "Mixed",
      urban = "Urban / industrial"
    )
  ) +
  
  labs(
    title    = "Layers of Landscape: Wild, Mixed, and Urban Over Time",
    subtitle = paste(
      "Each panel shows the share of wild, mixed, and urban landscapes",
      "by decade within an ecological phase, for Met and Tate combined.",
      sep = "\n"
    ),
    x = "Decade",
    y = "Share of landscapes in that decade"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    # tighter panel spacing = less white space
    panel.spacing = unit(0.3, "lines"),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 18
    ),
    plot.subtitle = element_text(
      hjust = 0.5, size = 10, lineheight = 1.2
    ),
    
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6)),
    axis.text.x  = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y  = element_text(size = 8),
    
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "grey95", colour = NA),
    
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    
    plot.margin = margin(5, 5, 5, 5)
  )

## --------------------------------------------------------
## iv). Save a high-res, less-white-space version for the poster

ggsave(
  "landscape_ribbons_optimized.png",
  p_ribbons,
  width  = 14,   # i can tweak for different aspect ratio later
  height = 10,
  dpi    = 400
)


# 3) ----> Ribbons version 3

# Use the actual last decade available for research from both datasets
last_decade <- max(ribbons_cont$decade)  # 2010

# i) Define phase boundaries once, for reference and labels, so Carbon & Crisis ends at last_decade
phase_bounds <- tibble(
  phase = c(
    "Steam & Coal",
    "Oil & Empire",
    "Motor & Machine Vision",
    "Plastic & Suburban Nature",
    "Carbon & Crisis"
  ),
  start = c(1820, 1870, 1920, 1960, 1990),
  end   = c(1870, 1920, 1960, 1990, last_decade)
) %>%
  mutate(mid = (start + end) / 2)

# ii) Build one continuous ribbon dataset per museum–decade
ribbons_cont <- all_art_full %>%
  filter(
    depiction_type %in% c("wild", "mixed", "urban"),
    !is.na(year),
    year >= 1820, year <= 2020
  ) %>%
  mutate(
    decade = floor(year / 10) * 10,
    museum = factor(museum, levels = c("Met", "Tate")),
    depiction_type = factor(
      depiction_type,
      levels = c("wild", "mixed", "urban")
    )
  ) %>%
  group_by(museum, decade, depiction_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(museum, decade) %>%
  mutate(
    total_decade = sum(n),
    prop = n / total_decade
  ) %>%
  ungroup()

# iii) Continuous ribbon plot with phase bands & minimal ticks

# landscape-feeling palette
cols_land <- c(
  wild  = "#4C8C6B",  # muted forest green
  mixed = "#B98FD3",  # soft violet
  urban = "#F29B63"   # warm urban clay
)

phase_bounds <- tibble(
  phase = c(
    "Steam & Coal",
    "Oil & Empire",
    "Motor & Machine Vision",
    "Plastic & Suburban Nature",
    "Carbon & Crisis"
  ),
  start = c(1820, 1870, 1920, 1960, 1990),
  end   = c(1870, 1920, 1960, 1990, 2020)
) %>%
  mutate(mid = (start + end) / 2)   # x-position for the label


p_ribbons_cont <- ggplot() +
  # --- (A) very light phase bands in the background ---
  geom_rect(
    data = phase_bounds,
    aes(
      xmin = start,
      xmax = end,
      ymin = 0,
      ymax = 1
    ),
    inherit.aes = FALSE,
    fill = "grey95",
    colour = NA
  ) +
  
  # --- (B) stacked ribbons for wild / mixed / urban ---
  geom_area(
    data = ribbons_cont,
    aes(
      x    = decade,
      y    = prop,
      fill = depiction_type
    ),
    position = "stack",
    colour   = NA,
    alpha    = 0.98
  ) +
  
  # phase names along the top, centred in each band
  geom_text(
    data = phase_bounds,
    aes(x = mid, y = 1.02, label = phase),
    inherit.aes = FALSE,
    size = 3.4,
    fontface = "bold",
    vjust = 0
  ) +
  
  # --- (C) phase labels along the top, centred in each band ---
  geom_text(
    data = phase_bounds,
    aes(x = mid, y = 1.02, label = phase),
    inherit.aes = FALSE,
    size = 3.4,
    fontface = "bold",
    vjust = 0
  ) +
  
  # --- (D) facet by museum only (Met on top, Tate below) ---
  facet_grid(museum ~ .) +
  
  # x: only key ticks = phase boundaries + 2020
  scale_x_continuous(
    breaks = c(1820, 1870, 1920, 1960, 1990, last_decade),
    labels = c("1820", "1870", "1920", "1960", "1990", last_decade),
    limits = c(1820, last_decade),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  
  # y: proportion as %
  scale_y_continuous(
    labels = percent_format(accuracy = 10),
    limits = c(0, 1.05),             # <-- tiny bit above 100%
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  scale_fill_manual(
    name   = "Landscape framing",
    values = cols_land,
    labels = c(
      wild  = "Wild / rural",
      mixed = "Mixed",
      urban = "Urban / industrial"
    )
  ) +
  
  labs(
    title = "Layers of Landscape: Wild, Mixed, and Urban Over Time",
    subtitle = paste(
      "Continuous ribbons show the share of wild, mixed, and urban landscapes by decade,",
      "for each museum from 1820–2020.",
      "Light background bands mark major ecological phases.",
      sep = "\n"
    ),
    x = "Decade",
    y = "Share of landscapes in that decade"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    # allow phase labels to sit just above panel
    panel.spacing      = unit(0.6, "lines"),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle      = element_text(hjust = 0.5, size = 10, lineheight = 1.2),
    axis.title.x       = element_text(margin = margin(t = 6)),
    axis.title.y       = element_text(margin = margin(r = 6)),
    axis.text.x        = element_text(size = 9),
    axis.text.y        = element_text(size = 9),
    strip.text.y.left  = element_text(face = "bold", size = 11),
    legend.position    = "bottom",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9),
    plot.margin        = margin(10, 10, 15, 10),
    panel.background   = element_rect(fill = "white", colour = NA)
  ) +
  coord_cartesian(clip = "off")   # so phase labels above the panel are visible


# iv) save for poster
ggsave(
  "landscape_ribbons_continuous3.png",
  p_ribbons_cont,
  width  = 14,
  height = 9,
  dpi    = 400
)

## MAX & MIN YEARS OF BOTH THE TATE & MET DATASETS AFTER LANDSCAPE FILTERING
ribbons_cont %>%
  group_by(museum) %>%
  summarise(
    min_decade = min(decade),
    max_decade = max(decade),
    .groups = "drop"
  )



# 4 ----> Ribbons with major events included Version 1

# i) Continuous ribbon dataset per museum–decade

ribbons_cont <- all_art_full %>%
  filter(
    depiction_type %in% c("wild", "mixed", "urban"),
    !is.na(year),
    year >= 1820, year <= 2020
  ) %>%
  mutate(
    decade = floor(year / 10) * 10,
    museum = factor(museum, levels = c("Met", "Tate")),
    depiction_type = factor(
      depiction_type,
      levels = c("wild", "mixed", "urban")
    )
  ) %>%
  group_by(museum, decade, depiction_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(museum, decade) %>%
  mutate(
    total_decade = sum(n),
    prop = n / total_decade
  ) %>%
  ungroup()

# Use the actual last decade available for research from both datasets
last_decade <- max(ribbons_cont$decade)  # 2010

# ii) Phase boundaries
phase_bounds <- tibble(
  phase = c(
    "Steam & Coal",
    "Oil & Empire",
    "Motor & Machine Vision",
    "Plastic & Suburban Nature",
    "Carbon & Crisis"
  ),
  start = c(1820, 1870, 1920, 1960, 1990),
  end   = c(1870, 1920, 1960, 1990, last_decade)
) %>%
  mutate(mid = (start + end) / 2)

# Phase labels only for the Met panel
phase_bounds_label <- phase_bounds %>%
  mutate(museum = "Met")   # only draw in Met facet

# Key events (shared years, clearer labels)
event_df <- tibble(
  year  = c(1859, 1916, 1945, 1969, 1992),
  label = c(
    "1859 · Darwin’s\nOrigin of Species",
    "1916 · US National\nPark Service founded",
    "1945 · Atomic bombings\n& postwar rebuilding",
    "1969 · Apollo 11\n“Earthrise” photo",
    "1992 · Rio Earth Summit\non sustainable development"
  )
)

# Event label positions (different heights, Met only)
event_label_df <- event_df %>%
  mutate(
   museum  = "Met",
    label_y = c(0.90, 0.78, 0.66, 0.84, 0.72)   # staggered vertically
  )

# Segments with a small “gap” behind each label (shared by both museums)
event_segments <- event_label_df %>%
  select(year, label_y) %>%
  tidyr::expand_grid(part = c("lower", "upper")) %>%
  mutate(
    y    = dplyr::if_else(part == "lower", 0, label_y + 0.03),
    yend = dplyr::if_else(part == "lower", label_y - 0.03, 1)
  )

## palette (same as before)
cols_land <- c(
  wild  = "#35624E",  # deeper forest green
  mixed = "#C4ABD9",  # soft violet haze
  urban = "#F09A5A"   # warm urban clay
)

## SAFETY: make sure ribbons_cont really has separate Met / Tate series
ribbons_cont <- ribbons_cont %>%
  mutate(museum = factor(museum))   # keep both levels as they are

p_ribbons_cont <- ggplot() +
  # --- ground–sky background in each panel ---
  geom_rect(
    aes(xmin = 1820, xmax = 2010, ymin = 0, ymax = 0.5),
    inherit.aes = FALSE,
    fill = "#F3F7F2",
    colour = NA
  ) +
  geom_rect(
    aes(xmin = 1820, xmax = 2010, ymin = 0.5, ymax = 1),
    inherit.aes = FALSE,
    fill = "#F7F2EE",
    colour = NA
  ) +
  
  # --- light vertical phase bands ---
  geom_rect(
    data = phase_bounds,
    aes(xmin = start, xmax = end, ymin = 0, ymax = 1),
    inherit.aes = FALSE,
    fill = "white",
    alpha = 0.25,
    colour = NA
  ) +
  
  # --- stacked ribbons (different per museum) ---
  geom_area(
    data = ribbons_cont,
    aes(
      x    = decade,
      y    = prop,
      fill = depiction_type
    ),
    position = "stack",
    colour   = NA,
    alpha    = 0.98
  ) +
  
  # --- horizon & contour lines ---
  geom_hline(
    yintercept = 0.5,
    colour     = "white",
    linewidth  = 0.4,
    linetype   = "dashed"
  ) +
  geom_hline(
    yintercept = c(0.25, 0.75),
    colour     = "white",
    linewidth  = 0.2,
    linetype   = "dotted"
  ) +
  
  # --- key event vertical lines (run through both panels) ---
  geom_segment(
    data = event_df,
    aes(
      x    = year,
      xend = year,
      y    = 0,
      yend = 1
    ),
    inherit.aes = FALSE,
    colour      = "grey20",
    linewidth   = 0.4,
    linetype    = "dotdash",
    alpha       = 0.8
  ) +
  
  # --- event labels: ONCE, inside Met panel, staggered on the lines ---
  geom_text(
    data = event_label_df,
    aes(
      x     = year,
      y     = label_y,
      label = label
    ),
    inherit.aes = FALSE,
    size        = 3.3,
    lineheight  = 1.05,
    vjust       = 0.5,
    hjust       = 0.5,
    fontface    = "bold",
    colour      = "black"
  ) +
  
  # --- phase labels: once, above Met, ridge style ---
  geom_text(
    data = phase_bounds_label,
    aes(
      x     = mid,
      y     = 1.04,
      label = toupper(phase)   # ALL CAPS
    ),
    inherit.aes = FALSE,
    size        = 3.8,
    fontface    = "bold",
    vjust       = 0,
    colour      = "black",
    letterspacing = 0.5
  ) +
  
  # --- facets: Met on top, Tate below ---
  facet_grid(museum ~ .) +
  
  # x: show major ticks + faint decade grid
  scale_x_continuous(
    limits       = c(1820, 2010),
    breaks       = c(1820, 1870, 1920, 1960, 1990, 2010),
    labels       = c("1820", "1870", "1920", "1960", "1990", "2010"),
    minor_breaks = seq(1820, 2010, by = 10),
    expand       = expansion(mult = c(0.01, 0.06))  # a bit more top space for labels
  ) +
  
  
  # y: proportion as %
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 10),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  scale_fill_manual(
    name   = "Landscape framing",
    values = cols_land,
    labels = c(
      wild  = "Wild / rural",
      mixed = "Mixed",
      urban = "Urban / industrial"
    )
  ) +
  
  labs(
    title = "Layers of Landscape: Wild, Mixed, and Urban Over Time",
    subtitle = paste(
      "Continuous ribbons show the share of wild, mixed, and urban landscapes by decade,",
      "for each museum from 1820–2010.",
      "Light background bands mark major ecological phases; vertical lines mark key ecological / visual turning points.",
      sep = "\n"
    ),
    x = "Decade",
    y = "Share of landscapes in that decade"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.spacing       = unit(0.6, "lines"),
    # decade guides ONLY in x
    panel.grid.minor.x  = element_line(
      colour    = "#FFFFFFB3",   # white with alpha (faint)
      linewidth = 0.25,
      linetype  = "dotted"
    ),
    panel.grid.minor.y  = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y  = element_line(
      colour    = "#FFFFFFCC",
      linewidth = 0.2
    ),
    plot.title          = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle       = element_text(hjust = 0.5, size = 10, lineheight = 1.2),
    axis.title.x        = element_text(margin = margin(t = 6)),
    axis.title.y        = element_text(margin = margin(r = 6)),
    axis.text.x         = element_text(size = 10),
    axis.text.y         = element_text(size = 9),
    strip.text.y.left   = element_text(face = "bold", size = 11),
    legend.position     = "bottom",
    legend.title        = element_text(size = 10),
    legend.text         = element_text(size = 9),
    plot.margin         = margin(18, 16, 24, 16),
    panel.background    = element_rect(fill = "transparent", colour = NA)
  ) +
  coord_cartesian(clip = "off")  # allow labels above Met panel

## SAVE (slightly bigger)
ggsave(
  "landscape_ribbons_landscapey_events_v3.png",
  p_ribbons_cont,
  width  = 22,   # inches
  height = 12,   # inches
  dpi    = 500
)







## Quick sanity check that Met and Tate are truly different

ribbons_cont %>%
  filter(depiction_type == "urban") %>%
  select(museum, decade, prop) %>%
  tidyr::pivot_wider(names_from = museum, values_from = prop)



## 5 ----> Giant ridge plot 

## i) Build ridge data: Met + Tate combined, by medium & depiction type ----
ridge_base <- all_art_full %>%
  filter(
    depiction_type %in% c("wild", "mixed", "urban"),
    !is.na(year),
    year >= 1820, year <= 2010
  ) %>%
  mutate(
    decade = floor(year / 10) * 10,
    depiction_type = factor(
      depiction_type,
      levels = c("wild", "mixed", "urban")
    )
  ) %>%
  # collapse very tiny media into an "Other / rare" bucket so ridges aren’t spiky noise
  mutate(
    medium_group = fct_lump_min(medium_group, min = 150, other_level = "Other / rare")
  ) %>%
  group_by(depiction_type, medium_group, decade) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(depiction_type, medium_group) %>%
  mutate(
    total_medium_type = sum(n),
    prop = n / total_medium_type
  ) %>%
  ungroup()

## ii) Light smoothing over decades (3-decade moving average) ----
ridge_smooth <- ridge_base %>%
  arrange(depiction_type, medium_group, decade) %>%
  group_by(depiction_type, medium_group) %>%
  mutate(
    prop_smooth = {
      p <- prop
      # 3-point centered moving average using base stats::filter
      k <- rep(1/3, 3)
      sm <- as.numeric(stats::filter(p, k, sides = 2))
      # keep original at edges where filter returns NA
      sm[is.na(sm)] <- p[is.na(sm)]
      sm
    }
  ) %>%
  ungroup()

## iii) Give each medium a vertical position (same order in all three rows) ----
medium_levels <- ridge_smooth %>%
  group_by(medium_group) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  mutate(y = row_number())   # 1 = biggest medium at top

ridge_df <- ridge_smooth %>%
  left_join(medium_levels, by = "medium_group")

## iv) Palette: earthy but varied, one color per medium ----
medium_cols <- c(
  "Oil painting"             = "#F6A45A",  # warm sienna
  "Water-based painting"     = "#6DA67A",  # soft green
  "Drawing / Works on paper" = "#7C9CD6",  # slate blue
  "Printmaking"              = "#C48FD9",  # violet
  "Photography"              = "#F7D96B",  # muted yellow
  "Digital/Media"            = "#FF9FB8",  # coral rose
  "Sculpture"                = "#B6C3C7",  # cool stone grey
  "Other"                    = "#A57C65",  # brown
  "Other / rare"             = "#8E8E8E"   # neutral
)

# keep only colors that actually appear
medium_cols <- medium_cols[names(medium_cols) %in% ridge_df$medium_group]

## v) Build the ridge plot ----
ridge_plot_media <- ggplot(
  ridge_df,
  aes(
    x      = decade,
    y      = y,
    height = prop_smooth,
    group  = medium_group,
    fill   = medium_group
  )
) +
  ggridges::geom_ridgeline(
    stat      = "identity",
    scale     = 12,       # taller ridges; tweak if too tall
    linewidth = 0.25,
    colour    = "black",
    alpha     = 0.98
  ) +
  scale_fill_manual(
    values = medium_cols,
    name   = "Medium"
  ) +
  scale_y_continuous(
    breaks = medium_levels$y,
    labels = medium_levels$medium_group,
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  scale_x_continuous(
    breaks       = seq(1820, 2010, by = 20),
    minor_breaks = seq(1820, 2010, by = 10),
    expand       = expansion(mult = c(0.02, 0.02))
  ) +
  facet_grid(
    rows = vars(depiction_type),
    labeller = labeller(
      depiction_type = c(
        wild  = "Wild / rural",
        mixed = "Mixed",
        urban = "Urban / industrial"
      )
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "black", colour = NA),
    panel.background = element_rect(fill = "black", colour = NA),
    strip.text.y     = element_text(
      colour = "white", face = "bold", size = 11,
      margin = margin(r = 6)
    ),
    axis.text.y      = element_text(colour = "white", size = 8),
    axis.text.x      = element_text(colour = "white", size = 8, margin = margin(t = 4)),
    panel.grid.minor.x = element_line(
      colour   = "#FFFFFF33",
      linewidth = 0.2,
      linetype = "dotted"
    ),
    panel.grid.major.x = element_line(
      colour   = "#FFFFFF55",
      linewidth = 0.25,
      linetype = "dashed"
    ),
    axis.title        = element_blank(),
    legend.position   = "bottom",
    legend.text       = element_text(colour = "white", size = 8),
    legend.title      = element_text(colour = "white", size = 9, face = "bold"),
    plot.margin       = margin(10, 20, 20, 20)
  )

ridge_plot_media

## vi) Save large for the bottom strip of the poster ----
ggsave(
  "landscape_ridges_media_wild_mixed_urban.png",
  ridge_plot_media,
  width  = 24,   # inches – long horizontal strip
  height = 9,    # adjust to fit my poster layout
  dpi    = 300
)


## downloading my created datasets for visualization use
write.csv(all_art_full, file = "all_art_full.csv", row.names = FALSE)
write.csv(all_art_model, file = "all_art_model.csv", row.names = FALSE)
write.csv(ribbons_cont, file = "ribbons_cont.csv", row.names = FALSE)
## -------------------------------------------------------------------


## 6 ----> Giant ridge plot Version 2

## baselines all rows for every ridge
# Baseline y for each ridge (already in ridge_levels)
baseline_lines <- ridge_levels$y

## i) Build base data: Met + Tate combined, by medium & wild/mixed/urban ----
ridge_base2 <- all_art_full %>%
  filter(
    depiction_type %in% c("wild", "mixed", "urban"),
    !is.na(year),
    year >= 1820, year <= 2010
  ) %>%
  mutate(
    decade = floor(year / 10) * 10,
    depiction_type = factor(
      depiction_type,
      levels = c("wild", "mixed", "urban")
    )
  ) %>%
  # lump very small media together so I don't get super noisy tiny ridges
  mutate(
    medium_group = fct_lump_min(medium_group, min = 150, other_level = "Other / rare")
  ) %>%
  group_by(medium_group, depiction_type, decade) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(medium_group, depiction_type) %>%
  mutate(
    total_md = sum(n),
    prop     = n / total_md
  ) %>%
  ungroup()

## ii) Light smoothing over decades (3-point moving average) ----
ridge_smooth2 <- ridge_base2 %>%
  arrange(medium_group, depiction_type, decade) %>%
  group_by(medium_group, depiction_type) %>%
  mutate(
    prop_smooth = {
      p <- prop
      k <- rep(1/3, 3)
      sm <- as.numeric(stats::filter(p, k, sides = 2))
      sm[is.na(sm)] <- p[is.na(sm)]
      sm
    }
  ) %>%
  ungroup()

## iii) Assign vertical positions & labels: each medium × type is a ridge ----
ridge_levels <- ridge_smooth2 %>%
  distinct(medium_group, depiction_type) %>%
  arrange(medium_group, depiction_type) %>%   # order: by medium, within that wild/mixed/urban
  mutate(
    ridge_id   = interaction(medium_group, depiction_type, sep = " – "),
    y          = row_number()
  )

ridge_df2 <- ridge_smooth2 %>%
  left_join(ridge_levels, by = c("medium_group", "depiction_type"))

## iv) Colour palette: many distinct colours, one per ridge ----
n_ridges <- nrow(ridge_levels)

# use hue palette for lots of distinct colours
ridge_cols <- setNames(
  scales::hue_pal(l = 70, c = 80)(n_ridges),
  ridge_levels$ridge_id
)

## v) Ridge plot: one big landscape of mediums × framings ----
ridge_plot_all <- ggplot(
  ridge_df2,
  aes(
    x      = decade,
    y      = y,
    height = prop_smooth,
    group  = ridge_id,
    fill   = ridge_id
  )
) +
  ggridges::geom_ridgeline(
    stat      = "identity",
    scale     = 14,         # how tall the hills are; tweak 8–14 depending on taste
    linewidth = 0.25,
    colour    = "black",
    alpha     = 0.97
  ) +
  
  # --- add faint horizontal lines under each ridge ---
  geom_hline(
    yintercept = baseline_lines,
    colour     = "#FFFFFF25",   # white at ~25% opacity
    linewidth  = 0.25,
    linetype   = "longdash"
  ) +

  scale_fill_manual(
    values = ridge_cols,
    name   = NULL
  ) +
  scale_y_continuous(
    breaks = ridge_levels$y,
    labels = ridge_levels$ridge_id,
    expand = expansion(mult = c(0.05, 0.10))
  ) +
  scale_x_continuous(
    breaks       = seq(1820, 2010, by = 20),
    minor_breaks = seq(1820, 2010, by = 10),
    expand       = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title    = "Media Ridges: How Wild, Mixed, and Urban Landscapes Flow Across Media",
    subtitle = "Each ridge is a medium × landscape framing (wild / mixed / urban), combining Met and Tate.\nHeights show the share of that medium–framing that appears in each decade from 1820–2010.",
    x        = "Decade",
    y        = NULL
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "black", colour = NA),
    panel.background = element_rect(fill = "black", colour = NA),
    axis.text.y      = element_text(colour = "white", size = 7),
    axis.text.x      = element_text(colour = "white", size = 8, margin = margin(t = 4)),
    axis.title.x     = element_text(colour = "white", size = 9, margin = margin(t = 6)),
    plot.title       = element_text(colour = "white", face = "bold", size = 16, hjust = 0.5),
    plot.subtitle    = element_text(colour = "white", size = 9, hjust = 0.5, lineheight = 1.2),
    panel.grid.minor.x = element_line(
      colour   = "#FFFFFF33",
      linewidth = 0.2,
      linetype = "dotted"
    ),
    panel.grid.major.x = element_line(
      colour   = "#FFFFFF55",
      linewidth = 0.25,
      linetype = "dashed"
    ),
    legend.position   = "none",     # labels are on the left already
    plot.margin       = margin(15, 20, 20, 80)  # extra left margin for labels
  )

ridge_plot_all

## vi) Save big for the bottom of the poster ----
ggsave(
  "ridges_allMedia-dashedLINES.png",
  ridge_plot_all,
  width  = 24,   # long strip across poster
  height = 16,    # adjust to fit my layout
  dpi    = 600
)




### 7 -----> Giant ridge plot on WHITE BACKGROUND

## baselines all rows for every ridge
# Baseline y for each ridge (already in ridge_levels)
baseline_lines <- ridge_levels$y

## i) Build base data: Met + Tate combined, by medium & wild/mixed/urban ----
ridge_base2 <- all_art_full %>%
  filter(
    depiction_type %in% c("wild", "mixed", "urban"),
    !is.na(year),
    year >= 1820, year <= 2010
  ) %>%
  mutate(
    decade = floor(year / 10) * 10,
    depiction_type = factor(
      depiction_type,
      levels = c("wild", "mixed", "urban")
    )
  ) %>%
  # lump very small media together so I don't get super noisy tiny ridges
  mutate(
    medium_group = fct_lump_min(medium_group, min = 150, other_level = "Other / rare")
  ) %>%
  group_by(medium_group, depiction_type, decade) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(medium_group, depiction_type) %>%
  mutate(
    total_md = sum(n),
    prop     = n / total_md
  ) %>%
  ungroup()

## ii) Light smoothing over decades (3-point moving average) ----
ridge_smooth2 <- ridge_base2 %>%
  arrange(medium_group, depiction_type, decade) %>%
  group_by(medium_group, depiction_type) %>%
  mutate(
    prop_smooth = {
      p <- prop
      k <- rep(1/3, 3)
      sm <- as.numeric(stats::filter(p, k, sides = 2))
      sm[is.na(sm)] <- p[is.na(sm)]
      sm
    }
  ) %>%
  ungroup()

## iii) Assign vertical positions & labels: each medium × type is a ridge ----
ridge_levels <- ridge_smooth2 %>%
  distinct(medium_group, depiction_type) %>%
  arrange(medium_group, depiction_type) %>%   # order: by medium, within that wild/mixed/urban
  mutate(
    ridge_id   = interaction(medium_group, depiction_type, sep = " – "),
    y          = row_number()
  )

ridge_df2 <- ridge_smooth2 %>%
  left_join(ridge_levels, by = c("medium_group", "depiction_type"))

## iv) Colour palette: many distinct colors, one per ridge ----
n_ridges <- nrow(ridge_levels)

## v) use hue palette for lots of distinct colors
ridge_cols <- setNames(
  scales::hue_pal(l = 70, c = 80)(n_ridges),
  ridge_levels$ridge_id
)

## vi) plotting the ridges

ridge_plot_all <- ggplot(
  ridge_df2,
  aes(
    x      = decade,
    y      = y,
    height = prop_smooth,
    group  = ridge_id,
    fill   = ridge_id
  )
) +
  # ridges
  ggridges::geom_ridgeline(
    stat      = "identity",
    scale     = 14,         # keep my height; lower (8–12) if it's too tall
    linewidth = 0.25,
    colour    = "grey15",
    alpha     = 0.95
  ) +
  
  # faint horizontal baselines under each ridge
  geom_hline(
    yintercept = ridge_levels$y,
    colour     = "grey85",
    linewidth  = 0.25,
    linetype   = "dotted"
  ) +
  
  # colour per ridge (same palette as before)
  scale_fill_manual(
    values = ridge_cols,
    name   = NULL
  ) +
  
  # y-axis: one tick per ridge, with its label
  scale_y_continuous(
    breaks = ridge_levels$y,
    labels = ridge_levels$ridge_id,
    expand = expansion(mult = c(0.05, 0.10))
  ) +
  
  # x-axis: decades
  scale_x_continuous(
    breaks       = seq(1820, 2010, by = 20),
    minor_breaks = seq(1820, 2010, by = 10),
    expand       = expansion(mult = c(0.02, 0.02))
  ) +
  
  labs(
    title    = "Media Ridges: How Wild, Mixed, and Urban Landscapes Flow Across Media",
    subtitle = "Each ridge is a medium × landscape framing (wild / mixed / urban), combining Met and Tate.\nHeights show the share of that medium–framing in each decade from 1820–2010.",
    x        = "Decade",
    y        = NULL
  ) +
  
  theme_void(base_size = 11) +
  theme(
    # white background instead of black
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    # axes text in dark grey/black
    axis.text.y      = element_text(colour = "grey15", size = 7),
    axis.text.x      = element_text(colour = "grey15", size = 8, margin = margin(t = 4)),
    axis.title.x     = element_text(colour = "grey20", size = 9, margin = margin(t = 6)),
    
    # title + subtitle in dark
    plot.title       = element_text(colour = "grey10", face = "bold", size = 16, hjust = 0.5),
    plot.subtitle    = element_text(colour = "grey20", size = 9, hjust = 0.5, lineheight = 1.2),
    
    # vertical grid lines (decade guides) in light grey
    panel.grid.minor.x = element_line(
      colour    = "grey90",
      linewidth = 0.2,
      linetype  = "dotted"
    ),
    panel.grid.major.x = element_line(
      colour    = "grey85",
      linewidth = 0.25,
      linetype  = "dashed"
    ),
    
    legend.position   = "none",     # labels are on the left already
    plot.margin       = margin(15, 20, 20, 80)  # extra left margin for labels
  )

ridge_plot_all

# save for poster
ggsave(
  "ridges_all_media_white.png",
  ridge_plot_all,
  width  = 24,   # adjust to my layout
  height = 16,
  dpi    = 600
)


### 8 -----> Giant ridge plot on WHITE BACKGROUND 
###--------Palette 1 - “Earth & Sky”---------------------

## baselines all rows for every ridge
# Baseline y for each ridge (already in ridge_levels)
baseline_lines <- ridge_levels$y

## i) Build base data: Met + Tate combined, by medium & wild/mixed/urban ----
ridge_base2 <- all_art_full %>%
  filter(
    depiction_type %in% c("wild", "mixed", "urban"),
    !is.na(year),
    year >= 1820, year <= 2010
  ) %>%
  mutate(
    decade = floor(year / 10) * 10,
    depiction_type = factor(
      depiction_type,
      levels = c("wild", "mixed", "urban")
    )
  ) %>%
  # lump very small media together so I don't get super noisy tiny ridges
  mutate(
    medium_group = fct_lump_min(medium_group, min = 150, other_level = "Other / rare")
  ) %>%
  group_by(medium_group, depiction_type, decade) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(medium_group, depiction_type) %>%
  mutate(
    total_md = sum(n),
    prop     = n / total_md
  ) %>%
  ungroup()

## ii) Light smoothing over decades (3-point moving average) ----
ridge_smooth2 <- ridge_base2 %>%
  arrange(medium_group, depiction_type, decade) %>%
  group_by(medium_group, depiction_type) %>%
  mutate(
    prop_smooth = {
      p <- prop
      k <- rep(1/3, 3)
      sm <- as.numeric(stats::filter(p, k, sides = 2))
      sm[is.na(sm)] <- p[is.na(sm)]
      sm
    }
  ) %>%
  ungroup()

## iii) Assign vertical positions & labels: each medium × type is a ridge ----
ridge_levels <- ridge_smooth2 %>%
  distinct(medium_group, depiction_type) %>%
  arrange(medium_group, depiction_type) %>%   # order: by medium, within that wild/mixed/urban
  mutate(
    ridge_id   = interaction(medium_group, depiction_type, sep = " – "),
    y          = row_number()
  )

ridge_df2 <- ridge_smooth2 %>%
  left_join(ridge_levels, by = c("medium_group", "depiction_type"))

## iv) Colour palette: many distinct colors, one per ridge ----
n_ridges <- nrow(ridge_levels)

## v) use hue palette for lots of distinct colors
## ---- PALETTE 1: Earth & Sky ----

earth_sky_palette <- c(
  "#355E3B", # deep forest green
  "#6A8E7F", # sage mist
  "#C4D7B2", # pale moss
  "#D4A373", # clay / soil
  "#8C6A4F", # bark brown
  "#52796F", # cedar shadow
  "#E9D8A6", # sand / sunlight
  "#6C757D", # stone grey
  "#B56576", # warm earth rose
  "#AEC6CF", # distant haze blue
  "#9A8C98", # muted lavender (mixed feeling)
  "#F4A261", # warm urban clay
  "#264653", # deep slate
  "#E76F51", # urban heat
  "#84A98C", # soft moss green
  "#5A7D7C"  # teal-grey shadow
)

ridge_cols <- setNames(
  rep(earth_sky_palette, length.out = n_ridges),
  ridge_levels$ridge_id
)


## vi) plotting the ridges

ridge_plot_all <- ggplot(
  ridge_df2,
  aes(
    x      = decade,
    y      = y,
    height = prop_smooth,
    group  = ridge_id,
    fill   = ridge_id
  )
) +
  # ridges
  ggridges::geom_ridgeline(
    stat      = "identity",
    scale     = 14,         # keep my height; lower (8–12) if it's too tall
    linewidth = 0.25,
    colour    = "grey15",
    alpha     = 0.95
  ) +
  
  # faint horizontal baselines under each ridge
  geom_hline(
    yintercept = ridge_levels$y,
    colour     = "grey85",
    linewidth  = 0.25,
    linetype   = "dotted"
  ) +
  
  # colour per ridge (same palette as before)
  scale_fill_manual(
    values = ridge_cols,
    name   = NULL
  ) +
  
  # y-axis: one tick per ridge, with its label
  scale_y_continuous(
    breaks = ridge_levels$y,
    labels = ridge_levels$ridge_id,
    expand = expansion(mult = c(0.05, 0.10))
  ) +
  
  # x-axis: decades
  scale_x_continuous(
    breaks       = seq(1820, 2010, by = 20),
    minor_breaks = seq(1820, 2010, by = 10),
    expand       = expansion(mult = c(0.02, 0.02))
  ) +
  
  labs(
    title    = "Media Ridges: How Wild, Mixed, and Urban Landscapes Flow Across Media",
    subtitle = "Each ridge is a medium × landscape framing (wild / mixed / urban), combining Met and Tate.\nHeights show the share of that medium–framing in each decade from 1820–2010.",
    x        = "Decade",
    y        = NULL
  ) +
  
  theme_void(base_size = 11) +
  theme(
    # white background instead of black
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    # axes text in dark grey/black
    axis.text.y      = element_text(colour = "grey15", size = 7),
    axis.text.x      = element_text(colour = "grey15", size = 8, margin = margin(t = 4)),
    axis.title.x     = element_text(colour = "grey20", size = 9, margin = margin(t = 6)),
    
    # title + subtitle in dark
    plot.title       = element_text(colour = "grey10", face = "bold", size = 16, hjust = 0.5),
    plot.subtitle    = element_text(colour = "grey20", size = 9, hjust = 0.5, lineheight = 1.2),
    
    # vertical grid lines (decade guides) in light grey
    panel.grid.minor.x = element_line(
      colour    = "grey90",
      linewidth = 0.2,
      linetype  = "dotted"
    ),
    panel.grid.major.x = element_line(
      colour    = "grey85",
      linewidth = 0.25,
      linetype  = "dashed"
    ),
    
    legend.position   = "none",     # labels are on the left already
    plot.margin       = margin(15, 20, 20, 80)  # extra left margin for labels
  )

ridge_plot_all

# save for poster
ggsave(
  "ridges_allmedia_EarthSky.png",
  ridge_plot_all,
  width  = 24,   # adjust to my layout
  height = 16,
  dpi    = 600
)


### 9 -----> Giant ridge plot on WHITE BACKGROUND 
###--------Palette 4 - “Pastel Ecology” (Soft, airy, minimal)---------------------

## baselines all rows for every ridge
# Baseline y for each ridge (already in ridge_levels)
baseline_lines <- ridge_levels$y

## i) Build base data: Met + Tate combined, by medium & wild/mixed/urban ----
ridge_base2 <- all_art_full %>%
  filter(
    depiction_type %in% c("wild", "mixed", "urban"),
    !is.na(year),
    year >= 1820, year <= 2010
  ) %>%
  mutate(
    decade = floor(year / 10) * 10,
    depiction_type = factor(
      depiction_type,
      levels = c("wild", "mixed", "urban")
    )
  ) %>%
  # lump very small media together so I don't get super noisy tiny ridges
  mutate(
    medium_group = fct_lump_min(medium_group, min = 150, other_level = "Other / rare")
  ) %>%
  group_by(medium_group, depiction_type, decade) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(medium_group, depiction_type) %>%
  mutate(
    total_md = sum(n),
    prop     = n / total_md
  ) %>%
  ungroup()

## ii) Light smoothing over decades (3-point moving average) ----
ridge_smooth2 <- ridge_base2 %>%
  arrange(medium_group, depiction_type, decade) %>%
  group_by(medium_group, depiction_type) %>%
  mutate(
    prop_smooth = {
      p <- prop
      k <- rep(1/3, 3)
      sm <- as.numeric(stats::filter(p, k, sides = 2))
      sm[is.na(sm)] <- p[is.na(sm)]
      sm
    }
  ) %>%
  ungroup()

## iii) Assign vertical positions & labels: each medium × type is a ridge ----
ridge_levels <- ridge_smooth2 %>%
  distinct(medium_group, depiction_type) %>%
  arrange(medium_group, depiction_type) %>%   # order: by medium, within that wild/mixed/urban
  mutate(
    ridge_id   = interaction(medium_group, depiction_type, sep = " – "),
    y          = row_number()
  )

ridge_df2 <- ridge_smooth2 %>%
  left_join(ridge_levels, by = c("medium_group", "depiction_type"))

## iv) Colour palette: many distinct colors, one per ridge ----
n_ridges <- nrow(ridge_levels)

## v) use hue palette for lots of distinct colors
soft_ecology <- c(
  "#8FB996", # eucalyptus
  "#C6D9C8", # pale mint
  "#BFB5AF", # warm grey clay
  "#E3D5CA", # dust rose
  "#E9ECEF", # cloud grey
  "#C3A995", # soft soil
  "#A98467", # bark tan
  "#748CAB", # cool mist blue
  "#9CB4CC", # sky tint
  "#6C757D", # stone
  "#E5989B", # warm blossom
  "#B5838D", # muted plum
  "#FFCDB2", # peach mineral
  "#FFE5D9", # warm light
  "#ADB5BD"  # dry fog grey
)

ridge_cols <- setNames(
  rep(soft_ecology, length.out = n_ridges),
  ridge_levels$ridge_id
)

## vi) plotting the ridges

ridge_plot_all <- ggplot(
  ridge_df2,
  aes(
    x      = decade,
    y      = y,
    height = prop_smooth,
    group  = ridge_id,
    fill   = ridge_id
  )
) +
  # ridges
  ggridges::geom_ridgeline(
    stat      = "identity",
    scale     = 14,         # keep my height; lower (8–12) if it's too tall
    linewidth = 0.25,
    colour    = "grey15",
    alpha     = 0.95
  ) +
  
  # faint horizontal baselines under each ridge
  geom_hline(
    yintercept = ridge_levels$y,
    colour     = "grey85",
    linewidth  = 0.25,
    linetype   = "dotted"
  ) +
  
  # colour per ridge (same palette as before)
  scale_fill_manual(
    values = ridge_cols,
    name   = NULL
  ) +
  
  # y-axis: one tick per ridge, with its label
  scale_y_continuous(
    breaks = ridge_levels$y,
    labels = ridge_levels$ridge_id,
    expand = expansion(mult = c(0.05, 0.10))
  ) +
  
  # x-axis: decades
  scale_x_continuous(
    breaks       = seq(1820, 2010, by = 20),
    minor_breaks = seq(1820, 2010, by = 10),
    expand       = expansion(mult = c(0.02, 0.02))
  ) +
  
  labs(
    title    = "Media Ridges: How Wild, Mixed, and Urban Landscapes Flow Across Media",
    subtitle = "Each ridge is a medium × landscape framing (wild / mixed / urban), combining Met and Tate.\nHeights show the share of that medium–framing in each decade from 1820–2010.",
    x        = "Decade",
    y        = NULL
  ) +
  
  theme_void(base_size = 11) +
  theme(
    # white background instead of black
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    # axes text in dark grey/black
    axis.text.y      = element_text(colour = "grey15", size = 7),
    axis.text.x      = element_text(colour = "grey15", size = 8, margin = margin(t = 4)),
    axis.title.x     = element_text(colour = "grey20", size = 9, margin = margin(t = 6)),
    
    # title + subtitle in dark
    plot.title       = element_text(colour = "grey10", face = "bold", size = 16, hjust = 0.5),
    plot.subtitle    = element_text(colour = "grey20", size = 9, hjust = 0.5, lineheight = 1.2),
    
    # vertical grid lines (decade guides) in light grey
    panel.grid.minor.x = element_line(
      colour    = "grey90",
      linewidth = 0.2,
      linetype  = "dotted"
    ),
    panel.grid.major.x = element_line(
      colour    = "grey85",
      linewidth = 0.25,
      linetype  = "dashed"
    ),
    
    legend.position   = "none",     # labels are on the left already
    plot.margin       = margin(15, 20, 20, 80)  # extra left margin for labels
  )

ridge_plot_all

# save for poster
ggsave(
  "ridges_allmedia_PastelEcology.png",
  ridge_plot_all,
  width  = 24,   # adjust to my layout
  height = 16,
  dpi    = 600
)

## 10 -----> Horizontal Proportion Bars for WILD Landscapes Across Phases

# --- i) Prepare dataset for wild works combined ---
wild_phase_df <- all_art_full %>%
  filter(
    depiction_type == "wild",
    !is.na(phase)
  ) %>%
  group_by(phase) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    total = sum(n),
    prop = n / total,
    phase = fct_relevel(
      phase,
      "Steam & Coal",
      "Oil & Empire",
      "Motor & Machine Vision",
      "Plastic & Suburban Nature",
      "Carbon & Crisis"
    )
  )

# --- ii) Simple clean horizontal bar plot ---
p_wild_bars <- ggplot(wild_phase_df, aes(
  x = prop,
  y = phase
)) +
  geom_col(
    fill = "white",       # muted green for wild landscapes
    color = "black",
    linewidth = 0.6,
    width = 0.55            # controls bar thickness (0.4–0.7 works well)
  ) +
  
  # percentage labels on bars
  geom_text(
    aes(label = scales::percent(prop, accuracy = 1)),
    hjust = -0.1,
    size = 4,
    color = "black"
  ) +
  
  scale_x_continuous(
    limits = c(0, max(wild_phase_df$prop) * 1.25),   # extra space to the right
    labels = scales::percent_format(accuracy = 10)
  ) +
  
  labs(
    title = "Proportion of Wild Landscapes Across Ecological Phases",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),   # removes horizontal gridlines between bars
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 40, 20, 20)    # adds space for Photoshop overlays
  )

p_wild_bars

# Save for Photoshop
ggsave(
  "wild_phase_bars-transperantBG.png",
  p_wild_bars,
  width = 10,
  height = 5,
  dpi = 600,
  bg = "transparent"
)


###-----------------------------------------------
# Establishing Palette & Theme for reuse
###------------------------------------------------

## ------------ Shared theme + colours ---------------------------------


# Pastel-ecology style colours (I can tweak hexes later)
cols_ecology <- c(
  "Met"      = "#4C8C90",
  "Tate"     = "#E08C7F",
  "Printmaking" = "#F0A974",
  "Drawing / Works on paper" = "#88B08F",
  "Photography" = "#C5B5E6",
  "Water-based painting" = "#8FC6C2",
  "Sculpture" = "#F2C893",
  "Other" = "#B9C6CF"
)

theme_pastel_ecology <- function() {
  theme_minimal(base_size = 12) +
    theme(
      text            = element_text(colour = "black"),
      axis.text       = element_text(colour = "black"),
      axis.title      = element_text(colour = "black"),
      plot.title      = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle   = element_text(size = 11, hjust = 0.5),
      panel.grid.minor= element_blank(),
      panel.grid.major.x = element_line(colour = "#DDDDDD"),
      panel.grid.major.y = element_line(colour = "#EEEEEE")
    )
}

write.csv(newdat1, file = "newdat1.csv", row.names = FALSE)

###----------------------------------------------------------------
# Regression models area charts START HERE (trendline + CI + scatter)
###----------------------------------------------------------------

## 11 ----> MODEL 1

## -------------------------------
## Model 1: is_urban ~ year
## Logistic regression plot
## -------------------------------

# i) Summarise data by decade for nicer scatter
decade_points <- all_art_model %>%
  group_by(decade) %>%
  summarise(
    prop_urban = mean(is_urban),
    n = n(),
    .groups = "drop"
  )

# ii). Prediction grid over years
newdat1 <- data.frame(
  year = seq(min(all_art_model$year), max(all_art_model$year), length.out = 300)
)

pred1 <- predict(
  model1,
  newdata = newdat1,
  type = "link",
  se.fit = TRUE
)

newdat1$prob_fit  <- plogis(pred1$fit)
newdat1$prob_low  <- plogis(pred1$fit - 1.96 * pred1$se.fit)
newdat1$prob_high <- plogis(pred1$fit + 1.96 * pred1$se.fit)

# iii) Plot: decade scatter + fitted curve + clear CI ribbon
p_model1 <- ggplot() +
  # Decade-level scatter (size = number of works)
  geom_point(
    data = decade_points,
    aes(x = decade, y = prop_urban, size = n),
    colour = cols_ecology["Met"],
    alpha  = 0.8
  ) +
  scale_size_continuous(name = "Number of works", range = c(1.5, 5)) +
  
  # CI ribbon
  geom_ribbon(
    data = newdat1,
    aes(x = year, ymin = prob_low, ymax = prob_high),
    fill  = cols_ecology["Water-based painting"],
    alpha = 0.35
  ) +
  
  # Fitted logistic curve
  geom_line(
    data = newdat1,
    aes(x = year, y = prob_fit),
    colour   = cols_ecology["Printmaking"],
    linewidth = 1
  ) +
  
  labs(
    title    = "Model 1: Time Predicts Urban Landscape Depictions",
    subtitle = "Decade-level share of urban works with logistic fit and 95% confidence interval",
    x        = "Year",
    y        = "Probability of Urban Depiction"
  ) +
  theme_pastel_ecology() +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

p_model1
# iv) saving plot
ggsave(
  "model1_regression_decade.png",
  p_model1,
  width = 10,
  height = 6,
  dpi = 600,
  bg = "white"
)


## 12 ---> MODEL 2

## -------------------------------
## Model 2: Urban probability by medium
## -------------------------------

# i) Summarise the observed data by medium (for scatter points)
medium_summary <- all_art_model %>%
  group_by(medium_group) %>%
  summarise(
    n          = n(),
    prop_urban = mean(is_urban),
    .groups    = "drop"
  )

medium_summary

# ii) Get model-predicted probabilities + 95% CI for each medium_group
newdat2 <- data.frame(
  medium_group = levels(all_art_model$medium_group)
)

pred2 <- predict(
  model2,
  newdata = newdat2,
  type    = "link",   # work on logit scale first
  se.fit  = TRUE
)

# Add predictions on probability scale
newdat2$prob_fit  <- plogis(pred2$fit)
newdat2$prob_low  <- plogis(pred2$fit - 1.96 * pred2$se.fit)
newdat2$prob_high <- plogis(pred2$fit + 1.96 * pred2$se.fit)

newdat2

# iii) Plot: observed proportions + model estimates + CI bars
p_model2 <- ggplot() +
  # Observed proportion by medium (one point per medium, size = n)
  geom_point(
    data = medium_summary,
    aes(
      x     = medium_group,
      y     = prop_urban,
      size  = n,
      color = medium_group
    ),
    alpha    = 0.7,
    shape    = 16,
    position = position_nudge(x = -0.12)
  ) +
  
  # Model-predicted probability + 95% CI as point + vertical error bar
  geom_pointrange(
    data = newdat2,
    aes(
      x    = medium_group,
      y    = prob_fit,
      ymin = prob_low,
      ymax = prob_high,
      color = medium_group
    ),
    fatten   = 1.2,
    linewidth = 0.7,
    position = position_nudge(x = 0.12)
  ) +
  
  #  pastel ecology palette for mediums
  scale_color_manual(
    values = cols_ecology,
    name   = "Medium"
  ) +
  scale_size_continuous(
    name  = "Number of works",
    range = c(3, 7)
  ) +
  
  labs(
    title    = "Model 2: Medium Predicts Urban Landscape Depictions",
    subtitle = "Observed share of urban works (left points) and model-estimated probabilities with 95% confidence intervals (right points)",
    x        = NULL,
    y        = "Probability of Urban Depiction"
  ) +
  theme_pastel_ecology() +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    axis.text.x      = element_text(angle = 25, hjust = 1),
    legend.position  = "right"
  )

p_model2

# vi) Save high-resolution version for the poster
ggsave(
  "model2_regression_medium.png",
  p_model2,
  width  = 10,
  height = 6,
  dpi    = 600,
  bg     = "white"
)


## 13 ----> MODEL 3

## MODEL 3 PLOT 
## Year + Medium + Phase predicting urban depiction
## -------------------------------

ref_medium <- "Printmaking"   # change if I want another medium

# i). Prediction grid (same as before)
year_grid <- seq(1820, 2010, by = 10)

newdata_m3 <- expand.grid(
  year         = year_grid,
  medium_group = ref_medium,
  phase        = levels(all_art_model$phase)
)

pred_m3 <- predict(
  model3,
  newdata = newdata_m3,
  type    = "link",
  se.fit  = TRUE
)

newdata_m3 <- newdata_m3 %>%
  mutate(
    eta       = pred_m3$fit,
    se_eta    = pred_m3$se.fit,
    prob      = plogis(eta),
    prob_low  = plogis(eta - 1.96 * se_eta),
    prob_high = plogis(eta + 1.96 * se_eta)
  )

# ii). Observed proportions by decade × phase for that medium
obs_m3 <- all_art_model %>%
  filter(medium_group == ref_medium) %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(phase, decade) %>%
  summarise(
    n          = n(),
    prop_urban = mean(is_urban),
    .groups    = "drop"
  )

# iii). Phase colours (pastel ecology)
phase_cols <- c(
  "Steam & Coal"              = "#7A9E7E",
  "Oil & Empire"              = "#F0A974",
  "Motor & Machine Vision"    = "#8FC6C2",
  "Plastic & Suburban Nature" = "#C5B5E6",
  "Carbon & Crisis"           = "#E08C7F"
)

# iv). Plot: observed points + model lines + CI ribbons
p_model3_clean <- ggplot() +
  # CI ribbons (model)
  geom_ribbon(
    data = newdata_m3,
    aes(
      x    = year,
      ymin = prob_low,
      ymax = prob_high,
      fill = phase
    ),
    alpha  = 0.15,
    colour = NA
  ) +
  # Fitted lines (model)
  geom_line(
    data = newdata_m3,
    aes(
      x      = year,
      y      = prob,
      colour = phase
    ),
    linewidth = 1
  ) +
  # Observed proportions (points)
  geom_point(
    data = obs_m3,
    aes(
      x      = decade,
      y      = prop_urban,
      colour = phase,
      size   = n
    ),
    alpha = 0.9
  ) +
  
  scale_colour_manual(values = phase_cols, name = "Ecological phase") +
  scale_fill_manual(values   = phase_cols, guide = "none") +
  scale_size_continuous(
    name = "Number of works",
    range = c(2, 6)
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 5),
    limits = c(0, 1),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_x_continuous(
    breaks       = seq(1820, 2010, by = 40),
    minor_breaks = seq(1820, 2010, by = 20)
  ) +
  
  labs(
    title    = "Model 3: Time, Phase, and Medium (Printmaking)",
    subtitle = paste(
      "Points = observed share of urban landscapes by decade and phase;",
      "lines & ribbons = model-estimated probabilities ± 95% CI."
    ),
    x = "Year",
    y = "Probability of Urban Depiction"
  ) +
  theme_pastel_ecology()

p_model3_clean

# v) saving plot
ggsave(
  "model3_time_phase_printmaking.png",
  p_model3_clean,
  width  = 14,
  height = 7,
  dpi    = 600,
  bg     = "white"
)

## 14 -----> MODEL 4

# MODEL 4 PLOT
# Time + Medium + Phase + Museum
## -------------------------------

# choosing a reference medium to visualize
ref_medium <- "Printmaking"   # I can change if I want plot for another medium

# --- i). Prediction grid across year, phase, and museum ----
year_grid <- seq(1820, 2010, by = 10)

newdata_m4 <- expand.grid(
  year         = year_grid,
  medium_group = ref_medium,
  phase        = levels(all_art_model$phase),
  museum       = levels(all_art_model$museum)
)

pred_m4 <- predict(
  model4,
  newdata = newdata_m4,
  type    = "link",   # log-odds
  se.fit  = TRUE
)

newdata_m4 <- newdata_m4 %>%
  mutate(
    eta       = pred_m4$fit,
    se_eta    = pred_m4$se.fit,
    prob      = plogis(eta),
    prob_low  = plogis(eta - 1.96 * se_eta),
    prob_high = plogis(eta + 1.96 * se_eta)
  )

# --- ii). Observed proportions by decade × phase × museum ----
obs_m4 <- all_art_model %>%
  filter(medium_group == ref_medium) %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(museum, phase, decade) %>%
  summarise(
    n          = n(),
    prop_urban = mean(is_urban),
    .groups    = "drop"
  )

# --- iii). Phase colours (reuse from earlier or define here) ----
phase_cols <- c(
  "Steam & Coal"              = "#7A9E7E",
  "Oil & Empire"              = "#F0A974",
  "Motor & Machine Vision"    = "#8FC6C2",
  "Plastic & Suburban Nature" = "#C5B5E6",
  "Carbon & Crisis"           = "#E08C7F"
)

# --- iv). Plot: observed points + model lines + CI ribbons ----
p_model4 <- ggplot() +
  # CI ribbons (model)
  geom_ribbon(
    data = newdata_m4,
    aes(
      x    = year,
      ymin = prob_low,
      ymax = prob_high,
      fill = phase
    ),
    alpha  = 0.15,
    colour = NA
  ) +
  # Fitted lines (model)
  geom_line(
    data = newdata_m4,
    aes(
      x      = year,
      y      = prob,
      colour = phase
    ),
    linewidth = 1
  ) +
  # Observed proportions (points)
  geom_point(
    data = obs_m4,
    aes(
      x      = decade,
      y      = prop_urban,
      colour = phase,
      size   = n
    ),
    alpha = 0.9
  ) +
  
  scale_colour_manual(values = phase_cols, name = "Ecological phase") +
  scale_fill_manual(values   = phase_cols, guide = "none") +
  scale_size_continuous(
    name  = "Number of works",
    range = c(2, 6)
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 5),
    limits = c(0, 1),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_x_continuous(
    breaks       = seq(1820, 2010, by = 40),
    minor_breaks = seq(1820, 2010, by = 20)
  ) +
  
  facet_wrap(~ museum, nrow = 2) +
  
  labs(
    title = paste0(
      "Model 4: Time, Phase, Medium (", ref_medium, "), and Museum"
    ),
    subtitle = paste(
      "Points = observed urban share by decade and phase; size = number of works.",
      "Lines & ribbons = model-estimated probabilities with 95% confidence intervals.",
      sep = " "
    ),
    x = "Year",
    y = "Probability of Urban Depiction"
  ) +
  theme_pastel_ecology()

p_model4

# --- v). Save high-res for poster ----
ggsave(
  "model4_time_phase_medium_museum.png",
  p_model4,
  width  = 14,
  height = 8,
  dpi    = 600,
  bg     = "white"
)


###----------------------------------------------------------------
# Regression models area charts END HERE 
###----------------------------------------------------------------


## 15 -----> Wilderness Score Line Graph with Phases shaded in BG

## i). Decade-level wildness index (Met + Tate together) -----------------
wild_decade <- all_art_model %>%          # uses only wild + urban works
  group_by(decade) %>%
  summarise(
    n_decade   = n(),
    prop_wild  = mean(1 - is_urban),      # share of works that are wild
    .groups = "drop"
  )

## ii). Ecological phase bands (same breaks you used elsewhere) ----------
phase_bounds <- tibble(
  phase = c(
    "Steam & Coal",
    "Oil & Empire",
    "Motor & Machine Vision",
    "Plastic & Suburban Nature",
    "Carbon & Crisis"
  ),
  start = c(1820, 1870, 1920, 1960, 1990),
  end   = c(1870, 1920, 1960, 1990, 2010)
)

# soft pastel fills for the background bands
phase_cols <- c(
  "Steam & Coal"              = soft_ecology[4],   # E3D5CA
  "Oil & Empire"              = soft_ecology[2],   # C6D9C8
  "Motor & Machine Vision"    = soft_ecology[13],  # FFCDB2
  "Plastic & Suburban Nature" = soft_ecology[8],   # 748CAB
  "Carbon & Crisis"           = soft_ecology[11]   # E5989B
)


## iii). Wildness Index plot ----------------------------------------------
wild_index_plot <- ggplot() +
  # phase background (full-height rectangles)
  geom_rect(
    data = phase_bounds,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = phase
    ),
    inherit.aes = FALSE,
    alpha = 0.35
  ) +
  
  # observed wild share by decade (thin line + small points)
  geom_line(
    data = wild_decade,
    aes(x = decade, y = prop_wild, colour = "Observed wild share"),
    linewidth = 0.7
  ) +
  geom_point(
    data = wild_decade,
    aes(x = decade, y = prop_wild, colour = "Observed wild share"),
    size = 1
  ) +
  
  # smoothed trend (dashed)
  geom_smooth(
    data = wild_decade,
    aes(x = decade, y = prop_wild, colour = "Smoothed trend"),
    method = "loess",
    se = FALSE,
    linetype = "dashed",
    linewidth = 0.7,
    span = 0.6
  ) +
  
  scale_fill_manual(
    name   = "Ecological phase",
    values = phase_cols
  ) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Observed wild share" = "#4C8C90",  # teal
      "Smoothed trend"      = "#E08C7F"   # warm coral
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(1820, 2010, by = 20),
    minor_breaks = seq(1820, 2010, by = 10)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 5)
  ) +
  
  labs(
    title    = "Wildness Index in Museum Landscapes",
    subtitle = "Share of wild nature depictions by decade, across Met and Tate.\nBackground bands show ecological phases.",
    x        = "Decade",
    y        = "Proportion wild"
  ) +
  theme_pastel_ecology() +
  theme(
    legend.position   = "right",
    panel.grid.minor.x = element_blank()
  )

wild_index_plot

## vi). Save high-res version --------------------------------------------
ggsave(
  "wildness_index_phases.png",
  wild_index_plot,
  width  = 8,
  height = 6,
  dpi    = 600
)


## 16 -----> Heatmap (Phases + Medium + Depiction Type:wild/urban)

## i). Summarise: share of URBAN within each medium × phase cell ---------
heat_df <- all_art_model %>%          # uses only wild + urban works
  group_by(phase, medium_group) %>%
  summarise(
    n_cell   = n(),
    n_urban  = sum(is_urban),
    prop_urb = n_urban / n_cell,      # 0 = all wild, 1 = all urban
    .groups  = "drop"
  )

## ii). Make shorter, wrapped labels for mediums ---------------------------
medium_labels <- c(
  "Digital/Media"             = "Digital\nmedia",
  "Photography"               = "Photo-\ngraphy",
  "Sculpture"                 = "Sculp-\nture",
  "Other"                     = "Other",
  "Oil painting"              = "Oil\npainting",
  "Printmaking"               = "Print-\nmaking",
  "Drawing / Works on paper"  = "Drawing /\nWorks on paper",
  "Water-based painting"      = "Water-\nbased\npainting"
)

heat_df <- heat_df %>%
  mutate(
    medium_pretty = recode(as.character(medium_group), !!!medium_labels),
    # keep mediums in a sensible order (left → right)
    medium_pretty = factor(medium_pretty, levels = medium_labels)
  )

## iii). Choose two colours from soft_ecology for the gradient --------------
# low  = mostly wild, high = mostly urban
wild_col  <- soft_ecology[2]   # pale greenish
urban_col <- soft_ecology[11]  # warm blossom

## iv). Heatmap ------------------------------------------------------------
heatmap_medium_phase <- ggplot(
  heat_df,
  aes(x = medium_pretty, y = phase, fill = prop_urb)
) +
  geom_tile(colour = "white", linewidth = 0.5) +
  
  scale_fill_gradient(
    name   = "Share urban\n(0 = all wild)",
    low    = wild_col,
    high   = urban_col,
    limits = c(0, 1),
    labels = percent_format(accuracy = 0.1)
  ) +
  
  labs(
    title    = "Wild vs Urban Landscapes by Medium and Ecological Phase",
    subtitle = "Each tile shows the share of URBAN depictions within a medium–phase cell\n(Met and Tate combined).",
    x        = "Medium",
    y        = "Ecological phase"
  ) +
  theme_pastel_ecology() +
  theme(
    panel.grid       = element_blank(),
    axis.text.x      = element_text(size = 8, lineheight = 0.9),
    axis.text.y      = element_text(size = 10),
    axis.title.x     = element_text(margin = margin(t = 8)),
    axis.title.y     = element_text(margin = margin(r = 8)),
    legend.position  = "right"
  )

heatmap_medium_phase

## v). Save for the poster -------------------------------------------------
ggsave(
  "heatmap_medium_phaseV2.png",
  heatmap_medium_phase,
  width  = 8,
  height = 5.5,
  dpi    = 600
)


## 17 -----> Heatmap all data wrapped, more gradient different

## i). Summarise: share of URBAN within each medium × phase cell -----------
heat_df <- all_art_model %>%          # uses only wild + urban works
  group_by(phase, medium_group) %>%
  summarise(
    n_cell   = n(),
    n_urban  = sum(is_urban),
    prop_urb = n_urban / n_cell,      # 0 = all wild, 1 = all urban
    .groups  = "drop"
  )

## ii). Wrapped labels for mediums (x) --------------------------------------
medium_labels <- c(
  "Digital/Media"             = "Digital\nmedia",
  "Photography"               = "Photo-\ngraphy",
  "Sculpture"                 = "Sculp-\nture",
  "Other"                     = "Other",
  "Oil painting"              = "Oil\npainting",
  "Printmaking"               = "Print-\nmaking",
  "Drawing / Works on paper"  = "Drawing /\nWorks on paper",
  "Water-based painting"      = "Water-\nbased\npainting"
)

## Wrapped labels for phases (y) ------------------------------------------
phase_labels <- c(
  "Steam & Coal"              = "Steam &\nCoal",
  "Oil & Empire"              = "Oil &\nEmpire",
  "Motor & Machine Vision"    = "Motor &\nMachine\nVision",
  "Plastic & Suburban Nature" = "Plastic &\nSuburban\nNature",
  "Carbon & Crisis"           = "Carbon &\nCrisis"
)

heat_df <- heat_df %>%
  mutate(
    medium_pretty = recode(as.character(medium_group), !!!medium_labels),
    medium_pretty = factor(medium_pretty, levels = medium_labels),
    phase_pretty  = recode(as.character(phase), !!!phase_labels),
    phase_pretty  = factor(phase_pretty, levels = phase_labels)
  )

## iii). Choose 2 contrasting colours from soft_ecology ----------------------
# low  = mostly wild, high = mostly urban
wild_col <- soft_ecology[1]   # 0%  (mostly wild) (green)
mid_col  <- "#F0F0F0"         # 50% very light grey
urban_col <- soft_ecology[11] # 100% (mostly urban) (warm pink)

## iv). Heatmap -------------------------------------------------------------
heatmap_medium_phase <- ggplot(
  heat_df,
  aes(x = medium_pretty, y = phase_pretty, fill = prop_urb)
) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    name   = "Share urban\n(0 = all wild)",
    colours = c(wild_col, mid_col, urban_col),
    values  = c(0, 0.5, 1),        # 0% -> 50% -> 100%
    limits  = c(0, 1),
    labels  = scales::percent_format(accuracy = 1)
  ) +
  
  labs(
    title    = "Wild vs Urban Landscapes by Medium and Ecological Phase",
    subtitle = "Each tile shows the share of URBAN depictions within a medium–phase cell (Met and Tate combined).",
    x        = "Medium",
    y        = "Ecological phase"
  ) +
  theme_pastel_ecology() +
  theme(
    panel.grid       = element_blank(),
    axis.text.x      = element_text(size = 8, lineheight = 0.9),
    axis.text.y      = element_text(size = 8, lineheight = 0.9),
    axis.title.x     = element_text(margin = margin(t = 8)),
    axis.title.y     = element_text(margin = margin(r = 8)),
    legend.position  = "right"
  )

heatmap_medium_phase

## v). Save with a less “long” aspect ratio (tiles thinner horizontally) ---
ggsave(
  "heatmap_medium_phaseContrast.png",
  heatmap_medium_phase,
  width  = 7.25,    # narrower → less stretched horizontally
  height = 5.5,
  dpi    = 600
)
























