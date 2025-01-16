library(tidyverse) # manage data
library(DT) # visualise tables in html
library(viridis) # use color-blind friendly palettes
library(ggrepel) # avoid overlapping texts in the figures
library(codyn) # calculate community dynamics metrics
library(vegan) # calculate community ecology metrics
library(eulerr) # calculate Venn and Euler diagrams


theme_set(theme_minimal()+
            theme(axis.title = element_text(size = 15),
                  axis.line = element_line(color = "black",
                                           linewidth = 0.5),
                  panel.grid.major = element_blank(),
                  axis.text = element_text(color = "black", size = 12),
                  strip.text.x = element_text(size = 12),
                  axis.ticks = element_line(color = "black")))


ants_raw <- read.csv("data/ants_logged.csv")


# Database structure

ants_raw %>% 
  distinct(block, plot, treatment) # extract the different combinations of block, plot and treatment


ants_raw %>% 
  summarise(n_species = n_distinct(code), # number of different codes (i.e. species)
            n_years = n_distinct(year)) # number of different years

ants_raw %>% 
  distinct(year, date) %>% # extract the different combinations of year and date
  DT::datatable() # a function to improve the visualization of the table in the html output


# NA values

anyNA(ants_raw) # is there any missing value? (Boolean answer)


# how are NAs distributed? And what might NAs exactly mean, in this data set?
# let's see it by year, for example
ants_raw %>% 
  mutate(cell_content = case_when(is.na(abundance) ~ "NA",
                                  abundance == 0 ~ "0",
                                  T ~ ">0")) %>%
  # we create a new variable capturing cell content
  # as we are interested in defining 3 different situations, we use the case_when function
  ggplot(aes(x = year)) +
  geom_histogram(aes(fill = cell_content),
                 binwidth = .5, center = 0) +
  scale_fill_discrete(drop = F) +
  labs(y = "Number of rows")


ants_raw %>% 
  filter(year == 2007) %>% 
  DT::datatable()


# and let's see it by year x species too
ants_raw %>% 
  mutate(cell_content = case_when(is.na(abundance) ~ "NA",
                                  abundance == 0 ~ "0",
                                  T ~ ">0")) %>% 
  ggplot(aes(x = year, y = code, fill = cell_content)) +
  geom_tile(color = "white") +
  # scale_x_continuous(breaks = seq(from = 1970, to = 2015, by = 5)) +
  labs(y = "species code") +
  theme(axis.text.y = element_text(size = 8, face = "italic"))


# Adapt the longer data set to a yearly resolution (not date)
ants_long <- ants_raw %>%
  select(year, genus, species, code, abundance) %>% # the variables we will work with
  group_by(year, genus, species, code) %>% 
  summarise(total_abundance = sum(abundance)) %>% # calculate the total abundance per year and species
  ungroup()


ants_wide <- ants_long %>%
  pivot_wider(id_cols = year,
              names_from = c(genus, species),
              values_from = total_abundance,
              values_fill = list(total_abundance = 0),
              names_sep = " ") %>% 
  column_to_rownames(var = "year")


dim(ants_wide)  


ants_long %>% 
  group_by(code) %>% 
  mutate(n_sp = n_distinct(paste(genus, species))) %>% 
  filter(n_sp > 1) %>% # keep those rows with more than one species name per code
  distinct(genus, species, code)


ants_long <- ants_long %>% 
  mutate(species = if_else(species == "lognispinosus", # if the specific epithet in that row is wrong
                           "longispinosus", # change it by the correct one
                           species)) # but conserve the specific epithet of the other rows

ants_wide <- ants_long %>%
  pivot_wider(id_cols = year,
              names_from = c(genus, species),
              values_from = total_abundance,
              values_fill = list(total_abundance = 0),
              names_sep = " ") %>% 
  column_to_rownames(var = "year")

dim(ants_wide)


ants_long %>% 
  group_by(year) %>% 
  summarise(n_species = n_distinct(code)) %>% # count the number of different species by year
  ggplot(aes(x = year, y = n_species)) +
  geom_line() +
  geom_point(size = 3) +
  geom_smooth(se = F) +
  #making the plot fancier:
  geom_text(aes(label = n_species), nudge_y = 1) + # adding the number of species as a label
  geom_vline(aes(xintercept = 2005, color = "logging"), linetype = "dashed") + # indicate logging year
  guides(color = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2)) +
  # a sequence taking the limitis of the axis
  labs(y = "richness")


ants_long %>% 
  group_by(year) %>% 
  summarise(total_abundance = sum(total_abundance)) %>% 
  ggplot(aes(x = year, y = total_abundance)) +
  geom_line() + geom_point(size = 3) +
  geom_smooth(se = F) +
  geom_vline(aes(xintercept = 2005, color = "logging"), linetype = "dashed") + # indicate logging year
  guides(color = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2)) +
  labs(y = "Number of ants\nacross all species")


ants_long %>%
  group_by(genus, species) %>% 
  summarise(total_abundance = sum(total_abundance,
                                  na.rm = T)) %>% 
  ggplot(aes(x = reorder(paste(genus, species),
                         -total_abundance), # x axis are complete species names ordered by their abundance
             y = total_abundance)) + 
  geom_col(color = "black", fill = "grey") +
  labs(y = "Number of ants\nacross all years") +
  theme(axis.text.x = element_text(face = "italic",
                                   angle = 90,
                                   vjust = .25,
                                   hjust = 1),
        axis.title.x = element_blank()) 


# let's have first the order we obtained in the overall RAD
ordered_ab <- ants_long %>%
  group_by(code) %>% 
  summarise(total_abundance = sum(total_abundance, na.rm = T)) %>% 
  arrange(desc(total_abundance))

# and apply this overall order to yearly RADs
ants_long %>% 
  mutate(code = factor(code,levels = ordered_ab$code)) %>%
  split(.$year) %>% 
  map2(names(.),
#  map(
    ~ ggplot(data = .x, aes(x = code, y = total_abundance)) + 
      geom_col(color = "black", fill = "grey") +
      scale_x_discrete(drop = F) +
      ylim(0, 130) +
      theme(axis.text.x = element_text(face = "italic", angle = 90, vjust = .25, hjust = 1,
                                       size = 8),
            strip.text = element_text(size = ),
            axis.title.x = element_blank(),
            panel.border = element_rect(fill = NA))
#  )
 + labs(title = .y))


ants_turnover <- ants_long %>% 
  mutate(total_abundance = 1) %>% # to include 2007 in the turnover analysis
  codyn::turnover(time.var = "year",
                  species.var = "code",
                  abundance.var = "total_abundance")


ants_turnover %>% 
  ggplot(aes(x = year, y = total)) +
  geom_point() + geom_line() +
  geom_vline(aes(xintercept = 2005), linetype = "dashed", color = "black") +
  scale_color_viridis_d(direction = -1) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) + 
  labs(title = "Community turnover relative to the preceding sample",
       x = "Year",
       y = "Turnover") +
  ylim(0, 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))


ants_cols <- ants_long %>% 
  mutate(total_abundance = 1) %>% 
  turnover(time.var = "year",
           species.var = "code",
           abundance.var = "total_abundance",
           metric = "appearance")

ants_exts <- ants_long %>% 
  mutate(total_abundance = 1) %>% 
  turnover(time.var = "year",
           species.var = "code",
           abundance.var = "total_abundance",
           metric = "disappearance")


ants_cols %>% 
  left_join(ants_exts) %>% 
  pivot_longer(cols = -year,
               names_to = "metric",
               values_to = "rate") %>%
  ggplot(aes(x = year, y = rate)) +
  geom_col(aes(fill = metric)) +
  geom_point(data = ants_turnover, aes(y = total)) +
  geom_line(data = ants_turnover, aes(y = total)) +
  geom_vline(aes(xintercept = 2005), linetype = "dashed", color = "black") +
  scale_fill_viridis_d(begin = .5) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) + 
  ylim(0,1) + 
  labs(title = "Community turnover relative to the preceding sample",
       x = "Year",
       y = "Turnover") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))


ants_long %>% 
  filter(year %in% c(2003, 2018)) %>% # keep the years of interest
  split(.$year) %>% # create a list with a dataframe per year
  purrr::map(select, code) %>% # for each dataframe, select the code variable
  purrr::map(unlist) %>% # create a vector from the selected variable
  eulerr::venn() %>% # calculate the parameters for the euler diagram
  plot(quantities = T) # and plot it


(pcoa_hell <- ants_wide %>% 
    na.omit() %>% 
    vegan::vegdist(method = "hellinger") %>%
    # this is a common metric in PCoA (it gives the same weight to all species when the total number of individuals is the same between samples)
    cmdscale(eig = T))


# The variation explained by each axis is its eigenvalue divided by the sum of the positive eigenvalues.
var_exp <- pcoa_hell$eig[1:2]/sum(pcoa_hell$eig[pcoa_hell$eig > 0])

pcoa_years <- pcoa_hell$points %>% 
  as.data.frame()

pcoa_species <- cor(na.omit(ants_wide), pcoa_years) %>% 
  as.data.frame()

# Visualisation in the ordination diagram
ggplot() +
  geom_segment(data = pcoa_species %>% 
                 rownames_to_column(var = "sp"),
               aes(x = 0, y = 0, xend = V1, yend = V2),
               color = "grey",
               arrow = arrow()) +
  geom_text_repel(data = pcoa_species %>% 
                    rownames_to_column(var = "sp"),
                  aes(x = V1, y = V2, label = sp),
                  color = "grey",
                  max.overlaps = 30) +
  geom_point(data = pcoa_years %>% 
               rownames_to_column(var = "year"),
             aes(x = V1, y = V2),
             size = 2) +
  geom_text_repel(data = pcoa_years %>% 
                    rownames_to_column(var = "year"),
                  aes(x = V1, y = V2, label = year),
                  max.overlaps = 13) +
  geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed") +
  labs(title = "PCoA using Hellinger distance",
       subtitle = paste0("Variance explained = ",
                         round(sum(var_exp)*100), "%"),
       x = paste0("PCo1 (", round(var_exp[1]*100), "% var)"),
       y = paste0("PCo2 (", round(var_exp[2]*100), "% var)"))






hell_vector <- ants_wide %>% 
  na.omit() %>% 
  vegdist(method = "hellinger") %>% 
  as.numeric()

lag_vector <- ants_wide %>% 
  na.omit() %>% 
  rownames() %>% 
  as.numeric() %>% 
  vegdist(method = "euclidean") %>% 
  as.numeric()

lag_df <- data.frame(hell_dist = hell_vector,
                     time_lag = lag_vector)

linear <- glm(hell_dist ~ poly(time_lag, 2), data = lag_df)
linear <- glm(hell_dist ~ time_lag, data = lag_df)
summary(linear)

lag_df %>% 
  ggplot(aes(x = time_lag, y = hell_dist)) +
  geom_point(alpha = .5, size = 4) +
  geom_smooth(method = "lm", se = F, linewidth = 1, color = "black") + # fit a regression line
  geom_smooth(method = "glm", formula = y ~ x + I(x^2), se = F, linewidth = 1) + # fit a regression line
  scale_x_continuous(breaks = seq(2, 12, by = 2)) +
  labs(x = "Time lag (years)",
       y = "Hellinger distance")

















