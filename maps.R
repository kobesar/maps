library(tidyverse)
library(ggrepel)
library(ggmap)

colors <- str_split("#003f5c
#2f4b7c
#665191
#a05195
#d45087
#f95d6a
#ff7c43
#ffa600", "\n")[[1]]

colors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00')

data_raw <- read.csv("/Users/kobesarausad/Downloads/DECENNIALPL2020.P1_2023-05-05T031730/DECENNIALPL2020.P1-Data.csv")

election2020 <- read.csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv") %>% 
  mutate(win = ifelse(votes_gop > votes_dem, "gop", "dem"))

scale_custom <- function(x) {
  mu <- mean(x)
  sd <- sd(x)
  
  if (mu == 0) {
    return(x)
  } else {
    return((x-mu)/sd)
  }
}

# Select all estimate columns
data <- data_raw

data_cols <- data[1,]

data <- data %>% 
  tail(-1)

# Convert to numeric
data[,3:ncol(data)] <- lapply(data[,3:ncol(data)], as.numeric)

data <- data %>% 
  select(!ends_with("NA"))

data[is.na(data)] <- 0

data <- data %>% 
  mutate_at(4:74, funs(. / data$P1_001N))

data_prop <- data

data_num <- data[-1,3:ncol(data)] %>% 
  mutate_all(scale_custom)

kmeans(data_num, 2)

withins <- c()

for (k in 2:15) {
  mod <- kmeans(data_num, k)
  
  withins[k] <- mod$tot.withinss
}

ggplot() +
  geom_line(aes(x = 1:15, y = withins))

final_mod <- kmeans(data_num, 10)

data_svd <- svd(data_num)

data_final <- data.frame(data[-1,1:2], cluster = final_mod$cluster, x = data_svd$u[,1], y = data_svd$u[,2], data_prop[-1,-c(1:2)])

data_final %>%
  ggplot() +
  geom_point(aes(x = x, y = y, color = as.factor(cluster))) 
  # geom_text_repel(aes(x = x, y = y, label = ifelse(str_detect(NAME, ", Washington"), NAME, "")))
  # geom_label_repel(aes(x = x, y = y, label = ifelse(cluster == 5, NAME, "")), max.overlaps = Inf)

# Make map

data_final$FIPS <- as.numeric(str_sub(data_final[,1], 10, 14))

maps::county.fips %>%
  as.tibble %>% 
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") ->
  dfips

map_data("county") %>% 
  left_join(dfips) ->
  dall

dall %>% 
  left_join(data_final, by = c("fips" = "FIPS")) %>% 
  left_join(election2020[,c(2,4:11)], by = c("fips" = "county_fips")) ->
  dall

dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=as.factor(cluster)), color="gray70") +
  coord_map() +
  theme_nothing() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_manual(values = colors)

dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=as.factor(win)), color="gray70") +
  coord_map() +
  theme_nothing() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("#0000ff", "#E81B23"))

dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=per_gop), color="gray90") +
  coord_map() +
  theme_nothing() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_gradient(low = "#f1f1f1", high = "#ff0803")

dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=per_dem), color="gray90") +
  coord_map() +
  theme_nothing() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_gradient(low = "#f1f1f1", high = "#0063b4")

dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=per_dem), color="gray90") +
  coord_map() +
  theme_nothing() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_gradient(low = "#ff0803", high = "#0063b4")

dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=P1_003N), color="gray90") +
  coord_map() +
  theme_nothing() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c")

dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=P1_004N), color="gray90") +
  coord_map() +
  theme_nothing() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c")

dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=P1_005N), color="gray90") +
  coord_map() +
  theme_nothing() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c")

dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=P1_006N), color="gray90") +
  coord_map() +
  theme_nothing() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c")

dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=P1_007N), color="gray90") +
  coord_map() +
  theme_nothing() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c")


dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=P1_013N), color="gray90") +
  coord_map() +
  theme_nothing() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c")
