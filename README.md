# Context

The arms production industry has long been dominated by the United States, European Union, Russia, and, more recently, China.

This project utilizes the Stockholm International Peace Research Institute (SIPRI) database to analyze key metrics of the global arms trade. It compares the performance of the US, EU, Russia, and China from 1995 to 2020, providing historical data for context.

<b>About the Arms Trade Data:</b>
1. [How has the trade volume evolved over time?](https://github.com/lucacasu/Global-Arms-Trade-EDA?tab=readme-ov-file#how-has-trade-volume-changed)
2. What is the value of the assets being traded?
3. How has the value of these items changed?
4. How have different categories ranked in each decade?

<b>About the Competition:</b>
1. Have suppliers expanded their spheres of influence?
2. Who are the most frequent buyers for each supplier?
3. How have market shares shifted?
4. How dependent is each country on Western or Eastern suppliers?

<details>
<summary>Long context</summary>
The late 20th century was a period of turbulent transition. The end of the imminent mutual nuclear destruction threat, marked by the dissolution of the Soviet Union and the conclusion of the Cold War, leveraged an era of renewed international cooperation effort.

In 1993, American President Bill Clinton and Russian President Boris Yeltsin agreed on negotiating a comprehensive test ban on nuclear weapons, reaffirming the commitment to the 1968 Non-Proliferation Treaty (NPT). The year had seen the lowest number of nuclear tests while international politics researchers elaborated over a future without the preceding bipolar world order.

The concept of self-determination re-emerged, driving the reorganization of alliances and shifting the balance of power. A time of relative peace allowed previously constrained countries to focus on social and political advancement as democracies capitalized on the technological progress and inter-state relations developed during the past decades. The European Union was officially created in 1995, seeing further expansion in the following decade. Western and Eastern economies became intertwined by the trade of critical resources and goods while countries like China, South Korea, Turkey, United Arab Emirates, Brazil and India underwent significant modernization and assumed pronounced positions in global trade.

Most of the major armed conflicts in the following decades were internal territorial disputes by indigenous groups in the Middle-East and Africa, added to localized conflicts in the post-soviet countries. Inter-state hostilities were mostly limited to political rhetorics and commercial disputes.

Nevertheless, instability continued to be the norm in international politics. With improved economies, military power imbalance was gradually reduced as nations raced to achieve regional parity and establish deterrence to future aggression. Players in a free market, countries engaged in fierce competition for either supply or demand of military resources, offering a rnewed path to the international arms trade.

For importers, the cost of security is beyond financial. Supplier countries may be legally bound by international treaties to the consequences of the use of such weapons by the receiving country, including violations of human rights or humanitarian law. In most cases, the transfers come as a result, or predecessor, of significant inter-state cooperation, effectively restricting the trade possibilities to the supplier’s antagonists. Beyond the political implications, the cost of operating a diverse set of weapons from different countries is a significant long-term financial burden for smaller nations.

For exporters, the sale of weapons is a major diplomatic soft power, opening or solidifying partnerships that may now or in the future guarantee their own geopolitical interests. The privileged access to critical natural and technological resources, such as oil, metals or microprocessors, along with the control of buffer zones that set them apart from future direct confrontation are some of the key benefits of the arms trade.

The United States, European Union, Russia, and more recently China have dominated the arms production sector. The runner-up, while China is mainly focused on its own modernization, specialists suggest its military budget is far higher than what it officially reports, emphasizing the investments in research and development programs dedicated to achieve parity to its American counterpart.

The United States and European Union intensely cooperated throughout the decades, leveraging state-of-the-art projects such as the Joint Strike Fighter program, leading to the fifth-generation F-35 fighter. The North Atlantic Treaty Organization (NATO), initially founded to defend against the Soviet threat, expanded to most of Eastern Europe. Despite the standards governing the issuing arms trade licenses evolving along the past decades, both suppliers are linked to major violations of human rights through the use of its assets.

Russia leveraged the well-established Soviet-era military complex and its vast stockpiles of machinery and ordinance to continue influencing nations in Africa and the Middle-East, where low-tech conflicts are mostly impacted by its supplies. Despite its comparatively limited technological advancements, Russia still delivered significant achievements. In partnership with India, the leading buyer of conventional arms, it developed the medium-range ramjet supersonic cruise missile BrahMos, capable of flying at Mach 3.5 and overcoming most modern air defence systems.

Using the Stockholm International Peace Research Institute (SIPRI) database, **this project analyzes key metrics of the global arms trade between 1995 and 2020**, with historical data for context. I also analyse how the leading global arms suppliers leverage exports as a foreign policy tool.

The European Union (EU) member states list evolves along the 26-year series, according to the respective year of ascension of each member.
</details>

# The data

The Stockholm International Peace Research Institute (SIPRI) database is the most comprehensive resource for analyzing  imports and exports of conventional weapons. It is maintained by an independent institute dedicated to the study of international conflicts, armaments, arms control and disarmament policies.

**The key metric in the Arms Transfers database is the Total Indicator Value (TIV)**. It overcomes challenges imposed by currency and market value of assets by assigning a standardized value to military equipment based on its type, capability and estimated production cost. Thus, it is not a direct representation of financial value but instead a measure of military capability.

<details>
<summary><b>[CODE] Import packages and datasets</b></summary>
  
```r
library(tidyverse)
library(skimr)
library(knitr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(ggrepel)
library(gganimate)
library(reshape2)
library(plotly)
library(ggtern)
```

```r
f <- read.csv("trade.csv", skip = 11)
trade <- as_tibble(f)

f2 <- read.csv("country_region.csv")
region_map <- as_tibble(f2)
```
</details>

## Inspect and pre-process

The data contains the names of supplier and receiver countries, but has no indication of their region or economic blocks. This information in fundamental throughout this analysis. As explained further in the next sections, this analysis will mainly cover the supply from United States, European Union, Russia and China.

The following code maps a "EU" label to European countries according to their date of ascension to the block. Lists for each milestone year (1994, 1995, 2004, 2007, 2013) are set with the respective members.

Labels for United States (US), Russia (RU), and China (CN) are assigned based on their names. Remaining countries are labeled OTHER.

<details>
<summary><b>[CODE] Clean columns</b></summary>

```r
trade <- janitor::clean_names(trade)

trade <- select(trade, order_date, supplier, recipient, numbers_delivered,
                armament_category, description, status, sipri_estimate,
                tiv_deal_unit, tiv_delivery_values)

# Shorten column names
trade <- trade |>
  rename(
    order_year = order_date,
    arm_cat = armament_category,
    tiv_unit = tiv_deal_unit,
    tiv_delivery = tiv_delivery_values,
    items = numbers_delivered
  )

# Factors
trade$supplier <- factor(trade$supplier, ordered = FALSE)
trade$recipient <- factor(trade$recipient, ordered = FALSE)
trade$arm_cat <- factor(trade$arm_cat, ordered = FALSE)
```
</details>

While inspecting the data I noted that tiv_unit is rounded to two decimal places, which caused some items to be valued at 0 TIV when they were transferred in used condition. In such cases, the TIV value is multiplied by 0.4. I fixed this rounding omission by recalculating the unit value with the sipri_estimate * 0.4 where tiv_unit was 0.

<details>
<summary><b>[CODE] Fix rounding omission</b></summary>
  
```r
# Fixing rounding omission

trade$tiv_unit <- case_when(
  trade$tiv_unit == 0 ~ trade$sipri_estimate * 0.4,
  TRUE ~ trade$tiv_unit
)
```
</details>

<details>
<summary><b>[CODE] Map EU countries by ascension date</b></summary>

```r
# EU member list by year
eu_list_94 = c(
  "Belgium", 
  "Denmark", "France", "Germany", "Greece", 
  "Ireland", "Italy", "Luxembourg", 
  "Netherlands", "Portugal", 
  "Spain", "United Kingdom"
)

eu_list_95 = c(
  "Austria", "Belgium", 
  "Denmark", "Finland", "France", "Germany", "Greece", 
  "Ireland", "Italy", "Luxembourg", 
  "Netherlands", "Portugal", 
  "Spain", "Sweden", "United Kingdom"
)

eu_list_2004 = c(
  "Austria", "Belgium", "Cyprus", "Czechia", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
  "Malta", "Netherlands", "Poland", "Portugal", "Slovakia", 
  "Slovenia", "Spain", "Sweden", "United Kingdom"
)

eu_list_2007 = c(
  "Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", 
  "Slovenia", "Spain", "Sweden", "United Kingdom"
)

eu_list_2013 = c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", 
  "Slovenia", "Spain", "Sweden", "United Kingdom"
)


# Fix column names in region_map file
region_map <- region_map |>
  rename(
    recipient = COUNTRY,
    region = REGION
  )

# Join region name to supplier name
trade <- trade |>
  left_join(region_map, by = "recipient") |> 
  rename(rec_region =  region) |>
  relocate(rec_region, .after = order_year)



group_order = c("US", "EU", "RU", "CN", "OTHER")

# Map label in sup_region to EU contries by ascension year
trade <- trade |>
  mutate(
    sup_region = case_when(
      supplier == "Russia" ~ "RU",
      supplier == "United States" ~ "US",
      supplier == "China" ~ "CN",
      
      order_year <= 1994 & supplier %in% eu_list_94 ~ "EU",
      order_year %in% c(1995:2004) & supplier %in% eu_list_94 ~ "EU",
      order_year %in% c(2005:2007) & supplier %in% eu_list_94 ~ "EU",
      order_year %in% c(2008:2020) & supplier %in% eu_list_94 ~ "EU",
      
      TRUE ~ "OTHER"
    ),
    sup_region = factor(sup_region, levels = group_order, ordered = TRUE)
  )
```
</details>

<details>
<summary><b>[CODE] Basic statistics</b></summary>

```r
# N suppliers
stat_n_sup <- length(unique(trade$supplier))

# N recipient
stat_n_rec <- length(unique(trade$recipient))

# N armament_category
stat_n_arm_cat <- length(unique(trade$arm_cat))

# Total TIV transactioned
stat_tiv_trans <- sum(trade$tiv_delivery)

# Transactions
stat_n_trans <- length(trade$order_year)

# Transactions per sup_region
stat_n_trans_sup_region <- kable(
  sort(table(trade$sup_region), decreasing = TRUE),
  col.names = c("Supplier", "Transactions"),
  format.args = list(big.mark = ','))

sipri_stats <- tribble(
  ~Statistic, ~Value,
  "Suppliers", stat_n_sup,
  "Receivers", stat_n_rec,
  "Arms Categories", stat_n_arm_cat,
  "TIV Transfered", stat_tiv_trans,
  "Transfers", stat_n_trans
) |> 
  mutate(
    Value = format(Value, big.mark = ",")
  ) |> kable()

sipri_stats
```
</details>

## How has trade volume changed?

<details>
<summary><b>[CODE] Global Trade Volume</b></summary>
  
```r
global_tiv_focused <- trade |>
  filter(
    order_year %in% c(1995:2020)
  ) |>
  group_by(order_year) |>
  summarise(
    total = sum(tiv_delivery)
  ) |>
  
  ggplot(
    aes(order_year, y = total/1000)
  ) +
  theme_minimal() +
  
  geom_line(
     aes(alpha = .5),
     show.legend = FALSE,
     linewidth = .6
     ) +
  
  geom_point(size = .6) + 
  
  geom_smooth(
    method = "gam",
    level = .95,
    alpha = .3
  ) +
  
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  labs(
    title = "Trends in Global Arms Transfers - Total TIV",
    subtitle = "From 1995 to 2020 by order date.",
    y = "Total TIV (in thousands)",
    x = "Order Year",
    alpha = element_blank()
    ) +
  theme(plot.title = element_text(face = "bold"))
  
  
global_tiv_alltime <- trade |>
  filter(
    order_year %in% c(1950:2020)
  ) |>
  group_by(order_year) |>
  summarise(
    total = sum(tiv_delivery)
  ) |>
  
  ggplot(
    aes(order_year, y = total/1000)
  ) +
  theme_minimal() +
  
  geom_rect(
    data = data.frame(xmin=1995, xmax=2020, ymin=0, ymax=Inf),
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "darkgoldenrod2",
      color = NA,
      linewidth = 0,
      alpha = .3) +
  
  geom_line(
     aes(alpha = .5),
     show.legend = FALSE,
     linewidth = .6
     ) +
  
  geom_point(size = .6) + 
  
  geom_smooth(
    method = "gam",
    level = .95,
    alpha = .3
  ) +
  
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  labs(
    title = element_blank(),
    subtitle = "From 1950 to 2020.",
    y = element_blank(),
    x = element_blank(),
    alpha = element_blank(),
    caption = "Stockholm International Peace Research Institute (SIPRI),
    SIPRI Arms Transfers Database, accessed [November, 2023],
    https://www.sipri.org/databases/armstransfers."
  )

global_tiv_focused + global_tiv_alltime
```
</details>

![image](https://github.com/lucacasu/Arms-Trade/blob/main/plot-images/global_tiv.png)


## How valuable are the assets traded?
This box plot displays the distribution of TIV values for each Arms Category. Since values range from 0.004 to 1250, with some lower value categories being nearly omitted in the plot, I scaled the x-axis by log10() to better represent their distribution. Along the plot code, an auxiliary column mean_sort is used to organize the y-axis in descending order by the mean unit TIV.

To inform the actual values, the stats_tiv_unit_table shows the mean, median, min and max, and the coefficient of variation of each category.

<details>
<summary><b>[CODE] Distribution of Unit TIV per category</b></summary>
  
```r
# TIV unit per category (all time)

stats_tiv_unit_table <- trade |>
  filter(
    order_year %in% c(1950:2020)
  ) |>
  group_by(arm_cat) |>
  summarise(
    Mean = round(mean(tiv_unit),2),
    Median = round(median(tiv_unit),2),
    Min = round(min(tiv_unit), 2),
    Max = round(max(tiv_unit), 2),
    CV = sprintf("%1.0f%%",round((sd(tiv_unit)/mean(tiv_unit)) *100,1))
    ) |>
  arrange(desc(Mean)) 

stats_tiv_unit_table <- kable(stats_tiv_unit_table,
                              col.names = c("Arms Category",
                                            names(stats_tiv_unit_table)[-1]))

stats_tiv_unit_plot <- trade |>
  filter(
    order_year %in% c(1950:2020)
  ) |>
  group_by(arm_cat) |>
  mutate(mean_sort = mean(tiv_unit)) |>
  ggplot(
    aes(log10(tiv_unit), reorder(arm_cat, mean_sort))
  ) +
  geom_boxplot(
    outliers = TRUE,
    linewidth = .1,
    outlier.size = .8,
    outlier.stroke = NA,
    outlier.colour = "red3"
    ) +
  scale_x_continuous(
    breaks = log10(c(0.01, 0.1, 1, 10, 100, 1000)),
    labels = c(0.01, 0.1, 1, 10, 100, 1000) 
  ) +
  theme_minimal() +
  labs(
    title = "Distribution of TIV Unit by Arms Category",
    subtitle = "Log scale | From 1950 to 2023.",
    y = element_blank(),
    x = "TIV Unit (log10)",
    caption = "Stockholm International Peace Research Institute (SIPRI),
    SIPRI Arms Transfers Database, accessed [November, 2023],
    https://www.sipri.org/databases/armstransfers."
  ) +
  theme(plot.title = element_text(face = "bold"))

stats_tiv_unit_plot
stats_tiv_unit_table
```
</details>

![image](https://github.com/lucacasu/Arms-Trade/blob/main/plot-images/tiv_value.png)

![image](https://github.com/lucacasu/Arms-Trade/blob/main/plot-images/tiv_stats.png)


## How Have Items' Value Changed?

<details>
<summary><b>[CODE] Mean Unit TIV Change per Category</b></summary>

```r
# TIV Unit per category year breakdown

# Satelites are removed as price remained constant (50)

# Using mean instad of median because it matters that
# highly valuable assets were transfered.
# Outliers are relevant.

mean_unit_tiv_yearly_plot <- trade |>
  filter(
    order_year %in% c(1950:2020),
    arm_cat != "Satellites"
  ) |>
  mutate(
   group = case_when(
      arm_cat %in% c("Ships", "Naval weapons") ~ "Naval",
      arm_cat %in% c("Air defence systems", "Aircraft") ~ "Air",
      arm_cat %in% c("Satellites", "Sensors") ~ "High-tech",
      arm_cat %in% c("Engines","Armoured vehicles") ~ "Engineering",
      arm_cat %in% c("Artillery", "Missiles") ~ "Ordinance",
      arm_cat %in% c("Other") ~ "Other",
    )
  ) |>
  group_by(order_year, arm_cat) |>
  summarise(
    Mean = mean(tiv_unit),
    group = group
    ) |>
  ggplot(
    aes(order_year, Mean, color = arm_cat)
  ) +
  geom_smooth(se=FALSE) +
  facet_wrap(~group, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(
    title = "Time Series of Mean Unit TIV by Arms Category",
    subtitle = "From 1950 to 2020.",
    y = element_blank(),
    x = element_blank(),
    color = element_blank(),
    caption = "Stockholm International Peace Research Institute (SIPRI),
    SIPRI Arms Transfers Database, accessed [November, 2023],
    https://www.sipri.org/databases/armstransfers."
  ) +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
  ) 

mean_unit_tiv_yearly_plot
```
</details>

![image](https://github.com/lucacasu/Arms-Trade/blob/main/plot-images/tiv_change.png)

## How Have Categories Ranked in Each Decade?

<details>
<summary><b>[CODE] Categories Ranked</b></summary>

```r
# Define the base for the next charts. Add Category Group + year bin
trade_decades_category <- trade |>
  filter(
    order_year %in% c(1950:2023)
  ) |>
  mutate(
   group = case_when(
      arm_cat %in% c("Ships", "Naval weapons") ~ "Naval",
      arm_cat %in% c("Air defence systems", "Aircraft") ~ "Air",
      arm_cat %in% c("Satellites", "Sensors") ~ "High-tech",
      arm_cat %in% c("Engines","Armoured vehicles") ~ "Engineering",
      arm_cat %in% c("Artillery", "Missiles") ~ "Ordinance",
      arm_cat %in% c("Other") ~ "Other",
    )
  ) |>
  mutate(
    bin = cut(order_year, 
      breaks = seq(1950, 2020, by = 10),
      include.lowest = TRUE, 
      right = FALSE,
      labels = c("1950", "1960", "1970", "1980", "1990", "2000", "2010"))
  )

# Total TIV traded per category, decade
trade_decades_category_sum <- trade_decades_category |>
  filter(order_year %in% c(1950:2020)) |>
  group_by(arm_cat, bin) |>
  summarise(
    sum_cat_decade = sum(tiv_delivery)
  ) |>
  select(bin, sum_cat_decade)

# Total TIV traded per decade
# To calculate the relative % of each category
total_delivery_decades <- trade_decades_category |>
  filter(order_year %in% c(1950:2020)) |>
  group_by(bin) |>
  summarise(
    sum_decade = sum(tiv_delivery)
  )

# Combine the sum data for (category + decade) with (decade)
total_delivery_decades_combined <- left_join(total_delivery_decades,
                                    trade_decades_category_sum, by = "bin")

total_delivery_decades_combined <- total_delivery_decades_combined |>
  relocate(arm_cat, .after = bin)


# Relative % of each category
total_delivery_decades_pct <- total_delivery_decades_combined |>
  group_by(bin, arm_cat) |>
  summarise(
    pct = (sum_cat_decade/sum_decade) * 100
  ) |>
  group_by(bin) |>
  mutate(
    rank = rank(-pct)
  )

# Colors to highlight the categories with intersting changes
custom_colors <- c(
  "Air defence systems" = "#F8766D",
  "Artillery" = "#64B200",
  "Missiles" = "#B385FF",
  "Sensors" = "#00A6FF",
  "Aircraft" = "grey80",
  "Armoured vehicles" = "grey80",
  "Engines" = "grey80",
  "Naval weapons" = "grey80",
  "Ships" = "grey80",
  "Satellites" = "grey80",
  "Other" = "grey80"
)

total_delivery_rank_plot <- total_delivery_decades_pct |>
  ungroup() |>
  mutate(is_last = bin == "2010") |>
  ggplot(
    aes(bin, rank, color = arm_cat, group = arm_cat)
  ) +
  geom_line(linewidth = 1.5, show.legend = FALSE) +
  geom_point(show.legend = FALSE, size = 2) +
  geom_label(
    aes(label = arm_cat, hjust = 0),
    data = total_delivery_decades_pct |> filter(bin == "2010"),  
    show.legend = FALSE, nudge_x = .1
  ) +
  scale_x_discrete(expand = expansion(add = c(0, 2))) +
  scale_y_reverse(
    breaks = seq(1, 11, 1),
    labels = seq(1, 11, 1)   # Match labels to breaks
  ) +
  theme_minimal() +
  scale_color_manual(values = custom_colors) +
  theme(panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
        ) +
  labs(
    title = "Arms Category Rank of Total TIV value by Decade",
    subtitle = "From 1950 to 2020.",
    y = "Rank",
    x = element_blank(),
    caption = "Stockholm International Peace Research Institute (SIPRI),
    SIPRI Arms Transfers Database, accessed [November, 2023],
    https://www.sipri.org/databases/armstransfers."
  )

# Arm category total TIV per decade table

total_tiv_decades_arm_cat <- trade_decades_category |>
  filter(order_year %in% c(1950:2020)) |>
  group_by(arm_cat, bin) |>
  summarise(
    Total = sum(tiv_delivery),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = bin, values_from = Total) |>
  mutate(
    mean = rowMeans(across('1950':'2010'), na.rm = TRUE),
    across(`1950`:`2010`, ~ format(round(.), big.mark = ","))
  ) |>
  arrange(desc(mean)) |>
  select(-mean) |>
  rename('Arm Category' = arm_cat) |>
  kable()
 
# OUTPUT:
# total_delivery_rank_plot
# total_tiv_decades_arm_cat
```
</details>

![image](https://github.com/lucacasu/Arms-Trade/blob/main/plot-images/tiv_rank.png)
![image](https://github.com/lucacasu/Arms-Trade/blob/main/plot-images/rank_stats.png)


## Have Suppliers Expanded Their Sphere of Influence? 

<details>
<summary><b>[CODE] Sphere of Influence</b></summary>

```r
custom_colors <- c(
  "US" = "#003E8A",
  "EU" = "#0090E9",
  "RU" = "#D62718",
  "RU+SU" = "#D62718",
  "CN" = "#FF9D00"
)

# How many countries have been supplied by each power? (focused)

countries_suplied_focused_plot <- trade |>
  select(order_year, rec_region, sup_region, supplier, recipient,
         arm_cat, tiv_unit, tiv_delivery) |>
  mutate(
    sup_region = case_when(
      supplier %in% c("Russia", "Soviet Union") ~ "RU+SU",
      TRUE ~ sup_region)) |>
  filter(
    order_year %in% c(1995:2020),
    sup_region != "OTHER"
  ) |>
  group_by(order_year,sup_region) |>
  summarise(
    n = length(unique(recipient)),
  ) |>
  ggplot(
    aes(order_year, n, group = sup_region, color = sup_region)
  ) +
  theme_minimal() +
  
  geom_line(
     aes(alpha = .5),
     show.legend = FALSE,
     linewidth = .4
     ) +
  
  geom_point(size = .5,
             show.legend = FALSE) + 
  
  geom_smooth(
    method = "gam",,
    linewidth = .6,
    level = .95,
    alpha = .3,
    show.legend = FALSE
  ) +
  
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  labs(
    title = "Time Series of Countries Supplied by Supplier",
    subtitle = "From 1995 to 2020.",
    y = "Countries",
    x = element_blank(),
    alpha = element_blank()
    ) +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(values = custom_colors)
  
  
countries_suplied_alltime_plot <- trade |>
  select(order_year, rec_region, sup_region, supplier, recipient,
         arm_cat, tiv_unit, tiv_delivery) |>
  mutate(
    sup_region = case_when(
      supplier %in% c("Russia", "Soviet Union") ~ "RU+SU",
      TRUE ~ sup_region
    )
  ) |>
  filter(
    order_year %in% c(1950:2020),
    sup_region != "OTHER"
  ) |>
  group_by(order_year,sup_region) |>
  summarise(
    n = length(unique(recipient)),
  ) |>
  
  ggplot(
    aes(order_year, n, group = sup_region, color = sup_region)
  ) +
  geom_rect(
    data = data.frame(xmin=1995, xmax=2020, ymin=0, ymax=Inf),
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = "gold2",
          color = NA,
          linewidth = 0,
          alpha = 0.3,
          show.legend = FALSE) +
  
  geom_line(
     aes(alpha = .5),
     show.legend = FALSE,
     linewidth = .4
     ) +
  
  geom_point(size = .5,
             show.legend = FALSE) + 
  
  geom_smooth(
    method = "gam",
    linewidth = .6,
    level = .95,
    alpha = .3
  ) +
  
    theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  theme(legend.position = "bottom") +
  labs(
    title = element_blank(),
    subtitle = "From 1950 to 2020.",
    y = element_blank(),
    x = element_blank(),
    alpha = element_blank(),
    color = "Supplier",
    caption = "Stockholm International Peace Research Institute (SIPRI),
    SIPRI Arms Transfers Database, accessed [November, 2023],
    https://www.sipri.org/databases/armstransfers."
  ) +
  scale_color_manual(values = custom_colors)

countries_suplied_focused_plot + countries_suplied_alltime_plot
```
</details>

![image](https://github.com/lucacasu/Arms-Trade/blob/main/plot-images/sphere.png)

### Linear Regression

<details>
<summary><b>[CODE] Linear Regression</b></summary>

```r
data <- trade |>
  select(order_year, rec_region, sup_region, supplier, recipient,
         arm_cat, tiv_unit, tiv_delivery) |>
  filter(
    order_year %in% c(1995:2020),
    sup_region != "OTHER"
  ) |>
  group_by(order_year,sup_region) |>
  summarise(
    n = length(unique(recipient)),
  ) |>
  arrange(sup_region, order_year) |>
  relocate(sup_region) |>
  ungroup()

# outliers

outlier_lm_plot <- data |>
  ggplot(
    aes(sup_region,n, group = sup_region, color = sup_region)
  ) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribution of Countries Supplied by Supplier",
    subtitle = "From 1995 to 2020.",
    y = "Countries",
    x = element_blank(),
    color = element_blank(),
    caption = "Stockholm International Peace Research Institute (SIPRI),
    SIPRI Arms Transfers Database, accessed [November, 2023],
    https://www.sipri.org/databases/armstransfers."
  ) +
  scale_color_manual(values = custom_colors) +
  theme(plot.title = element_text(face = "bold"))

# Filter the data by Supplier
data_us <- filter(data, sup_region == "US")
data_eu <- filter(data, sup_region == "EU")
data_ru <- filter(data, sup_region == "RU")
data_cn <- filter(data, sup_region == "CN")

# Linear model
model_us <- lm(n ~ order_year, data = data_us)
model_eu <- lm(n ~ order_year, data = data_eu)
model_ru <- lm(n ~ order_year, data = data_ru)
model_cn <- lm(n ~ order_year, data = data_cn)

# Pull slope
slope_us <- round(summary(model_us)$coefficients["order_year", "Estimate"], 4)
slope_eu <- round(summary(model_eu)$coefficients["order_year", "Estimate"], 4)
slope_ru <- round(summary(model_ru)$coefficients["order_year", "Estimate"], 4)
slope_cn <- round(summary(model_cn)$coefficients["order_year", "Estimate"], 4)


# Adjusted p value for a right tailed test
p_us <- round(summary(model_us)$coefficients["order_year", "Pr(>|t|)"] / 2, 4) 
p_eu <- round(summary(model_eu)$coefficients["order_year", "Pr(>|t|)"] / 2, 4)
p_ru <- round(summary(model_ru)$coefficients["order_year", "Pr(>|t|)"] / 2, 4)
p_cn <- round(summary(model_cn)$coefficients["order_year", "Pr(>|t|)"] / 2, 4)


# Pull Standard error
Sb_us <- summary(model_us)$coefficients["order_year", "Std. Error"]
Sb_eu <- summary(model_eu)$coefficients["order_year", "Std. Error"]
Sb_ru <- summary(model_ru)$coefficients["order_year", "Std. Error"]
Sb_cn <- summary(model_cn)$coefficients["order_year", "Std. Error"]

# right tailed
t_critic <- qt(0.95,24) # right tailed 

# Confidence Interval
ci_us_lower <- round(slope_us - t_critic * Sb_us, 4)
ci_us_upper <- round(slope_us + t_critic * Sb_us, 4)

ci_eu_lower <- round(slope_eu - t_critic * Sb_eu, 4)
ci_eu_upper <- round(slope_eu + t_critic * Sb_eu, 4)

ci_ru_lower <- round(slope_ru - t_critic * Sb_ru, 4)
ci_ru_upper <- round(slope_ru + t_critic * Sb_ru, 4)

ci_cn_lower <- round(slope_cn - t_critic * Sb_cn, 4)
ci_cn_upper <- round(slope_cn + t_critic * Sb_cn, 4)


lm_results_table <- tribble(
  ~Supplier, ~Slope, ~p_value, ~ci_lower, ~ci_upper, ~Decision, 
  "US", slope_us, p_us, ci_us_lower, ci_us_upper, "Reject Null",
  "EU", slope_eu, p_eu, ci_eu_lower, ci_eu_upper, "Fail to Reject Null", 
  "RU", slope_ru, p_ru, ci_ru_lower, ci_ru_upper, "Fail to Reject Null", 
  "CN", slope_cn, p_cn, ci_cn_lower, ci_cn_upper, "Reject Null", 
)

lm_results_table$p_value[1] <- "<0.0001"
lm_results_table$p_value[4] <- "<0.0001"

lm_results_table_csv <- lm_results_table # to be used in Task 2

lm_results_table <- lm_results_table |> kable()

lm_results_table
```
</details>

![image](https://github.com/lucacasu/Arms-Trade/blob/main/plot-images/reg_stats.png)

## Who Are the Most Frequent Buyers?

<details>
<summary><b>[CODE] Distribution of Transactions</b></summary>

```r
# Get outliers threshold for each supplier (number of transactions)

# This table is not plottled. It gives the upper bound
# to define extreme values in the next plot

sup_trans_table_param <- trade |>
  select(order_year, rec_region, sup_region,
         recipient, arm_cat, tiv_unit, tiv_delivery) |>
  filter(
    order_year %in% c(1995:2020)
  ) |>
  group_by(sup_region, recipient) |>
  summarise(
    n = n()
  ) |>
  group_by(sup_region) |>
  summarise(
    IQR = IQR(n),
    Q1 = quantile(n, c(.25)),
    Q2 = quantile(n, c(.5)),
    Q3 = quantile(n, c(.75)),
    lower_bound = Q1 - 3 * IQR,
    upper_bound = Q3 + 3 * IQR
  ) |>
  select(-lower_bound) |>
  rename(Supplier = sup_region) |>
  arrange(desc(upper_bound))

sup_trans_table_param <- kable(sup_trans_table_param)


# Filter only outliers to be used as labels in the next chart
outliers <- trade |>
  select(order_year, rec_region, sup_region, recipient,
         arm_cat, tiv_unit, tiv_delivery) |>
  filter(
    order_year %in% c(1995:2020),
    sup_region != "OTHER"
  ) |>
  group_by(sup_region, recipient) |>
  summarise(
    n = n()
  ) |>
  ungroup() |>
  filter(
    (sup_region == "US" & n > 247) |
    (sup_region == "EU" & n > 177) |
    (sup_region == "RU" & n > 74) |
    (sup_region == "CN" & n > 44)
  ) |> 
  arrange(sup_region, n)


sup_trans_plot <- trade |>
  select(order_year, rec_region, sup_region, recipient,
         arm_cat, tiv_unit, tiv_delivery) |>
  filter(
    order_year %in% c(1995:2020),
    sup_region != "OTHER"
  ) |>
  group_by(sup_region, recipient) |>
  summarise(
    n = n()
  ) |>
  ggplot(
    aes(sup_region, n),
    label = recipient
  ) +
  geom_boxplot(notch = TRUE) +
  
  geom_point(
    data = outliers,
    aes(sup_region, n, color = "red2"),
    show.legend = FALSE
    ) +
  geom_text_repel(
    data = outliers,
    aes(sup_region, n, label = recipient),direction = "y",
    nudge_x = -.1, min.segment.length = 0.1, hjust = 1
  ) +
  scale_x_discrete(expand = expansion(add = c(1, 1))) +
  theme_minimal() +
  labs(
    title =
      "Distribution of Transactions",
    subtitle =
      "From 1995 to 2020. Extreme values (3 * IQR) highlighted in red.",
    y = "Transactions",
    x = element_blank(),
    color = element_blank(),
    caption = "Stockholm International Peace Research Institute (SIPRI),
    SIPRI Arms Transfers Database, accessed [November, 2023],
    https://www.sipri.org/databases/armstransfers."
  ) +
  theme(plot.title = element_text(face = "bold"))

sup_trans_plot
```
</details>

![image](https://github.com/lucacasu/Arms-Trade/blob/main/plot-images/transactions.png)

## Supplier’s Share of Arms Category

<details>
<summary><b>[CODE] Supplier’s Share of Arms Category</b></summary>

```r
# BETWEEN 1995-2020 ----------

cat_share_plot <- trade |>
  filter(
    order_year %in% c(1995:2020),
    #sup_region != "OTHER"
  ) |>
  
  mutate(
    bin = cut(order_year, 
      breaks = seq(1950, 2020, by = 5),
      include.lowest = TRUE, 
      right = FALSE,
      labels = c("1950", "1955",
                 "1960", "1965", 
                 "1970", "1975", 
                 "1980", "1985",
                 "1990", "1995",
                 "2000", "2005",
                 "2010", "2015")
      )
    ) |>
  
  group_by(sup_region, arm_cat, bin) |>
  summarise(
    total = sum(tiv_delivery)
  ) |>
  
  ggplot(
    aes(bin, y = total/1000, fill = sup_region)
  ) +
  theme_minimal() +
  
  geom_col(
    #position = "fill"
  ) +
  
  facet_wrap(~arm_cat, nrow = 4, scales = "free_y") +
  
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    title = "Trends in Global Arms Transfers - Total TIV",
    subtitle = "From 1995 to 2020 in 5-year bins.",
    y = "Total TIV (in thousands)",
    x = element_blank(),
    fill = "Supplier",
    caption = "Stockholm International Peace Research Institute (SIPRI),
    SIPRI Arms Transfers Database, accessed [November, 2023],
    https://www.sipri.org/databases/armstransfers."
    ) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 100)
  ) 
  
  cat_share_plot
```
</details>

![image](https://github.com/lucacasu/Arms-Trade/blob/main/plot-images/cat_share.png)


## Afinity to Supplier

The following plot maps the receiver countries by the share of TIV acquired from suppliers in the West (US, EU), East (RU, CN) and Others. The bubble size indicates the country’s total TIV between 1995 and 2020. 

To split the plot by market concentration, I mapped the Herfindahl–Hirschman index (HHI) to the bubble colours, where darker indicates higher concentration by one supplier axis. The HHI is the sum of the supplier’s squared market shares, input as a whole number. $\sum(s^2_1 + s^2_2 + ...s^2_n + )$

Three key insights are observed:

- Those who mainly buy from Other suppliers account for decimal shares of the global market. Powerful nations tend to be aligned to either Western or Eastern suppliers. In Q1, the major buyer from Others in the Western axis is the United States.
- **Q2, Q3, Q4:** More countries are strongly aligned to Western suppliers, and these countries tend to have larger budgets.  In ⁰Q3, the single major trade in the Eastern axis  happens between its own suppliers (China and Russia).
- **Q1:** India and Pakistan are the exception among countries with low market concentration. Buyers closer to the Eastern axis are mostly small nations that also buy 30-60% from Others. In the Western axis, Indonesia, Thailand, Malaysia and Iraq account for the most of the trade.


<details>
<summary><b>[CODE] Affinity to Supplier</b></summary>

```r
shares_tiv <- trade |>
  filter(
    sup_region %in% c("US", "EU", "RU", "CN", "OTHER"),
    order_year %in% c(1995:2020)
    ) |>
  group_by(sup_region, recipient) |>
  
  summarise(
    total_tiv = round(sum(tiv_delivery), 4),
    .groups = "drop"
    )

shares_perc <- shares_tiv |>
  group_by(recipient) |>
  arrange(recipient) |>
  mutate(
    share = total_tiv / sum(total_tiv) * 100
  ) |>
  select(-total_tiv) |>
  pivot_wider(
    names_from = sup_region, values_from = share, values_fill = 0
  ) |> relocate(OTHER, .after = CN) |>
  mutate(
    WEST = rowSums(across(US:EU)),
    EAST = rowSums(across(CN:RU))
  ) |>
  select(recipient, WEST, EAST, OTHER)

HHI <- shares_perc |>
  mutate(
    across(c(WEST:OTHER), ~.^2),
    HHI = rowSums(across(WEST:OTHER))
  ) |>
  arrange(HHI) |>
  mutate(
    across(WEST:OTHER, ~ sqrt(.))
  )

shares_tiv_sum <- shares_tiv |> group_by(recipient) |> summarise( total = sum(total_tiv))

HHI <- HHI |>
  left_join(shares_tiv_sum, by = "recipient", unmatched = "drop") |>
  left_join(trade |> select(recipient, rec_region) |> distinct(), by = "recipient", unmatched = "drop")

HHI <- HHI |>
  left_join(cont_reference, by = "rec_region", unmatched = "drop")

HHI_quant <- quantile(HHI$HHI)

HHI2 <- HHI |>
  mutate(
    x = -WEST + EAST,
    y = OTHER,
    qt = case_when(
      HHI < HHI_quant[2] ~ "Q1",
      HHI >= HHI_quant[2] & HHI < HHI_quant[3] ~ "Q2",
      HHI >= HHI_quant[3] & HHI < HHI_quant[4] ~ "Q3",
      HHI >= HHI_quant[4] ~ "Q4"
    ),
    qt = factor(qt, levels = c("Q1","Q2","Q3","Q4"), ordered = TRUE)
  )
```

```r
affinity_plot <- ggtern::ggtern(HHI2,
               aes(WEST, OTHER, EAST, fill = qt, size = total, alpha = .8)) +
  geom_point(
   shape = 21,
   color = 'black'
  ) +
  theme_showarrows() +
  theme_nomask() +
  scale_fill_brewer(palette = "Set1") +
  theme_nogrid_minor() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "grey",linetype = 2),
    axis.text = element_text(face = "bold"), 
    tern.axis.arrow = element_line(size = 2)
  ) +
  Tlab("") +
  Llab("") +
  Rlab("") +
  Tarrowlab("BUYS FROM OTHERS") +
  Larrowlab("BUYS FROM US / EU") +
  Rarrowlab("BUYS FROM RU / CN") +
  labs(
    title = "Affinity to Supplier",
    subtitle = "From 1995 to 2020.",
    size = "Total TIV",
    fill = "HHI Quantile"
  ) +
  scale_size_continuous(range=c(1,12)) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  scale_alpha(guide = 'none')

affinity_plot_faceted <- affinity_plot + facet_wrap(~qt)


affinity_plot
affinity_plot_faceted
```
</details>

![image](https://github.com/lucacasu/Arms-Trade/blob/main/plot-images/affinity_split.png)

