library(dplyr)        
library(ggplot2)      
library(tidyr)        
library(purrr)        
library(scales)       
library(ineq)        
library(car)          
library(lmtest)       
library(ggfortify)    
library(broom)

countries <- c("CZ", "ES", "SE")

all_data_combined <- map_dfr(countries, function(cty) {
  e <- new.env()
  load(file = paste0("./data/all_data_combined_", cty, ".Rdata"), envir = e)
  get("all_data_combined", envir = e)
})

# Filter for individuals aged 25–64 with valid sex, education, and positive household income
# Recode Education to Low/Medium/High
# Education codes vary by year => harmonize them into 3 categories
individuals <- all_data_combined %>%
  select(country, year, hhid, RB030, PB140, PB150, PE040, PE041, PB180, PB180_F, personal_income, age) %>%
    filter(
    age >= 25, age <= 64,
    !is.na(PB180) & PB180_F == 1,
    !is.na(PB150), # Valid sex
    !is.na(PE040) | !is.na(PE041), # Valid education
    personal_income > 0 # Positive household income
  ) %>%
  mutate(
    edu_cat = case_when(
      year < 2014 & PE040 %in% c(0,1,2) ~ "Low",
      year < 2014 & PE040 %in% c(3,4) ~ "Medium",
      year < 2014 & PE040 %in% c(5,6) ~ "High",
      year >= 2014 & year < 2021 & PE040 %in% c(000,100,200) ~ "Low",
      year >= 2014 & year < 2021 & PE040 >= 300 & PE040 <= 490 ~ "Medium",
      year >= 2014 & year < 2021 & PE040 >= 500 & PE040 <= 800 ~ "High",
      year >= 2021 & PE041 %in% c(000,100,200) ~ "Low",
      year >= 2021 & PE041 >= 300 & PE041 <= 490 ~ "Medium",
      year >= 2021 & PE041 >= 500 & PE041 <= 800 ~ "High",
      TRUE ~ NA_character_
    ),
    edu_cat = factor(edu_cat, levels = c("Low", "Medium", "High"), ordered = TRUE)
  )

n_by_gender <- individuals %>%
  group_by(country, year, sex = ifelse(PB150 == 1, "Male", "Female")) %>%
  summarise(n = n(), .groups = "drop")

write.csv(n_by_gender, file = "output/n_by_gender.csv", row.names = FALSE)


# Identify Households with 1 Male and 1 Female
males <- individuals %>% filter(PB150 == 1)
females <- individuals %>% filter(PB150 == 2)

hh_counts <- individuals %>%
  group_by(country, year, hhid) %>%
  summarise(n_male = sum(PB150 == 1), n_female = sum(PB150 == 2), .groups = "drop") %>%
  filter(n_male == 1 & n_female == 1)

# Match Males and Females into Couples
couples <- inner_join(
  males %>% filter(hhid %in% hh_counts$hhid),
  females %>% filter(hhid %in% hh_counts$hhid),
  by = c("hhid", "year", "country"),
  suffix = c("_m", "_f")
) %>%
  mutate(total_income = personal_income_m + personal_income_f)

n_couples <- couples %>%
  group_by(country, year) %>%
  summarise(n_couples = n(), .groups = "drop")

write.csv(n_couples, file = "output/n_couples.csv", row.names = FALSE)

# Classify Match Type
# Hypergamous: = Male > Female in education
# Hypogamous: Female > Male
# Homogamous: = Equal education level
couples <- couples %>%
  mutate(
    match_type = case_when(
      edu_cat_m == edu_cat_f ~ "Homogamous",
      edu_cat_m > edu_cat_f ~ "Hypergamous",
      edu_cat_m < edu_cat_f ~ "Hypogamous"
    ),
    match_type = factor(match_type, levels = c("Homogamous", "Hypergamous", "Hypogamous"))
  )

# Share of Match Type Over Time
match_type_share <- couples %>%
  group_by(country, year, match_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(country, year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

write.csv(match_type_share, "output/match_type_share_over_time.csv", row.names = FALSE)

p_match_type_share <- ggplot(match_type_share, aes(x = year, y = share, fill = match_type)) +
  geom_area(position = "stack") +
  facet_wrap(~country) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Share of Each Match Type Over Time",
    x = "Year", y = "Share", fill = "Match Type"
  ) +
  theme_minimal()

ggsave("output/match_type_share_over_time.png", p_match_type_share, width = 10, height = 6, dpi = 300)


# Inequality Metrics
s80s20 <- function(x) {
  x_sorted <- sort(x)
  n <- length(x_sorted)
  q20 <- ceiling(0.2 * n)
  s20 <- mean(x_sorted[1:q20])
  s80 <- mean(x_sorted[(n - q20 + 1):n])
  return(s80 / s20)
}

gini_actual <- couples %>%
  group_by(country, year) %>%
  summarise(
    gini_actual = Gini(total_income),
    s80s20_actual = s80s20(total_income),
    .groups = "drop"
  )

# Simulate Counterfactual Couples (Random Matching)
simulate_random_couples <- function(df) {
  males <- df %>% filter(PB150 == 1)
  females <- df %>% filter(PB150 == 2)
  
  n_pairs <- min(nrow(males), nrow(females))
  if (n_pairs == 0) return(NULL)
  
  males <- males %>%
    slice_sample(n = n_pairs) %>%
    mutate(rand_id = row_number(), personal_income_m = personal_income)
  
  females <- females %>%
    slice_sample(n = n_pairs) %>%
    mutate(rand_id = row_number(), personal_income_f = personal_income)
  
  paired <- inner_join(
    males %>% select(country, year, personal_income_m, rand_id),
    females %>% select(personal_income_f, rand_id),
    by = "rand_id"
  ) %>%
    mutate(total_income = personal_income_m + personal_income_f)
  
  return(paired)
}

set.seed(42)
random_data <- individuals %>%
  filter(PB150 %in% c(1, 2)) %>%
  group_split(country, year) %>%
  map_dfr(simulate_random_couples)

gini_random <- random_data %>%
  group_by(country, year) %>%
  summarise(
    gini_random = Gini(total_income),
    s80s20_random = s80s20(total_income),
    .groups = "drop"
  )

gini_comparison <- left_join(gini_actual, gini_random, by = c("country", "year"))
gini_comparison

# Gini by Match Type
gini_by_type_year <- couples %>%
  group_by(country, year, match_type) %>%
  summarise(
    gini = Gini(total_income),
    s80s20 = s80s20(total_income),
    count = n(), .groups = "drop"
  )

write.csv(gini_by_type_year, file = "output/gini_by_match_type_year.csv", row.names = FALSE)

matchtype_counts <- couples %>%
  group_by(country, year, match_type) %>%
  summarise(n = n(), .groups = "drop")

write.csv(matchtype_counts, file = "output/match_type_counts.csv", row.names = FALSE)

# Additional variables
couples <- couples %>%
  mutate(
    avg_age = (age_m + age_f) / 2,
    edu_level_household = pmax(as.numeric(edu_cat_m), as.numeric(edu_cat_f), na.rm = TRUE),
    edu_label = factor(edu_level_household, levels = 1:3, labels = c("Low", "Medium", "High")),
    age_group = cut(avg_age, breaks = c(25, 34, 44, 54, 64), labels = c("25–34", "35–44", "45–54", "55–64"))
  )

income_by_edu <- couples %>%
  group_by(country, year, edu_label) %>%
  summarise(
    median_income = median(total_income),
    mean_income = mean(total_income),
    count = n(),
    .groups = "drop"
  )

write.csv(income_by_edu, file = "output/income_by_education.csv", row.names = FALSE)

income_by_age_group <- couples %>%
  group_by(country, year, age_group) %>%
  summarise(
    median_income = median(total_income),
    mean_income = mean(total_income),
    count = n(),
    .groups = "drop"
  )

write.csv(income_by_age_group, file = "output/income_by_age_group.csv", row.names = FALSE)


# Income Plots
# Combined chart
ggplot(couples, aes(x = match_type, y = total_income, fill = match_type)) +
  geom_boxplot() +
  labs(title = "Household Income by Match Type (All Countries)", x = "Match Type", y = "Household Income") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/income_by_match_type_all.png", width = 8, height = 5)

# By country
for (cty in unique(couples$country)) {
  p <- ggplot(filter(couples, country == cty), aes(x = match_type, y = total_income, fill = match_type)) +
    geom_boxplot() +
    labs(title = paste("Household Income by Match Type in", cty),
         x = "Match Type", y = "Household Income") +
    scale_y_continuous(labels = comma) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(paste0("output/income_by_match_type_", cty, ".png"), p, width = 7, height = 5)
}


# Combined
ggplot(couples, aes(x = age_group, y = total_income, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Household Income by Age Group (All Countries)", x = "Age Group", y = "Household Income") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/income_by_age_group_all.png", width = 8, height = 5)

# Per country
for (cty in unique(couples$country)) {
  p <- ggplot(filter(couples, country == cty), aes(x = age_group, y = total_income, fill = age_group)) +
    geom_boxplot() +
    labs(title = paste("Income by Age Group in", cty), x = "Age Group", y = "Household Income") +
    scale_y_continuous(labels = comma) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(paste0("output/income_by_age_", cty, ".png"), p, width = 7, height = 5)
}


# Combined
ggplot(couples, aes(x = edu_label, y = total_income, fill = edu_label)) +
  geom_boxplot() +
  labs(title = "Income by Household Education Level (All Countries)", x = "Education", y = "Household Income") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/income_by_education_all.png", width = 8, height = 5)

# Per country
for (cty in unique(couples$country)) {
  p <- ggplot(filter(couples, country == cty), aes(x = edu_label, y = total_income, fill = edu_label)) +
    geom_boxplot() +
    labs(title = paste("Income by Household Education Level in", cty),
         x = "Education", y = "Household Income") +
    scale_y_continuous(labels = comma) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(paste0("output/income_by_education_", cty, ".png"), p, width = 7, height = 5)
}


# Gini Over Time — Actual vs Random
gini_plot_data <- gini_comparison %>%
  pivot_longer(cols = starts_with("gini"), names_to = "group", values_to = "gini") %>%
  mutate(group = case_when(
    group == "gini_actual" ~ "Actual",
    group == "gini_random" ~ "Random",
    TRUE ~ group
  ))

# Combined Gini Plot 
ggplot(gini_plot_data, aes(x = year, y = gini, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ country) +
  labs(
    title = "Gini Coefficient Over Time (Actual vs. Random)",
    x = "Year", y = "Gini Coefficient"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave("output/gini_trends_facet_by_country.png", width = 10, height = 6, dpi = 300)

# Separate Plot for Each Country
countries_list <- unique(gini_plot_data$country)
for (cty in countries_list) {
  p <- ggplot(gini_plot_data %>% filter(country == cty),
              aes(x = year, y = gini, color = group)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = paste("Gini Over Time in", cty),
      x = "Year", y = "Gini Coefficient"
    ) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  ggsave(paste0("output/gini_trend_", cty, ".png"), p, width = 7, height = 5, dpi = 300)
}

# S80/S20 Over Time Plot (Actual vs Random)
s80s20_plot_data <- gini_comparison %>%
  select(country, year, s80s20_actual, s80s20_random) %>%
  pivot_longer(cols = starts_with("s80s20"), names_to = "group", values_to = "s80s20") %>%
  mutate(group = case_when(
    group == "s80s20_actual" ~ "Actual",
    group == "s80s20_random" ~ "Random"
  ))

ggplot(s80s20_plot_data, aes(x = year, y = s80s20, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~country) +
  labs(
    title = "S80/S20 Ratio Over Time (Actual vs. Random)",
    x = "Year", y = "S80/S20 Ratio", color = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave("output/s80s20_trends_by_country.png", width = 10, height = 6, dpi = 300)

write.csv(s80s20_plot_data, "output/s80s20_trends_by_country.csv", row.names = FALSE)

# Regression-Based Decomposition
reg_data <- couples %>%
  select(total_income, match_type, avg_age, edu_label, year, country) %>%
  filter(!is.na(total_income), !is.na(match_type), !is.na(avg_age), !is.na(edu_label)) %>%
  mutate(
    ln_income = log(total_income),
    match_type = factor(match_type),
    edu_label = factor(edu_label),
    year = factor(year),
    country = factor(country)
  )

couples_overview <- couples %>%
  group_by(country, year, match_type) %>%
  summarise(
    avg_income = mean(total_income),
    median_income = median(total_income),
    count = n(),
    .groups = "drop"
  )

write.csv(couples_overview, "output/couples_overview.csv", row.names = FALSE)


# Split reg_data by country
reg_by_country <- reg_data %>% group_split(country)
country_names <- reg_data %>% distinct(country) %>% pull()

# Reg model for each country
models <- map2(reg_by_country, country_names, function(df, cty) {
  model <- lm(ln_income ~ (match_type + avg_age + edu_label) * year, data = droplevels(df))
  list(country = cty, model = model, coef = tidy(model))
})

model_coefs_df <- bind_rows(map(models, "coef"), .id = "country_index")
model_coefs_df$country <- country_names[as.integer(model_coefs_df$country_index)]

write.csv(model_coefs_df, file = "output/regression_by_country_coefficients.csv", row.names = FALSE)

hypotheses <- list(
  "Homogamous" = "match_typeHomogamous:year2023 = match_typeHomogamous:year2005",
  "Hypergamous" = "match_typeHypergamous:year2023 = match_typeHypergamous:year2005",
  "Hypogamous" = "match_typeHypogamous:year2023 = match_typeHypogamous:year2005"
)

for (m in models) {
  cat("\n=== Linear Hypothesis Test for", m$country, "===\n")
  
  coef_names <- names(coef(m$model))
  
  for (type in names(hypotheses)) {
    hypothesis <- hypotheses[[type]]
    
    terms <- unlist(strsplit(hypothesis, " = "))
    
    if (all(terms %in% coef_names)) {
      cat("Testing:", hypothesis, "\n")
      print(car::linearHypothesis(m$model, hypothesis))
    } else {
      cat("Skipping", type, "— term not found in model coefficients.\n")
    }
  }
}

# Model Diagnostics
for (m in models) {
  cat("\n=== Diagnostics for", m$country, "===\n")
  print(summary(m$model))
  print(vif(m$model))
  print(bptest(m$model))
  autoplot(m$model, which = 1:2)
}

reg_summary <- bind_rows(lapply(models, function(m) {
  broom::glance(m$model) %>% mutate(country = m$country)
}))

write.csv(reg_summary, "output/regression_summaries.csv", row.names = FALSE)


write.csv(gini_comparison, file = "output/gini_comparison.csv", row.names = FALSE)

#Education Composition Chart
edu_dist <- individuals %>%
  group_by(country, year, edu_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(country, year) %>%
  mutate(share = n / sum(n))

p_edu_comp <- ggplot(edu_dist, aes(x = year, y = share, fill = edu_cat)) +
  geom_area(position = "stack") +
  facet_wrap(~country) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Educational Composition of Individuals (Aged 25–64)",
    x = "Year", y = "Share", fill = "Education Level"
  ) +
  theme_minimal()

ggsave("output/education_composition_over_time.png", p_edu_comp, width = 10, height = 6, dpi = 300)

#QQ Plot for Each Country
for (m in models) {
  country_code <- m$country
  qq_plot <- ggplot2::autoplot(m$model, which = 2)[[1]]
  ggsave(paste0("output/qq_plot_", country_code, ".png"), plot = qq_plot, width = 7, height = 5, dpi = 300)
}

model_coefs_df <- bind_rows(lapply(models, function(m) {
  broom::tidy(m$model, conf.int = TRUE) %>%
    mutate(country = m$country)
}), .id = "model_id")

#Regression Coefficient Summary Table
write.csv(model_coefs_df, "output/regression_coefficients_summary.csv", row.names = FALSE)

#Model Fit Summary Table
write.csv(reg_summary, "output/regression_model_fit_summary.csv", row.names = FALSE)

#Diagnostics Summary Table
diagnostics_df <- bind_rows(lapply(models, function(m) {
country <- m$country
residuals <- residuals(m$model)

data.frame(
  country = country,
  vif_mean = mean(car::vif(m$model)),  # mean VIF
  bptest_p = lmtest::bptest(m$model)$p.value,
  shapiro_p = shapiro.test(residuals)$p.value
)
}))

write.csv(diagnostics_df, "output/regression_diagnostics_summary.csv", row.names = FALSE)

#Residuals vs Fitted
resid_plot <- ggplot2::autoplot(m$model, which = 1)[[1]]
ggsave(paste0("output/resid_plot_", country_code, ".png"), plot = resid_plot, width = 7, height = 5, dpi = 300)

#inter-individual income inequality
  gini_individual <- individuals %>%
  group_by(country, year) %>%
  summarise(gini_indiv = Gini(personal_income), .groups = "drop")

ggplot(gini_individual, aes(x = year, y = gini_indiv)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Gini of Individual Income by Country and Year",
       x = "Year", y = "Gini Coefficient") +
  theme_minimal()

ggsave("output/gini_individual_over_time.png", width = 10, height = 6)

