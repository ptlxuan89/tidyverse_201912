#' ---
#' title: "Consumer attitude towards fish sauce products"
#' author: "Xuan Pham"
#' date: "12/17/2019"
#' always_allow_html: true
#' # output: powerpoint_presentation
#' output:
#'   html_document:
#'     toc: yes
#'     toc_depth: 3
#'   word_document:
#'     toc: yes
#'   pdf_document:
#'     toc: yes
#'     highlight: tango
#'     df_print: kable
#'     citation_package: natbib
#'     keep_tex: true
#' editor_options:
#'   chunk_output_type: console
#' ---
#' <!-- <style> -->
#' <!--      body .main-container { -->
#' <!--          max-width: 1000px; -->
#' <!--      } -->
#' <!-- </style> -->
#' 
#' 
#' # 1. Setting up the working environment
#' 
## ---- include=F----------------------------------------------------------
kable_adj <- function(data, names = NA, ncol = NA, width = NA) {
  
  p.value.exist <- data %>% 
    names() %>% 
    str_detect("p.value") %>% 
    sum()
  
  if(p.value.exist >0){
    data <- data %>% mutate(p.value.check = ifelse(p.value <= 0.05, 1, 0))
  }
  
  selected_col <- data %>% summarise_if(is.numeric, ~min(., na.rm = T)) %>% select_if(., ~(.<0.01)) %>% names()
  
  data <- data %>% mutate_at(vars(selected_col), ~formatC(., format = "g", digits = 3))

  if(p.value.exist >0){
    data <- data %>% 
      mutate(p.value = cell_spec(p.value, 
                                 "html", 
                                 color = ifelse(
                                   p.value.check == "   1", 
                                   "red", 
                                   "#333333"
                                   ))) %>%
      select(-p.value.check)
  }
  
  default_table <- function(data) {
    knitr::kable(
      data, 
      booktabs = TRUE,
      format = "html",
      escape = F,
      digits = 3,
      format.args = list(big.mark = ","),
      col.names = names
      ) %>% 
      kableExtra::kable_styling(
        full_width = F,
        position = "left",
        bootstrap_options = c("condensed", "bordered")
        ) %>% 
      row_spec(0, bold = T, color = "black", background = "#61bbed")
  }
  if (is.na(sum(ncol))) {
    default_table(data)
  } else {
    default_table(data) %>% 
      column_spec(ncol, width)
  }
}

v_palette_pander <- c("#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#999999","#E69F00","#56B4E9","#009E73")

#' 
## ------------------------------------------------------------------------
library(tidyverse)
library(knitr)
library(kableExtra)

#' 
#' # 2. Import data
#' 
## ------------------------------------------------------------------------
df_input <- read_csv("survey_input.csv")

#' 
## ------------------------------------------------------------------------
# import excel ------------------------------------------------------------
library(readxl)

df_names <- read_excel("df_names.xlsx", sheet = "df_names")
df_names_a <- read_excel("df_names.xlsx", sheet = "df_names_section_A")
df_names_b <- read_excel("df_names.xlsx", sheet = "df_names_section_B")

#' 
#' # 3. Sample structure understanding
#' 
#' ## 3.1 Categorical variables
#' 
#' Majority of respondents are female in their 20s (20-24 yo), obtained University degree, having full-time jobs and originally from the South. 
#' 
#' ### 3.1.1 Sample structure table summary
## ------------------------------------------------------------------------
# sample structure --------------------------------------------------------
df_input <- df_input %>% 
  mutate(native.village = factor(native.village, 
                                 levels = c("North", "Middle", "South"),
                                 labels = c("North", "Central", "South")),
         gender = factor(gender, 
                         levels = c("male", "female"),
                         labels = c("Male", "Female")),
         job = factor(job, 
                      levels = c("student", "part.time", "full.time", "housework"), 
                      labels = c("Student", "Part-time", "Full-time", "Housework")),
         educationlevel = factor(educationlevel, 
                                 levels = c("below.hs", "highschool", "university"),
                                 labels = c("Below high school", "High school", "University")),
         age.range = factor(age.range, 
                            levels = c("20_24t", "25_35t", ">35t"),
                            labels = c("20-24 yo", "25-35 yo", "Above 35 yo")))

# extract demographic section column names
v_d <- df_names %>% filter(section == "D") %>% .[["col_name"]]
df_demo <- df_input %>% select(ID, v_d)

df_sample_structure <-
  df_demo %>% 
  select_if(negate(is.double)) %>%
  gather(variable, value, 2:ncol(.)) %>%
  group_by(variable, value) %>%
  summarise(no_of_respondent = n()) %>%
  mutate(proportion = round(no_of_respondent / 300 * 100, 2)) %>%
  ungroup() %>%
  mutate(variable = factor(variable, 
                           levels = c("age.range", "educationlevel", "gender", "job", "native.village"),
                           labels = c("Age range", "Education level", "Gender", "Job", "Native village"))) %>% 
  arrange(variable, desc(proportion))

df_sample_structure %>% kable_adj(names = c("Variable", "Level", "No. of respondents", "% Respondents"))

#' 
#' ### 3.1.2 Sample structure plot
## ------------------------------------------------------------------------
ggplot(df_sample_structure, aes(reorder(value,no_of_respondent), no_of_respondent))+
  geom_col()+
  coord_flip()+
  facet_wrap(~variable, scales = "free_y")+
  theme_bw()

df_sample_structure %>% 
  ggplot(aes(reorder(value, no_of_respondent), no_of_respondent))+
  geom_col(fill = v_palette_pander[1])+
  geom_text(aes(y = no_of_respondent + 25, label = proportion))+
  facet_wrap(~variable, scales = "free_y")+
  labs(x="", y = "Number of respondents", title = "Number of respondents by age groups")+
  theme_bw()+
  geom_hline(yintercept = 30)+
  coord_flip()

#' 
#' ## 3.2 Continuous variables
#' 
#' Respondent age is not normally distributed. 1/3 respondents are 22 years old. 
#' 
## ------------------------------------------------------------------------
ggplot(df_demo, aes(age))+
  geom_histogram(binwidth = 1,
                 fill = v_palette_pander[1])+
  scale_x_continuous(breaks = 20:70)+
  geom_vline(xintercept = 22, col = "red")+
  labs(title = "Respondents' age distribution", x = "Age", y = "Number of respondents")+
  theme_bw()

#' 
#' ## 3.3 Continuous vs. categorical variables
#' 
#' Female respondents age is extremely right skewed. 
## ------------------------------------------------------------------------
df_demo %>% 
  ggplot(aes(age))+
  geom_density(aes(fill = gender, col = gender), alpha = 0.4)+
  scale_fill_manual(values = v_palette_pander[c(4,5)])+
  scale_color_manual(values = v_palette_pander[c(4,5)])+
  labs(title = "Respondents' age distribution by gender", 
      x = "Age", 
      y = "Density")+
  theme_bw()

#' 
#' Age distributions of those who are from the South and Central are wider spread than the North group.
## ------------------------------------------------------------------------
df_demo %>% 
  ggplot(aes(native.village, age, col = native.village))+
  geom_boxplot(alpha = 0)+
  geom_jitter()+
  scale_color_manual(values = v_palette_pander[c(4,5,7)])+
  labs(title = "Respondents' age distribution by native village",
       x = "Native village",
       y = "Age")+
  theme_bw()

#' 
#' # 4. Data cleaning
#' 
#' Coding error is spotted! Each variable in brand section should have two levels only. Khai hoan brand has 4 levels.  
## ------------------------------------------------------------------------
# extract C-1 section column names 
v_c_1 <- df_names %>% filter(section == "C", question_no == 1) %>% .[["col_name"]]

df_input %>% 
  select(ID, v_c_1) %>% 
  mutate_at(vars(v_c_1), factor) %>%
  summary()

#' 
#' Nam Ngu is the most popular brand and almost triple the second player - Chinsu in terms of the number of responses.
#' 
## ------------------------------------------------------------------------
# recode multiple answers
df_brand <- df_input %>% 
  select(ID, v_c_1) %>%
  gather(brand, value, 2:ncol(.)) %>%
  mutate(recoded_value = ifelse(brand == value, "Y", "N")) %>%
  mutate(recoded_value = replace_na(recoded_value, "N")) %>%
  filter(recoded_value == "Y") %>%
  select(-value,-recoded_value)


# brand summary -----------------------------------------------------------
df_brand_summary <- df_brand %>% 
  group_by(brand) %>%
  summarise(no_of_response = n()) %>%
  arrange(desc(no_of_response)) %>%
  mutate(proportion = round(no_of_response / 300 * 100, 1))

# absolute
df_brand_summary %>%  
  ggplot(aes(reorder(brand, no_of_response), no_of_response))+
  geom_col(fill = v_palette_pander[1])+
  geom_text(aes(y = 170, label = proportion))+
  coord_flip()+
  labs(x = "Brands", y = "Number of Respondents",
       caption = "Numbers represent % respondents")+
  theme_bw()

ggsave(filename = "brand.png", width = 10, height = 6)

#' 
#' There are few respondents gave more than the required number of responses (maximum is 2).
## ------------------------------------------------------------------------
df_brand_count <- df_brand %>% 
  group_by(ID) %>% 
  summarise(brand_count = n_distinct(brand)) %>% 
  ungroup() 

df_brand %>% 
  group_by(ID) %>% 
  mutate(brandID = row_number()) %>% 
  ungroup() %>% 
  select(ID, brand, brandID) %>% 
  spread(brandID, brand) %>% 
  left_join(df_brand_count, by = "ID") %>% 
  arrange(desc(brand_count)) %>% 
  head(n = 10) %>% 
  kable_adj(names = c("Respondent ID", "Brand 1", "Brand 2", "Brand 3", "Brand 4", "Number of brands declared"))

#' 
#' # 5. Data exploration
#' ## 5.1 Key influencing factors
#' 
#' Average rating score of 6 key factors:
## ------------------------------------------------------------------------
# section A ---------------------------------------------------------------
# extract A section column names 
v_a <- df_names %>% filter(section == "A") %>% .[["col_name"]]

df_a <- df_input %>% select(v_a) %>% select(-ID)
names(df_a) <- df_names_a[["col_label"]]

df_a %>% 
  summarise_all(~round(mean(.), 2)) %>% 
  kable_adj()

#' **Quality** seems to be the most influencing factor. 
#' 
#' 
#' The challenge of visualing ordinal categorical variables:
## ------------------------------------------------------------------------
ggplot(df_a, aes(Health, Sensory))+
  geom_point()+
  theme_bw()

#' 
#' Using 'jitter' trick...
#' 
## ------------------------------------------------------------------------
library(GGally)

df_a %>% 
  ggpairs(lower = list(continuous=wrap("points", position="jitter", alpha = 0.2)))

#' 
#' ... or using dimension reduction technique?
## ------------------------------------------------------------------------
library(psych)
l_res_pca <- principal(df_a, nfactors=2, rotate="none")
df_res_pca_score <- l_res_pca[["scores"]] %>% tbl_df()

respondent_labels <- 1:nrow(df_input)
respondent_col <- rainbow(nrow(df_input))

df_res_pca_loading <- 
  tibble("item" = names(df_a),
         "PC1" = l_res_pca[["loadings"]][1:6],
         "PC2" = l_res_pca[["loadings"]][7:12])

ggplot(df_res_pca_loading, aes(PC1, PC2))+
  geom_text(aes(label = item))+
  scale_x_continuous(breaks = seq(0,1, 0.1), limits = c(0,1))+
  scale_y_continuous(breaks = seq(-1, 1, 0.1), limits = c(-1,1))+
  theme_bw()

respondent_labels <- 1:nrow(df_input)
# respondent_col <- rainbow(nrow(df_input))

bind_cols(df_res_pca_score, df_demo) %>% 
  ggplot(aes(PC1, PC2))+
  geom_text(aes(label = respondent_labels, col = gender), 
            position = position_jitter(height = 0.4))+
  theme_bw()+
  theme(legend.position = "bottom")
#' 
#' **Revealing special cases:**
#' 
#' **Respondent 19:**
## ------------------------------------------------------------------------
df_a %>% filter(row_number() == 19) %>% kable_adj()

#' 
#' **Respondent 133:**
## ------------------------------------------------------------------------
df_a %>% filter(row_number() == 133) %>% kable_adj()

#' 
#' 
#' ## 5.2 Important factors
#' 
#' Looking into sub-factors, the **product origin** seems to be the most concerning factor when choosing fish sauce products. 
## ------------------------------------------------------------------------
# section B ---------------------------------------------------------------
# extract B section column names 
v_b <- df_names %>% filter(section == "B") %>% .[["col_name"]]

df_input %>% 
  summarise_at(vars(v_b), ~round(mean(.),2)) %>% 
  gather(variable, value, 1:ncol(.)) %>% 
  left_join(df_names_b, by = c("variable" = "col_name")) %>%
  arrange(col_group, col_des) %>% 
  select(col_des, value) %>% 
  mutate(value = ifelse(value > 4,
                  cell_spec(value, color = "#1e90ff", bold = T),
                  cell_spec(value, color = "black"))) %>% 
  kable_adj(names = c("Factors", "Value")) %>% 
  pack_rows("Convenience", 1, 4) %>%
  pack_rows("Health", 5, 7) %>%
  pack_rows("Price", 8, 10) %>%
  pack_rows("Quality and safety", 11, 14) %>%
  pack_rows("Sensory appeal", 15, 19) %>%
  pack_rows("Traditional value", 20, 24)
  
df_b_long <- df_input %>% 
  select(ID, v_b) %>% 
  gather(variable, value, 2:ncol(.)) %>% 
  left_join(df_names_b, by = c("variable" = "col_name"))

df_b_long_summary <- df_b_long %>% 
  group_by(variable, col_des, col_des_vn, col_group) %>% 
  summarise(value = round(mean(value), 1)) %>% 
  ungroup() %>% 
  arrange(value)

#' 
## ----fig.height=8, fig.width=10------------------------------------------
df_b_long %>% 
  group_by(variable, col_des, col_des_vn, col_group, value) %>% 
  summarise(no_of_response = n()) %>%  
  ungroup() %>% 
  ggplot(aes(col_des, no_of_response))+
  geom_col(aes(fill = value))+
  geom_text(aes(y = 320, label = value, col = value, size = value), data = df_b_long_summary, fontface = "bold")+
  coord_flip()+
  scale_fill_gradient(high = "#132B43", low = "#56B1F7")+
  scale_color_gradient(high = "#132B43", low = "#56B1F7")+
  facet_grid(col_group ~ ., scales = "free_y")+
  theme(axis.text = element_text(size = 12),
        legend.position = "bottom")+
  guides(col = F,
         size = F)+
  labs(x = "", y = "Number of respondents", fill = "Rating score")+
  theme_bw()

#' 
#' The origin factor is highly important for both male and female groups. 
#' Male respondents seem to give higher scores in most of the factors compared to their counterparts. However, female respondents seem to care more about whether the products are available in shops, keeping them healthy and reflecting the local culture than male. 
#' 
## ----fig.height=8, fig.width=10------------------------------------------
df_input %>% 
  group_by(gender) %>% 
  summarise_at(vars(v_b), ~round(mean(.),2)) %>% 
  gather(variable, value, 2:ncol(.)) %>% 
  left_join(df_names_b, by = c("variable" = "col_name")) %>% 
  select(col_group, col_des, gender, value) %>% 
  ggplot(aes(value, reorder(col_des, value), col = gender, group = gender, shape = gender))+
  geom_point(size = 2)+
  scale_color_manual(values = v_palette_pander[c(4,5)])+
  facet_grid(col_group ~., scales = "free_y")+
  labs(x = "Rating score", y = "Factors", fill = "Gender")+
  theme_bw()

#' 
## ------------------------------------------------------------------------
df_input %>% 
  group_by(gender) %>% 
  summarise_at(vars(v_b), ~round(mean(.),2)) %>% 
  gather(variable, value, 2:ncol(.)) %>% 
  left_join(df_names_b, by = c("variable" = "col_name")) %>% 
  select(col_group, col_des, gender, value) %>% 
  spread(gender, value) %>% 
  arrange(col_group, col_des) %>% 
  select(col_des, Male, Female) %>% 
  mutate_if(is.numeric, function(x) {
    cell_spec(x, bold = T, 
              color = spec_color(x, end = 0.9),
              font_size = spec_font_size(x))
  }) %>% 
  kable_adj(names = c("Factors", "Male", "Female")) %>% 
  pack_rows("Convenience", 1, 4) %>%
  pack_rows("Health", 5, 7) %>% 
   pack_rows("Price", 8, 10) %>%
  pack_rows("Quality and safety", 11, 14) %>%
  pack_rows("Sensory appeal", 15, 19) %>%
  pack_rows("Traditional value", 20, 24)

#' 

