library(tidyverse)

# %>% 

# read_csv()
# read_excel()

# select()
# filter()
# arrange()
# mutate()
# arrange()


# import CSV --------------------------------------------------------------
# df_input.csv <- read.csv("survey_input.csv") %>% print()
df_input <- read_csv("survey_input.csv")
# print(df_input)
# df_input %>% print()
# df_input <- read_csv("survey_input.csv") %>% print()

# import excel ------------------------------------------------------------
library(readxl)

df_names <- read_excel("df_names.xlsx", sheet = "df_names")
df_names %>% arrange(section)

df_names_a <- read_excel("df_names.xlsx", sheet = "df_names_section_A")
df_names_b <- read_excel("df_names.xlsx", sheet = "df_names_section_B")


# sample structure --------------------------------------------------------
# extract demographic section column names
v_d <- df_names %>% filter(section == "D") %>% .[["col_name"]]

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

df_sample_structure %>% print(n = nrow(.))

df_sample_structure %>% 
  filter(variable == "Age range") %>% 
  ggplot(aes(value, no_of_respondent))+
  geom_col()

df_sample_structure %>% 
  filter(variable == "Age range") %>% 
  ggplot(aes(reorder(value, no_of_respondent), no_of_respondent))+
  geom_col()+
  geom_text(aes(y = no_of_respondent + 15, label = proportion))+
  labs(x="Age group", y = "Number of respondents", title = "Number of respondents by age groups")+
  theme_bw()

df_sample_structure %>% 
  ggplot(aes(reorder(value, no_of_respondent), no_of_respondent))+
  geom_col()+
  geom_text(aes(y = no_of_respondent + 15, label = proportion))+
  facet_wrap(~variable, scales = "free_y")+
  labs(x="", y = "Number of respondents", title = "Number of respondents by age groups")+
  theme_bw()+
  geom_hline(yintercept = 30)+
  coord_flip()

ggplot(df_demo, aes(age))+
  geom_histogram()

ggplot(df_demo, aes(age))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(breaks = 20:70)+
  geom_vline(xintercept = 22, col = "red")+
  theme_bw()

df_demo %>% 
  ggplot(aes(age))+
  geom_density(aes(fill = gender), alpha = 0.4)

df_demo %>% 
  ggplot(aes(native.village, age, col = native.village))+
  geom_boxplot(alpha = 0)+
  geom_jitter()+
  theme_bw()

# data cleaning -----------------------------------------------------------
# df_input %>% select(ID, phu.quoc, phan.thiet)

# extract C-1 section column names 
v_c_1 <- df_names %>% filter(section == "C", question_no == 1) %>% .[["col_name"]]

df_input %>% 
  select(ID, v_c_1) %>% 
  mutate_at(vars(v_c_1), factor) %>%
  summary()

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
  geom_col()+
  coord_flip()+
  labs(x = "Brands", y = "Number of Respondents")

# proportion
df_brand_summary %>% 
  ggplot(aes(reorder(brand, proportion), proportion))+
  geom_col()+
  coord_flip()+
  labs(x = "Brands", y = "% Respondents")

ggsave(filename = "brand.png", width = 10, height = 6)

# brand duplication -------------------------------------------------------
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
  head(n = 10)

# section A ---------------------------------------------------------------
# extract A section column names 
v_a <- df_names %>% filter(section == "A") %>% .[["col_name"]]

respondent_labels <- 1:nrow(df_input)
respondent_col <- rainbow(nrow(df_input))


df_a <- df_input %>% select(v_a) %>% select(-ID)
names(df_a) <- df_names_a[["col_label"]]

ggplot(df_a, aes(Health, Sensory))+
  geom_point()

ggplot(df_a, aes(Health, Sensory))+
  geom_jitter(aes(col = Tradition))

ggplot(df_a, aes(Health, Sensory))+
  geom_jitter(aes(col = Tradition))+
  scale_x_continuous(breaks = 1:5, limits = c(1,5))+
  scale_y_continuous(breaks = 1:5, limits = c(1,5))

library(psych)
l_res_pca <- principal(df_a, nfactors=2, rotate="none")
df_res_pca_score <- l_res_pca[["scores"]] %>% tbl_df()

ggplot(df_res_pca_score, aes(PC1, PC2))+
  geom_text(aes(label = respondent_labels, col = respondent_col))+
  theme(legend.position = "none")

df_a %>% filter(row_number() == 19)
df_a %>% filter(row_number() == 133)

df_res_pca_loading <- 
  tibble("item" = names(df_a),
         "PC1" = l_res_pca[["loadings"]][1:6],
         "PC2" = l_res_pca[["loadings"]][7:12])

ggplot(df_res_pca_loading, aes(PC1, PC2))+
  geom_text(aes(label = item))

# section B ---------------------------------------------------------------
# extract B section column names 
v_b <- df_names %>% filter(section == "B") %>% .[["col_name"]]

df_input %>% summarise_at(vars(v_b), ~mean(.))

df_input %>% 
  group_by(gender) %>% 
  summarise_at(vars(v_b), ~mean(.))

df_b_long <- df_input %>% 
  select(ID, v_b) %>% 
  gather(variable, value, 2:ncol(.)) %>% 
  left_join(df_names_b, by = c("variable" = "col_name"))

df_b_long_summary <- df_b_long %>% 
  group_by(variable, col_des, col_des_vn, col_group) %>% 
  summarise(value = round(mean(value), 1)) %>% 
  ungroup() %>% 
  arrange(value)

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
  labs(x = "", y = "Number of respondents", fill = "Rating score")


