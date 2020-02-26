library(tidyverse)
library(tidycensus)

label <- load_variables(year = 2018, dataset = "acs1") %>% 
  rename(variable = name) %>% 
  select(-concept)

asn_lb <- load_variables(year = 2018, dataset = "acs1", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02015")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02015_004", "B02015_010", "B02015_014", "B02015_015", "B02015_017", "B02015_019", "B02015_023", "B02015_024", "B02015_025"))
asn_lb[1,2] <- "Asian American"
asn_lb[6,2] <- "Chinese"


nhpi_lb <- load_variables(year = 2018, dataset = "acs1", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02016")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2", "var3", "label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(variable %in% c("B02016_001", "B02016_002", "B02016_003", "B02016_004", "B02016_009"))
nhpi_lb[1,2] <- "Pacific Islander"


#pop2 alone--------------
data <- get_acs(variables = "B01003_001", year = 2018, survey = "acs1", geography = "us") %>% 
  mutate(group = "US Total",
         label = "pop2") %>% 
  select(group, label, estimate)

asn_lb <- load_variables(year = 2018, dataset = "acs1", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02015")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02015_004", "B02015_010", "B02015_014", "B02015_015", "B02015_017", "B02015_019", "B02015_023", "B02015_024", "B02015_025"))
asn_lb[1,2] <- "Asian American"
asn_lb[6,2] <- "Chinese"

data2 <- get_acs(table = "B02015", year = 2018, survey = "acs1", geography = "us") %>% 
  filter(!variable %in% c("B02015_004", "B02015_010", "B02015_014", "B02015_015", "B02015_017", "B02015_019", "B02015_023", "B02015_024", "B02015_025")) %>% 
  left_join(asn_lb) %>% 
  rename(group = label) %>% 
  mutate(label = "pop2") %>% 
  select(group, label, estimate)

nhpi_lb <- load_variables(year = 2018, dataset = "acs1", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02016")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2", "var3", "label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(variable %in% c("B02016_001", "B02016_002", "B02016_003", "B02016_004", "B02016_009"))
nhpi_lb[1,2] <- "Pacific Islander"

data3 <- get_acs(table = "B02016", year = 2018, survey = "acs1", geography = "us") %>% 
  filter(variable %in% c("B02016_001", "B02016_002", "B02016_003", "B02016_004", "B02016_009")) %>% 
  left_join(nhpi_lb) %>% 
  rename(group = label) %>% 
  mutate(label = "pop2") %>% 
  select(group, label, estimate)

dta_pop2 <- rbind(data, data2, data3)
dta_pop2$group <- factor(dta_pop2$group, levels = c("US Total", "Asian American","Pacific Islander","Asian Indian",
                                                    "Bangladeshi","Burmese","Cambodian","Chinese","Fijian",
                                                    "Filipino","Hmong","Japanese","Korean","Laotian",
                                                    "Native Hawaiian","Nepalese","Pakistani","Samoan",
                                                    "Taiwanese","Thai","Tongan","Vietnamese"))
dta_pop2 <- dta_pop2 %>% 
  spread(label, estimate) %>% 
  arrange(group) %>% 
  write_csv("nationaldata_2018/dta_pop2.csv")

#pop (alone or in combo)--------------
##data labels
asn_lb <- load_variables(year = 2018, dataset = "acs1", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02018")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02018_004", "B02018_010", "B02018_014", "B02018_015", "B02018_017", "B02018_019", "B02018_023", "B02018_024", "B02018_025"))
asn_lb[1,2] <- "Asian American"
asn_lb[6,2] <- "Chinese"

nhpi_lb <- load_variables(year = 2018, dataset = "acs1", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02019")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2", "var3", "label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(variable %in% c("B02019_001", "B02019_002", "B02019_003", "B02019_004", "B02019_009"))
nhpi_lb[1,2] <- "Pacific Islander"

data <- get_acs(variables = "B01003_001", year = 2018, survey = "acs1", geography = "us") %>% 
  mutate(group = "US Total",
         label = "pop") %>% 
  select(group, label, estimate)

data2 <- get_acs(table = "B02018", year = 2018, survey = "acs1", geography = "us") %>% 
  filter(!variable %in% c("B02018_004", "B02018_010", "B02018_014", "B02018_015", "B02018_017", "B02018_019", "B02018_023", "B02018_024", "B02018_025")) %>% 
  left_join(asn_lb) %>% 
  rename(group = label) %>% 
  mutate(label = "pop") %>% 
  select(group, label, estimate)

data3 <- get_acs(table = "B02019", year = 2018, survey = "acs1", geography = "us") %>% 
  filter(variable %in% c("B02019_001", "B02019_002", "B02019_003", "B02019_004", "B02019_009")) %>% 
  left_join(nhpi_lb) %>% 
  rename(group = label) %>% 
  mutate(label = "pop") %>% 
  select(group, label, estimate)

dta_pop <- rbind(data, data2, data3)
dta_pop$group <- factor(dta_pop$group, levels = c("US Total", "Asian American","Pacific Islander","Asian Indian",
                                                    "Bangladeshi","Burmese","Cambodian","Chinese","Fijian",
                                                    "Filipino","Hmong","Japanese","Korean","Laotian",
                                                    "Native Hawaiian","Nepalese","Pakistani","Samoan",
                                                    "Taiwanese","Thai","Tongan","Vietnamese"))
dta_pop <- dta_pop %>% 
  spread(label, estimate) %>% 
  arrange(group) %>% 
  write_csv("nationaldata_2018/dta_pop.csv")

#pop growth------------
dta_hist <- read_csv("nationaldata_2018/pop_2010_2000.csv")
dta_growth <- dta_pop2 %>% 
  left_join(dta_hist) %>% 
  mutate(pct_pop_growth_2010_alone = (pop2 - alone_2010)/alone_2010,
         pct_pop_growth_2000_alone = (pop2 - alone_2000)/alone_2000,
         pct_pop_growth_2010_combo = (pop2 - combo_2010)/combo_2010,
         pct_pop_growth_2000_combo = (pop2 - combo_2000)/combo_2000) %>% 
  select(-pop2, -alone_2010, -alone_2000,
         -combo_2010, -combo_2000)

dta_growth$group <- factor(dta_growth$group, levels = c("US Total", "Asian American","Pacific Islander","Asian Indian",
                                                  "Bangladeshi","Burmese","Cambodian","Chinese","Fijian",
                                                  "Filipino","Hmong","Japanese","Korean","Laotian",
                                                  "Native Hawaiian","Nepalese","Pakistani","Samoan",
                                                  "Taiwanese","Thai","Tongan","Vietnamese"))
write_csv(dta_growth, "nationaldata_2018/dta_growth.csv")


#Top States------
data <- get_acs(variables = "B01003_001", year = 2018, survey = "acs1", geography = "state") %>% 
  mutate(group = "US Total") %>% 
  select(group, NAME, estimate)

asn_lb <- load_variables(year = 2018, dataset = "acs1", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02015")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2","label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(!variable %in% c("B02015_004", "B02015_010", "B02015_014", "B02015_015", "B02015_017", "B02015_019", "B02015_023", "B02015_024", "B02015_025"))
asn_lb[1,2] <- "Asian American"
asn_lb[6,2] <- "Chinese"

data2 <- get_acs(table = "B02015", year = 2018, survey = "acs1", geography = "state") %>% 
  filter(!variable %in% c("B02015_004", "B02015_010", "B02015_014", "B02015_015", "B02015_017", "B02015_019", "B02015_023", "B02015_024", "B02015_025")) %>% 
  left_join(asn_lb) %>% 
  rename(group = label) %>% 
  select(group, NAME, estimate)

nhpi_lb <- load_variables(year = 2018, dataset = "acs1", cache = TRUE) %>%
  mutate(keep = str_detect(name, "B02016")) %>% filter(keep == "TRUE") %>%
  separate(label, c("var1", "var2", "var3", "label"), sep = "!!") %>%
  rename(variable = name) %>% dplyr::select(variable, label) %>% 
  filter(variable %in% c("B02016_001", "B02016_002", "B02016_003", "B02016_004", "B02016_009"))
nhpi_lb[1,2] <- "Pacific Islander"

data3 <- get_acs(table = "B02016", year = 2018, survey = "acs1", geography = "state") %>% 
  filter(variable %in% c("B02016_001", "B02016_002", "B02016_003", "B02016_004", "B02016_009")) %>% 
  left_join(nhpi_lb) %>% 
  rename(group = label) %>% 
  select(group, NAME, estimate)

dta_topstates <- rbind(data, data2, data3)
dta_topstates$group <- factor(dta_topstates$group, levels = c("US Total", "Asian American","Pacific Islander","Asian Indian",
                                                    "Bangladeshi","Burmese","Cambodian","Chinese","Fijian",
                                                    "Filipino","Hmong","Japanese","Korean","Laotian",
                                                    "Native Hawaiian","Nepalese","Pakistani","Samoan",
                                                    "Taiwanese","Thai","Tongan","Vietnamese"))
dta_top1 <- dta_topstates %>% 
  group_by(group) %>% 
  arrange(desc(estimate)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 5) %>% 
  summarise(top_states =paste(NAME,collapse=', ')) %>% 
  ungroup()

dta_top2 <- dta_topstates %>% 
  group_by(group) %>% 
  arrange(desc(estimate)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 5) %>% 
  mutate(label = paste("state", rank, sep = "")) %>% 
  select(-NAME, -rank) %>% 
  spread(label, estimate) %>% 
  arrange(group)

dta_top <- dta_topstates %>% 
  group_by(group) %>% 
  arrange(desc(estimate)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 1) %>% 
  ungroup() %>% select(group, estimate) %>% 
  rename(tot_state_pop = estimate) %>% 
  left_join(dta_top1) %>% left_join(dta_top2) %>% 
  arrange(group) %>% 
  write_csv("nationaldata_2018/dta_topstates.csv")

#educational attainment------
lessHS <- c("B15002_003", "B15002_004", "B15002_005", "B15002_006", "B15002_007", 
            "B15002_008", "B15002_009", "B15002_010", "B15002_020", "B15002_021", 
            "B15002_022", "B15002_023", "B15002_024", "B15002_025", "B15002_026", 
            "B15002_027")
HS <- c("B15002_011", "B15002_028")

BAhigher <- c("B15002_015", "B15002_016", "B15002_017", "B15002_018", 
              "B15002_032", "B15002_033", "B15002_034", "B15002_035")
data <- get_acs(table = "B15002", geography = "us", year = 2018, summary_var = "B15002_001") %>% 
  left_join(label) %>% 
  rename(lb = label) %>% 
  mutate(group = "US Total",
         label = case_when(
    variable %in% lessHS ~"pct_lesshs",
    variable %in% HS ~"pct_hsged",
    variable %in% BAhigher ~"pct_bahigher",
    TRUE ~NA_character_)) %>% 
  filter(is.na(label)==F) %>% 
  select(group, label, estimate, summary_est) %>% 
  group_by(label) %>% 
  mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% 
  select(group, label, pct)

data2 <- get_acs(table = "C15002D", geography = "us", year = 2018, summary_var = "C15002D_001") %>% 
  left_join(label) %>% 
  mutate(group = "Asian American",
         lb = case_when(
    label=="Estimate!!Total!!Male!!Less than high school diploma" ~"pct_lesshs",
    label=="Estimate!!Total!!Female!!Less than high school diploma" ~"pct_lesshs",
    label=="Estimate!!Total!!Male!!High school graduate (includes equivalency)" ~"pct_hsged",
    label=="Estimate!!Total!!Female!!High school graduate (includes equivalency)" ~"pct_hsged",
    label=="Estimate!!Total!!Male!!Bachelor's degree or higher" ~"pct_bahigher",
    label=="Estimate!!Total!!Female!!Bachelor's degree or higher" ~"pct_bahigher",
    TRUE ~NA_character_)) %>% 
  filter(is.na(lb)==F) %>% 
  select(group, lb, estimate, summary_est) %>% 
  group_by(lb) %>% 
  mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% 
  select(group, lb, pct) %>% 
  rename(label = lb)

data3 <- get_acs(table = "C15002E", geography = "us", year = 2018, summary_var = "C15002E_001") %>% 
  left_join(label) %>% 
  mutate(group = "Pacific Islander",
         lb = case_when(
           label=="Estimate!!Total!!Male!!Less than high school diploma" ~"pct_lesshs",
           label=="Estimate!!Total!!Female!!Less than high school diploma" ~"pct_lesshs",
           label=="Estimate!!Total!!Male!!High school graduate (includes equivalency)" ~"pct_hsged",
           label=="Estimate!!Total!!Female!!High school graduate (includes equivalency)" ~"pct_hsged",
           label=="Estimate!!Total!!Male!!Bachelor's degree or higher" ~"pct_bahigher",
           label=="Estimate!!Total!!Female!!Bachelor's degree or higher" ~"pct_bahigher",
           TRUE ~NA_character_)) %>% 
  filter(is.na(lb)==F) %>% 
  select(group, lb, estimate, summary_est) %>% 
  group_by(lb) %>% 
  mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% 
  select(group, lb, pct) %>% 
  rename(label = lb)

dta_edu <- rbind(data, data2, data3)
dta_edu$group <- factor(dta_edu$group, levels = c("US Total", "Asian American","Pacific Islander","Asian Indian",
                                                              "Bangladeshi","Burmese","Cambodian","Chinese","Fijian",
                                                              "Filipino","Hmong","Japanese","Korean","Laotian",
                                                              "Native Hawaiian","Nepalese","Pakistani","Samoan",
                                                              "Taiwanese","Thai","Tongan","Vietnamese"))
dta_edu <- dta_edu %>% 
  spread(label, pct) %>% 
  select(group, pct_lesshs, pct_hsged, pct_bahigher) %>% 
  write_csv("nationaldata_2018/dta_edu.csv")

#poverty------------
data <- get_acs(table = "B17001", geography = "us", year = 2018, summary_var = "B17001_001") %>% 
  mutate(group = "US Total",
         label = case_when(
    variable %in% c("B17001_004", "B17001_005", "B17001_006", 
                    "B17001_007", "B17001_008", "B17001_009", 
                    "B17001_018", "B17001_019", "B17001_020", 
                    "B17001_021", "B17001_022", "B17001_023") ~"pct_pov_youth",
    variable %in% c("B17001_015", "B17001_016", "B17001_029", "B17001_030") ~"pct_pov_senior",
    variable %in% c("B17001_002") ~"pct_pov",
    TRUE ~NA_character_)) %>% 
  select(group, label, estimate, summary_est) %>% 
  filter(is.na(label)==F) %>%
    group_by(label) %>% 
    mutate(estimate = sum(estimate)) %>% 
    ungroup() %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% 
  select(group, label, pct) %>% 
  spread(label, pct)

data2 <- get_acs(table = "B17001D", geography = "us", year = 2018, summary_var = "B17001D_001") %>% 
  mutate(group = "Asian American",
         label = case_when(
           variable %in% c("B17001D_004", "B17001D_005", "B17001D_006", 
                           "B17001D_007", "B17001D_008", "B17001D_009", 
                           "B17001D_018", "B17001D_019", "B17001D_020", 
                           "B17001D_021", "B17001D_022", "B17001D_023") ~"pct_pov_youth",
           variable %in% c("B17001D_015", "B17001D_016", "B17001D_029", "B17001D_030") ~"pct_pov_senior",
           variable %in% c("B17001D_002") ~"pct_pov",
           TRUE ~NA_character_)) %>% 
  select(group, label, estimate, summary_est) %>% 
  filter(is.na(label)==F) %>%
  group_by(label) %>% 
  mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% 
  select(group, label, pct) %>% 
  spread(label, pct)

data3 <- get_acs(table = "B17001E", geography = "us", year = 2018, summary_var = "B17001E_001") %>% 
  select(variable, estimate, summary_est) %>% 
  mutate(group = "Pacific Islander",
         label = case_when(
           variable %in% c("B17001E_004", "B17001E_005", "B17001E_006", 
                           "B17001E_007", "B17001E_008", "B17001E_009", 
                           "B17001E_018", "B17001E_019", "B17001E_020", 
                           "B17001E_021", "B17001E_022", "B17001E_023") ~"pct_pov_youth",
           variable %in% c("B17001E_015", "B17001E_016", "B17001E_029", "B17001E_030") ~"pct_pov_senior",
           variable %in% c("B17001E_002") ~"pct_pov",
           TRUE ~NA_character_)) %>% 
  select(group, label, estimate, summary_est) %>% 
  filter(is.na(label)==F) %>%
  group_by(label) %>% 
  mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% 
  select(group, label, pct) %>% 
  spread(label, pct)

dta_pov <- rbind(data, data2, data3)
write_csv(dta_pov, "nationaldata_2018/dta_pov.csv")

#nativity----------
 #1.foreign born-----
data <- get_acs(table = "B05003", geography = "us", year = 2018, summary_var = "B05003_001") %>% 
  mutate(group = "US Total",
         label1 = case_when(
           variable %in% c("B05003_005", "B05003_010", 
                           "B05003_016", "B05003_021") ~"pct_foreign",
           TRUE ~NA_character_)) %>% 
  select(group, label1, estimate, summary_est) %>% 
  group_by(label1) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label1)==F) %>% select(group, label1, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

data2 <- get_acs(table = "B05003D", geography = "us", year = 2018, summary_var = "B05003D_001") %>% 
  mutate(group = "Asian American",
         label1 = case_when(
           variable %in% c("B05003D_005", "B05003D_010", 
                           "B05003D_016", "B05003D_021") ~"pct_foreign",
           TRUE ~NA_character_)) %>% 
  select(group, label1, estimate, summary_est) %>% 
  group_by(label1) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label1)==F) %>% select(group, label1, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

data3 <- get_acs(table = "B05003E", geography = "us", year = 2018, summary_var = "B05003E_001") %>% 
  mutate(group = "Pacific Islander",
         label1 = case_when(
           variable %in% c("B05003E_005", "B05003E_010", 
                           "B05003E_016", "B05003E_021") ~"pct_foreign",
           TRUE ~NA_character_)) %>% 
  select(group, label1, estimate, summary_est) %>% 
  group_by(label1) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label1)==F) %>% select(group, label1, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

dta1 <- rbind(data, data2, data3)
dta1 <- dta1 %>% spread(label1, pct)

#2.citizen-----
data <- get_acs(table = "B05003", geography = "us", year = 2018, summary_var = "B05003_001") %>% 
  mutate(group = "US Total",
         label2 = case_when(
           variable %in% c("B05003_004", "B05003_006", "B05003_009", 
                           "B05003_011", "B05003_015", "B05003_017", 
                           "B05003_020", "B05003_022") ~"pct_citizen",
           TRUE ~NA_character_)) %>% 
  select(group, label2, estimate, summary_est) %>% 
  group_by(label2) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label2)==F) %>% 
  select(group, label2, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

data2 <- get_acs(table = "B05003D", geography = "us", year = 2018, summary_var = "B05003D_001") %>% 
  mutate(group = "Asian American",
         label2 = case_when(
           variable %in% c("B05003D_004", "B05003D_006", "B05003D_009", 
                           "B05003D_011", "B05003D_015", "B05003D_017", 
                           "B05003D_020", "B05003D_022") ~"pct_citizen",
           TRUE ~NA_character_)) %>% 
  select(group, label2, estimate, summary_est) %>% 
  group_by(label2) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label2)==F) %>% 
  select(group, label2, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

data3 <- get_acs(table = "B05003E", geography = "us", year = 2018, summary_var = "B05003E_001") %>% 
  mutate(group = "Pacific Islander",
         label2 = case_when(
           variable %in% c("B05003E_004", "B05003E_006", "B05003E_009", 
                           "B05003E_011", "B05003E_015", "B05003E_017", 
                           "B05003E_020", "B05003E_022") ~"pct_citizen",
           TRUE ~NA_character_)) %>% 
  select(group, label2, estimate, summary_est) %>% 
  group_by(label2) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label2)==F) %>% 
  select(group, label2, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

dta2 <- rbind(data, data2, data3)
dta2 <- dta2 %>% spread(label2, pct)

#3.CVAP-----
data <- get_acs(table = "B05003", geography = "us", year = 2018, summary_var = "B05003_001") %>% 
  mutate(group = "US Total",
         label3 = case_when(
           variable %in% c("B05003_009", "B05003_011", 
                           "B05003_020", "B05003_022") ~"pct_cvap",
           TRUE ~NA_character_)) %>% 
  select(group, label3, estimate, summary_est) %>% 
  group_by(label3) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label3)==F) %>% 
  select(group, label3, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

data2 <- get_acs(table = "B05003D", geography = "us", year = 2018, summary_var = "B05003D_001") %>% 
  mutate(group = "Asian American",
         label3 = case_when(
           variable %in% c("B05003D_009", "B05003D_011", 
                           "B05003D_020", "B05003D_022") ~"pct_cvap",
           TRUE ~NA_character_)) %>% 
  select(group, label3, estimate, summary_est) %>% 
  group_by(label3) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label3)==F) %>% 
  select(group, label3, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

data3 <- get_acs(table = "B05003E", geography = "us", year = 2018, summary_var = "B05003E_001") %>% 
  mutate(group = "Pacific Islander",
         label3 = case_when(
           variable %in% c("B05003E_009", "B05003E_011", 
                           "B05003E_020", "B05003E_022") ~"pct_cvap",
           TRUE ~NA_character_)) %>% 
  select(group, label3, estimate, summary_est) %>% 
  group_by(label3) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label3)==F) %>% 
  select(group, label3, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

dta3 <- rbind(data, data2, data3)
dta3 <- dta3 %>% spread(label3, pct)

#foreign born citizen
data <- get_acs(table = "B05003", geography = "us", year = 2018, summary_var = "B05003_001") %>% 
  mutate(group = "US Total",
         label4 = case_when(
           variable %in% c("B05003_006", "B05003_011", 
                           "B05003_017", "B05003_022") ~"pct_foreign_cit",
           TRUE ~NA_character_)) %>% 
  select(group, label4, estimate, summary_est) %>% 
  group_by(label4) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label4)==F) %>% 
  select(group, label4, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

data2 <- get_acs(table = "B05003D", geography = "us", year = 2018, summary_var = "B05003D_001") %>% 
  mutate(group = "Asian American",
         label4 = case_when(
           variable %in% c("B05003D_006", "B05003D_011", 
                           "B05003D_017", "B05003D_022") ~"pct_foreign_cit",
           TRUE ~NA_character_)) %>% 
  select(group, label4, estimate, summary_est) %>% 
  group_by(label4) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label4)==F) %>% 
  select(group, label4, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

data3 <- get_acs(table = "B05003E", geography = "us", year = 2018, summary_var = "B05003E_001") %>% 
  mutate(group = "Pacific Islander",
         label4 = case_when(
           variable %in% c("B05003E_006", "B05003E_011", 
                           "B05003E_017", "B05003E_022") ~"pct_foreign_cit",
           TRUE ~NA_character_)) %>% 
  select(group, label4, estimate, summary_est) %>% 
  group_by(label4) %>% mutate(estimate = sum(estimate)) %>% ungroup() %>% 
  filter(is.na(label4)==F) %>% 
  select(group, label4, estimate, summary_est) %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(-estimate, -summary_est)

dta4 <- rbind(data, data2, data3)
dta4 <- dta4 %>% spread(label4, pct)

dta_nativity <- dta1 %>% left_join(dta2) %>% left_join(dta3) %>% left_join(dta4)
dta_nativity$group <- factor(dta_nativity$group, levels = c("US Total", 
                                                              "Asian American",
                                                              "Pacific Islander","Asian Indian"))
dta_nativity <- dta_nativity %>% arrange(group) %>% 
  write_csv("nationaldata_2018/dta_nativity.csv")

#insurance---------------
#1.no health insurance------
data <- get_acs(table = "B27001", geography = "us", year = 2018, summary_var = "B27001_001") %>% 
  left_join(label) %>% 
  mutate(no_ins = stringr::str_detect(label, "No health insurance")) %>% 
  filter(no_ins == T) %>% select(estimate, summary_est) %>% 
  mutate(group = "US Total", label = "pct_no_insurance") %>% 
  group_by(label) %>% mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(group, label, pct)

data2 <- get_acs(table = "C27001D", geography = "us", year = 2018, summary_var = "C27001D_001") %>% 
  left_join(label) %>% 
  mutate(no_ins = stringr::str_detect(label, "No health insurance")) %>% 
  filter(no_ins == T) %>% select(estimate, summary_est) %>% 
  mutate(group = "Asian American", label = "pct_no_insurance") %>% 
  group_by(label) %>% mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(group, label, pct)

data3 <- get_acs(table = "C27001E", geography = "us", year = 2018, summary_var = "C27001E_001") %>% 
  left_join(label) %>% 
  mutate(no_ins = stringr::str_detect(label, "No health insurance")) %>% 
  filter(no_ins == T) %>% select(estimate, summary_est) %>% 
  mutate(group = "Pacific Islander", label = "pct_no_insurance") %>% 
  group_by(label) %>% mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(group, label, pct)

dta_noins <- rbind(data, data2, data3)
dta_noins$group <- factor(dta_noins$group, levels = c("US Total",
                                                      "Asian American",
                                                      "Pacific Islander"))
dta_noins <- dta_noins %>% spread(label, pct)

#2.private insurance-------
data <- get_acs(table = "B27002", geography = "us", year = 2018, summary_var = "B27002_001") %>% 
  left_join(label) %>% 
  mutate(pri_ins = stringr::str_detect(label, "With private health insurance")) %>% 
  filter(pri_ins == T) %>% select(estimate, summary_est) %>% 
  mutate(group = "US Total", label = "pct_pri_insurance") %>% 
  group_by(label) %>% mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% select(group, label, pct) %>% 
  spread(label, pct)
write_csv(data, "nationaldata_2018/dta_pri.csv")
write_csv(dta_noins, "nationaldata_2018/dta_noins.csv")

#homeownership--------
data <- get_acs(variables = "B25003_002", geography = "us", year = 2018, summary_var = "B25003_001") %>% 
  mutate(group = "US Total",
         label = "pct_home",
         pct = estimate / summary_est) %>% 
  select(group, label, pct) %>% spread(label, pct)

data2 <- get_acs(variables = "B25003D_002", geography = "us", year = 2018, summary_var = "B25003D_001") %>% 
  mutate(group = "Asian American",
         label = "pct_home",
         pct = estimate / summary_est) %>% 
  select(group, label, pct) %>% spread(label, pct)

data3 <- get_acs(variables = "B25003E_002", geography = "us", year = 2018, summary_var = "B25003E_001") %>% 
  mutate(group = "Pacific Islander",
         label = "pct_home",
         pct = estimate / summary_est) %>% 
  select(group, label, pct) %>% spread(label, pct)
dta_home <- rbind(data, data2, data3)

write_csv(dta_home, "nationaldata_2018/dta_home.csv")

#age distribution----------
data <- get_acs(table = "B01001", geography = "us", year = 2018, summary_var = "B01001_001") %>% 
  left_join(label) %>% 
  mutate(var2 = as.numeric(str_remove_all(variable, "B01001_"))) %>% 
  mutate(lb = case_when(
    var2 >= 3 & var2 <= 6 ~"pct_agep1",
    var2 >= 27 & var2 <= 30 ~"pct_agep1",
    var2 >= 7 & var2 <= 15 ~"pct_agep2",
    var2 >= 31 & var2 <= 39 ~"pct_agep2",
    var2 >= 16 & var2 <= 19 ~"pct_agep3",
    var2 >= 40 & var2 <= 43 ~"pct_agep3",
    var2 >= 20 & var2 <= 25 ~"pct_agep4",
    var2 >= 44 & var2 <= 49 ~"pct_agep4",
    TRUE ~NA_character_)) %>% filter(is.na(lb)==F) %>% 
  select(lb, estimate, summary_est) %>% 
  group_by(lb) %>% 
  mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% unique() %>% 
mutate(group = "US Total",
       pct = estimate / summary_est) %>% 
  select(group, lb, pct) %>% 
  spread(lb, pct) %>% 
  write_csv("nationaldata_2018/dta_age.csv")
#lep----------

#SPEAK OTHER LANG------
data <- get_acs(variables = c("B16005_003", "B16005_025"), geography = "us", year = 2018, summary_var = "B16005_001") %>% 
  select(estimate, summary_est) %>% 
  mutate(group = "US Total") %>% 
  mutate(estimate = sum(estimate),
         pct_other_lang = 1 - (estimate / summary_est),
         other_lang = summary_est -estimate ) %>% unique() %>% 
  select(group, pct_other_lang)

data2 <- get_acs(variables = c("B16005D_004", "B16005D_009"), geography = "us", year = 2018, summary_var = "B16005D_001") %>% 
  select(estimate, summary_est) %>% 
  mutate(group = "Asian American") %>% 
  mutate(estimate = sum(estimate),
         pct_other_lang = (estimate / summary_est),
         other_lang = summary_est -estimate ) %>% unique() %>% 
  select(group, pct_other_lang)

data3 <- get_acs(variables = c("B16005E_004", "B16005E_009"), geography = "us", year = 2018, summary_var = "B16005E_001") %>% 
  select(estimate, summary_est) %>% 
  mutate(group = "Pacific Islander") %>% 
  mutate(estimate = sum(estimate),
         pct_other_lang = (estimate / summary_est),
         other_lang = summary_est -estimate ) %>% unique() %>% 
  select(group, pct_other_lang)

dta_lang <- rbind(data, data2, data3)

#2.lep--------
data <- get_acs(table = "B16005", geography = "us", year = 2018, summary_var = "B16005_001") %>% 
  left_join(label) %>% 
  mutate(group = "US Total",
         lb = case_when(
           str_detect(label, "\"well\"")==T ~"pct_lep",
           str_detect(label, "\"not well\"")==T ~"pct_lep",
           str_detect(label, "\"not at all\"")==T ~"pct_lep",
           TRUE ~NA_character_)) %>% 
  filter(is.na(lb) == F) %>% 
  select(group, lb, estimate, summary_est) %>% 
  mutate(estimate = sum(estimate)) %>% 
  ungroup() %>% unique() %>% 
  mutate(pct = estimate / summary_est) %>% 
  select(group, lb, pct) %>% spread(lb, pct)
           
data2 <- get_acs(variables = c("B16005D_006", "B16005D_011"), geography = "us", year = 2018, summary_var = "B16005D_001") %>% 
  mutate(estimate = sum(estimate),
         group = "Asian American") %>% 
  select(group, estimate, summary_est) %>% unique() %>% 
  mutate(pct_lep = estimate / summary_est) %>% 
  select(group, pct_lep)

data3 <- get_acs(variables = c("B16005E_006", "B16005E_011"), geography = "us", year = 2018, summary_var = "B16005E_001") %>% 
  mutate(estimate = sum(estimate),
         group = "Pacific Islander") %>% 
  select(group, estimate, summary_est) %>% unique() %>% 
  mutate(pct_lep = estimate / summary_est) %>% 
  select(group, pct_lep)

dta_lep <- rbind(data, data2, data3)

dta <- dta_lang %>% left_join(dta_lep) %>% 
  write_csv("nationaldata_2018/dta_lep.csv")
