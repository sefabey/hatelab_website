library(tidyverse)
library(fs)
library(lubridate)

# Data Import=====
folder <- "/Users/sefaozalp/Desktop/dash_comparison" #without forward slash

# read the csv file to be classified
data_raw <- dir_ls(folder, type = "file") %>%
  read_csv(col_types = cols(id_str = col_character()))

# read classification results
antimuslim <- read_csv("/Users/sefaozalp/Desktop/dash_comparison/Anti-muslimClassifier/Anti-muslimClassifier-sep_2019_tweets_to_be_classified.csv", col_types = cols(id_str = col_character())) %>%
  mutate(antimuslim=No) %>%
  select(-No)

antisemitism <- read_csv("/Users/sefaozalp/Desktop/dash_comparison/Anti-semitism/Anti-semitism-sep_2019_tweets_to_be_classified.csv", col_types = cols(id_str = col_character())) %>%
  mutate(antisemitism=No)%>%
  select(-No)

sex_orient <- read_csv("/Users/sefaozalp/Desktop/dash_comparison/SexualOrientationClassifier/SexualOrientationClassifier-sep_2019_tweets_to_be_classified.csv", col_types = cols(id_str = col_character())) %>%
  mutate(sex_orient=No)%>%
  select(-No)


farright <- read_csv("/Users/sefaozalp/Desktop/dash_comparison/FarRightClassifier/FarRightClassifier-sep_2019_tweets_to_be_classified.csv", col_types = cols(id_str = col_character())) %>%
  mutate(farright=No)%>%
  select(-No)


xrw <- read_csv("/Users/sefaozalp/Desktop/dash_comparison/ExtrameRightWingClassifier/ExtrameRightWingClassifier-sep_2019_tweets_to_be_classified.csv", col_types = cols(id_str = col_character())) %>%
  mutate(xrw=No)%>%
  select(-No)

# create aggregates======

# join classified data
classified_data <- data_raw %>%
  left_join(antimuslim) %>%
  left_join(antisemitism) %>%
  left_join(sex_orient) %>%
  left_join(farright) %>%
  left_join(xrw) %>%
  mutate_at(vars(c("antimuslim", "antisemitism", "sex_orient", "farright", "xrw" )),
             ~ifelse(.x=="Yes", TRUE, FALSE))

# process classified data
classified_data <- classified_data %>%
  mutate(date_minutes=floor_date(timestamp_parsed,unit = "minute")) %>%   # round to the nearest minute
  filter(timestamp_parsed> dmy("01-09-2019") & timestamp_parsed< dmy("01-10-2019"))  %>% # filter sept
  mutate(is_hate= ifelse (antimuslim|antisemitism|sex_orient|farright|xrw==TRUE, TRUE, FALSE)) # create a column for tweets classified as any hate speech


# calculate aggregates for each hate class
hate_counts <- classified_data %>%
  filter(is_hate==TRUE) %>%
  count(date_minutes, is_hate, name="is_hate_n") %>%
  select(-is_hate)

antimuslim_counts <- classified_data %>%
  filter(antimuslim==TRUE) %>%
  count(date_minutes, antimuslim,name="antimuslim_n") %>%
  select(-antimuslim)

antisemitism_counts <- classified_data %>%
  filter(antisemitism==TRUE) %>%
  count(date_minutes, antisemitism, name="antisemitism_n") %>%
  select(-antisemitism)

sex_orient_counts <- classified_data %>%
  filter(sex_orient==TRUE) %>%
  count(date_minutes, sex_orient, name="sex_orient_n") %>%
  select(-sex_orient)

farright_counts <- classified_data %>%
  filter(farright==TRUE) %>%
  count(date_minutes, farright, name= "farright_n") %>%
  select(-farright)

xrw_counts <- classified_data %>%
  filter(xrw==TRUE) %>%
  count(date_minutes, xrw, name="xrw_n") %>%
  select(-xrw)

# merge all aggregate hate counts
final_hate_counts <- hate_counts %>%
  left_join(antimuslim_counts) %>%
  left_join(antisemitism_counts) %>%
  left_join(sex_orient_counts) %>%
  left_join(farright_counts) %>%
  left_join(xrw_counts) %>%
  replace_na(list( is_hate_n=0, antimuslim_n=0, antisemitism_n=0, sex_orient_n=0, farright_n=0, xrw_n=0))


# visualisations=======
static_plot <- final_hate_counts %>%
  ggplot(aes(x=date_minutes))+
  geom_line(aes(y=is_hate_n), colour= "aquamarine3",size=0.5)+
  geom_line(aes(y=antimuslim_n), colour= "chartreuse3",size=0.5)+
  geom_line(aes(y=antisemitism_n), colour= "darkorchid2",size=0.5)+
  geom_line(aes(y=sex_orient_n), colour= "darkorange",size=0.5)+
  geom_line(aes(y=farright_n), colour= "deepskyblue2",size=0.5)+
  geom_line(aes(y=xrw_n), colour= "darkslategrey",size=0.5) +
  hrbrthemes::theme_ipsum_rc()

static_plot

static_plot2 <- final_hate_counts %>%
  ggplot(aes(x=date_minutes))+
  geom_line(aes(y=is_hate_n, colour= "Any Hate Class"),size=0.2, alpha = 0.7)+
  geom_line(aes(y=farright_n, colour= "Far Right"),size=0.2, alpha = 0.7)+
  geom_line(aes(y=xrw_n, colour="XRW"),size=0.2, alpha = 0.7) +
  geom_line(aes(y=antisemitism_n, colour= "Antisemitism") ,size=0.2, alpha = 0.7)+
  geom_line(aes(y=antimuslim_n, colour="Antimuslim"),size=0.2, alpha = 0.7)+
  geom_line(aes(y=sex_orient_n, colour= "Sexual Orientation"),size=0.2, alpha = 0.7)+


  hrbrthemes::theme_ipsum_rc()+
  scale_colour_manual(name = "Classifier",
                      values = c( "Any Hate Class"="darkred", "Antimuslim"="deepskyblue2", "Antisemitism"="darkorchid2", "Sexual Orientation"="darkorange", "Far Right"="chartreuse3", "XRW"=  "darkslategrey")
                      )+
  theme(legend.position="bottom")+
  guides(colour = guide_legend(override.aes = list(size=3,linetype=1)))+
  labs(title= "The Number of Brexit Related Tweets Classified as Hate Speech in September 2019",
       subtitle = "Data were collected from Twitter streaming API using the keyword 'brexit' in real time",
       caption = "@SefaOzalp, HateLab, 2019",
       y="Number of Tweets (per minute)")+
  NULL

# static_plot2


ggsave(static_plot2,
       filename = "output/hatedash_static_Sep19.pdf",
       device = cairo_pdf,
       scale = 0.8,
       height = 9, width = 16,dpi = 500)


# Aggregate Hourly======

