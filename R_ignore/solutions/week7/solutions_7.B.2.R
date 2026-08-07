
library(tidyverse)
library(readxl)
library(here)

type_vec <- c("date", rep("numeric", 8))

dat <- read_excel(here("data_raw", "Defra Particles CPC 2000 Final.xls"), sheet = 1, col_types = type_vec)

dat_clean <- dat |>
  rename(sample_time = 1,
         Belfast_centre = "Belfast Cetre / Particles cm3",
         Birmingham_centre = "Birmingham Centre / Particles cm3",
         Glasgow_centre = "Glasgow Centre / Particles cm3",
         London_Bloomsbury = "London Bloomsbury / Particles cm3",
         Manchester_Piccadilly = "Manchester Piccadilly / Particles cm3",
         North_Kensington = "North Kensington / Particles cm3",
         Port_Talbot = "Port Talbot / Particles cm3",
         London_Marylebone = "London Marylebone / Particles cm3")

dat_clean |>
  pivot_longer(cols = -sample_time) |>
  head()

dat_long <- dat_clean |>
  pivot_longer(cols = -sample_time, names_to = "Area", values_to = "Measurement")

dat_long |>
  mutate(month = month(sample_time, label = TRUE)) |>
  ggplot() + theme_bw() +
  geom_boxplot(aes(x = month, y = Measurement)) +
  facet_wrap(~Area) +
  ylim(c(0, 1e5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


