### Temperature Data Prep
### July 18, 2025
### Heili Lowman

#### README ####

# The following script will perform some initial data tidying
# of the stream temperature data.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)

# Load data.
dat <- read_csv("data_raw/HubbardBrook_weekly_stream_chemistry_1963-2024.csv")

#### Offset Calculation ####

# Trim down dataset to include only watersheds 5 & 6.
dat_temp_trim <- dat %>%
  filter(site %in% c("W5", "W6")) %>%
  select(site, date, temp)

dat_temp_wide <- dat_temp_trim %>%
  group_by(site, date) %>%
  summarize(temp = mean(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = "site",
              values_from = "temp")

# First, plot stream temperature in W5 as a function of W6.
ggplot(dat_temp_wide, 
       aes(x = W6,
           y = W5)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") +
  labs(x = "W6 Stream Temperature",
       y = "W5 Stream Temperature") +
  theme_bw()

# Seems to be nearly a 1:1 relationship, but let's check to see where 
# the greatest deviations are.
dat_temp_wide <- dat_temp_wide %>%
  mutate(diff = W5- W6)
# Lots of positive deviations in the 80s, following the removal (168 >1).
# Fewer negative deviations at various times (<100 >-1).

# Fit a linear model to determine the offset to be applied
# in order to generate the long-term record for W5 using the
# W6 data.
lm_temp <- lm(W5 ~ W6, data = dat_temp_wide)

# Examine model output.
summary(lm_temp)
# y = 1.000574x + 0.044111
# Multiple R^2 = 0.9847
# p < 2.2e-16

# Examine model residuals.
plot(lm_temp)

# Trying the same model with log-transformed values.
lm_temp2 <- lm(log(W5) ~ log(W6), data = dat_temp_wide)
summary(lm_temp2)
plot(lm_temp2)
# Residuals look worse, and there's no need to overcomplicate
# things here, so using the original formula.

#### Offset Application ####

# End of script.

