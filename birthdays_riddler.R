library(furrr)
library(tidyverse)

trials <- 1e6
plan(multisession)


results <- tibble(people=seq(10, 70), prob=map_dbl(seq(10, 70), function(x) (sum(future_map_lgl(1:trials, ~sum(duplicated(sample(365, x, replace = T))) >= 2 )))/trials ))

ggplot(results, aes(x=people, prob)) +geom_line(color='dodgerblue', size=1.5) +geom_hline(yintercept=0.5, linetype=5, color='red', size=1.5) +
  theme_bw() +scale_y_continuous(limits=c(0, 1)) +scale_x_continuous(breaks = seq(10,100,10)) +
  labs(title='Riddler Classic: Triple Birthday Problem', x='Size of group', y='Probability',
       subtitle='Probability of three or more people having same birthday by size of group\nP > 0.5 for n=36 based on 1,000,000 trials for each group size', caption = 'By @cortinah, 10/4/2019')



results2 <- tibble(people=seq(10, 70), prob=map_dbl(seq(10, 70), function(x) (sum(future_map_lgl(1:trials, ~sum(duplicated(sample(365, x, replace = T))) >= 3 )))/trials ))
results3 <- tibble(people=seq(10, 70), prob=map_dbl(seq(10, 70), function(x) (sum(future_map_lgl(1:trials, ~sum(duplicated(sample(365, x, replace = T))) >= 4 )))/trials ))

all_results <- cbind(results, results2, results3)
colnames(all_results) <- c('people', 'prob3','peo2', 'prob4', 'peo3','prob5')
all_results <- all_results %>% select(people, prob3, prob4, prob5)
colnames(all_results) <- c('people','3 birthdays', '4 birthdays', '5 birthdays')

all_results <- pivot_longer(all_results, cols = 2:4)

ggplot(all_results, aes(x=people, y=value, color=name)) +geom_line(size=1.5) +geom_hline(yintercept=0.5, linetype=5, color='red', size=1.5) +
  theme_bw() +scale_y_continuous(limits=c(0, 1)) +scale_x_continuous(breaks = seq(10,100,10)  )  +
  scale_color_viridis_d()+
  labs(title='Riddler Classic: Triple Birthday Problem', x='Size of group', y='Probability',
       subtitle='Probability of 3, 4, or 5 people having same birthday by size of group', caption = 'By @cortinah, 10/4/2019') +
  theme(legend.title = element_blank())
  

