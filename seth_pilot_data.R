# install.packages("pacman")

pacman::p_load(tidyverse, googlesheets, lubridate)
theme_set(theme_bw(base_size = 18))

data <- 
    gs_title("seth pilot data ") %>% 
    gs_read(ws = 1) %>% 
    gather(month, val, Jan:May) %>% 
    mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May")))

data %>% 
    ggplot(aes(x = month, y = val, color = as.factor(Intervention))) +
    geom_jitter(width = 0.1, height = 0.1) +
    geom_line(aes(group = student_id), alpha = 0.3) +
    facet_wrap(~ Outcome, scales = "free", ncol = 1)


data %>% 
    group_by(Outcome, Intervention, month) %>% 
    summarize(Mean = median(val)) %>% 
    ggplot(aes(x = month, y = Mean, color = as.factor(Intervention))) +
    geom_line(aes(group = Intervention)) +
    facet_wrap(~ Outcome, scales = "free", ncol = 1) +
    labs(x = "Month", color = "Intervention")

data %>% 
    sample_frac(1) %>% 
    spread(Outcome, val) %>% 
    ggplot(aes(x = Bounce, y = ODR, color = as.factor(Intervention))) +
    geom_jitter(width = 0.1, height = 0.1) +
    facet_wrap(~ month) +
    labs(color = "Intervention")

?geom_jitter


data %>% 
    mutate(
        month = as.character(month)
    ) %>% 
    mutate(
        Intervention = ifelse(Intervention == 1, "yes", "no"),
        intervention_period = ifelse(month == "May", "after", "before"),
        intervention_period = factor(intervention_period, levels = c("before", "after"))
    ) %>% 
    group_by(Outcome, Intervention, intervention_period) %>% 
    summarize(mean = mean(val)) %>% 
    ggplot(aes(x = Intervention, y = mean, fill = intervention_period)) +
    geom_col(position = "dodge") +
    facet_wrap(~ Outcome, ncol = 1, scales = "free")


data %>% 
    mutate(
        month = as.character(month)
    ) %>% 
    mutate(
        Intervention = ifelse(Intervention == 1, "yes", "no"),
        intervention_period = ifelse(month == "May", "may", "before"),
        intervention_period = factor(intervention_period, levels = c("before", "may"))
    ) %>% 
    group_by(Outcome, Intervention, intervention_period) %>% 
    summarize(mean = mean(val)) %>% 
    ggplot(aes(x = intervention_period, y = mean, color = Intervention)) +
    geom_point() +
    geom_line(aes(group = Intervention)) +
    facet_wrap(~ Outcome, ncol = 1, scales = "free") +
    labs(x  =  "When")
