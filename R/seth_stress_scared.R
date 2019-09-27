library(tidyverse)
theme_set(theme_bw(base_size = 14))
data <- read_csv("seth_stress_scared.csv")



data %>%
    mutate(
        diff = scared_after - scared_before,
        participant = fct_reorder(as.factor(participant), diff)
    ) %>%
    ggplot() +
    geom_segment(
        aes(x = participant, xend = participant, y = scared_before, yend = scared_after),
        size = 1,
        arrow = arrow(length = unit(0.2, "cm"))
    )

mean(data$scared_before - data$scared_after, na.rm = TRUE)

okay <-
    data %>%
    rename(stress_after = stressed_after) %>%
    gather(var, val, -participant) %>%
    separate(var, c("A", "B"), "_") %>%
    spread(B, val) %>%
    mutate(trt = ifelse(participant %in% seq(1, 11, 2), "Biofeedback", "Control")) %>%
    mutate(A = ifelse(A == "scared", "Anxiety inventory", "Stress inventory"))

okay %>%
    ggplot() +
    geom_point(aes(x = participant, y = before, color = before > after)) +
    geom_segment(
        data = okay %>% slice(-2),
        aes(x = participant, xend = participant, y = before, yend = after, color = before > after),
        size = 1,
        arrow = arrow(length = unit(0.25, "cm"))
    ) +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "Student", y = "Score",
         title = "Biofeedback has positive effect on mental health",
         subtitle = "Student 7 was absent at follow up") +
    guides(color = FALSE) +
    facet_grid(A ~ trt, scales = "free_y")

data %>%
    mutate(trt = ifelse(participant %in% seq(1, 11, 2), "Biofeedback", "Control")) %>%
    gather(var, val, stress_before, stressed_after)
    ggplot() +
    geom_point(aes(x = participant, y = scared_before, color = scared_before > scared_after)) +
    geom_segment(
        aes(x = participant, xend = participant, y = scared_before, yend = scared_after, color = scared_before > scared_after),
        size = 1,
        arrow = arrow(length = unit(0.25, "cm"))
    ) +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "Student", y = "SCARED score",
         title = "Biofeedback has positive effect on mental health",
         subtitle = "Student 7 was absent at follow up") +
    guides(color = FALSE) +
    facet_wrap(~ trt, scales = "free_x")


