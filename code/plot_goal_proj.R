
# Helper packages
library (tidyverse)

# Plotting
library (cowplot)

# Custom functions
source ("ucm4.R")


seg_var <- quo(segm)

ucm_subj <- ucm.df %>%
  ungroup() %>%
  mutate (segm = ifelse (grepl("PELVIC", kinem), "pelvic", 
                         ifelse (grepl("RACR", kinem), "trunk", "hand"))) %>%
  group_by(subj, task, !!seg_var, cycle) %>%
  first () %>%
  group_by(subj, task, !!seg_var, cycle) %>%
  nest() %>%
  mutate (ucm_res = map (data, ucm, kinem))

y_res <- ucm_subj  %>%
  mutate (y_ucm = map (ucm_res,  "y_ucm"),
          y_ort = map (ucm_res,  "y_ort")) 


y_ucm_df <- y_res$y_ucm [[1]]
y_ort_df <- y_res$y_ort [[1]]

plot_df <- rbind(y_ucm_df [1:20,], y_ort_df [1:20,]) %>%
  as.data.frame() %>%
  mutate (repetitions = rep (1:20, times = 2),
          Plane = rep (c("GEV", "NGEV"), each = 20)) %>%
  rename ("Pelvic forward" = "V1",
          "Pelvic vertical" = "V2") %>%
  pivot_longer(cols = starts_with("Pelvic"),
               names_to = "Kinematics",
               values_to = "displ") %>%
  ggplot () +
  geom_point (aes (x = repetitions, y = displ, color = Plane)) +
  scale_color_manual (values = c("blue", "red")) +
  facet_wrap(~ Kinematics, nrow = 1, scales = "fixed") +
  ylab ("Mean-free displacement (m)") +
  xlab ("Repetitions") + 
  theme_half_open() + 
  theme(text = element_text(size=20))
  
plot_df
