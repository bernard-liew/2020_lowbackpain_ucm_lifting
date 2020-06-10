df <- df.plot[grepl("DISPL", names (df.plot))] %>%
  map (~ pivot_longer(data = ., cols = starts_with ("V"), names_to = "cycle", values_to = "displ") %>% mutate (cycle = as.numeric (str_remove (cycle, "V"))))


form <- displ ~ grp + s (cycle, by = grp, bs = "cr", k = 15) + s(subj, bs = "re")

df_res <- df %>%
  map (~ gam (form, data = .))

grp_fit <- function (mod) {
  
  f <- get_fitted (mod ,rm.ranef = TRUE, as.data.frame = TRUE)
  f <- f %>%
    select (-subj, -displ, -rm.ranef) %>%
    group_by(grp, cycle) %>%
    distinct()%>%
    ungroup()
    
    return (f)

}

df_plot <- df_res %>%
  map (~ grp_fit(.)) 



kin.ord <-  c("PELVIC","RACR")

vert <- "vert"



kin.plot <- df_plot %>%
  bind_rows(.id = "var") %>%
  mutate (phase = ifelse (grepl ("LIFT", var), "lift", "lower"),
          region = str_replace_all(var, c("_.*"), ""),
          region = ifelse (region == "Mid", "Mid_Shd", region),
          direction = ifelse(grepl ("AP", var), "ap", "vert")) %>%
  select ( grp, phase, region, direction, everything ()) %>%
  filter (region %in% kin.ord) %>%
  mutate (phase = factor (phase, levels = c ("lift", "lower")),
          region = factor (region, levels = kin.ord, labels = c("Pelvis", "Trunk") ),
          direction = factor (direction, levels = c("ap", "vert")),
          grp = factor (grp, levels = c("con", "rlbp", "clbp"), labels = c("con", "rLBP", "cLBP"))) 

  # Vertical direction plot
  
  p1 <- kin.plot %>%
  filter (direction == "vert") %>%
  ggplot () +
  geom_line (aes (x = cycle, y = fit, color = grp)) +
  geom_ribbon(aes(x = cycle, ymin = fit - CI, ymax = fit + CI, fill = grp), alpha = 0.2) + 
  facet_wrap(region ~ phase, ncol = 6, scales = "free_y") +
  scale_colour_manual(values = c("black", "blue", "red")) +
  scale_fill_manual(values = c("black", "blue", "red")) +
  labs (x = "Cycle (0-100%)",
        y = "Displacement (m)",
        colour = "Group") +
  # Manually create values to expand the scale, by finding "pretty" 
  # values that are slightly larger than the range of y-axis values 
  # within each facet; set alpha = 0 since they aren't meant to be seen
  geom_point(data = . %>% 
               group_by(region , phase) %>% #group by facet variable
               summarise(y.min = pretty(fit)[1],
                         y.max = pretty(fit)[length(pretty(fit))]) %>%
               tidyr::gather(key, value, -region, -phase), 
             aes(x = 1, y = value),
             inherit.aes = FALSE, alpha = 0) +
  # Turn off automatical scale expansion, & manually set scale breaks
  # as an evenly spaced sequence (with the "pretty" values created above
  # providing the limits for each facet). If there are many facets to
  # show, I recommend no more than 3 labels in each facet, to keep things
  # simple.
  scale_y_continuous(breaks = function(x) seq(from = x[1], 
                                              to = x[2], 
                                              length.out = 3), 
                     expand = c(0, 0),
                     labels = scales::number_format(accuracy = 0.01)) +
  guides(fill = FALSE) + 
  theme_half_open()

  p2 <- kin.plot %>%
    filter (direction == "ap") %>%
    ggplot () +
    geom_line (aes (x = cycle, y = fit, color = grp)) +
    geom_ribbon(aes(x = cycle, ymin = fit - CI, ymax = fit + CI, fill = grp), alpha = 0.2) + 
    facet_wrap(region ~ phase, ncol = 6, scales = "free_y") +
    scale_colour_manual(values = c("black", "blue", "red")) +
    scale_fill_manual(values = c("black", "blue", "red")) +
    labs (x = "Cycle (0-100%)",
          y = "Displacement (m)",
          colour = "Group") +
    # Manually create values to expand the scale, by finding "pretty" 
    # values that are slightly larger than the range of y-axis values 
    # within each facet; set alpha = 0 since they aren't meant to be seen
    geom_point(data = . %>% 
                 group_by(region , phase) %>% #group by facet variable
                 summarise(y.min = pretty(fit)[1],
                           y.max = pretty(fit)[length(pretty(fit))]) %>%
                 tidyr::gather(key, value, -region, -phase), 
               aes(x = 1, y = value),
               inherit.aes = FALSE, alpha = 0) +
    # Turn off automatical scale expansion, & manually set scale breaks
    # as an evenly spaced sequence (with the "pretty" values created above
    # providing the limits for each facet). If there are many facets to
    # show, I recommend no more than 3 labels in each facet, to keep things
    # simple.
    scale_y_continuous(breaks = function(x) seq(from = x[1], 
                                                to = x[2], 
                                                length.out = 3), 
                       expand = c(0, 0),
                       labels = scales::number_format(accuracy = 0.01)) +
    guides(fill = FALSE) + 
    theme_half_open()
  
  legend <- get_legend(p1+
                         guides(color = guide_legend(nrow = 1)) +
                         theme(legend.direction = "horizontal",legend.justification="center" ,legend.box.just = "bottom"))
  
  p <- cowplot::plot_grid(p1+ theme(legend.position="none", axis.title.x = element_blank()),
                          p2+ theme(legend.position="none"), 
                          labels = c("a", "b"),
                          hjust = -1,
                          nrow = 2)
  
  p <-  plot_grid(p, legend, ncol = 1, rel_heights = c(1, .1))