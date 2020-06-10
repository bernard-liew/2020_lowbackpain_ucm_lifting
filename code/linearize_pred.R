
obs <- augment.df %>%
  group_by(subj, task, kinem, cycle) %>%
  mutate(reps = row_number())%>%
  ungroup() %>%
  select (subj, task, cycle, kinem, reps, displ) %>%
  pivot_wider(names_from = "cycle",
              values_from = "displ") %>%
  group_by(task, kinem) %>%
  nest(.key = "actual_mat") %>%
  mutate (actual_mat = map (actual_mat, ~.[,-c(1:2)] %>% as.matrix))


pred <- augment.df %>%
  group_by(subj, task, kinem, cycle) %>%
  mutate(reps = row_number())%>%
  ungroup() %>%
  select (subj, task, cycle, kinem, reps, .fitted) %>%
  pivot_wider(names_from = "cycle",
              values_from = ".fitted") %>%
  group_by(task, kinem) %>%
  nest(.key = "pred_mat") %>%
  mutate (pred_mat = map (pred_mat, ~.[,-c(1:2)] %>% as.matrix))

err_df <- obs %>%
  inner_join(pred, by = c("task", "kinem")) %>%
  mutate (err = pmap (list (actual_mat = actual_mat, pred_mat = pred_mat), RMSE),
          relerr = pmap (list (actual_mat = actual_mat, pred_mat = pred_mat), relRMSE),
          corerr = pmap (list (actual_mat = actual_mat, pred_mat = pred_mat), cor_fun)) %>%
  mutate (err_mean = map (err, mean),
          err_sd = map (err, sd),
          relerr_mean = map (relerr, mean),
          relerr_sd = map (relerr, sd),
          corerr_mean = map (corerr, mean),
          corerr_sd = map (corerr, sd)) %>%
  select (matches("task|kinem|mean|sd"))%>%
  filter (grepl ("PELVIC|RACR", kinem))%>%
  mutate_if (is.list, unlist) %>%
  ungroup () %>%
  mutate (task = str_remove(task, "_ln"),
          kinem = str_replace_all (kinem, 
                                   c("PELVIC_DISPL" = "Pelvis",
                                     "RACR_DISPL" = "Trunk",
                                     "AP" = "forward",
                                     "VERT" = "vertical")))


p1 <- err_df %>%
  ggplot () +
  geom_point (aes(x = task, y = err_mean, color = kinem), 
              position=position_dodge(0.5), size = 2) +
  geom_errorbar(aes(x = task, ymin= err_mean-err_sd, ymax = err_mean+err_sd, colour = kinem), width=.2,
                position=position_dodge(0.5)) +
  scale_color_manual("Kinematics", values = c("black", "red", "blue", "darkgreen")) + 
  ylab ("RMSE (cm)") +
  xlab ("Task") +
  theme_half_open()

p2 <- err_df %>%
  ggplot () +
  geom_point (aes(x = task, y = relerr_mean, color = kinem), 
              position=position_dodge(0.5), size = 2) +
  geom_errorbar(aes(x = task, ymin= relerr_mean - relerr_sd, ymax = relerr_mean+relerr_sd, colour = kinem), width=.2,
                position=position_dodge(0.5)) +
  scale_color_manual("Kinematics", values = c("black", "red", "blue", "darkgreen")) + 
  ylab ("relRMSE (%)") +
  xlab ("Task") +
  theme_half_open()


legend <- get_legend(p1+
                       guides(color = guide_legend(nrow = 1)) +
                       theme(legend.direction = "horizontal",legend.justification="center" ,legend.box.just = "bottom"))

p <- cowplot::plot_grid(p1+ theme(legend.position="none", axis.title.x = element_blank()),
                        p2+ theme(legend.position="none"), 
                        labels = c("a", "b"),
                        hjust = -1,
                        align = "v",
                        nrow = 2)

p <-  plot_grid(p, legend, ncol = 1, rel_heights = c(1, .1))

tiff("../../pred_err.tiff", width = 6, height = 4, units = 'in', res = 200, compression = 'none')
p
dev.off()
