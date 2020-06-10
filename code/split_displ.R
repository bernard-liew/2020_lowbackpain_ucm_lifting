by_grp <- displ %>%
  pivot_longer(contains ("DISPL"), 
               names_to = "var",
               values_to = "displ") %>%
  group_by(subj, task, var) 

by_keys <- by_grp %>%
  group_keys() %>%
  unite (col = "subj_task_var", subj, task, var, sep = "_") %>%
  pull ()


modify_displ <- function (x) {
  x %>%
  select (-c(reps2, reps, trial, max_rep, task)) %>%
    arrange (cycle, rep) %>%
    pivot_wider(names_from = c(rep, cycle),
                values_from = displ) %>%
    select_if(is.numeric)%>%
    as.numeric()
    

}

library (furrr)

plan(multiprocess)

displ.list <- by_grp %>%
  group_split()%>%
  future_map (modify_displ)


displ.list2 <- list (lift = list(pelvic_ap = displ.list[grepl("lift.*PELVIC.*AP", by_keys)], 
                                 pelvic_vt = displ.list[grepl("lift.*PELVIC.*VERT", by_keys)], 
                                 racr_ap = displ.list[grepl("lift.*RACR.*AP", by_keys)], 
                                 racr_vt = displ.list[grepl("lift.*RACR.*VERT", by_keys)]),
                     lower = list(pelvic_ap = displ.list[grepl("lower.*PELVIC.*AP", by_keys)], 
                                  pelvic_vt = displ.list[grepl("lower.*PELVIC.*VERT", by_keys)], 
                                  racr_ap = displ.list[grepl("lower.*RACR.*AP", by_keys)], 
                                  racr_vt = displ.list[grepl("lower.*RACR.*VERT", by_keys)]))
                     
                     
