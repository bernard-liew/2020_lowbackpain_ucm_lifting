##############
ucm = function (x,  ...) {

  groups <- enquos (...)

  # Get beta coefficients, these are the jacobians
  betas <- x %>%
    select (contains ("jac")) %>%
    unique () %>%
    as.matrix ()

  J <- betas # jacobian

  # degrees of freedom
  es = ncol(betas) # DOF
  ts = nrow (betas) # goal

  # deviation from mean across reps
  dev <- x %>%
    group_by(!!!groups) %>%
    mutate_at(vars(matches ("V", ignore.case = FALSE)), scale, center = TRUE, scale = FALSE) %>%
    ungroup() %>%
    select (matches ("V", ignore.case = FALSE))%>%
    as.matrix()


  ############################################################################
  # Distributional properties and variance-stabilizing transformations for
  # measures of uncontrolled manifold effects
  # Journal of Neuroscience Methods
  # Volume 191, Issue 2, 30 August 2010, Pages 166-170
  ############################################################################

  # get variance in each projection
  C = cov (dev)
  totv = Trace(C)/ es
  ngev = Trace (t (orth (t (J))) %*% C %*% orth (t(J)))/ts
  gev = Trace (t (null (J)) %*% C %*% null (J))/ (es-ts)

  S = gev/ngev
  S_trans = log (S)

  ############################################################################
  # Motor abundance and control structure in the golf swing
  # Human movement science
  # 2016, 46:129-147
  ############################################################################

  # get deviation in each projection
  ucm <- 0

  for (n in 1:ncol (null (J))) {

    ucm <- ucm + crossprod((t(null (J)[,n]) %*% t(dev)), null(J)[,n])

  }

  orth <- dev - ucm

  # get length of projected deviation normalized
  gev_dev <- (apply (ucm, 1, norm_vec))/(es-ts)#sqrt(es-ts)
  ngev_dev <- (apply (orth, 1, norm_vec))/(ts)#sqrt(ts)

  y_ucm <- t(betas %*% t(ucm))[1:20,]
  y_orth <- t(betas %*% t(orth))[1:20,]



  result = list (S_trans = S_trans,
                 GEV = gev,
                 NGEV = ngev,
                 ucm_dev = gev_dev,
                 ort_dev = ngev_dev,
                 y_ucm = y_ucm,
                 y_ort = y_orth)

  return (result )
}

################

# get magnitude of vector
norm_vec <- function(x) sqrt(sum(x^2))

# get ucm results from nest

get_ucm <- function(x, var) {

  x[[var]]

}

# get summary deviation results from nest

norm_dev<- function(x) {

  mean (apply (x, 1, norm_vec))

}

# get first slice of group data

first <-  function(x, n = 1) x %>% nest %>% ungroup %>% slice(n) %>% unnest(data)

# extract p vals for anova models

extract_grp_sig <- function (x) {

  x[which (rownames(aov_mod[[1]]) == "group"), "Pr(>F)"]

}

# add variable names to anova tables

add_var2_aovtable <- function (x) {

  x$var <- rownames(x)

  return (x)

}

#  Pairwise contrast for mgcv models

infer.posthoc <- function (data = h, mod, dv, mult_comp = 6, task_type, ref_grp) {

  df <- data %>%
    as.data.frame() %>%
    mutate_if (is.character, as.factor)

  n <- n_distinct(df$subj)

  #https://stats.libretexts.org/Bookshelves/Computing_and_Modeling/Supplemental_Modules_(Computing_and_Modeling)/Regression_Analysis/Simple_linear_regression/Simultaneous_Inference

  se_fac <- qt(1- (.05/(2 * mult_comp )), n - 1)


  pair_diff <- df %>%
    make_newdata(cycle = c(1:101),
                 grp = levels (df$grp),
                 task = factor (task_type, levels = levels (df$task))) %>%
    add_term(mod, term = "grp", reference = list(grp = ref_grp), se_mult = se_fac) %>%
    filter (grp != ref_grp)  %>%
    mutate(xtrast = paste0(grp, "-", ref_grp))

  pair_diff$dv <- rep (dv, nrow (pair_diff))

  return (pair_diff)

}

# Extract W weights from NMF

basis_merge <- function (x) {

  map (x, NMF::basis) %>%
    map (as.data.frame) %>%
    bind_rows()
}

# Extract H weights from NMF
coef_merge <- function (x) {

  map (x, NMF::coef) %>%
    map (t) %>%
    map (as.data.frame) %>%
    bind_rows()
}

# mean centring

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm))

# Reorder synergies

matchSynergies <- function (syn_ref, syn_ext) {

  # create a synergy that is ordered
  syn_ext_order <- syn_ext

  # extract the basis
  w_ref  <-  basis (syn_ref)
  w_ext <-  basis (syn_ext)

  # extract the coefficients (aka timing coef)
  h_ref  <-  coef (syn_ref)
  h_ext <-  coef (syn_ext)

  # copy
  w_ext_order <- w_ext
  h_ext_order <- h_ext


  m <- nrow (w_ref)# number of muscles
  n <- ncol (w_ref) # number of synergies extracted

  # vector matching
  matchVector <- rep(0, n)

  # correlation
  dotArray=cor(w_ref,w_ext)

  #taking the absolute value of the dot array matrix if the largest dot
  #product value is negative flips the sign of the whole extraxted synergy
  dotArrayAbs <- abs(dotArray)

  for (k in 1:n){# iterate across all synergies

    maxValue <- max(dotArrayAbs)
    loc.max <- which (dotArrayAbs ==  maxValue, arr.ind = T)
    rowsOfMaxes <- loc.max[1]
    colsOfMaxes <- loc.max[2]
    matchVector[colsOfMaxes] <- rowsOfMaxes

    # ifelse (dotArray[rowsOfMaxes,colsOfMaxes] < 0,
    #         w_ext_order[,rowsOfMaxes] <- -w_ext[,colsOfMaxes],
    #         w_ext_order[,rowsOfMaxes] <- w_ext[,colsOfMaxes]
    #         )
    #
    # ifelse (dotArray[rowsOfMaxes,colsOfMaxes] < 0,
    #                  h_ext_order[rowsOfMaxes,] <- -h_ext[colsOfMaxes,],
    #                  h_ext_order[rowsOfMaxes,] <- h_ext[colsOfMaxes,]
    #                  )

    w_ext_order[,rowsOfMaxes] <- w_ext[,colsOfMaxes]
    h_ext_order[rowsOfMaxes,] <- h_ext[colsOfMaxes,]

    dotArrayAbs[rowsOfMaxes,] <- 0
    dotArrayAbs[,colsOfMaxes] <- 0
  }

  syn_ext_order@fit@W <- w_ext_order
  syn_ext_order@fit@H <- h_ext_order

  return (syn_ext_order)

}

# UCM plots

plot_ucm <- function (df, outcome, ylabel, facet_var) {

  df %>%
    filter (dv == outcome) %>%
    ggplot () +
    geom_bar (aes (x = task, y = Mean, fill = Group, colour = Group),
              position = position_dodge(width = 0.9), stat = "identity") +
    geom_errorbar(aes (x = task, ymin = Mean, ymax = Mean + Err, colour = Group),
                  position = position_dodge(width = 0.9), stat = "identity", width = 0.3) +
    scale_fill_manual(values = c("black", "blue", "red")) +
    scale_colour_manual(values = c("black", "blue", "red")) +
    xlab ("Phase") +
    ylab (ylabel) +
    theme_half_open() +
    theme(text = element_text(size=20))  +
    facet_wrap(vars(!!!facet_var), scales = "fixed", ncol = 4)


}
