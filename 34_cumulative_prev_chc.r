
prev_data <-
  data.table(
    chc_col = chc_cols,
    condition = rep(chc_groups, each = 5),
    to_year = rep(6:10, length(chc_groups)),
    n = as.integer(NA),
    p = as.numeric(NA),
    ll95 = as.numeric(NA),
    ul95 = as.numeric(NA)
  )

# Calculate p and 95% CI using Agresti-Coull interval due to small %.

for (i in 1:nrow(prev_data)) {
  
  cur_col <- prev_data[i, chc_col]
  
  n_trials <- length(cohort_eligible[, get(cur_col) == T])
  n_success <- sum(cohort_eligible[, get(cur_col) == T])
  
  n_tilde <- n_trials + 1.96^2
  p_tilde <- (n_success + 0.5 * 1.96^2) / n_tilde
  
  ul <- p_tilde + 1.96 * sqrt((p_tilde / n_tilde) * (1 - p_tilde))
  ll <- p_tilde - 1.96 * sqrt((p_tilde / n_tilde) * (1 - p_tilde))
  
  prev_data[i, n := n_success]
  prev_data[i, p := (n_success / n_trials) * 100]
  prev_data[i, ll95 := ll * 100]
  prev_data[i, ul95 := ul * 100]
  
}

rm(ul, ll, i, n_tilde, p_tilde, n_trials, n_success, cur_col)

p1 <- ggplot(prev_data[condition == "hardelid"],
             aes(x = to_year,
                 y = p)) +
  geom_line(size = 1.2) +
  scale_y_continuous(limits = c(0, 20),
                     expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  xlab("To end of academic year") +
  ylab("Children with condition\n(cumulative %)") +
  ggtitle("A: Hardelid conditions") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        strip.background = element_blank())

p2 <- ggplot(prev_data[condition != "hardelid"],
             aes(x = to_year,
                 y = p,
                 colour = condition)) +
  geom_line(size = 1.2) +
  scale_y_continuous(limits = c(0, 5),
                     expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  xlab("To end of academic year") +
  ylab("Children with condition\n(cumulative %)") +
  ggtitle("B: Other conditions") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        strip.background = element_blank())

write.csv(prev_data, file = "3_DESCRIPTIVE_STUDY/outputs/chc_cumul_prevalence_data.csv",
          row.names = F)

rm(p1, p2, prev_data)
