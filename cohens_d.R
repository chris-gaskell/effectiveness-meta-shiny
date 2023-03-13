
# cohen's d ---------------------------------------------------------------

ni = 1646
pre.mean = 14.96
pre.sd = 3.28
post.mean = 10.15
r = .5

d.unadjusted <-
  round(
    (pre.mean - post.mean) / pre.sd,
    3
  )

estr <-
  round(
    r + r*(1-r*r)/2/(ni-4),
    3
  )

d.adjusted <-
  round( (1-3/(4*ni-5))*d.unadjusted, 2)

var <-
  round(
    2*(1-estr)/ni+d.adjusted*d.adjusted/(2*ni),
    4
  )

ci.lb <-
  round(
    d.adjusted-1.96*sqrt(var),
    3
  )

ci.ub <-
  round(
    d.adjusted+1.96*sqrt(var),
    3
  )

d.lookup <- format(d.adjusted, nsmall = 2)

# lookup.tab <- lookup %>%
#  mutate(es = format(es, nsmall = 2)) %>%
#   arrange(es) %>%
#   mutate(es = stringr::str_replace(es, fixed(" "), "")) %>%
#   distinct()
#

setting <- "all"
group <- "other"

lookup %>% filter(es == d.lookup & setting == setting & group == group) %>%
  distinct(2, 3, .keep_all = T) %>% select(final.rank) %>% nrow()

norm.datasets %>%
  filter(setting == "All" & group == "Other") %>%
  mutate(reference = "benchmarks") %>%
  add_row(es = d.adjusted, reference = "study", n = ni, ci.lb = ci.lb, ci.ub = ci.ub) %>%
  arrange(es) %>%
tibble::rowid_to_column("ID") %>%
  mutate(effective.group =
           case_when(
             centile <=  25 ~ "Lower (≤25th percentile)",
             centile > 75 ~ "Upper (≥75th percentile)",
             centile >= 25 ~ "Middle (25th and 75th percentile)",
             TRUE ~ "Your Service"
           )) %>%
  ggplot(aes(x = ID, y = es, col = effective.group)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub)) +
  theme_bw(base_size = 12)  +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  labs(title = paste("Forest plot of effect sizes (and 95% CI) for", group, "outcomes in ", setting, "settings."), x = "Participating Services", y = "Cohen's d", col = NULL) +
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



freedom.mod.table %>%
  mutate(moderator = str_to_title(moderator),
         moderator  = recode(moderator,
                             "Dose" = "Treatment Length",
                             "Risk Of Bias" = "Overall Risk of Bias",
                             "Modality" = "Treatment Modality",
                             "Delivery" = "Treatment Delivery Format")) %>%
  ggplot(aes(x = level, y = pred)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub)) +
  geom_hline(aes(yintercept = freedom.report$es),
             linetype = "dashed",
             color = "black") +
  facet_grid(
    rows = vars(moderator),
    scales = "free", space = "free", labeller =
      labeller(moderator = label_wrap_gen(8))
  ) +
  coord_flip() +
  theme_bw() +
  labs(x = "Sub-group Moderator Level", y = paste("Seizure freedom rate (%)")) +
  theme(strip.text.y.right = element_text(angle = 270, size = 12, face = "bold"),
        strip.placement = "left",
        legend.position = "bottom",
        plot.title.position = "plot",
        text = element_text(size = 12),
        #strip.text = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        panel.grid.minor = element_blank()
  ) + scale_y_continuous(labels = scales::percent)










d.unadjusted <- round((pre.mean - post.mean) / pre.sd, 2)
estr <-         round(r + r*(1-r*r)/2/(ni-4), 2)
d.adjusted <-   round((1-3/(4*ni-5))*d.unadjusted, 2)
#d.adjusted.2 <- if (input$direction == "scores increase") {d.adjusted()*-1} else {d.adjusted()})
var <-
  round(2*(1-estr)/ni+d.adjusted*d.adjusted/(2*ni), 4)
ci.lb <-        round(d.adjusted-1.96*sqrt(var), 2)
ci.ub <-        round(d.adjusted+1.96*sqrt(var), 2)

