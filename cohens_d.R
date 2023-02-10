
# cohen's d ---------------------------------------------------------------

ni = 20
pre.mean = 40
pre.sd = 4
post.mean = 35
r = .4

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

lookup.tab <- lookup %>%
 mutate(es = format(es, nsmall = 2)) %>%
  arrange(es) %>%
  mutate(es = stringr::str_replace(es, fixed(" "), "")) %>%
  distinct()




