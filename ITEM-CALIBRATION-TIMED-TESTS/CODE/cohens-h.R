# Cohen's h is a measure of the effect size of the difference between two
# proportions. POMS is a proportion: it's the achieved score divided by the
# maximum possible score.

library(pwr)

POMS_t1 <- 0.63
POMS_t2 <- 0.77

ES.h(POMS_t2, POMS_t1)

# Interpretation of h, per Cohen
# h = 0.20: "small effect size".
# h = 0.50: "medium effect size".
# h = 0.80: "large effect size".
