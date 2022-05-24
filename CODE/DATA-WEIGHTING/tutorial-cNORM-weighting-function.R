################################################################################
#' Tutorial on the usage of raking weights in regression-based norming
#' with cNORM
#'
#'The script contains a step-by-step tutorial for the usage of raking weights
#'in regression-based norming with R package cNORM based on the norm sample
#'of the Peabody Picture Vocabulary Test (PPVT).
#'
################################################################################

# Load data set ----------------------------------------------------------------
library(cNORM)

# Firstly, the data set is loaded and assigned to the variable norm.data
norm.data <- ppvt
View(norm.data)

# Secondly, assigning population marginals to variable marginals.ppvt
# Note: The marginals have to be in the shown format, especially the order
# of the columns must be the variables names first, secondly, the single levels
# of the variables, and finally the single proportions.
# Each level of each factor needs its own row.
marginals.ppvt <- data.frame(var = c("sex", "sex", "migration", "migration"),
                             level = c(1,2,0,1),
                             prop = c(0.5100, 0.4900, 0.6500, 0.3500))
View(marginals.ppvt)


# In comparison: The actual composition ot the sample regarding sex and
# migration status (just for demonstration purposes)
prop.table(xtabs(~sex, data=norm.data)) # shares of males / females
prop.table(xtabs(~migration, data=norm.data)) # shares of migration no / yes
prop.table(xtabs(~migration + sex, data=norm.data)) # single cells



# Step 1: Compute and standardize raking weights -------------------------------

# Calculate standardized raking weights using computeWeights()
weights.ppvt <- computeWeights(data = norm.data, population.margins = marginals.ppvt)


# Let's check, which weights resulted (just for demo; not necessary)
library(dplyr)
norm.data$weights <- weights.ppvt
norm.data.split <- norm.data %>% group_by(sex, migration) %>% summarize(weights = unique(weights))
View(norm.data.split)


# Step 2: ranking and modeling is done in a single step ------------------------
model.ppvt <- cnorm(raw     =  norm.data$raw,
                    group   = norm.data$group,
                    weights = weights.ppvt)

# further steps like model selection
plot(model.ppvt, "subset")
normTable(c(5, 6, 7), model.ppvt)

