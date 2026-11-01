# Cookies
# Nicholas Vollmer
# 11/1/26
# One-way ANOVA on zombie transformation scores for three cookie recipes


# Observed transformation scores (Recipe C is the control with no ingredient X)
cookieData <- data.frame(
  RecipeA = c(0, 4, 4, 5, 4, 1, 2, 3, 3, 5),
  RecipeB = c(7, 6, 4, 2, 3, 0, 8, 6, 2, 3),
  RecipeC = c(1, 0, 1, 0, NA, 1, 1, NA, 1, 0)
)


# Stack into values/ind columns and drop the rows with NA values
stackedData <- na.omit(stack(cookieData))
cat("stacked data frame:\n")
print(stackedData)


# Summary of the stacked data
# Response: values (transformation score), Predictor: ind (recipe)
initialSummary <- summary(stackedData)
cat("initial summary:\n")
print(initialSummary)


# One-way ANOVA using formula notation (response ~ predictor)
# H0: the mean transformation score is the same for all three recipes
# H1: at least one recipe has a different mean transformation score
anovaResult <- aov(values ~ ind, data = stackedData)


# Summary of the ANOVA results
anovaSummary <- summary(anovaResult)
cat("anova summary:\n")
print(anovaSummary)


# Reject H0 when the p-value is below the 0.05 significance level
pValue <- anovaSummary[[1]]$`Pr(>F)`[1]
cat("p_value is", pValue, "\n")
if (pValue < 0.05) {
  cat("Sufficient evidence to reject the null hypothesis.\n")
  cat("Ingredient X may have an effect on the results.\n")
} else {
  cat("Insufficient evidence to reject the null hypothesis.\n")
  cat("Ingredient X may not have an effect on the results.\n")
}