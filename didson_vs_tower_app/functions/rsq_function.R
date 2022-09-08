#putting this function in a separate file bc otherwise rsq is only recognized when the app is run from the UI
rsq <- function(x, y) summary(lm(y~x))$r.squared