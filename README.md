# Air Travel COVID-19 testing simulator

This R Shiny app (available at https://theaviationdoctor.shinyapps.io/covid19testing/) is meant to simulate the effectiveness of pre-departure and post-arrival COVID-19 testing on decreasing the residual risk of case importation using the Bayesian theorem. It models the following variables:

### Disease characteristics
- Point prevalence of COVID-19 at the point of origin
- Point prevalence of COVID-19 at the point of destination

### Test characteristics
- Clinical sensitivity (true positives over all tests)
- Clinical specificity (true negatives over all tests)