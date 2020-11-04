# Air Travel COVID-19 testing simulator

This R Shiny app is meant to test different scenarios of air traveler screening for COVID-19 at departure and/or arrival. It models the following variables:

### Disease characteristics
- Disease prevalence in the departure state (derived from published medical data, or entered manually by the user)
- Disease prevalence in the arrival state (derived from published medical data, or entered manually by the user)
- Viral load ramp-up (to detectability)

### Passenger characteristics
- Number of passengers traveling from the departure state to the arrival state (derived from AirportIS data, or entered manually by the user)
- Proportion of passengers getting tested among them (i.e. systematic or sample-based testing)

### Test characteristics
- Limit of detection (analytical sensitivity), in viral copies per milliliter
- Clinical sensitivity (true positives over all tests)
- Clinical specificity (true negatives over all tests)

### Operational characteristics
- Whether the test is performed before departure, after arrival, or both
- Time of testing relative to departure or arrival