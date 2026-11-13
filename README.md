# tennisCareer

An R package to retrieve Tennis Abstract career matches and plot
player age against player-minus-opponent age, colored by Win/Loss.

## Install

```r
remotes::install_github("UCB-stat159-f26/hw03-2-jingwei-chen01")
```

Requires the remotes package and access to this repository.

## Usage

```r
library(tennisCareer)

serena <- get_player_matches("serena williams")
plot_age_difference(serena)

rafa <- get_player_matches("Rafael Nadal")
plot_age_difference(rafa)

# Invalid names produce an informative error.
get_player_matches("GastonSanchez")
```

Spaced names and identifiers such as "SerenaWilliams" are supported.
Data retrieval requires internet access. Ages derived from birth dates
use days divided by 365.25; missing ages are omitted from plots.

## Tests

```sh
Rscript -e 'testthat::test_local()'
```

Tests use mock data without contacting live servers.
All 24 assertions passed, and R CMD check returned Status: OK.

## Personal reflections

I learned how to turn scripts into an installable R package with
documented functions. Handling female opponent birth dates and male
opponent ages showed me why data formats matter. Small functions and
offline tests made the calculations easier to check.
