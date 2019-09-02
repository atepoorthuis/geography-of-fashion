Group Totals
================
Ate Poorthuis
02/09/2019

Group Totals
------------

We now add group totals for the `nationality`, `gender` and `type` variables for each query.

``` r
hexagons_raw_counts <- readRDS(here("analysis/data/derived_data/global_hex_joined.rds")) %>% st_set_geometry(NULL)
metadata <- fread(here("analysis/data/raw_data/metadata.csv"))
```

### Nationalities

``` r
nationalities <- metadata %>% 
  select(Nationality) %>% 
  filter(str_length(Nationality) > 0) %>% 
  distinct() %>% 
  pull(Nationality)

add_rowsums_nationality <- function (nat) {
  hits <- metadata %>% 
    filter(Nationality == nat) %>% 
    pull(UniqueID) %>% 
    intersect(., colnames(hexagons_raw_counts))
  name <- paste0("NAT_", nat)
  hexagons_raw_counts <<- mutate(hexagons_raw_counts, !!name := rowSums(hexagons_raw_counts[, hits]))
  invisible(nat)
}

pblapply(nationalities, function(nat) add_rowsums_nationality(nat))
```

    ## [[1]]
    ## [1] "FRANCE"
    ## 
    ## [[2]]
    ## [1] "CHINA"
    ## 
    ## [[3]]
    ## [1] "SWEDEN"
    ## 
    ## [[4]]
    ## [1] "GERMANY"
    ## 
    ## [[5]]
    ## [1] "ITALY"
    ## 
    ## [[6]]
    ## [1] "UK"
    ## 
    ## [[7]]
    ## [1] "USA"
    ## 
    ## [[8]]
    ## [1] "JAPAN"
    ## 
    ## [[9]]
    ## [1] "INDIA"
    ## 
    ## [[10]]
    ## [1] "SPAIN"
    ## 
    ## [[11]]
    ## [1] "SOUTH AFRICA"
    ## 
    ## [[12]]
    ## [1] "BELGIUM"
    ## 
    ## [[13]]
    ## [1] "BRAZIL"
    ## 
    ## [[14]]
    ## [1] "AUSTRALIA"
    ## 
    ## [[15]]
    ## [1] "AUSTRIA"
    ## 
    ## [[16]]
    ## [1] "KOREA"
    ## 
    ## [[17]]
    ## [1] "SWITZERLAND"
    ## 
    ## [[18]]
    ## [1] "VENEZUELA"
    ## 
    ## [[19]]
    ## [1] "UKRAINE"
    ## 
    ## [[20]]
    ## [1] "PERU"
    ## 
    ## [[21]]
    ## [1] "NIGERIA"
    ## 
    ## [[22]]
    ## [1] "QATAR"
    ## 
    ## [[23]]
    ## [1] "EGYPT"
    ## 
    ## [[24]]
    ## [1] "INDONESIA"
    ## 
    ## [[25]]
    ## [1] "UNITED ARAB EMIRATES"
    ## 
    ## [[26]]
    ## [1] "RUSSIA"
    ## 
    ## [[27]]
    ## [1] "IRELAND"
    ## 
    ## [[28]]
    ## [1] "GHANA"
    ## 
    ## [[29]]
    ## [1] "CANADA"
    ## 
    ## [[30]]
    ## [1] "NETHERLANDS"
    ## 
    ## [[31]]
    ## [1] "UK/USA"
    ## 
    ## [[32]]
    ## [1] "TURKEY"
    ## 
    ## [[33]]
    ## [1] "GREECE"
    ## 
    ## [[34]]
    ## [1] "NORWAY"
    ## 
    ## [[35]]
    ## [1] "PHILLIPPINES"
    ## 
    ## [[36]]
    ## [1] "ISRAEL"
    ## 
    ## [[37]]
    ## [1] "KENYA"
    ## 
    ## [[38]]
    ## [1] "TUNISIA"
    ## 
    ## [[39]]
    ## [1] "LEBANON"
    ## 
    ## [[40]]
    ## [1] "TURKEY/CANADA"
    ## 
    ## [[41]]
    ## [1] "PORTUGAL"
    ## 
    ## [[42]]
    ## [1] "COLUMBIA"
    ## 
    ## [[43]]
    ## [1] "DOMINICAN REPUBLIC"
    ## 
    ## [[44]]
    ## [1] "NEPAL"
    ## 
    ## [[45]]
    ## [1] "SERBIA"
    ## 
    ## [[46]]
    ## [1] "THAILAND"
    ## 
    ## [[47]]
    ## [1] "GEORGIA"
    ## 
    ## [[48]]
    ## [1] "JAPAN/KOREA"
    ## 
    ## [[49]]
    ## [1] "SINGAPORE"
    ## 
    ## [[50]]
    ## [1] "SAUDI ARABIA"
    ## 
    ## [[51]]
    ## [1] "KUWAIT"
    ## 
    ## [[52]]
    ## [1] "ARGENTINA"
    ## 
    ## [[53]]
    ## [1] "FRANCE/MOROCCO"
    ## 
    ## [[54]]
    ## [1] "VIETNAM"
    ## 
    ## [[55]]
    ## [1] "CHILE"
    ## 
    ## [[56]]
    ## [1] "ICELAND"
    ## 
    ## [[57]]
    ## [1] "BELGUIM"
    ## 
    ## [[58]]
    ## [1] "DENMARK"
    ## 
    ## [[59]]
    ## [1] "PUERTO RICA"
    ## 
    ## [[60]]
    ## [1] "ETHIOPIA"
    ## 
    ## [[61]]
    ## [1] "MALAYSIA"
    ## 
    ## [[62]]
    ## [1] "KAZAKHSTAN"
    ## 
    ## [[63]]
    ## [1] "LUXEMBOURG"

``` r
hexagons_raw_counts[1:5,]
```

    ## # A tibble: 5 x 1,714
    ##     hex `Retailers-Pers… `Retailers-Pers… `Retailers-Pers… `Retailers-Pers…
    ##   <dbl>            <dbl>            <dbl>            <dbl>            <dbl>
    ## 1    85                0                0                0                0
    ## 2    86                0                0                0                0
    ## 3    87                0                0                0                0
    ## 4    88                0                0                0                0
    ## 5    89                0                0                0                0
    ## # … with 1,709 more variables: `Retailers-Personal-Names-052` <dbl>,
    ## #   `Retailers-Personal-Names-052_fol` <dbl>,
    ## #   `Retailers-Personal-Names-051` <dbl>,
    ## #   `Retailers-Personal-Names-051_fol` <dbl>,
    ## #   `Retailers-Personal-Names-049` <dbl>,
    ## #   `Retailers-Personal-Names-049_fol` <dbl>,
    ## #   `Retailers-Personal-Names-047` <dbl>,
    ## #   `Retailers-Personal-Names-047_fol` <dbl>,
    ## #   `Retailers-Personal-Names-043` <dbl>,
    ## #   `Retailers-Personal-Names-043_fol` <dbl>,
    ## #   `Retailers-Personal-Names-039` <dbl>,
    ## #   `Retailers-Personal-Names-039_fol` <dbl>,
    ## #   `Retailers-Personal-Names-038` <dbl>,
    ## #   `Retailers-Personal-Names-038_fol` <dbl>,
    ## #   `Retailers-Personal-Names-037` <dbl>,
    ## #   `Retailers-Personal-Names-037_fol` <dbl>,
    ## #   `Retailers-Personal-Names-035` <dbl>,
    ## #   `Retailers-Personal-Names-035_fol` <dbl>,
    ## #   `Retailers-Personal-Names-034` <dbl>,
    ## #   `Retailers-Personal-Names-034_fol` <dbl>,
    ## #   `Retailers-Personal-Names-033` <dbl>,
    ## #   `Retailers-Personal-Names-033_fol` <dbl>,
    ## #   `Retailers-Personal-Names-031` <dbl>,
    ## #   `Retailers-Personal-Names-031_fol` <dbl>,
    ## #   `Retailers-Personal-Names-028` <dbl>,
    ## #   `Retailers-Personal-Names-028_fol` <dbl>,
    ## #   `Retailers-Personal-Names-027` <dbl>,
    ## #   `Retailers-Personal-Names-027_fol` <dbl>,
    ## #   `Retailers-Personal-Names-026` <dbl>,
    ## #   `Retailers-Personal-Names-026_fol` <dbl>,
    ## #   `Retailers-Personal-Names-025` <dbl>,
    ## #   `Retailers-Personal-Names-025_fol` <dbl>,
    ## #   `Retailers-Personal-Names-024` <dbl>,
    ## #   `Retailers-Personal-Names-024_fol` <dbl>,
    ## #   `Retailers-Personal-Names-023` <dbl>,
    ## #   `Retailers-Personal-Names-023_fol` <dbl>,
    ## #   `Retailers-Personal-Names-020` <dbl>,
    ## #   `Retailers-Personal-Names-020_fol` <dbl>,
    ## #   `Retailers-Personal-Names-019` <dbl>,
    ## #   `Retailers-Personal-Names-019_fol` <dbl>,
    ## #   `Retailers-Personal-Names-018` <dbl>,
    ## #   `Retailers-Personal-Names-018_fol` <dbl>,
    ## #   `Retailers-Personal-Names-017` <dbl>,
    ## #   `Retailers-Personal-Names-017_fol` <dbl>,
    ## #   `Retailers-Personal-Names-016` <dbl>,
    ## #   `Retailers-Personal-Names-016_fol` <dbl>,
    ## #   `Retailers-Personal-Names-015` <dbl>,
    ## #   `Retailers-Personal-Names-015_fol` <dbl>,
    ## #   `Retailers-Personal-Names-014` <dbl>,
    ## #   `Retailers-Personal-Names-014_fol` <dbl>,
    ## #   `Retailers-Personal-Names-013` <dbl>,
    ## #   `Retailers-Personal-Names-013_fol` <dbl>,
    ## #   `Retailers-Personal-Names-012` <dbl>,
    ## #   `Retailers-Personal-Names-012_fol` <dbl>,
    ## #   `Retailers-Personal-Names-010` <dbl>,
    ## #   `Retailers-Personal-Names-010_fol` <dbl>,
    ## #   `Retailers-Personal-Names-008` <dbl>,
    ## #   `Retailers-Personal-Names-008_fol` <dbl>,
    ## #   `Retailers-Personal-Names-006` <dbl>,
    ## #   `Retailers-Personal-Names-006_fol` <dbl>,
    ## #   `Retailers-Personal-Names-005` <dbl>,
    ## #   `Retailers-Personal-Names-005_fol` <dbl>,
    ## #   `Retailers-Personal-Names-004` <dbl>,
    ## #   `Retailers-Personal-Names-004_fol` <dbl>,
    ## #   `Retailers-Personal-Names-003` <dbl>,
    ## #   `Retailers-Personal-Names-003_fol` <dbl>,
    ## #   `Retailers-Personal-Names-002` <dbl>,
    ## #   `Retailers-Personal-Names-002_fol` <dbl>,
    ## #   `Retailers-Personal-Names-001` <dbl>,
    ## #   `Retailers-Personal-Names-001_fol` <dbl>, `Retailers-023` <dbl>,
    ## #   `Retailers-023_fol` <dbl>, `Retailers-022` <dbl>,
    ## #   `Retailers-022_fol` <dbl>, `Retailers-021` <dbl>,
    ## #   `Retailers-021_fol` <dbl>, `Retailers-020` <dbl>,
    ## #   `Retailers-020_fol` <dbl>, `Retailers-019` <dbl>,
    ## #   `Retailers-019_fol` <dbl>, `Retailers-018` <dbl>,
    ## #   `Retailers-018_fol` <dbl>, `Retailers-017` <dbl>,
    ## #   `Retailers-017_fol` <dbl>, `Retailers-015` <dbl>,
    ## #   `Retailers-015_fol` <dbl>, `Retailers-014` <dbl>,
    ## #   `Retailers-014_fol` <dbl>, `Retailers-013` <dbl>,
    ## #   `Retailers-013_fol` <dbl>, `Retailers-012` <dbl>,
    ## #   `Retailers-012_fol` <dbl>, `Retailers-011` <dbl>,
    ## #   `Retailers-011_fol` <dbl>, `Retailers-010` <dbl>,
    ## #   `Retailers-010_fol` <dbl>, `Retailers-009` <dbl>,
    ## #   `Retailers-009_fol` <dbl>, `Retailers-008` <dbl>,
    ## #   `Retailers-008_fol` <dbl>, …

### Gender

``` r
genders <- metadata %>% 
  select(Gender) %>% 
  filter(str_length(Gender) > 0) %>% 
  distinct() %>% 
  pull(Gender)

add_rowsums_gender <- function (gender) {
  hits <- metadata %>% 
    filter(Gender == gender) %>% 
    pull(UniqueID) %>% 
    intersect(., colnames(hexagons_raw_counts))
  name <- paste0("GEN_", gender)
  hexagons_raw_counts <<- mutate(hexagons_raw_counts, !!name := rowSums(hexagons_raw_counts[, hits]))
  invisible(gender)
}
pblapply(genders, function(gender) add_rowsums_gender(gender))
```

    ## [[1]]
    ## [1] "ORGANISATION"
    ## 
    ## [[2]]
    ## [1] "FEMALE"
    ## 
    ## [[3]]
    ## [1] "MALE"

``` r
hexagons_raw_counts[1:5,]
```

    ## # A tibble: 5 x 1,717
    ##     hex `Retailers-Pers… `Retailers-Pers… `Retailers-Pers… `Retailers-Pers…
    ##   <dbl>            <dbl>            <dbl>            <dbl>            <dbl>
    ## 1    85                0                0                0                0
    ## 2    86                0                0                0                0
    ## 3    87                0                0                0                0
    ## 4    88                0                0                0                0
    ## 5    89                0                0                0                0
    ## # … with 1,712 more variables: `Retailers-Personal-Names-052` <dbl>,
    ## #   `Retailers-Personal-Names-052_fol` <dbl>,
    ## #   `Retailers-Personal-Names-051` <dbl>,
    ## #   `Retailers-Personal-Names-051_fol` <dbl>,
    ## #   `Retailers-Personal-Names-049` <dbl>,
    ## #   `Retailers-Personal-Names-049_fol` <dbl>,
    ## #   `Retailers-Personal-Names-047` <dbl>,
    ## #   `Retailers-Personal-Names-047_fol` <dbl>,
    ## #   `Retailers-Personal-Names-043` <dbl>,
    ## #   `Retailers-Personal-Names-043_fol` <dbl>,
    ## #   `Retailers-Personal-Names-039` <dbl>,
    ## #   `Retailers-Personal-Names-039_fol` <dbl>,
    ## #   `Retailers-Personal-Names-038` <dbl>,
    ## #   `Retailers-Personal-Names-038_fol` <dbl>,
    ## #   `Retailers-Personal-Names-037` <dbl>,
    ## #   `Retailers-Personal-Names-037_fol` <dbl>,
    ## #   `Retailers-Personal-Names-035` <dbl>,
    ## #   `Retailers-Personal-Names-035_fol` <dbl>,
    ## #   `Retailers-Personal-Names-034` <dbl>,
    ## #   `Retailers-Personal-Names-034_fol` <dbl>,
    ## #   `Retailers-Personal-Names-033` <dbl>,
    ## #   `Retailers-Personal-Names-033_fol` <dbl>,
    ## #   `Retailers-Personal-Names-031` <dbl>,
    ## #   `Retailers-Personal-Names-031_fol` <dbl>,
    ## #   `Retailers-Personal-Names-028` <dbl>,
    ## #   `Retailers-Personal-Names-028_fol` <dbl>,
    ## #   `Retailers-Personal-Names-027` <dbl>,
    ## #   `Retailers-Personal-Names-027_fol` <dbl>,
    ## #   `Retailers-Personal-Names-026` <dbl>,
    ## #   `Retailers-Personal-Names-026_fol` <dbl>,
    ## #   `Retailers-Personal-Names-025` <dbl>,
    ## #   `Retailers-Personal-Names-025_fol` <dbl>,
    ## #   `Retailers-Personal-Names-024` <dbl>,
    ## #   `Retailers-Personal-Names-024_fol` <dbl>,
    ## #   `Retailers-Personal-Names-023` <dbl>,
    ## #   `Retailers-Personal-Names-023_fol` <dbl>,
    ## #   `Retailers-Personal-Names-020` <dbl>,
    ## #   `Retailers-Personal-Names-020_fol` <dbl>,
    ## #   `Retailers-Personal-Names-019` <dbl>,
    ## #   `Retailers-Personal-Names-019_fol` <dbl>,
    ## #   `Retailers-Personal-Names-018` <dbl>,
    ## #   `Retailers-Personal-Names-018_fol` <dbl>,
    ## #   `Retailers-Personal-Names-017` <dbl>,
    ## #   `Retailers-Personal-Names-017_fol` <dbl>,
    ## #   `Retailers-Personal-Names-016` <dbl>,
    ## #   `Retailers-Personal-Names-016_fol` <dbl>,
    ## #   `Retailers-Personal-Names-015` <dbl>,
    ## #   `Retailers-Personal-Names-015_fol` <dbl>,
    ## #   `Retailers-Personal-Names-014` <dbl>,
    ## #   `Retailers-Personal-Names-014_fol` <dbl>,
    ## #   `Retailers-Personal-Names-013` <dbl>,
    ## #   `Retailers-Personal-Names-013_fol` <dbl>,
    ## #   `Retailers-Personal-Names-012` <dbl>,
    ## #   `Retailers-Personal-Names-012_fol` <dbl>,
    ## #   `Retailers-Personal-Names-010` <dbl>,
    ## #   `Retailers-Personal-Names-010_fol` <dbl>,
    ## #   `Retailers-Personal-Names-008` <dbl>,
    ## #   `Retailers-Personal-Names-008_fol` <dbl>,
    ## #   `Retailers-Personal-Names-006` <dbl>,
    ## #   `Retailers-Personal-Names-006_fol` <dbl>,
    ## #   `Retailers-Personal-Names-005` <dbl>,
    ## #   `Retailers-Personal-Names-005_fol` <dbl>,
    ## #   `Retailers-Personal-Names-004` <dbl>,
    ## #   `Retailers-Personal-Names-004_fol` <dbl>,
    ## #   `Retailers-Personal-Names-003` <dbl>,
    ## #   `Retailers-Personal-Names-003_fol` <dbl>,
    ## #   `Retailers-Personal-Names-002` <dbl>,
    ## #   `Retailers-Personal-Names-002_fol` <dbl>,
    ## #   `Retailers-Personal-Names-001` <dbl>,
    ## #   `Retailers-Personal-Names-001_fol` <dbl>, `Retailers-023` <dbl>,
    ## #   `Retailers-023_fol` <dbl>, `Retailers-022` <dbl>,
    ## #   `Retailers-022_fol` <dbl>, `Retailers-021` <dbl>,
    ## #   `Retailers-021_fol` <dbl>, `Retailers-020` <dbl>,
    ## #   `Retailers-020_fol` <dbl>, `Retailers-019` <dbl>,
    ## #   `Retailers-019_fol` <dbl>, `Retailers-018` <dbl>,
    ## #   `Retailers-018_fol` <dbl>, `Retailers-017` <dbl>,
    ## #   `Retailers-017_fol` <dbl>, `Retailers-015` <dbl>,
    ## #   `Retailers-015_fol` <dbl>, `Retailers-014` <dbl>,
    ## #   `Retailers-014_fol` <dbl>, `Retailers-013` <dbl>,
    ## #   `Retailers-013_fol` <dbl>, `Retailers-012` <dbl>,
    ## #   `Retailers-012_fol` <dbl>, `Retailers-011` <dbl>,
    ## #   `Retailers-011_fol` <dbl>, `Retailers-010` <dbl>,
    ## #   `Retailers-010_fol` <dbl>, `Retailers-009` <dbl>,
    ## #   `Retailers-009_fol` <dbl>, `Retailers-008` <dbl>,
    ## #   `Retailers-008_fol` <dbl>, …

### Type

``` r
types <- metadata %>% 
  select(Type) %>% 
  filter(str_length(Type) > 0) %>% 
  distinct() %>% 
  pull(Type)

add_rowsums_type <- function (type) {
  hits <- metadata %>% 
    filter(Type == type) %>% 
    pull(UniqueID) %>% 
    intersect(., colnames(hexagons_raw_counts))
  name <- paste0("TYPE_", type)
  hexagons_raw_counts <<- mutate(hexagons_raw_counts, !!name := rowSums(hexagons_raw_counts[, hits]))
  invisible(type)
}
pblapply(types, function(type) add_rowsums_type(type))
```

    ## [[1]]
    ## [1] "Associated_companies"
    ## 
    ## [[2]]
    ## [1] "Catalysts"
    ## 
    ## [[3]]
    ## [1] "Catalysts-II"
    ## 
    ## [[4]]
    ## [1] "Creatives"
    ## 
    ## [[5]]
    ## [1] "Designers"
    ## 
    ## [[6]]
    ## [1] "Executives"
    ## 
    ## [[7]]
    ## [1] "Fashion2.0"
    ## 
    ## [[8]]
    ## [1] "Media"
    ## 
    ## [[9]]
    ## [1] "Models"
    ## 
    ## [[10]]
    ## [1] "Retailers"
    ## 
    ## [[11]]
    ## [1] "Retailers-Personal-Names"

``` r
hexagons_raw_counts[1:5,]
```

    ## # A tibble: 5 x 1,728
    ##     hex `Retailers-Pers… `Retailers-Pers… `Retailers-Pers… `Retailers-Pers…
    ##   <dbl>            <dbl>            <dbl>            <dbl>            <dbl>
    ## 1    85                0                0                0                0
    ## 2    86                0                0                0                0
    ## 3    87                0                0                0                0
    ## 4    88                0                0                0                0
    ## 5    89                0                0                0                0
    ## # … with 1,723 more variables: `Retailers-Personal-Names-052` <dbl>,
    ## #   `Retailers-Personal-Names-052_fol` <dbl>,
    ## #   `Retailers-Personal-Names-051` <dbl>,
    ## #   `Retailers-Personal-Names-051_fol` <dbl>,
    ## #   `Retailers-Personal-Names-049` <dbl>,
    ## #   `Retailers-Personal-Names-049_fol` <dbl>,
    ## #   `Retailers-Personal-Names-047` <dbl>,
    ## #   `Retailers-Personal-Names-047_fol` <dbl>,
    ## #   `Retailers-Personal-Names-043` <dbl>,
    ## #   `Retailers-Personal-Names-043_fol` <dbl>,
    ## #   `Retailers-Personal-Names-039` <dbl>,
    ## #   `Retailers-Personal-Names-039_fol` <dbl>,
    ## #   `Retailers-Personal-Names-038` <dbl>,
    ## #   `Retailers-Personal-Names-038_fol` <dbl>,
    ## #   `Retailers-Personal-Names-037` <dbl>,
    ## #   `Retailers-Personal-Names-037_fol` <dbl>,
    ## #   `Retailers-Personal-Names-035` <dbl>,
    ## #   `Retailers-Personal-Names-035_fol` <dbl>,
    ## #   `Retailers-Personal-Names-034` <dbl>,
    ## #   `Retailers-Personal-Names-034_fol` <dbl>,
    ## #   `Retailers-Personal-Names-033` <dbl>,
    ## #   `Retailers-Personal-Names-033_fol` <dbl>,
    ## #   `Retailers-Personal-Names-031` <dbl>,
    ## #   `Retailers-Personal-Names-031_fol` <dbl>,
    ## #   `Retailers-Personal-Names-028` <dbl>,
    ## #   `Retailers-Personal-Names-028_fol` <dbl>,
    ## #   `Retailers-Personal-Names-027` <dbl>,
    ## #   `Retailers-Personal-Names-027_fol` <dbl>,
    ## #   `Retailers-Personal-Names-026` <dbl>,
    ## #   `Retailers-Personal-Names-026_fol` <dbl>,
    ## #   `Retailers-Personal-Names-025` <dbl>,
    ## #   `Retailers-Personal-Names-025_fol` <dbl>,
    ## #   `Retailers-Personal-Names-024` <dbl>,
    ## #   `Retailers-Personal-Names-024_fol` <dbl>,
    ## #   `Retailers-Personal-Names-023` <dbl>,
    ## #   `Retailers-Personal-Names-023_fol` <dbl>,
    ## #   `Retailers-Personal-Names-020` <dbl>,
    ## #   `Retailers-Personal-Names-020_fol` <dbl>,
    ## #   `Retailers-Personal-Names-019` <dbl>,
    ## #   `Retailers-Personal-Names-019_fol` <dbl>,
    ## #   `Retailers-Personal-Names-018` <dbl>,
    ## #   `Retailers-Personal-Names-018_fol` <dbl>,
    ## #   `Retailers-Personal-Names-017` <dbl>,
    ## #   `Retailers-Personal-Names-017_fol` <dbl>,
    ## #   `Retailers-Personal-Names-016` <dbl>,
    ## #   `Retailers-Personal-Names-016_fol` <dbl>,
    ## #   `Retailers-Personal-Names-015` <dbl>,
    ## #   `Retailers-Personal-Names-015_fol` <dbl>,
    ## #   `Retailers-Personal-Names-014` <dbl>,
    ## #   `Retailers-Personal-Names-014_fol` <dbl>,
    ## #   `Retailers-Personal-Names-013` <dbl>,
    ## #   `Retailers-Personal-Names-013_fol` <dbl>,
    ## #   `Retailers-Personal-Names-012` <dbl>,
    ## #   `Retailers-Personal-Names-012_fol` <dbl>,
    ## #   `Retailers-Personal-Names-010` <dbl>,
    ## #   `Retailers-Personal-Names-010_fol` <dbl>,
    ## #   `Retailers-Personal-Names-008` <dbl>,
    ## #   `Retailers-Personal-Names-008_fol` <dbl>,
    ## #   `Retailers-Personal-Names-006` <dbl>,
    ## #   `Retailers-Personal-Names-006_fol` <dbl>,
    ## #   `Retailers-Personal-Names-005` <dbl>,
    ## #   `Retailers-Personal-Names-005_fol` <dbl>,
    ## #   `Retailers-Personal-Names-004` <dbl>,
    ## #   `Retailers-Personal-Names-004_fol` <dbl>,
    ## #   `Retailers-Personal-Names-003` <dbl>,
    ## #   `Retailers-Personal-Names-003_fol` <dbl>,
    ## #   `Retailers-Personal-Names-002` <dbl>,
    ## #   `Retailers-Personal-Names-002_fol` <dbl>,
    ## #   `Retailers-Personal-Names-001` <dbl>,
    ## #   `Retailers-Personal-Names-001_fol` <dbl>, `Retailers-023` <dbl>,
    ## #   `Retailers-023_fol` <dbl>, `Retailers-022` <dbl>,
    ## #   `Retailers-022_fol` <dbl>, `Retailers-021` <dbl>,
    ## #   `Retailers-021_fol` <dbl>, `Retailers-020` <dbl>,
    ## #   `Retailers-020_fol` <dbl>, `Retailers-019` <dbl>,
    ## #   `Retailers-019_fol` <dbl>, `Retailers-018` <dbl>,
    ## #   `Retailers-018_fol` <dbl>, `Retailers-017` <dbl>,
    ## #   `Retailers-017_fol` <dbl>, `Retailers-015` <dbl>,
    ## #   `Retailers-015_fol` <dbl>, `Retailers-014` <dbl>,
    ## #   `Retailers-014_fol` <dbl>, `Retailers-013` <dbl>,
    ## #   `Retailers-013_fol` <dbl>, `Retailers-012` <dbl>,
    ## #   `Retailers-012_fol` <dbl>, `Retailers-011` <dbl>,
    ## #   `Retailers-011_fol` <dbl>, `Retailers-010` <dbl>,
    ## #   `Retailers-010_fol` <dbl>, `Retailers-009` <dbl>,
    ## #   `Retailers-009_fol` <dbl>, `Retailers-008` <dbl>,
    ## #   `Retailers-008_fol` <dbl>, …

### (Category) Totals

``` r
file_names <- dir(here("analysis/data/raw_data/twitter/fashion/"), pattern =".*csv", full.names = T)
raw_columns <- gsub(basename(file_names), pattern = ".csv", replacement = "")

hexagons_raw_counts <- hexagons_raw_counts %>% 
                          mutate(
                              total = rowSums(hexagons_raw_counts[, raw_columns]),
                              total_fol = rowSums(hexagons_raw_counts[, paste0(raw_columns, "_fol")]),
                              creatives = TYPE_Creatives + TYPE_Designers,
                              business = TYPE_Executives + TYPE_Associated_companies + TYPE_Fashion2.0 + TYPE_Catalysts + `TYPE_Catalysts-II` + TYPE_Retailers + `TYPE_Retailers-Personal-Names`,
                              marketing = TYPE_Media + TYPE_Models
                              )
```

### Odds Ratio

``` r
calculate_odds_ratio <- function(phenomenon_i, phenomenon_total, random_i, random_total) {
  (phenomenon_i / phenomenon_total) / ((random_i + 1) / random_total)
}

calculate_odds_ratio_ci <- function(odds_ratio, phenomenon_i, phenomenon_total, random_i, random_total, z = 1.96) {
  exp(log(odds_ratio) - z * sqrt(
      (  1 / (phenomenon_i + 1) +
         1 / phenomenon_total + 
         1 / (random_i + 1) + 
         1 / random_total
      )
    )
  )
}

random_total <- sum(hexagons_raw_counts$random)

# type
map(types, function(type) {
  name <- paste0("TYPE_", type)
  phenomenon_total <- sum(hexagons_raw_counts[,name])
  
  name_or <-  paste0(name, "_OR")
  name_orc <- paste0(name, "_ORc")
  hexagons_raw_counts <<- hexagons_raw_counts %>% 
    mutate(!!name_or := calculate_odds_ratio(!!as.symbol(name), phenomenon_total, random, random_total),
           !!name_orc := calculate_odds_ratio_ci(!!as.symbol(name_or), 
                                                 !!as.symbol(name), phenomenon_total, random, random_total))
  invisible(name)
})
```

    ## [[1]]
    ## [1] "TYPE_Associated_companies"
    ## 
    ## [[2]]
    ## [1] "TYPE_Catalysts"
    ## 
    ## [[3]]
    ## [1] "TYPE_Catalysts-II"
    ## 
    ## [[4]]
    ## [1] "TYPE_Creatives"
    ## 
    ## [[5]]
    ## [1] "TYPE_Designers"
    ## 
    ## [[6]]
    ## [1] "TYPE_Executives"
    ## 
    ## [[7]]
    ## [1] "TYPE_Fashion2.0"
    ## 
    ## [[8]]
    ## [1] "TYPE_Media"
    ## 
    ## [[9]]
    ## [1] "TYPE_Models"
    ## 
    ## [[10]]
    ## [1] "TYPE_Retailers"
    ## 
    ## [[11]]
    ## [1] "TYPE_Retailers-Personal-Names"

``` r
# gender
map(genders, function(gender) {
  name <- paste0("GEN_", gender)
  phenomenon_total <- sum(hexagons_raw_counts[,name])
  
  name_or <-  paste0(name, "_OR")
  name_orc <- paste0(name, "_ORc")
  hexagons_raw_counts <<- hexagons_raw_counts %>% 
    mutate(!!name_or := calculate_odds_ratio(!!as.symbol(name), phenomenon_total, random, random_total),
           !!name_orc := calculate_odds_ratio_ci(!!as.symbol(name_or), 
                                                 !!as.symbol(name), phenomenon_total, random, random_total))
  invisible(name)
})
```

    ## [[1]]
    ## [1] "GEN_ORGANISATION"
    ## 
    ## [[2]]
    ## [1] "GEN_FEMALE"
    ## 
    ## [[3]]
    ## [1] "GEN_MALE"

``` r
# nationalities
map(nationalities, function(nat) {
  name <- paste0("NAT_", nat)
  phenomenon_total <- sum(hexagons_raw_counts[,name])
  
  name_or <-  paste0(name, "_OR")
  name_orc <- paste0(name, "_ORc")
  hexagons_raw_counts <<- hexagons_raw_counts %>% 
    mutate(!!name_or := calculate_odds_ratio(!!as.symbol(name), phenomenon_total, random, random_total),
           !!name_orc := calculate_odds_ratio_ci(!!as.symbol(name_or), 
                                                 !!as.symbol(name), phenomenon_total, random, random_total))
  invisible(name)
})
```

    ## [[1]]
    ## [1] "NAT_FRANCE"
    ## 
    ## [[2]]
    ## [1] "NAT_CHINA"
    ## 
    ## [[3]]
    ## [1] "NAT_SWEDEN"
    ## 
    ## [[4]]
    ## [1] "NAT_GERMANY"
    ## 
    ## [[5]]
    ## [1] "NAT_ITALY"
    ## 
    ## [[6]]
    ## [1] "NAT_UK"
    ## 
    ## [[7]]
    ## [1] "NAT_USA"
    ## 
    ## [[8]]
    ## [1] "NAT_JAPAN"
    ## 
    ## [[9]]
    ## [1] "NAT_INDIA"
    ## 
    ## [[10]]
    ## [1] "NAT_SPAIN"
    ## 
    ## [[11]]
    ## [1] "NAT_SOUTH AFRICA"
    ## 
    ## [[12]]
    ## [1] "NAT_BELGIUM"
    ## 
    ## [[13]]
    ## [1] "NAT_BRAZIL"
    ## 
    ## [[14]]
    ## [1] "NAT_AUSTRALIA"
    ## 
    ## [[15]]
    ## [1] "NAT_AUSTRIA"
    ## 
    ## [[16]]
    ## [1] "NAT_KOREA"
    ## 
    ## [[17]]
    ## [1] "NAT_SWITZERLAND"
    ## 
    ## [[18]]
    ## [1] "NAT_VENEZUELA"
    ## 
    ## [[19]]
    ## [1] "NAT_UKRAINE"
    ## 
    ## [[20]]
    ## [1] "NAT_PERU"
    ## 
    ## [[21]]
    ## [1] "NAT_NIGERIA"
    ## 
    ## [[22]]
    ## [1] "NAT_QATAR"
    ## 
    ## [[23]]
    ## [1] "NAT_EGYPT"
    ## 
    ## [[24]]
    ## [1] "NAT_INDONESIA"
    ## 
    ## [[25]]
    ## [1] "NAT_UNITED ARAB EMIRATES"
    ## 
    ## [[26]]
    ## [1] "NAT_RUSSIA"
    ## 
    ## [[27]]
    ## [1] "NAT_IRELAND"
    ## 
    ## [[28]]
    ## [1] "NAT_GHANA"
    ## 
    ## [[29]]
    ## [1] "NAT_CANADA"
    ## 
    ## [[30]]
    ## [1] "NAT_NETHERLANDS"
    ## 
    ## [[31]]
    ## [1] "NAT_UK/USA"
    ## 
    ## [[32]]
    ## [1] "NAT_TURKEY"
    ## 
    ## [[33]]
    ## [1] "NAT_GREECE"
    ## 
    ## [[34]]
    ## [1] "NAT_NORWAY"
    ## 
    ## [[35]]
    ## [1] "NAT_PHILLIPPINES"
    ## 
    ## [[36]]
    ## [1] "NAT_ISRAEL"
    ## 
    ## [[37]]
    ## [1] "NAT_KENYA"
    ## 
    ## [[38]]
    ## [1] "NAT_TUNISIA"
    ## 
    ## [[39]]
    ## [1] "NAT_LEBANON"
    ## 
    ## [[40]]
    ## [1] "NAT_TURKEY/CANADA"
    ## 
    ## [[41]]
    ## [1] "NAT_PORTUGAL"
    ## 
    ## [[42]]
    ## [1] "NAT_COLUMBIA"
    ## 
    ## [[43]]
    ## [1] "NAT_DOMINICAN REPUBLIC"
    ## 
    ## [[44]]
    ## [1] "NAT_NEPAL"
    ## 
    ## [[45]]
    ## [1] "NAT_SERBIA"
    ## 
    ## [[46]]
    ## [1] "NAT_THAILAND"
    ## 
    ## [[47]]
    ## [1] "NAT_GEORGIA"
    ## 
    ## [[48]]
    ## [1] "NAT_JAPAN/KOREA"
    ## 
    ## [[49]]
    ## [1] "NAT_SINGAPORE"
    ## 
    ## [[50]]
    ## [1] "NAT_SAUDI ARABIA"
    ## 
    ## [[51]]
    ## [1] "NAT_KUWAIT"
    ## 
    ## [[52]]
    ## [1] "NAT_ARGENTINA"
    ## 
    ## [[53]]
    ## [1] "NAT_FRANCE/MOROCCO"
    ## 
    ## [[54]]
    ## [1] "NAT_VIETNAM"
    ## 
    ## [[55]]
    ## [1] "NAT_CHILE"
    ## 
    ## [[56]]
    ## [1] "NAT_ICELAND"
    ## 
    ## [[57]]
    ## [1] "NAT_BELGUIM"
    ## 
    ## [[58]]
    ## [1] "NAT_DENMARK"
    ## 
    ## [[59]]
    ## [1] "NAT_PUERTO RICA"
    ## 
    ## [[60]]
    ## [1] "NAT_ETHIOPIA"
    ## 
    ## [[61]]
    ## [1] "NAT_MALAYSIA"
    ## 
    ## [[62]]
    ## [1] "NAT_KAZAKHSTAN"
    ## 
    ## [[63]]
    ## [1] "NAT_LUXEMBOURG"

``` r
# all queries
map(file_names, function(file) {
  name <- file %>% 
    basename() %>% 
    gsub(pattern = ".csv", replacement = "")
  phenomenon_total <- sum(hexagons_raw_counts[,name])
  
  name_or <-  paste0(name, "_OR")
  name_orc <- paste0(name, "_ORc")
  hexagons_raw_counts <<- hexagons_raw_counts %>% 
    mutate(!!name_or := calculate_odds_ratio(!!as.symbol(name), phenomenon_total, random, random_total),
           !!name_orc := calculate_odds_ratio_ci(!!as.symbol(name_or), 
                                                 !!as.symbol(name), phenomenon_total, random, random_total))
  invisible(name)
})
```

    ## [[1]]
    ## [1] "Associated_companies-001"
    ## 
    ## [[2]]
    ## [1] "Associated_companies-002"
    ## 
    ## [[3]]
    ## [1] "Associated_companies-003"
    ## 
    ## [[4]]
    ## [1] "Associated_companies-004"
    ## 
    ## [[5]]
    ## [1] "Associated_companies-005"
    ## 
    ## [[6]]
    ## [1] "Associated_companies-006"
    ## 
    ## [[7]]
    ## [1] "Associated_companies-007"
    ## 
    ## [[8]]
    ## [1] "Associated_companies-008"
    ## 
    ## [[9]]
    ## [1] "Associated_companies-009"
    ## 
    ## [[10]]
    ## [1] "Associated_companies-010"
    ## 
    ## [[11]]
    ## [1] "Associated_companies-011"
    ## 
    ## [[12]]
    ## [1] "Associated_companies-012"
    ## 
    ## [[13]]
    ## [1] "Associated_companies-013"
    ## 
    ## [[14]]
    ## [1] "Associated_companies-014"
    ## 
    ## [[15]]
    ## [1] "Associated_companies-015"
    ## 
    ## [[16]]
    ## [1] "Associated_companies-016"
    ## 
    ## [[17]]
    ## [1] "Associated_companies-017"
    ## 
    ## [[18]]
    ## [1] "Associated_companies-018"
    ## 
    ## [[19]]
    ## [1] "Associated_companies-019"
    ## 
    ## [[20]]
    ## [1] "Associated_companies-020"
    ## 
    ## [[21]]
    ## [1] "Associated_companies-022"
    ## 
    ## [[22]]
    ## [1] "Associated_companies-024"
    ## 
    ## [[23]]
    ## [1] "Associated_companies-025"
    ## 
    ## [[24]]
    ## [1] "Associated_companies-027"
    ## 
    ## [[25]]
    ## [1] "Associated_companies-028"
    ## 
    ## [[26]]
    ## [1] "Associated_companies-030"
    ## 
    ## [[27]]
    ## [1] "Associated_companies-032"
    ## 
    ## [[28]]
    ## [1] "Associated_companies-033"
    ## 
    ## [[29]]
    ## [1] "Associated_companies-034"
    ## 
    ## [[30]]
    ## [1] "Associated_companies-035"
    ## 
    ## [[31]]
    ## [1] "Associated_companies-036"
    ## 
    ## [[32]]
    ## [1] "Associated_companies-037"
    ## 
    ## [[33]]
    ## [1] "Associated_companies-038"
    ## 
    ## [[34]]
    ## [1] "Associated_companies-039"
    ## 
    ## [[35]]
    ## [1] "Associated_companies-040"
    ## 
    ## [[36]]
    ## [1] "Associated_companies-042"
    ## 
    ## [[37]]
    ## [1] "Associated_companies-043"
    ## 
    ## [[38]]
    ## [1] "Associated_companies-044"
    ## 
    ## [[39]]
    ## [1] "Associated_companies-045"
    ## 
    ## [[40]]
    ## [1] "Associated_companies-046"
    ## 
    ## [[41]]
    ## [1] "Associated_companies-047"
    ## 
    ## [[42]]
    ## [1] "Associated_companies-048"
    ## 
    ## [[43]]
    ## [1] "Associated_companies-049"
    ## 
    ## [[44]]
    ## [1] "Associated_companies-050"
    ## 
    ## [[45]]
    ## [1] "Associated_companies-051"
    ## 
    ## [[46]]
    ## [1] "Associated_companies-052"
    ## 
    ## [[47]]
    ## [1] "Associated_companies-053"
    ## 
    ## [[48]]
    ## [1] "Associated_companies-055"
    ## 
    ## [[49]]
    ## [1] "Associated_companies-056"
    ## 
    ## [[50]]
    ## [1] "Associated_companies-057"
    ## 
    ## [[51]]
    ## [1] "Associated_companies-058"
    ## 
    ## [[52]]
    ## [1] "Associated_companies-059"
    ## 
    ## [[53]]
    ## [1] "Associated_companies-061"
    ## 
    ## [[54]]
    ## [1] "Associated_companies-062"
    ## 
    ## [[55]]
    ## [1] "Associated_companies-063"
    ## 
    ## [[56]]
    ## [1] "Associated_companies-064"
    ## 
    ## [[57]]
    ## [1] "Associated_companies-065"
    ## 
    ## [[58]]
    ## [1] "Associated_companies-067"
    ## 
    ## [[59]]
    ## [1] "Associated_companies-068"
    ## 
    ## [[60]]
    ## [1] "Associated_companies-069"
    ## 
    ## [[61]]
    ## [1] "Associated_companies-070"
    ## 
    ## [[62]]
    ## [1] "Associated_companies-071"
    ## 
    ## [[63]]
    ## [1] "Associated_companies-072"
    ## 
    ## [[64]]
    ## [1] "Associated_companies-073"
    ## 
    ## [[65]]
    ## [1] "Associated_companies-074"
    ## 
    ## [[66]]
    ## [1] "Associated_companies-075"
    ## 
    ## [[67]]
    ## [1] "Associated_companies-076"
    ## 
    ## [[68]]
    ## [1] "Associated_companies-079"
    ## 
    ## [[69]]
    ## [1] "Associated_companies-080"
    ## 
    ## [[70]]
    ## [1] "Associated_companies-081"
    ## 
    ## [[71]]
    ## [1] "Associated_companies-083"
    ## 
    ## [[72]]
    ## [1] "Associated_companies-084"
    ## 
    ## [[73]]
    ## [1] "Associated_companies-086"
    ## 
    ## [[74]]
    ## [1] "Associated_companies-087"
    ## 
    ## [[75]]
    ## [1] "Associated_companies-088"
    ## 
    ## [[76]]
    ## [1] "Associated_companies-089"
    ## 
    ## [[77]]
    ## [1] "associated_companies-090"
    ## 
    ## [[78]]
    ## [1] "Associated_companies-091"
    ## 
    ## [[79]]
    ## [1] "Associated_companies-092"
    ## 
    ## [[80]]
    ## [1] "Associated_companies-093"
    ## 
    ## [[81]]
    ## [1] "Associated_companies-094"
    ## 
    ## [[82]]
    ## [1] "Associated_companies-095"
    ## 
    ## [[83]]
    ## [1] "Associated_companies-096"
    ## 
    ## [[84]]
    ## [1] "Associated_companies-097"
    ## 
    ## [[85]]
    ## [1] "Associated_companies-098"
    ## 
    ## [[86]]
    ## [1] "Associated_companies-099"
    ## 
    ## [[87]]
    ## [1] "Associated_companies-100"
    ## 
    ## [[88]]
    ## [1] "Associated_companies-101"
    ## 
    ## [[89]]
    ## [1] "Associated_companies-102"
    ## 
    ## [[90]]
    ## [1] "Associated_companies-103"
    ## 
    ## [[91]]
    ## [1] "Catalysts-002"
    ## 
    ## [[92]]
    ## [1] "Catalysts-003"
    ## 
    ## [[93]]
    ## [1] "Catalysts-005"
    ## 
    ## [[94]]
    ## [1] "Catalysts-006"
    ## 
    ## [[95]]
    ## [1] "Catalysts-007"
    ## 
    ## [[96]]
    ## [1] "Catalysts-008"
    ## 
    ## [[97]]
    ## [1] "Catalysts-009"
    ## 
    ## [[98]]
    ## [1] "Catalysts-010"
    ## 
    ## [[99]]
    ## [1] "Catalysts-011"
    ## 
    ## [[100]]
    ## [1] "Catalysts-012"
    ## 
    ## [[101]]
    ## [1] "Catalysts-013"
    ## 
    ## [[102]]
    ## [1] "Catalysts-015"
    ## 
    ## [[103]]
    ## [1] "Catalysts-016"
    ## 
    ## [[104]]
    ## [1] "Catalysts-017"
    ## 
    ## [[105]]
    ## [1] "Catalysts-018"
    ## 
    ## [[106]]
    ## [1] "Catalysts-019"
    ## 
    ## [[107]]
    ## [1] "Catalysts-020"
    ## 
    ## [[108]]
    ## [1] "Catalysts-021"
    ## 
    ## [[109]]
    ## [1] "Catalysts-022"
    ## 
    ## [[110]]
    ## [1] "Catalysts-023"
    ## 
    ## [[111]]
    ## [1] "Catalysts-024"
    ## 
    ## [[112]]
    ## [1] "Catalysts-025"
    ## 
    ## [[113]]
    ## [1] "Catalysts-027"
    ## 
    ## [[114]]
    ## [1] "Catalysts-029"
    ## 
    ## [[115]]
    ## [1] "Catalysts-031"
    ## 
    ## [[116]]
    ## [1] "Catalysts-032"
    ## 
    ## [[117]]
    ## [1] "Catalysts-033"
    ## 
    ## [[118]]
    ## [1] "Catalysts-034"
    ## 
    ## [[119]]
    ## [1] "Catalysts-035"
    ## 
    ## [[120]]
    ## [1] "Catalysts-036"
    ## 
    ## [[121]]
    ## [1] "Catalysts-038"
    ## 
    ## [[122]]
    ## [1] "Catalysts-039"
    ## 
    ## [[123]]
    ## [1] "Catalysts-040"
    ## 
    ## [[124]]
    ## [1] "Catalysts-042"
    ## 
    ## [[125]]
    ## [1] "Catalysts-044"
    ## 
    ## [[126]]
    ## [1] "Catalysts-045"
    ## 
    ## [[127]]
    ## [1] "Catalysts-046"
    ## 
    ## [[128]]
    ## [1] "Catalysts-047"
    ## 
    ## [[129]]
    ## [1] "Catalysts-048"
    ## 
    ## [[130]]
    ## [1] "Catalysts-049"
    ## 
    ## [[131]]
    ## [1] "Catalysts-050"
    ## 
    ## [[132]]
    ## [1] "Catalysts-051"
    ## 
    ## [[133]]
    ## [1] "Catalysts-052"
    ## 
    ## [[134]]
    ## [1] "Catalysts-053"
    ## 
    ## [[135]]
    ## [1] "Catalysts-054"
    ## 
    ## [[136]]
    ## [1] "Catalysts-055"
    ## 
    ## [[137]]
    ## [1] "Catalysts-056"
    ## 
    ## [[138]]
    ## [1] "Catalysts-057"
    ## 
    ## [[139]]
    ## [1] "Catalysts-058"
    ## 
    ## [[140]]
    ## [1] "Catalysts-059"
    ## 
    ## [[141]]
    ## [1] "Catalysts-060"
    ## 
    ## [[142]]
    ## [1] "Catalysts-061"
    ## 
    ## [[143]]
    ## [1] "Catalysts-062"
    ## 
    ## [[144]]
    ## [1] "Catalysts-063"
    ## 
    ## [[145]]
    ## [1] "Catalysts-064"
    ## 
    ## [[146]]
    ## [1] "Catalysts-065"
    ## 
    ## [[147]]
    ## [1] "Catalysts-067"
    ## 
    ## [[148]]
    ## [1] "Catalysts-II-001"
    ## 
    ## [[149]]
    ## [1] "Catalysts-II-002"
    ## 
    ## [[150]]
    ## [1] "Catalysts-II-003"
    ## 
    ## [[151]]
    ## [1] "Catalysts-II-004"
    ## 
    ## [[152]]
    ## [1] "Catalysts-II-005"
    ## 
    ## [[153]]
    ## [1] "Catalysts-II-006"
    ## 
    ## [[154]]
    ## [1] "Catalysts-II-007"
    ## 
    ## [[155]]
    ## [1] "Catalysts-II-008"
    ## 
    ## [[156]]
    ## [1] "Catalysts-II-009"
    ## 
    ## [[157]]
    ## [1] "Catalysts-II-010"
    ## 
    ## [[158]]
    ## [1] "Catalysts-II-011"
    ## 
    ## [[159]]
    ## [1] "Catalysts-II-012"
    ## 
    ## [[160]]
    ## [1] "Catalysts-II-013"
    ## 
    ## [[161]]
    ## [1] "Catalysts-II-014"
    ## 
    ## [[162]]
    ## [1] "Catalysts-II-015"
    ## 
    ## [[163]]
    ## [1] "Catalysts-II-017"
    ## 
    ## [[164]]
    ## [1] "Catalysts-II-018"
    ## 
    ## [[165]]
    ## [1] "Catalysts-II-019"
    ## 
    ## [[166]]
    ## [1] "Catalysts-II-020"
    ## 
    ## [[167]]
    ## [1] "Catalysts-II-021"
    ## 
    ## [[168]]
    ## [1] "Catalysts-II-022"
    ## 
    ## [[169]]
    ## [1] "Catalysts-II-023"
    ## 
    ## [[170]]
    ## [1] "Catalysts-II-024"
    ## 
    ## [[171]]
    ## [1] "Creatives-001"
    ## 
    ## [[172]]
    ## [1] "Creatives-002"
    ## 
    ## [[173]]
    ## [1] "Creatives-004"
    ## 
    ## [[174]]
    ## [1] "Creatives-005"
    ## 
    ## [[175]]
    ## [1] "Creatives-006"
    ## 
    ## [[176]]
    ## [1] "Creatives-007"
    ## 
    ## [[177]]
    ## [1] "Creatives-008"
    ## 
    ## [[178]]
    ## [1] "Creatives-009"
    ## 
    ## [[179]]
    ## [1] "Creatives-010"
    ## 
    ## [[180]]
    ## [1] "Creatives-011"
    ## 
    ## [[181]]
    ## [1] "Creatives-012"
    ## 
    ## [[182]]
    ## [1] "Creatives-013"
    ## 
    ## [[183]]
    ## [1] "Creatives-014"
    ## 
    ## [[184]]
    ## [1] "Creatives-016"
    ## 
    ## [[185]]
    ## [1] "Creatives-017"
    ## 
    ## [[186]]
    ## [1] "Creatives-018"
    ## 
    ## [[187]]
    ## [1] "Creatives-019"
    ## 
    ## [[188]]
    ## [1] "Creatives-020"
    ## 
    ## [[189]]
    ## [1] "Creatives-021"
    ## 
    ## [[190]]
    ## [1] "Creatives-022"
    ## 
    ## [[191]]
    ## [1] "Creatives-023"
    ## 
    ## [[192]]
    ## [1] "Creatives-024"
    ## 
    ## [[193]]
    ## [1] "Creatives-026"
    ## 
    ## [[194]]
    ## [1] "Creatives-027"
    ## 
    ## [[195]]
    ## [1] "Creatives-028"
    ## 
    ## [[196]]
    ## [1] "Creatives-029"
    ## 
    ## [[197]]
    ## [1] "Creatives-030"
    ## 
    ## [[198]]
    ## [1] "Creatives-031"
    ## 
    ## [[199]]
    ## [1] "Creatives-032"
    ## 
    ## [[200]]
    ## [1] "Creatives-033"
    ## 
    ## [[201]]
    ## [1] "Creatives-034"
    ## 
    ## [[202]]
    ## [1] "Creatives-035"
    ## 
    ## [[203]]
    ## [1] "Creatives-036"
    ## 
    ## [[204]]
    ## [1] "Creatives-037"
    ## 
    ## [[205]]
    ## [1] "Creatives-038"
    ## 
    ## [[206]]
    ## [1] "Creatives-039"
    ## 
    ## [[207]]
    ## [1] "Creatives-040"
    ## 
    ## [[208]]
    ## [1] "Creatives-041"
    ## 
    ## [[209]]
    ## [1] "Creatives-042"
    ## 
    ## [[210]]
    ## [1] "Creatives-043"
    ## 
    ## [[211]]
    ## [1] "Creatives-044"
    ## 
    ## [[212]]
    ## [1] "Creatives-045"
    ## 
    ## [[213]]
    ## [1] "Creatives-046"
    ## 
    ## [[214]]
    ## [1] "Creatives-047"
    ## 
    ## [[215]]
    ## [1] "Creatives-048"
    ## 
    ## [[216]]
    ## [1] "Creatives-049"
    ## 
    ## [[217]]
    ## [1] "Creatives-050"
    ## 
    ## [[218]]
    ## [1] "Creatives-051"
    ## 
    ## [[219]]
    ## [1] "Creatives-052"
    ## 
    ## [[220]]
    ## [1] "Creatives-053"
    ## 
    ## [[221]]
    ## [1] "Creatives-054"
    ## 
    ## [[222]]
    ## [1] "Creatives-056"
    ## 
    ## [[223]]
    ## [1] "Creatives-057"
    ## 
    ## [[224]]
    ## [1] "Creatives-058"
    ## 
    ## [[225]]
    ## [1] "Creatives-059"
    ## 
    ## [[226]]
    ## [1] "Creatives-060"
    ## 
    ## [[227]]
    ## [1] "Creatives-062"
    ## 
    ## [[228]]
    ## [1] "Creatives-063"
    ## 
    ## [[229]]
    ## [1] "Creatives-064"
    ## 
    ## [[230]]
    ## [1] "Creatives-065"
    ## 
    ## [[231]]
    ## [1] "Creatives-066"
    ## 
    ## [[232]]
    ## [1] "Creatives-067"
    ## 
    ## [[233]]
    ## [1] "Creatives-068"
    ## 
    ## [[234]]
    ## [1] "Creatives-069"
    ## 
    ## [[235]]
    ## [1] "Creatives-070"
    ## 
    ## [[236]]
    ## [1] "Creatives-071"
    ## 
    ## [[237]]
    ## [1] "Creatives-072"
    ## 
    ## [[238]]
    ## [1] "Creatives-073"
    ## 
    ## [[239]]
    ## [1] "Creatives-074"
    ## 
    ## [[240]]
    ## [1] "Creatives-075"
    ## 
    ## [[241]]
    ## [1] "Creatives-076"
    ## 
    ## [[242]]
    ## [1] "Creatives-077"
    ## 
    ## [[243]]
    ## [1] "Creatives-078"
    ## 
    ## [[244]]
    ## [1] "Creatives-079"
    ## 
    ## [[245]]
    ## [1] "Creatives-080"
    ## 
    ## [[246]]
    ## [1] "Creatives-081"
    ## 
    ## [[247]]
    ## [1] "Creatives-082"
    ## 
    ## [[248]]
    ## [1] "Creatives-083"
    ## 
    ## [[249]]
    ## [1] "Creatives-084"
    ## 
    ## [[250]]
    ## [1] "Creatives-085"
    ## 
    ## [[251]]
    ## [1] "Creatives-086"
    ## 
    ## [[252]]
    ## [1] "Creatives-087"
    ## 
    ## [[253]]
    ## [1] "Creatives-088"
    ## 
    ## [[254]]
    ## [1] "Creatives-089"
    ## 
    ## [[255]]
    ## [1] "Creatives-090"
    ## 
    ## [[256]]
    ## [1] "Creatives-091"
    ## 
    ## [[257]]
    ## [1] "Creatives-093"
    ## 
    ## [[258]]
    ## [1] "Creatives-095"
    ## 
    ## [[259]]
    ## [1] "Creatives-097"
    ## 
    ## [[260]]
    ## [1] "Creatives-098"
    ## 
    ## [[261]]
    ## [1] "Creatives-099"
    ## 
    ## [[262]]
    ## [1] "Creatives-101"
    ## 
    ## [[263]]
    ## [1] "Creatives-102"
    ## 
    ## [[264]]
    ## [1] "Creatives-103"
    ## 
    ## [[265]]
    ## [1] "Creatives-104"
    ## 
    ## [[266]]
    ## [1] "Creatives-105"
    ## 
    ## [[267]]
    ## [1] "Creatives-106"
    ## 
    ## [[268]]
    ## [1] "Creatives-107"
    ## 
    ## [[269]]
    ## [1] "Creatives-108"
    ## 
    ## [[270]]
    ## [1] "Creatives-109"
    ## 
    ## [[271]]
    ## [1] "Creatives-110"
    ## 
    ## [[272]]
    ## [1] "Creatives-111"
    ## 
    ## [[273]]
    ## [1] "Creatives-112"
    ## 
    ## [[274]]
    ## [1] "Creatives-113"
    ## 
    ## [[275]]
    ## [1] "Creatives-114"
    ## 
    ## [[276]]
    ## [1] "Creatives-115"
    ## 
    ## [[277]]
    ## [1] "Creatives-116"
    ## 
    ## [[278]]
    ## [1] "Creatives-118"
    ## 
    ## [[279]]
    ## [1] "Creatives-119"
    ## 
    ## [[280]]
    ## [1] "Creatives-120"
    ## 
    ## [[281]]
    ## [1] "Creatives-121"
    ## 
    ## [[282]]
    ## [1] "Creatives-122"
    ## 
    ## [[283]]
    ## [1] "Creatives-123"
    ## 
    ## [[284]]
    ## [1] "Creatives-124"
    ## 
    ## [[285]]
    ## [1] "Creatives-125"
    ## 
    ## [[286]]
    ## [1] "Creatives-127"
    ## 
    ## [[287]]
    ## [1] "Creatives-128"
    ## 
    ## [[288]]
    ## [1] "Creatives-129"
    ## 
    ## [[289]]
    ## [1] "Creatives-130"
    ## 
    ## [[290]]
    ## [1] "Designers-002"
    ## 
    ## [[291]]
    ## [1] "Designers-003"
    ## 
    ## [[292]]
    ## [1] "Designers-004"
    ## 
    ## [[293]]
    ## [1] "Designers-005"
    ## 
    ## [[294]]
    ## [1] "Designers-006"
    ## 
    ## [[295]]
    ## [1] "Designers-007"
    ## 
    ## [[296]]
    ## [1] "Designers-008"
    ## 
    ## [[297]]
    ## [1] "Designers-009"
    ## 
    ## [[298]]
    ## [1] "Designers-010"
    ## 
    ## [[299]]
    ## [1] "Designers-011"
    ## 
    ## [[300]]
    ## [1] "Designers-012"
    ## 
    ## [[301]]
    ## [1] "Designers-013"
    ## 
    ## [[302]]
    ## [1] "Designers-014"
    ## 
    ## [[303]]
    ## [1] "Designers-015"
    ## 
    ## [[304]]
    ## [1] "Designers-016"
    ## 
    ## [[305]]
    ## [1] "Designers-017"
    ## 
    ## [[306]]
    ## [1] "Designers-018"
    ## 
    ## [[307]]
    ## [1] "Designers-019"
    ## 
    ## [[308]]
    ## [1] "Designers-020"
    ## 
    ## [[309]]
    ## [1] "Designers-021"
    ## 
    ## [[310]]
    ## [1] "Designers-023"
    ## 
    ## [[311]]
    ## [1] "Designers-024"
    ## 
    ## [[312]]
    ## [1] "Designers-025"
    ## 
    ## [[313]]
    ## [1] "Designers-026"
    ## 
    ## [[314]]
    ## [1] "Designers-027"
    ## 
    ## [[315]]
    ## [1] "Designers-028"
    ## 
    ## [[316]]
    ## [1] "Designers-029"
    ## 
    ## [[317]]
    ## [1] "Designers-030"
    ## 
    ## [[318]]
    ## [1] "Designers-031"
    ## 
    ## [[319]]
    ## [1] "Designers-032"
    ## 
    ## [[320]]
    ## [1] "Designers-033"
    ## 
    ## [[321]]
    ## [1] "Designers-034"
    ## 
    ## [[322]]
    ## [1] "Designers-035"
    ## 
    ## [[323]]
    ## [1] "Designers-037"
    ## 
    ## [[324]]
    ## [1] "Designers-038"
    ## 
    ## [[325]]
    ## [1] "Designers-039"
    ## 
    ## [[326]]
    ## [1] "Designers-040"
    ## 
    ## [[327]]
    ## [1] "Designers-041"
    ## 
    ## [[328]]
    ## [1] "Designers-042"
    ## 
    ## [[329]]
    ## [1] "Designers-043"
    ## 
    ## [[330]]
    ## [1] "Designers-044"
    ## 
    ## [[331]]
    ## [1] "Designers-045"
    ## 
    ## [[332]]
    ## [1] "Designers-046"
    ## 
    ## [[333]]
    ## [1] "Designers-047"
    ## 
    ## [[334]]
    ## [1] "Designers-048"
    ## 
    ## [[335]]
    ## [1] "Designers-049"
    ## 
    ## [[336]]
    ## [1] "Designers-050"
    ## 
    ## [[337]]
    ## [1] "Designers-051"
    ## 
    ## [[338]]
    ## [1] "Designers-052"
    ## 
    ## [[339]]
    ## [1] "Designers-053"
    ## 
    ## [[340]]
    ## [1] "Designers-054"
    ## 
    ## [[341]]
    ## [1] "Designers-055"
    ## 
    ## [[342]]
    ## [1] "Designers-056"
    ## 
    ## [[343]]
    ## [1] "Designers-057"
    ## 
    ## [[344]]
    ## [1] "Designers-058"
    ## 
    ## [[345]]
    ## [1] "Designers-059"
    ## 
    ## [[346]]
    ## [1] "Designers-060"
    ## 
    ## [[347]]
    ## [1] "Designers-061"
    ## 
    ## [[348]]
    ## [1] "Designers-062"
    ## 
    ## [[349]]
    ## [1] "Designers-063"
    ## 
    ## [[350]]
    ## [1] "Designers-064"
    ## 
    ## [[351]]
    ## [1] "Designers-065"
    ## 
    ## [[352]]
    ## [1] "Designers-066"
    ## 
    ## [[353]]
    ## [1] "Designers-067"
    ## 
    ## [[354]]
    ## [1] "Designers-068"
    ## 
    ## [[355]]
    ## [1] "Designers-069"
    ## 
    ## [[356]]
    ## [1] "Designers-070"
    ## 
    ## [[357]]
    ## [1] "Designers-071"
    ## 
    ## [[358]]
    ## [1] "Designers-073"
    ## 
    ## [[359]]
    ## [1] "Designers-074"
    ## 
    ## [[360]]
    ## [1] "Designers-075"
    ## 
    ## [[361]]
    ## [1] "Designers-076"
    ## 
    ## [[362]]
    ## [1] "Designers-077"
    ## 
    ## [[363]]
    ## [1] "Designers-078"
    ## 
    ## [[364]]
    ## [1] "Designers-079"
    ## 
    ## [[365]]
    ## [1] "Designers-080"
    ## 
    ## [[366]]
    ## [1] "Designers-081"
    ## 
    ## [[367]]
    ## [1] "Designers-082"
    ## 
    ## [[368]]
    ## [1] "Designers-084"
    ## 
    ## [[369]]
    ## [1] "Designers-085"
    ## 
    ## [[370]]
    ## [1] "Designers-086"
    ## 
    ## [[371]]
    ## [1] "Designers-087"
    ## 
    ## [[372]]
    ## [1] "Designers-088"
    ## 
    ## [[373]]
    ## [1] "Designers-089"
    ## 
    ## [[374]]
    ## [1] "Designers-091"
    ## 
    ## [[375]]
    ## [1] "Designers-092"
    ## 
    ## [[376]]
    ## [1] "Designers-093"
    ## 
    ## [[377]]
    ## [1] "Designers-094"
    ## 
    ## [[378]]
    ## [1] "Designers-095"
    ## 
    ## [[379]]
    ## [1] "Designers-096"
    ## 
    ## [[380]]
    ## [1] "Designers-097"
    ## 
    ## [[381]]
    ## [1] "Designers-098"
    ## 
    ## [[382]]
    ## [1] "Designers-100"
    ## 
    ## [[383]]
    ## [1] "Designers-101"
    ## 
    ## [[384]]
    ## [1] "Designers-102"
    ## 
    ## [[385]]
    ## [1] "Designers-103"
    ## 
    ## [[386]]
    ## [1] "Designers-104"
    ## 
    ## [[387]]
    ## [1] "Designers-105"
    ## 
    ## [[388]]
    ## [1] "Designers-106"
    ## 
    ## [[389]]
    ## [1] "Designers-107"
    ## 
    ## [[390]]
    ## [1] "Designers-108"
    ## 
    ## [[391]]
    ## [1] "Designers-109"
    ## 
    ## [[392]]
    ## [1] "Designers-111"
    ## 
    ## [[393]]
    ## [1] "Designers-112"
    ## 
    ## [[394]]
    ## [1] "Designers-113"
    ## 
    ## [[395]]
    ## [1] "Designers-115"
    ## 
    ## [[396]]
    ## [1] "Designers-116"
    ## 
    ## [[397]]
    ## [1] "Designers-117"
    ## 
    ## [[398]]
    ## [1] "Designers-118"
    ## 
    ## [[399]]
    ## [1] "Designers-119"
    ## 
    ## [[400]]
    ## [1] "Designers-120"
    ## 
    ## [[401]]
    ## [1] "Designers-121"
    ## 
    ## [[402]]
    ## [1] "Designers-122"
    ## 
    ## [[403]]
    ## [1] "Designers-123"
    ## 
    ## [[404]]
    ## [1] "Designers-124"
    ## 
    ## [[405]]
    ## [1] "Designers-125"
    ## 
    ## [[406]]
    ## [1] "Designers-126"
    ## 
    ## [[407]]
    ## [1] "Designers-127"
    ## 
    ## [[408]]
    ## [1] "Designers-128"
    ## 
    ## [[409]]
    ## [1] "Designers-129"
    ## 
    ## [[410]]
    ## [1] "Designers-130"
    ## 
    ## [[411]]
    ## [1] "Designers-132"
    ## 
    ## [[412]]
    ## [1] "Designers-133"
    ## 
    ## [[413]]
    ## [1] "Designers-134"
    ## 
    ## [[414]]
    ## [1] "Designers-135"
    ## 
    ## [[415]]
    ## [1] "Designers-136"
    ## 
    ## [[416]]
    ## [1] "Designers-137"
    ## 
    ## [[417]]
    ## [1] "Designers-138"
    ## 
    ## [[418]]
    ## [1] "Designers-139"
    ## 
    ## [[419]]
    ## [1] "Designers-140"
    ## 
    ## [[420]]
    ## [1] "Designers-141"
    ## 
    ## [[421]]
    ## [1] "Designers-142"
    ## 
    ## [[422]]
    ## [1] "Designers-143"
    ## 
    ## [[423]]
    ## [1] "Designers-144"
    ## 
    ## [[424]]
    ## [1] "Designers-145"
    ## 
    ## [[425]]
    ## [1] "Designers-146"
    ## 
    ## [[426]]
    ## [1] "Designers-148"
    ## 
    ## [[427]]
    ## [1] "Designers-149"
    ## 
    ## [[428]]
    ## [1] "Designers-150"
    ## 
    ## [[429]]
    ## [1] "Designers-151"
    ## 
    ## [[430]]
    ## [1] "Designers-152"
    ## 
    ## [[431]]
    ## [1] "Designers-153"
    ## 
    ## [[432]]
    ## [1] "Designers-154"
    ## 
    ## [[433]]
    ## [1] "Designers-155"
    ## 
    ## [[434]]
    ## [1] "Designers-156"
    ## 
    ## [[435]]
    ## [1] "Designers-157"
    ## 
    ## [[436]]
    ## [1] "Designers-158"
    ## 
    ## [[437]]
    ## [1] "Designers-159"
    ## 
    ## [[438]]
    ## [1] "Designers-160"
    ## 
    ## [[439]]
    ## [1] "Designers-161"
    ## 
    ## [[440]]
    ## [1] "Designers-162"
    ## 
    ## [[441]]
    ## [1] "Designers-163"
    ## 
    ## [[442]]
    ## [1] "Designers-164"
    ## 
    ## [[443]]
    ## [1] "Designers-165"
    ## 
    ## [[444]]
    ## [1] "Designers-166"
    ## 
    ## [[445]]
    ## [1] "Designers-168"
    ## 
    ## [[446]]
    ## [1] "Designers-169"
    ## 
    ## [[447]]
    ## [1] "Designers-170"
    ## 
    ## [[448]]
    ## [1] "Designers-172"
    ## 
    ## [[449]]
    ## [1] "Designers-173"
    ## 
    ## [[450]]
    ## [1] "Designers-174"
    ## 
    ## [[451]]
    ## [1] "Designers-175"
    ## 
    ## [[452]]
    ## [1] "Designers-176"
    ## 
    ## [[453]]
    ## [1] "Designers-177"
    ## 
    ## [[454]]
    ## [1] "Designers-178"
    ## 
    ## [[455]]
    ## [1] "Executives-003"
    ## 
    ## [[456]]
    ## [1] "Executives-004"
    ## 
    ## [[457]]
    ## [1] "Executives-005"
    ## 
    ## [[458]]
    ## [1] "Executives-006"
    ## 
    ## [[459]]
    ## [1] "Executives-007"
    ## 
    ## [[460]]
    ## [1] "Executives-008"
    ## 
    ## [[461]]
    ## [1] "Executives-009"
    ## 
    ## [[462]]
    ## [1] "Executives-010"
    ## 
    ## [[463]]
    ## [1] "Executives-012"
    ## 
    ## [[464]]
    ## [1] "Executives-013"
    ## 
    ## [[465]]
    ## [1] "Executives-014"
    ## 
    ## [[466]]
    ## [1] "Executives-015"
    ## 
    ## [[467]]
    ## [1] "Executives-017"
    ## 
    ## [[468]]
    ## [1] "Executives-018"
    ## 
    ## [[469]]
    ## [1] "Executives-019"
    ## 
    ## [[470]]
    ## [1] "Executives-020"
    ## 
    ## [[471]]
    ## [1] "Executives-021"
    ## 
    ## [[472]]
    ## [1] "Executives-022"
    ## 
    ## [[473]]
    ## [1] "Executives-023"
    ## 
    ## [[474]]
    ## [1] "Executives-024"
    ## 
    ## [[475]]
    ## [1] "Executives-026"
    ## 
    ## [[476]]
    ## [1] "Executives-027"
    ## 
    ## [[477]]
    ## [1] "Executives-029"
    ## 
    ## [[478]]
    ## [1] "Executives-030"
    ## 
    ## [[479]]
    ## [1] "Executives-031"
    ## 
    ## [[480]]
    ## [1] "Executives-032"
    ## 
    ## [[481]]
    ## [1] "Executives-033"
    ## 
    ## [[482]]
    ## [1] "Executives-034"
    ## 
    ## [[483]]
    ## [1] "Executives-035"
    ## 
    ## [[484]]
    ## [1] "Executives-036"
    ## 
    ## [[485]]
    ## [1] "Executives-037"
    ## 
    ## [[486]]
    ## [1] "Executives-039"
    ## 
    ## [[487]]
    ## [1] "Executives-040"
    ## 
    ## [[488]]
    ## [1] "Executives-041"
    ## 
    ## [[489]]
    ## [1] "Executives-042"
    ## 
    ## [[490]]
    ## [1] "Executives-043"
    ## 
    ## [[491]]
    ## [1] "Executives-044"
    ## 
    ## [[492]]
    ## [1] "Executives-045"
    ## 
    ## [[493]]
    ## [1] "Executives-046"
    ## 
    ## [[494]]
    ## [1] "Executives-049"
    ## 
    ## [[495]]
    ## [1] "Executives-051"
    ## 
    ## [[496]]
    ## [1] "Executives-052"
    ## 
    ## [[497]]
    ## [1] "Executives-053"
    ## 
    ## [[498]]
    ## [1] "Executives-054"
    ## 
    ## [[499]]
    ## [1] "Executives-055"
    ## 
    ## [[500]]
    ## [1] "Executives-057"
    ## 
    ## [[501]]
    ## [1] "Executives-058"
    ## 
    ## [[502]]
    ## [1] "Executives-062"
    ## 
    ## [[503]]
    ## [1] "Executives-065"
    ## 
    ## [[504]]
    ## [1] "Executives-066"
    ## 
    ## [[505]]
    ## [1] "Executives-067"
    ## 
    ## [[506]]
    ## [1] "Executives-068"
    ## 
    ## [[507]]
    ## [1] "Executives-070"
    ## 
    ## [[508]]
    ## [1] "Executives-071"
    ## 
    ## [[509]]
    ## [1] "Executives-073"
    ## 
    ## [[510]]
    ## [1] "Executives-074"
    ## 
    ## [[511]]
    ## [1] "Executives-075"
    ## 
    ## [[512]]
    ## [1] "Executives-076"
    ## 
    ## [[513]]
    ## [1] "Executives-077"
    ## 
    ## [[514]]
    ## [1] "Executives-078"
    ## 
    ## [[515]]
    ## [1] "Executives-079"
    ## 
    ## [[516]]
    ## [1] "Executives-080"
    ## 
    ## [[517]]
    ## [1] "Executives-081"
    ## 
    ## [[518]]
    ## [1] "Executives-082"
    ## 
    ## [[519]]
    ## [1] "Executives-083"
    ## 
    ## [[520]]
    ## [1] "Executives-084"
    ## 
    ## [[521]]
    ## [1] "Executives-085"
    ## 
    ## [[522]]
    ## [1] "Executives-086"
    ## 
    ## [[523]]
    ## [1] "Executives-087"
    ## 
    ## [[524]]
    ## [1] "Executives-088"
    ## 
    ## [[525]]
    ## [1] "Executives-089"
    ## 
    ## [[526]]
    ## [1] "Executives-090"
    ## 
    ## [[527]]
    ## [1] "Executives-091"
    ## 
    ## [[528]]
    ## [1] "Executives-092"
    ## 
    ## [[529]]
    ## [1] "Executives-093"
    ## 
    ## [[530]]
    ## [1] "Executives-096"
    ## 
    ## [[531]]
    ## [1] "Executives-097"
    ## 
    ## [[532]]
    ## [1] "Executives-098"
    ## 
    ## [[533]]
    ## [1] "Executives-099"
    ## 
    ## [[534]]
    ## [1] "Executives-101"
    ## 
    ## [[535]]
    ## [1] "Executives-103"
    ## 
    ## [[536]]
    ## [1] "Executives-104"
    ## 
    ## [[537]]
    ## [1] "Executives-105"
    ## 
    ## [[538]]
    ## [1] "Executives-106"
    ## 
    ## [[539]]
    ## [1] "Executives-107"
    ## 
    ## [[540]]
    ## [1] "Executives-108"
    ## 
    ## [[541]]
    ## [1] "Executives-109"
    ## 
    ## [[542]]
    ## [1] "Executives-110"
    ## 
    ## [[543]]
    ## [1] "Executives-111"
    ## 
    ## [[544]]
    ## [1] "Executives-112"
    ## 
    ## [[545]]
    ## [1] "Executives-115"
    ## 
    ## [[546]]
    ## [1] "Executives-117"
    ## 
    ## [[547]]
    ## [1] "Executives-118"
    ## 
    ## [[548]]
    ## [1] "Executives-120"
    ## 
    ## [[549]]
    ## [1] "Executives-124"
    ## 
    ## [[550]]
    ## [1] "Executives-125"
    ## 
    ## [[551]]
    ## [1] "Executives-126"
    ## 
    ## [[552]]
    ## [1] "Fashion2.0-003"
    ## 
    ## [[553]]
    ## [1] "Fashion2.0-004"
    ## 
    ## [[554]]
    ## [1] "Fashion2.0-005"
    ## 
    ## [[555]]
    ## [1] "Fashion2.0-006"
    ## 
    ## [[556]]
    ## [1] "Fashion2.0-007"
    ## 
    ## [[557]]
    ## [1] "Fashion2.0-010"
    ## 
    ## [[558]]
    ## [1] "Fashion2.0-011"
    ## 
    ## [[559]]
    ## [1] "Fashion2.0-013"
    ## 
    ## [[560]]
    ## [1] "Fashion2.0-015"
    ## 
    ## [[561]]
    ## [1] "Fashion2.0-016"
    ## 
    ## [[562]]
    ## [1] "Fashion2.0-017"
    ## 
    ## [[563]]
    ## [1] "Fashion2.0-018"
    ## 
    ## [[564]]
    ## [1] "Fashion2.0-019"
    ## 
    ## [[565]]
    ## [1] "Fashion2.0-020"
    ## 
    ## [[566]]
    ## [1] "Fashion2.0-021"
    ## 
    ## [[567]]
    ## [1] "Fashion2.0-022"
    ## 
    ## [[568]]
    ## [1] "Fashion2.0-025"
    ## 
    ## [[569]]
    ## [1] "Fashion2.0-026"
    ## 
    ## [[570]]
    ## [1] "Fashion2.0-027"
    ## 
    ## [[571]]
    ## [1] "Fashion2.0-028"
    ## 
    ## [[572]]
    ## [1] "Fashion2.0-029"
    ## 
    ## [[573]]
    ## [1] "Fashion2.0-030"
    ## 
    ## [[574]]
    ## [1] "Fashion2.0-031"
    ## 
    ## [[575]]
    ## [1] "Fashion2.0-032"
    ## 
    ## [[576]]
    ## [1] "Fashion2.0-033"
    ## 
    ## [[577]]
    ## [1] "Fashion2.0-034"
    ## 
    ## [[578]]
    ## [1] "Fashion2.0-035"
    ## 
    ## [[579]]
    ## [1] "Fashion2.0-036"
    ## 
    ## [[580]]
    ## [1] "Fashion2.0-037"
    ## 
    ## [[581]]
    ## [1] "Fashion2.0-038"
    ## 
    ## [[582]]
    ## [1] "Fashion2.0-040"
    ## 
    ## [[583]]
    ## [1] "Fashion2.0-041"
    ## 
    ## [[584]]
    ## [1] "Fashion2.0-042"
    ## 
    ## [[585]]
    ## [1] "Fashion2.0-043"
    ## 
    ## [[586]]
    ## [1] "Fashion2.0-044"
    ## 
    ## [[587]]
    ## [1] "Fashion2.0-045"
    ## 
    ## [[588]]
    ## [1] "Media-001"
    ## 
    ## [[589]]
    ## [1] "Media-002"
    ## 
    ## [[590]]
    ## [1] "Media-003"
    ## 
    ## [[591]]
    ## [1] "Media-004"
    ## 
    ## [[592]]
    ## [1] "Media-005"
    ## 
    ## [[593]]
    ## [1] "Media-008"
    ## 
    ## [[594]]
    ## [1] "Media-009"
    ## 
    ## [[595]]
    ## [1] "Media-010"
    ## 
    ## [[596]]
    ## [1] "Media-012"
    ## 
    ## [[597]]
    ## [1] "Media-013"
    ## 
    ## [[598]]
    ## [1] "Media-014"
    ## 
    ## [[599]]
    ## [1] "Media-015"
    ## 
    ## [[600]]
    ## [1] "Media-016"
    ## 
    ## [[601]]
    ## [1] "Media-017"
    ## 
    ## [[602]]
    ## [1] "Media-018"
    ## 
    ## [[603]]
    ## [1] "Media-019"
    ## 
    ## [[604]]
    ## [1] "Media-020"
    ## 
    ## [[605]]
    ## [1] "Media-021"
    ## 
    ## [[606]]
    ## [1] "Media-022"
    ## 
    ## [[607]]
    ## [1] "Media-023"
    ## 
    ## [[608]]
    ## [1] "Media-024"
    ## 
    ## [[609]]
    ## [1] "Media-025"
    ## 
    ## [[610]]
    ## [1] "Media-026"
    ## 
    ## [[611]]
    ## [1] "Media-027"
    ## 
    ## [[612]]
    ## [1] "Media-028"
    ## 
    ## [[613]]
    ## [1] "Media-029"
    ## 
    ## [[614]]
    ## [1] "Media-030"
    ## 
    ## [[615]]
    ## [1] "Media-031"
    ## 
    ## [[616]]
    ## [1] "Media-032"
    ## 
    ## [[617]]
    ## [1] "Media-033"
    ## 
    ## [[618]]
    ## [1] "Media-034"
    ## 
    ## [[619]]
    ## [1] "Media-035"
    ## 
    ## [[620]]
    ## [1] "Media-038"
    ## 
    ## [[621]]
    ## [1] "Media-039"
    ## 
    ## [[622]]
    ## [1] "Media-040"
    ## 
    ## [[623]]
    ## [1] "Media-041"
    ## 
    ## [[624]]
    ## [1] "Media-042"
    ## 
    ## [[625]]
    ## [1] "Media-043"
    ## 
    ## [[626]]
    ## [1] "Media-044"
    ## 
    ## [[627]]
    ## [1] "Media-045"
    ## 
    ## [[628]]
    ## [1] "Media-046"
    ## 
    ## [[629]]
    ## [1] "Media-047"
    ## 
    ## [[630]]
    ## [1] "Media-048"
    ## 
    ## [[631]]
    ## [1] "Media-049"
    ## 
    ## [[632]]
    ## [1] "Media-050"
    ## 
    ## [[633]]
    ## [1] "Media-051"
    ## 
    ## [[634]]
    ## [1] "Media-052"
    ## 
    ## [[635]]
    ## [1] "Media-053"
    ## 
    ## [[636]]
    ## [1] "Media-054"
    ## 
    ## [[637]]
    ## [1] "Media-055"
    ## 
    ## [[638]]
    ## [1] "Media-057"
    ## 
    ## [[639]]
    ## [1] "Media-058"
    ## 
    ## [[640]]
    ## [1] "Media-059"
    ## 
    ## [[641]]
    ## [1] "Media-060"
    ## 
    ## [[642]]
    ## [1] "Media-061"
    ## 
    ## [[643]]
    ## [1] "Media-063"
    ## 
    ## [[644]]
    ## [1] "Media-065"
    ## 
    ## [[645]]
    ## [1] "Media-066"
    ## 
    ## [[646]]
    ## [1] "Media-067"
    ## 
    ## [[647]]
    ## [1] "Media-069"
    ## 
    ## [[648]]
    ## [1] "Media-070"
    ## 
    ## [[649]]
    ## [1] "Media-071"
    ## 
    ## [[650]]
    ## [1] "Media-072"
    ## 
    ## [[651]]
    ## [1] "Media-073"
    ## 
    ## [[652]]
    ## [1] "Media-075"
    ## 
    ## [[653]]
    ## [1] "Media-076"
    ## 
    ## [[654]]
    ## [1] "Media-077"
    ## 
    ## [[655]]
    ## [1] "Media-078"
    ## 
    ## [[656]]
    ## [1] "Media-080"
    ## 
    ## [[657]]
    ## [1] "Media-083"
    ## 
    ## [[658]]
    ## [1] "Media-084"
    ## 
    ## [[659]]
    ## [1] "Media-085"
    ## 
    ## [[660]]
    ## [1] "Media-086"
    ## 
    ## [[661]]
    ## [1] "Media-087"
    ## 
    ## [[662]]
    ## [1] "Media-088"
    ## 
    ## [[663]]
    ## [1] "Media-089"
    ## 
    ## [[664]]
    ## [1] "Media-090"
    ## 
    ## [[665]]
    ## [1] "Media-091"
    ## 
    ## [[666]]
    ## [1] "Media-092"
    ## 
    ## [[667]]
    ## [1] "Media-093"
    ## 
    ## [[668]]
    ## [1] "Media-094"
    ## 
    ## [[669]]
    ## [1] "Media-095"
    ## 
    ## [[670]]
    ## [1] "Media-096"
    ## 
    ## [[671]]
    ## [1] "Media-097"
    ## 
    ## [[672]]
    ## [1] "Media-098"
    ## 
    ## [[673]]
    ## [1] "Media-099"
    ## 
    ## [[674]]
    ## [1] "Media-100"
    ## 
    ## [[675]]
    ## [1] "Media-101"
    ## 
    ## [[676]]
    ## [1] "Media-102"
    ## 
    ## [[677]]
    ## [1] "Media-103"
    ## 
    ## [[678]]
    ## [1] "Media-104"
    ## 
    ## [[679]]
    ## [1] "Media-105"
    ## 
    ## [[680]]
    ## [1] "Media-106"
    ## 
    ## [[681]]
    ## [1] "Media-107"
    ## 
    ## [[682]]
    ## [1] "Media-109"
    ## 
    ## [[683]]
    ## [1] "Media-110"
    ## 
    ## [[684]]
    ## [1] "Media-111"
    ## 
    ## [[685]]
    ## [1] "Media-114"
    ## 
    ## [[686]]
    ## [1] "Media-115"
    ## 
    ## [[687]]
    ## [1] "Media-116"
    ## 
    ## [[688]]
    ## [1] "Media-117"
    ## 
    ## [[689]]
    ## [1] "Media-118"
    ## 
    ## [[690]]
    ## [1] "Media-120"
    ## 
    ## [[691]]
    ## [1] "Media-122"
    ## 
    ## [[692]]
    ## [1] "Media-123"
    ## 
    ## [[693]]
    ## [1] "Media-124"
    ## 
    ## [[694]]
    ## [1] "Media-125"
    ## 
    ## [[695]]
    ## [1] "Media-126"
    ## 
    ## [[696]]
    ## [1] "Media-127"
    ## 
    ## [[697]]
    ## [1] "Media-128"
    ## 
    ## [[698]]
    ## [1] "Media-129"
    ## 
    ## [[699]]
    ## [1] "Media-131"
    ## 
    ## [[700]]
    ## [1] "Media-132"
    ## 
    ## [[701]]
    ## [1] "Media-133"
    ## 
    ## [[702]]
    ## [1] "Media-134"
    ## 
    ## [[703]]
    ## [1] "Media-135"
    ## 
    ## [[704]]
    ## [1] "Media-136"
    ## 
    ## [[705]]
    ## [1] "Media-137"
    ## 
    ## [[706]]
    ## [1] "Media-138"
    ## 
    ## [[707]]
    ## [1] "Media-139"
    ## 
    ## [[708]]
    ## [1] "Media-140"
    ## 
    ## [[709]]
    ## [1] "Media-141"
    ## 
    ## [[710]]
    ## [1] "Models-001"
    ## 
    ## [[711]]
    ## [1] "Models-002"
    ## 
    ## [[712]]
    ## [1] "Models-003"
    ## 
    ## [[713]]
    ## [1] "Models-004"
    ## 
    ## [[714]]
    ## [1] "Models-005"
    ## 
    ## [[715]]
    ## [1] "Models-006"
    ## 
    ## [[716]]
    ## [1] "Models-008"
    ## 
    ## [[717]]
    ## [1] "Models-009"
    ## 
    ## [[718]]
    ## [1] "Models-010"
    ## 
    ## [[719]]
    ## [1] "Models-011"
    ## 
    ## [[720]]
    ## [1] "Models-012"
    ## 
    ## [[721]]
    ## [1] "Models-013"
    ## 
    ## [[722]]
    ## [1] "Models-014"
    ## 
    ## [[723]]
    ## [1] "Models-015"
    ## 
    ## [[724]]
    ## [1] "Models-016"
    ## 
    ## [[725]]
    ## [1] "Models-017"
    ## 
    ## [[726]]
    ## [1] "Models-018"
    ## 
    ## [[727]]
    ## [1] "Models-019"
    ## 
    ## [[728]]
    ## [1] "Models-020"
    ## 
    ## [[729]]
    ## [1] "Models-021"
    ## 
    ## [[730]]
    ## [1] "Models-022"
    ## 
    ## [[731]]
    ## [1] "Models-023"
    ## 
    ## [[732]]
    ## [1] "Models-024"
    ## 
    ## [[733]]
    ## [1] "Models-025"
    ## 
    ## [[734]]
    ## [1] "Models-026"
    ## 
    ## [[735]]
    ## [1] "Models-027"
    ## 
    ## [[736]]
    ## [1] "Models-028"
    ## 
    ## [[737]]
    ## [1] "Models-029"
    ## 
    ## [[738]]
    ## [1] "Models-030"
    ## 
    ## [[739]]
    ## [1] "Models-031"
    ## 
    ## [[740]]
    ## [1] "Models-032"
    ## 
    ## [[741]]
    ## [1] "Models-033"
    ## 
    ## [[742]]
    ## [1] "Models-034"
    ## 
    ## [[743]]
    ## [1] "Models-035"
    ## 
    ## [[744]]
    ## [1] "Models-036"
    ## 
    ## [[745]]
    ## [1] "Models-037"
    ## 
    ## [[746]]
    ## [1] "Models-038"
    ## 
    ## [[747]]
    ## [1] "Models-039"
    ## 
    ## [[748]]
    ## [1] "Models-040"
    ## 
    ## [[749]]
    ## [1] "Models-041"
    ## 
    ## [[750]]
    ## [1] "Models-042"
    ## 
    ## [[751]]
    ## [1] "Models-043"
    ## 
    ## [[752]]
    ## [1] "Models-044"
    ## 
    ## [[753]]
    ## [1] "Models-045"
    ## 
    ## [[754]]
    ## [1] "Models-046"
    ## 
    ## [[755]]
    ## [1] "Models-047"
    ## 
    ## [[756]]
    ## [1] "Models-048"
    ## 
    ## [[757]]
    ## [1] "Models-049"
    ## 
    ## [[758]]
    ## [1] "Models-050"
    ## 
    ## [[759]]
    ## [1] "Models-051"
    ## 
    ## [[760]]
    ## [1] "Models-052"
    ## 
    ## [[761]]
    ## [1] "Models-053"
    ## 
    ## [[762]]
    ## [1] "Models-054"
    ## 
    ## [[763]]
    ## [1] "Models-055"
    ## 
    ## [[764]]
    ## [1] "Models-056"
    ## 
    ## [[765]]
    ## [1] "Models-057"
    ## 
    ## [[766]]
    ## [1] "Retailers-001"
    ## 
    ## [[767]]
    ## [1] "Retailers-002"
    ## 
    ## [[768]]
    ## [1] "Retailers-003"
    ## 
    ## [[769]]
    ## [1] "Retailers-004"
    ## 
    ## [[770]]
    ## [1] "Retailers-005"
    ## 
    ## [[771]]
    ## [1] "Retailers-006"
    ## 
    ## [[772]]
    ## [1] "Retailers-007"
    ## 
    ## [[773]]
    ## [1] "Retailers-008"
    ## 
    ## [[774]]
    ## [1] "Retailers-009"
    ## 
    ## [[775]]
    ## [1] "Retailers-010"
    ## 
    ## [[776]]
    ## [1] "Retailers-011"
    ## 
    ## [[777]]
    ## [1] "Retailers-012"
    ## 
    ## [[778]]
    ## [1] "Retailers-013"
    ## 
    ## [[779]]
    ## [1] "Retailers-014"
    ## 
    ## [[780]]
    ## [1] "Retailers-015"
    ## 
    ## [[781]]
    ## [1] "Retailers-017"
    ## 
    ## [[782]]
    ## [1] "Retailers-018"
    ## 
    ## [[783]]
    ## [1] "Retailers-019"
    ## 
    ## [[784]]
    ## [1] "Retailers-020"
    ## 
    ## [[785]]
    ## [1] "Retailers-021"
    ## 
    ## [[786]]
    ## [1] "Retailers-022"
    ## 
    ## [[787]]
    ## [1] "Retailers-023"
    ## 
    ## [[788]]
    ## [1] "Retailers-Personal-Names-001"
    ## 
    ## [[789]]
    ## [1] "Retailers-Personal-Names-002"
    ## 
    ## [[790]]
    ## [1] "Retailers-Personal-Names-003"
    ## 
    ## [[791]]
    ## [1] "Retailers-Personal-Names-004"
    ## 
    ## [[792]]
    ## [1] "Retailers-Personal-Names-005"
    ## 
    ## [[793]]
    ## [1] "Retailers-Personal-Names-006"
    ## 
    ## [[794]]
    ## [1] "Retailers-Personal-Names-008"
    ## 
    ## [[795]]
    ## [1] "Retailers-Personal-Names-010"
    ## 
    ## [[796]]
    ## [1] "Retailers-Personal-Names-012"
    ## 
    ## [[797]]
    ## [1] "Retailers-Personal-Names-013"
    ## 
    ## [[798]]
    ## [1] "Retailers-Personal-Names-014"
    ## 
    ## [[799]]
    ## [1] "Retailers-Personal-Names-015"
    ## 
    ## [[800]]
    ## [1] "Retailers-Personal-Names-016"
    ## 
    ## [[801]]
    ## [1] "Retailers-Personal-Names-017"
    ## 
    ## [[802]]
    ## [1] "Retailers-Personal-Names-018"
    ## 
    ## [[803]]
    ## [1] "Retailers-Personal-Names-019"
    ## 
    ## [[804]]
    ## [1] "Retailers-Personal-Names-020"
    ## 
    ## [[805]]
    ## [1] "Retailers-Personal-Names-023"
    ## 
    ## [[806]]
    ## [1] "Retailers-Personal-Names-024"
    ## 
    ## [[807]]
    ## [1] "Retailers-Personal-Names-025"
    ## 
    ## [[808]]
    ## [1] "Retailers-Personal-Names-026"
    ## 
    ## [[809]]
    ## [1] "Retailers-Personal-Names-027"
    ## 
    ## [[810]]
    ## [1] "Retailers-Personal-Names-028"
    ## 
    ## [[811]]
    ## [1] "Retailers-Personal-Names-031"
    ## 
    ## [[812]]
    ## [1] "Retailers-Personal-Names-033"
    ## 
    ## [[813]]
    ## [1] "Retailers-Personal-Names-034"
    ## 
    ## [[814]]
    ## [1] "Retailers-Personal-Names-035"
    ## 
    ## [[815]]
    ## [1] "Retailers-Personal-Names-037"
    ## 
    ## [[816]]
    ## [1] "Retailers-Personal-Names-038"
    ## 
    ## [[817]]
    ## [1] "Retailers-Personal-Names-039"
    ## 
    ## [[818]]
    ## [1] "Retailers-Personal-Names-043"
    ## 
    ## [[819]]
    ## [1] "Retailers-Personal-Names-047"
    ## 
    ## [[820]]
    ## [1] "Retailers-Personal-Names-049"
    ## 
    ## [[821]]
    ## [1] "Retailers-Personal-Names-051"
    ## 
    ## [[822]]
    ## [1] "Retailers-Personal-Names-052"
    ## 
    ## [[823]]
    ## [1] "Retailers-Personal-Names-053"
    ## 
    ## [[824]]
    ## [1] "Retailers-Personal-Names-054"

``` r
# total & category totals
map(list('total', 'creatives', 'business', 'marketing'), function(name) {
  phenomenon_total <- sum(hexagons_raw_counts[,name])
  
  name_or <-  paste0(name, "_OR")
  name_orc <- paste0(name, "_ORc")
  hexagons_raw_counts <<- hexagons_raw_counts %>% 
    mutate(!!name_or := calculate_odds_ratio(!!as.symbol(name), phenomenon_total, random, random_total),
           !!name_orc := calculate_odds_ratio_ci(!!as.symbol(name_or), 
                                                 !!as.symbol(name), phenomenon_total, random, random_total))
  invisible(name)
})
```

    ## [[1]]
    ## [1] "total"
    ## 
    ## [[2]]
    ## [1] "creatives"
    ## 
    ## [[3]]
    ## [1] "business"
    ## 
    ## [[4]]
    ## [1] "marketing"

``` r
# total followers
random_fol_total <- sum(hexagons_raw_counts$random_fol)
map(list('total_fol'), function(name) {
  phenomenon_total <- sum(hexagons_raw_counts[,name])
  
  name_or <-  paste0(name, "_OR")
  name_orc <- paste0(name, "_ORc")
  hexagons_raw_counts <<- hexagons_raw_counts %>% 
    mutate(!!name_or := calculate_odds_ratio(!!as.symbol(name), phenomenon_total, random_fol, random_fol_total),
           !!name_orc := calculate_odds_ratio_ci(!!as.symbol(name_or), 
                                                 !!as.symbol(name), phenomenon_total, random_fol, random_fol_total))
  invisible(name)
})
```

    ## [[1]]
    ## [1] "total_fol"

### Diversity metrics

``` r
hexagons_raw_counts <- hexagons_raw_counts %>% 
                          mutate(
                              divShannon = vegan::diversity(hexagons_raw_counts[, raw_columns], index = "shannon"),
                              divSimpson = vegan::diversity(hexagons_raw_counts[, raw_columns], index = "invsimpson")
                              )
fwrite(hexagons_raw_counts, here("analysis/data/derived_data/global_hex_join_odds_ratios.csv"))
saveRDS(hexagons_raw_counts, here("analysis/data/derived_data/global_hex_join_odds_ratios.rds"))
```
