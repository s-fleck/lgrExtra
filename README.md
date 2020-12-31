
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lgrExtra

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/s-fleck/lgrExtra.svg?branch=master)](https://travis-ci.com/s-fleck/lgrExtra)
[![Codecov test
coverage](https://codecov.io/gh/s-fleck/lgrExtra/branch/master/graph/badge.svg)](https://codecov.io/gh/s-fleck/lgrExtra?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

Extra appenders for logging to push notifications, email, databases, and
other destinations for [lgr](https://github.com/s-fleck/lgr). Please
refer to the [function
reference](https://s-fleck.github.io/lgrExtra/reference/index.html) for
details and estrxamples.

# Development status

The appenders provided by lgrExtra are of varying stability. Especially
Database-appenders are still considered experimental and should probably
not be used in a production environment. I currently do not have much
time to work on lgrExtra, but you are welcome to submit feature
requests. If I see that the interest in lgrExtra increases I may
prioritize it over other projects.

| Appender           | Description                                  | Backend                                                       | Status                                                                                                                                          |
|--------------------|----------------------------------------------|---------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------|
| AppenderDbi        | Databases                                    | [DBI](https://cran.r-project.org/package=DBI)                 | [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) |
| AppenderDt         | In-memory data.table                         | [data.table](https://github.com/Rdatatable/data.table)        | [![Lifecycle: superseded](https://img.shields.io/badge/lifecycle-superseded-blue.svg)](https://www.tidyverse.org/lifecycle/#superseded)         |
| AppenderGmail      | E-mail via Gmail REST API                    | [gmailr](https://cran.r-project.org/package=gmailr)           | [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) |
| AppenderSendmail   | E-mail via SMTP                              | [sendmailR](https://cran.r-project.org/package=sendmailR)     | [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)              |
| AppenderPushbullet | Pushbullet (mobile phone push notifications) | [RPushbullet](https://cran.r-project.org/package=RPushbullet) | [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)              |
| AppenderSyslog     | Linux Syslog                                 | [rsyslog](https://cran.r-project.org/package=rsyslog)         | [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)              |

## Installation

You can install the stable version from CRAN

``` r
install.packages("lgrExtra")
```

or the current development version directly from github

``` r
#install.packages("remotes")
remotes::install_github("s-fleck/lgrExtra")
```
