
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lgrExtra

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/lgrExtra)](https://CRAN.R-project.org/package=lgrExtra)
<!-- badges: end -->

Extra appenders for logging to push notifications, email, databases, and
other destinations for [lgr](https://github.com/s-fleck/lgr). Please
refer to the [function
reference](https://s-fleck.github.io/lgrExtra/reference/index.html) for
details and examples

# Development status

The appenders provided by lgrExtra are of varying stability.
Elasticsearch and Dynatrace are ready for production use (I use them in
production), but Database-based appenders are still considered
experimental and more of a prove-of-concept. AppenderDynatrace is still
labeled `experimental` because the it is rather new and the default
layout might still change in minor ways.

| Appender | Description | Backend | Status |
|----|----|----|----|
| AppenderAWSCloudWatchLog | Amazon AWS CloudWatch Log | [paws.management](https://cran.r-project.org/package=paws.management) | [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) |
| AppenderElasticSearch | ElasticSearch | [elastic](https://cran.r-project.org/package=elastic) | [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) |
| AppenderDbi | Databases | [DBI](https://cran.r-project.org/package=DBI) | [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) |
| AppenderGmail | E-mail via Gmail REST API | [gmailr](https://cran.r-project.org/package=gmailr) | [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) |
| AppenderPool | Databases (via connection pool) | [pool](https://cran.r-project.org/package=pool) | [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) |
| AppenderPushbullet | Pushbullet (mobile phone push notifications) | [RPushbullet](https://cran.r-project.org/package=RPushbullet) | [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) |
| AppenderSendmail | E-mail via SMTP | [sendmailR](https://cran.r-project.org/package=sendmailR) | [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) |
| AppenderSyslog | Linux Syslog | [rsyslog](https://cran.r-project.org/package=rsyslog) | [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) |
| AppenderDynatrace | Dynatrace | [httr2](https://cran.r-project.org/package=httr2) | [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) |

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
