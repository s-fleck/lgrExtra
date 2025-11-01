# 0.2.2.9000 (dev)

- Add digits argument to `as_unix_epoch_ms()`


# 0.2.2

- Fix `AppenderDynatrace` default timestamp logging format


# 0.2.1

- Fix `AppenderElasticSearch` to not use deprecated `LayoutElasticSearch` by default
- Fix flaky test that was sensitive to time zones
 

# 0.2.0

- require lgr 0.5.0

- Added support for the `rawMsg` LogEvent-property introduced in lgr 0.5.0

- Deprecate `LayoutElasticSearch` and replace it by a customized `LayoutJson` 
  (which has become much more flexible in lgr 0.5.0)

- Update`AppenderDynatrace` to use `LayoutJson`

- Breaking: Remove long deprecated `AppenderDt`


# 0.1.1

- Adapt tests for compatibility with httr2 >= "1.1.2.9000"


# lgrExtra 0.1.0

- Add `AppenderDynatrace` (experimental preview)

- Add `AppenderPool` (thx @jimbrig)

- Add `AppenderAWSCloudWatchLog` for logging to [AWS CloudWatch](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html) (#9) (thx @DyfanJones)


# 0.0.9

- Fixes for tests related to recent changes in `data.table` 1.16.0


# 0.0.8

- Support functions for index names and index mappings in `AppenderElasticSearch`


# 0.0.7

- Rebuild docs for `R 4.2.0`


# 0.0.6

- add `AppenderElasticSearch` for logging to ElasticSearch (#5)


# 0.0.5

- Migration of experimental Appenders from [lgr](https://s-fleck.github.io/lgr/)

- `AppenderDbi`: The default setting for buffer_size has been changed to `0`. 
  This means every log event is written directly to the target database. If you
  want performance improvements, set the buffer to a non-zero value. 

- Removed `AppenderRjdbc`

- `AppenderGmail` is now compatible with gmailr 1.0.0
