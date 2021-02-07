## Test environments
- ubuntu 20.10, R 4.0.2
- ubuntu 16.04 (on travis-ci), R 4.0.2
- win-builder (devel)
- R-hub windows-x86_64-devel (r-devel)
- R-hub fedora-clang-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)


## R CMD check results

0 errors | 0 warnings | 0 note

- R CMD check may throw WARNINGs or NOTEs on systems for which the **rsyslog** 
  package is not available (such as Windows)
  
Resubmission:

- add examples to many R6 classes. Not all examples are practical because
  many of the extra Appenders provided by this package require things like
  API keys or access to an SMTP server
- Improved documentation by adding a `Value` section for R6 classes
