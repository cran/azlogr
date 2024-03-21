# azlogr 0.0.6

* Updated testing strategy for empty logging message. No change for end users.
* Fixed a typo on README.

# azlogr 0.0.5

* Slightly modified testing strategy for enforcing ASCII of logging messages because of different implementation of `base::iconv` by OS type. No change for end users.

# azlogr 0.0.4

* Bug fix - not trying to post to 'Azure Log Analytics' workspace when logging level does not meet the logging threshold criteria. Earlier it was posting an empty string in that scenario, that is fixed.
* Bug fix - `enforce_ascii` as `TRUE` works on systems having encoding ISO8859-1 as well. 
* Minor documentation update.

# azlogr 0.0.3

* Added ability to extract all the configurations at once in `get_log_config` function.
* Minor documentation update.

# azlogr 0.0.2

* Updated `README` and general package documentation.
* Added GitHub Actions to trigger code linting, testing, building the package documentation and doing `R CMD check`.

# azlogr 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Initial release with minimum features required to configure logging set up in one step. And, optionally, add additional meta-data to be collected while logging.
