---
title: "NEWS"
output: html_document
---

### Version 0.0.2
- Improved scraping code in `oddschecker2()` - almost a complete rewrite. Little resemblance now to the `oddschecker()` function from **{gambleR}**. 
- Removed majority of cleaning in `get_arb_single()` as no longer necessary due to `oddschecker2()` changes. This results in a much faster implementation of `get_arb_single()` and in turn `get_arbs`.
- Function `get_arbs()` now calls `get_arb_single()` iteratively in parallel, vastly reducing runtime. This can be disabled by setting `parallel = FALSE`.
- Function `remove_form()` deleted as bug no longer occurs when creating odds table in `oddschecker2()`. This shouldn't have been exported in the first place to be honest.
- New argument `print_urls` added to `get_arbs()`. This was previously input through `...` and passed to `get_arb_single()` but now this is a given explicitly as an argument, but of course still passed to `get_arb_single()`.
- New argument `parallel` added to `get_arbs()`, allows the user to run operations in parallel or sequential fashion.
- Non-exported function `implied_probability()` given almost complete rewrite. Odds now expected to be input as fractional character strings, but this should improve efficiency. This function is likely to be exported in future releases.
- Formatting errors in description fixed.


### Version 0.0.1
First release
