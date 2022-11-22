---

# expirest 0.1.5

- Expired and (possibly) invalid URLs were removed.

---

# expirest 0.1.4

- Due to a misconception of object type tests the same value was reported for
  each rl value if the cics model was the appropriate model although the
  correct values were calculated. Now, the correct values (i.e. Delta, WCSL
  and Shelf.Life) are reported.
- The output of the summary() function has been harmonised between cics models
  on the one hand and dics / dids models on the other.
- The deprecated ggplot2 function aes_string() has been replaced by aes().
  Variables containing column names as a character vector (var_name) are
  now called via .data[[var_name]]. In addition, size has been replaced by
  linewidth when used in the geom_line() function. In addition, visible binding
  for global variables was added.
  
---

# expirest 0.1.3

- The graphical output of the dids model was not appropriate because the 
  prediction was based on the whole model with interaction term (group x time,
  where group is a categorical variable and time is a continuous variable).
  Now, the prediction is based on individual models fitted to the different
  levels of group.
- Before, a decimal point was shown in case of numbers without decimal digits.
- Comparisons of type class() == "string" were replaced by inherits() calls.

---

# expirest 0.1.2

- Examples that were wrapped by \\dontrun{} although they are executable in
  < 5 sec are now unwrapped.
- Errors from examples that deliberately produce errors are caught by tryCatch()
  and are no longer wrapped by \\dontrun{}.

---

# expirest 0.1.1

- CRAN submission
- Exclusively use secure links
- Correct spelling errors

---

# expirest 0.1.0

- Add further tests
- Add further and more illustrative examples
- Review documentation, correct spelling errors and assess all links
- Tidy up the DESCRIPTION
- Replace for loops with apply family functions
- Simplify code (add helper functions)
- Add generic functions

---

# expirest 0.1.0.900x
