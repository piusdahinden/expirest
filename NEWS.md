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
