---

# expirest 0.1.6

- So far, exclusively the result from fitting individual models to each batch
  were reported in case of the different intercepts / different slopes (dids).
  model. With the new version, the results of the dids model obtained with
  pooled mean square error (dids.pmse) will be shown in addition. The results
  of the dids and the dids.pmse are reported generally in summaries.
- A new parameter has been added to the two plot functions allowing to select
  the model to be displayed. The show_grouping parameter is not relevant any
  more and is thus marked as deprecated.
- So far, the shelf life assessment failed when the results of a single batch
  were used. The new version allows to perform the assessment with a single
  batch, and the corresponding results are reported as dids model (while the
  model type will be n.a.).
- The function ggplot2::coord_cartesian() is used to prevent the loss of 
  information displayed on graphs when the intervals have values going beyond 
  the edges of the plot.
- In case of a specification with two sides, the shelf life assessment had to
  be done for each side one at a time. In the new version, the ordinary shelf
  life estimation will be performed on both sides, and the shortest shelf life
  result will be reported (together with the information about the side). For
  the what-if shelf life analysis, this does not apply because for this kind of
  assessment the critical side is known.
- The requirements on rl has been relaxed by taking into account the number 
  of significant digits specified for the specification limit (sl_sf). For
  example, if ivl_side = "upper", sl = 0.5 and sl_sf = 1, rl values between
  0.50 and 0.54 are accepted because the corresponding rounded rl values (to
  one significant digit) all are 0.5 and thus not greater than 0.5.
- An error in the calculation of sl and rl in case of sf_option = "loose" was
  corrected. The correction make sure that 5 is subtracted at the (sl_sf + 1)th
  or (rl_sf + 1)th figure if ivl_side is "lower" or that 4 is added at the
  (sl_sf + 1)th or (rl_sf + 1)th figure if ivl_side is "upper".
- Cyclomatic complexity was reduced as far as possible, i.e. the complexity of
  functions was reduced in favor of more (internal) functions.
- Examples have been slimmed down and the execution of some of the example
  code is prevented to reduce the execution time of examples.

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
