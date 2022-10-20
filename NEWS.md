# ggshadow

* TODO: https://www.tidyverse.org/blog/2022/09/playing-on-the-same-team-as-your-dependecy/ advice to get rid of the calls to `getFromNamespace` which are a way of accessing the non-public interface of `ggplot2`. This will be an easy fix in some cases, and more challeging in others.

* Version 0.0.4 has a small fix to support the next version of `ggplot2`.
The function `new_data_frame` was removed from `ggplot2`. It was replaced by an equivalent function 
from from `vctrs`.

* Version 0.0.2 is the initial CRAN release.
