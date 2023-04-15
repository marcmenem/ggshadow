# ggshadow development

* Update `GeomShadowPath` and `GeomGlowPath` to support the linewidth parameter for ggplot2 following the [guidance from Thomas Lin Pedersen in this blog post](https://www.tidyverse.org/blog/2022/08/ggplot2-3-4-0-size-to-linewidth/).
* Remove calls to `getFromNamespace` (following the recommendation in [this blog post](https://www.tidyverse.org/blog/2022/09/playing-on-the-same-team-as-your-dependecy/) to copy code directly into the package) for non-exported functions from ggplot2.
* Add [cli](https://github.com/r-lib/cli) to Imports.
* Replace markdown README with README.Rmd file.
* Add [Eli Pousson](https://github.com/elipousson) to contributors.

# ggshadow 0.0.4

* Version 0.0.4 has a small fix to support the next version of `ggplot2`. The function `new_data_frame` was removed from `ggplot2`. It was replaced by an equivalent function from from `vctrs`.

# ggshadow 0.0.2

* Version 0.0.2 is the initial CRAN release.
