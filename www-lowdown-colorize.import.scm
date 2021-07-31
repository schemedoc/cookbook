(module www-lowdown-colorize

        ;; Exports:
        (enable-www-lowdown-colorize!)

        ;; Chicken core:
        (import (scheme))
        (import (chicken base))
        (import (chicken string))

        ;; Chicken eggs:
        (import (srfi 13))
        (import (colorize))
        (import (comparse))
        (import (clojurian syntax))
        (import (html-parser))
        (import (lowdown))
        (import (lowdown lolevel))

        (include "www-lowdown-colorize.scm"))
