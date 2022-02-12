# See https://blog.flowblok.id.au/2013-02/shell-startup-scripts.html

# This file gets run in the following cases:
# 1. non-login interactive shell
# 2. remote shell (over ssh or similar)

. ~/.config/bash/env
. ~/.config/bash/interactive
