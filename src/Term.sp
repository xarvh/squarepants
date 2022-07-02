
#
# Terminal shenanigans
#

# TODO these code are for JS only. Move them in the JS platform?

color as Text: Text: Text =
    code: text:

    code .. text .. "\x1b[0m"


blue as Text: Text =
    color  "\x1b[34m"


green as Text: Text =
    color "\x1b[32m"


yellow as Text: Text =
    color "\x1b[33m"


red as Text: Text =
    color "\x1b[31m"

