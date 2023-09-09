#
# Terminal shenanigans
#

# TODO these code are for JS only. Move them in the JS platform?

color as fn Text: fn Text: Text =
    fn code:
    fn text:
    code .. text .. "\x1b[0m"


blue as fn Text: Text =
    color "\x1b[34m"


green as fn Text: Text =
    color "\x1b[32m"


yellow as fn Text: Text =
    color "\x1b[33m"


red as fn Text: Text =
    color "\x1b[31m"
