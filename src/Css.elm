module Css exposing (..)


css =
    """
.row { display: flex; }
.col { display: flex; flex-direction: column; }

.mt { margin-top: 1em; }
.mb { margin-bottom: 1em; }
.ml { margin-left: 1em; }
.mr { margin-right: 1em; }

.p { padding: 1em; }

.header {
  font-family: sans;
  font-weight: bold;
}

.border { border: 1px solid lightgray; }

.thirds > * {
  /* min-width: 300px; */
}

    """