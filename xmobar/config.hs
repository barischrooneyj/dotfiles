Config
  { font     = "xft:Fira Code Retina:pixelsize=28"
  , bgColor  = "#1e2029"
  , fgColor  = "#f8f8f2"
  , commands =
    [ Run StdinReader
    , Run Date "%a %b %_d %l:%M" "date" 10
    ]
  , template = " %StdinReader% }{ <fc=#ffb86c>%date%</fc> "
  }
