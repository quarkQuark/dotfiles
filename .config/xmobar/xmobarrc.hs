Config { position = Top
       , font     = "xft:Source Code Pro:size=12:bold:antialias=true"
       , fgColor = "#ffffff"
       , bgColor = "#000000"
       , alpha = 70  -- 0 transparent, 255 opaque
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% } %time% { %music% | %battery%            "
       , commands = [ Run StdinReader
                    , Run Date "%_I:%M" "time" 300 -- every 30s
                    --, Run Date "%A %_d %B" "date" 18000 -- every 30min
                    -- CPU core temperature
                    , Run CoreTemp [ "--template" , "<core0>/<core1>°C"
                                   , "--Low"      , "70"
                                   , "--High"     , "80"
                                   , "--low"      , "#33BB33"
                                   , "--normal"   , "#AA8800"
                                   , "--high"     , "#FF0000"
                                   ] 50 -- every 5s
                    , Run Battery  [ "--template" , "<acstatus>"
                                   , "--Low"      , "15"
                                   , "--High"     , "80"
                                   , "--low"      , "#FF0000"
                                   , "--normal"   , "#AA8800"
                                   , "--high"     , "#33BB33"
                                   -- Battery-specific options
                                   , "--"
                                   , "-o" , "<left>% (<timeleft> left)"      -- discharging
                                   , "-O" , "<fc=#DDDD00>+<left>%+</fc>" -- charging
                                   , "-i" , "<fc=#00FF00></fc>"  -- fully charged
                                   ] 50 -- every 5s
                    , Run Com "/bin/sh" ["-c","~/.scripts/get-spotify"] "music" 1000
                    ]
       }
