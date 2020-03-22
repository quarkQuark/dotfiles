local gears = require("gears")
local awful = require("awful")
              require("awful.autofocus")
local hotkeys_popup = require("awful.hotkeys_popup")

local apps = require("apps")
modkey = "Mod4"

local globalkeys = gears.table.join(

    -- Application

    awful.key({modkey}, "Return",
               function () awful.spawn(apps.terminal) end,
              {description = "terminal", group = "Application"}),

    awful.key({ modkey }, "e", function () awful.spawn(apps.text_editor) end,
              {description = "text editor", group = "Application"}),
    awful.key({ modkey }, "w", function () awful.spawn(apps.web_browser_light) end,
              {description = "web browser", group = "Application"}),

    -- Awesome
    awful.key({ modkey }, "s",      hotkeys_popup.show_help,
              {description="show help", group="Awesome"}),
    --awful.key({ modkey }, "w", function () mymainmenu:show() end,
              --{description = "show main menu", group = "awesome"}),
    awful.key({ modkey }, "q", awesome.restart,
              {description = "reload awesome", group = "Awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "Awesome"}),
    awful.key({ modkey }, "x",
        function ()
            awful.prompt.run {
                prompt       = "Run Lua code: ",
                textbox      = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
        end,
        {description = "lua execute prompt", group = "Awesome"}),

    -- Client
    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "cycle focused client", group = "Client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "cycle focused client", group = "Client"}
    ),

    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap clients", group = "Client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap clients", group = "Client"}),

    --awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              --{description = "focus the next screen", group = "Screen"}),
    --awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              --{description = "focus the previous screen", group = "Screen"}),

    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "Client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "Client"}),

    -- Tag
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "switch tag", group = "Tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "switch tag", group = "Tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "Tag"}),

    -- Layout
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "change master width", group = "Layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "change master width", group = "Layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "change number of master clients", group = "Layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "change number of master clients", group = "Layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "change number of columns", group = "Layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "change number of columns", group = "Layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "cycle layouts", group = "Layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "cycle layouts", group = "Layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        "request::activate", "key.unminimize", {raise = true}
                    )
                  end
              end,
              {description = "restore minimised", group = "Client"}),

    -- Launcher
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "Launcher"}),
    awful.key({ modkey }, "p", function() menubar.show() end,
              {description = "show the menubar", group = "Launcher"}),
    awful.key({ "Control" }, "Escape", function() awful.spawn("dmenu_run") end,
              {description = "dmenu_run", group = "Launcher"}),
    awful.key({ modkey, "Shift" }, "e", function() awful.spawn.with_shell(". ~/.config/dmenu/edit-configs.sh") end,
      {description = "edit configs", group = "Launcher"})
)

return globalkeys
