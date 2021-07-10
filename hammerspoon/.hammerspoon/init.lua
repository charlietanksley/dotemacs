hs.hotkey.bind({"cmd", "alt", "ctrl"}, "H", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "L", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.w / 2 - 1
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "M", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h
  win:setFrame(f)
end)

hs.hotkey.bind({nil}, "end", function()
      hs.caffeinate.systemSleep()
end)


local function screenshot()
   local args = "-s"
   local filename = hs.fs.pathToAbsolute("~").."/screenshots/Screen Shot at "..os.date("%Y-%m-%d %I.%M.%S %p", os.time())..".png"
   hs.task.new("/usr/sbin/screencapture", nil, {args, filename}):start()
end

local utilsTable = {
   { title = "screenshot", fn = screenshot },
   { title = "sleep", fn = function() hs.caffeinate.systemSleep() end },
}

local utilsMenu = hs.menubar.new()
utilsMenu:setMenu(utilsTable)
utilsMenu:setTitle("Utils")
