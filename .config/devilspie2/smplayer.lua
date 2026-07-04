-- generated_rule Smplayer
if string.find(get_window_name(), "SMPlayer") then
   undecorate_window()
   focus_window()
   maximize()
   set_window_workspace(4)
   debug_print("match smplayer")
end
