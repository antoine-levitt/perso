-- generated_rule Chrome
if string.find(get_window_name(), "Google Chrome") then
   undecorate_window()
   maximize()
   set_window_workspace(2)
   maximize_vertically()
   maximize_horizontally()
   debug_print("match")
end
