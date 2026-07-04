-- generated_rule Slack
if string.find(get_window_name(), "Slack") then
   -- undecorate_window()
   -- focus_window()
   maximize()
   set_window_workspace(4)
   debug_print("match slack")
end
