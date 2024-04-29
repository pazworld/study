-module(countdown_gui).
-export([start/1]).
-include_lib("wx/include/wx.hrl").

start(Seconds) when is_integer(Seconds) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Countdown"),
    Counter = wxStaticText:new(Frame, ?wxID_ANY, integer_to_list(Seconds)),
    wxFrame:show(Frame),
    countdown(Seconds -1, Counter),
    timer:sleep(10000).

countdown(Seconds, _) when Seconds < 0 ->
    ok;
countdown(Seconds, Counter) ->
    timer:sleep(1000),
    wxStaticText:setLabel(Counter, integer_to_list(Seconds)),
    countdown(Seconds -1, Counter).
