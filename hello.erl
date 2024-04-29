-module(hello).
-include_lib("wx/include/wx.hrl").
-export([start/0]).

start() ->
    wx:new(),
    Opt = ?wxICON_QUESTION bor ?wxYES bor ?wxNO bor ?wxYES_DEFAULT,
    M = wxMessageDialog:new(wx:null(), "Do you love Erlang?", [{style, Opt}]),
    wxMessageDialog:showModal(M),
    wx:destroy().
