-module(draw_bitmap).

-include_lib("wx/include/wx.hrl").

-export([start/0]).

start() ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), -1, "bitmaps"),

    wxFrame:show(Frame),
    FileName = filename:join(code:lib_dir(wx,priv), "erlang-logo64.png"),
    Image = wxImage:new(FileName),
    Logo = wxBitmap:new(Image),
    CDC = wxClientDC:new(Frame),
    wxDC:drawBitmap(CDC, Logo, {10,10}),
    ok.
