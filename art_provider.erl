-module(art_provider).

-include_lib("wx/include/wx.hrl").

-export([start/0]).

start() ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), -1, "bitmaps"),

    wxFrame:show(Frame),

    DC = wxClientDC:new(Frame),
    ErrorBmp = wxArtProvider:getBitmap("wxART_ERROR", [{size, {100,100}}]),
    wxDC:drawBitmap(DC, ErrorBmp, {0,0}),
    wxBitmap:destroy(ErrorBmp),
    wxClientDC:destroy(DC),
    ok.
