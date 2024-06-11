-module(semitransparent).

-include_lib("wx/include/wx.hrl").

-export([start/0]).

start() ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), -1, "semitransparent", [{size, {64*8+100, 64*4+100}}]),

    wxFrame:show(Frame),

    DC = wxClientDC:new(Frame),
    wxDC:drawRectangle(DC, {10,10,100,100}),
    wxDC:setBrush(DC, wxBrush:new({255,0,0,255})), %% red with alpha
    wxDC:drawRectangle(DC, {50,50,100,100}),
    % BM = wxBitmap:new(wxImage:new("semitransparent.png")),
    BM = wxBitmap:new("semitransparent.png", [{type, ?wxBITMAP_TYPE_PNG}]),
    wxDC:drawBitmap(DC, BM, {80,80}),
    ok.
