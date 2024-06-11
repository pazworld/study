-module(draw).

-export([start/0]).

-include_lib("wx/include/wx.hrl").

start() ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), -1, "Test1"),
    wxFrame:show(Frame),
    DC = wxClientDC:new(Frame),
    wxDC:drawText(DC, "wxErlang is cool!", {50,50}),

    %% Drawing Lines
    wxDC:drawLine(DC, {50,70}, {150, 70}),
    Pen = wxPen:new({255,0,0}, [{width, 3}, {style, ?wxDOT_DASH}]),
    wxDC:setPen(DC, Pen),
    wxDC:drawLine(DC, {50,80}, {150, 80}),

    %% Drawing Shapes
    wxDC:drawRectangle(DC, {50,90,100,20}),
    Brush = wxBrush:new({0,0,255}, [{style, ?wxCROSSDIAG_HATCH}]),
    wxDC:setBrush(DC, Brush),
    wxDC:drawRectangle(DC, {200,50,100,20}),

    %% Clipping and Logical Functions
    wxDC:setClippingRegion(DC, {70, 30, 30, 100}),
    wxDC:setLogicalFunction(DC, ?wxCOPY),
    YellowBrush = wxBrush:new({255,255,0}, [{style, ?wxSOLID}]),
    wxDC:setBrush(DC, YellowBrush),
    wxDC:drawRectangle(DC, {0,0,200,200}),
    ok.
