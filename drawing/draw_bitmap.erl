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

    Bitmap = wxBitmap:new(wxBitmap:getWidth(Logo)+40, wxBitmap:getHeight(Logo)+40),
    MDC = wxMemoryDC:new(Bitmap),
    wxDC:setBackground(MDC, wxBrush:new({10,10,10})),
    wxDC:clear(MDC),
    wxDC:drawBitmap(MDC, Logo, {20,20}),

    Heart = fun(DC, X, Y) ->
        wxDC:setPen(DC, wxPen:new({255,0,0})),
        wxDC:drawLines(DC, [{X,Y},{X-20,Y+30}, {X-40,Y}]),
        wxDC:setBrush(DC, wxBrush:new({255,0,0})),
        wxDC:drawArc(DC, {X,Y}, {X-20,Y}, {X-10,Y}),
        wxDC:drawArc(DC, {X-20,Y}, {X-40,Y}, {X-30,Y}),
        wxDC:floodFill(DC, {X-20, Y+10}, {255,0,0}, [{style, 2}])
    end,
    Heart(MDC, wxBitmap:getWidth(Bitmap) - 5, 15),
    wxMemoryDC:destroy(MDC),
    wxDC:drawBitmap(CDC, Bitmap, {150,10}),

    %% save bitmap into png file
    % wxBitmap:saveFile(Bitmap, "love_erlang.png", 15),
    ok.
