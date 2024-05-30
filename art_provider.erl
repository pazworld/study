-module(art_provider).

-include_lib("wx/include/wx.hrl").

-export([start/0]).

start() ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), -1, "bitmaps", [{size, {64*8+100, 64*4+100}}]),

    wxFrame:show(Frame),

    DC = wxClientDC:new(Frame),
    DrawFun = fun(Title, Idx) ->
        X = Idx rem 8,
        Y = Idx div 8,
        Bitmap = getArtBitmap(Title),
        wxDC:drawBitmap(DC, Bitmap, {X * 64, Y * 64}),
        wxBitmap:destroy(Bitmap),
        Idx + 1
    end,
    ArtTitles = getArtTitles(),
    lists:foldl(DrawFun, 0, ArtTitles),
    wxClientDC:destroy(DC),
    ok.

getArtBitmap(Title) ->
    wxArtProvider:getBitmap(Title, [{size, {64,64}}]).

getArtTitles() ->
    [
        "wxART_ADD_BOOKMARK",
        "wxART_DEL_BOOKMARK",
        "wxART_HELP_SIDE_PANEL",
        "wxART_HELP_SETTINGS",
        "wxART_HELP_BOOK",
        "wxART_HELP_FOLDER",
        "wxART_HELP_PAGE",
        "wxART_GO_BACK",
        "wxART_GO_FORWARD",
        "wxART_GO_UP",
        "wxART_GO_DOWN",
        "wxART_GO_TO_PARENT",
        "wxART_GO_HOME",
        "wxART_FILE_OPEN",
        "wxART_PRINT",
        "wxART_HELP",
        "wxART_TIP",
        "wxART_REPORT_VIEW",
        "wxART_LIST_VIEW",
        "wxART_NEW_DIR",
        "wxART_FOLDER",
        "wxART_GO_DIR_UP",
        "wxART_EXECUTABLE_FILE",
        "wxART_NORMAL_FILE",
        "wxART_TICK_MARK",
        "wxART_CROSS_MARK",
        "wxART_ERROR",
        "wxART_QUESTION",
        "wxART_WARNING",
        "wxART_INFORMATION",
        "wxART_MISSING_IMAGE"
    ].
