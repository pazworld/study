-module(drawn_board).

-behaviour(wx_object).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        handle_event/2, handle_sync_event/3, terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

start_link() ->
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "chessboard",
			[{style, ?wxDEFAULT_FRAME_STYLE bor ?wxFULL_REPAINT_ON_RESIZE}]),
    Panel = wxPanel:new(Frame, [{size, {200, 200}}]),
    wxPanel:connect(Panel, paint, [callback]),
    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),
    White = {140, 220, 120},
    WhiteBrush = wxBrush:new(White),
    Black = {80, 160, 60},
    BlackBrush = wxBrush:new(Black),
    Layout = init_board(),
    ImageMap = load_images(),
    wxFrame:refresh(Frame),
    {Frame, #{panel => Panel, image_map => ImageMap, layout => Layout,
        white_brush => WhiteBrush, black_brush => BlackBrush}}.

% paint event
handle_sync_event(#wx{event=#wxPaint{}}, _, State) ->
    paint_board(State),
    ok.

% window closed
handle_event(#wx{event=#wxClose{}}, State) ->
    {stop, normal, State};

% got other events
handle_event(#wx{} = Event, State) ->  
    io:format("got ~p~n", [Event]),	
    {noreply, State}.

handle_call(Request, From, State) ->
    io:format("handle_call: Request: ~p, From: ~p~n", [Request, From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    io:format("handle_cast: Msg: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("handle_info: Info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    destroy_resources(State),
    wx:destroy().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

square_size(W, H) ->
    ((min(W, H) div 8) div 2) * 2.

square_colour(Column, Row) ->
    case (Column + Row) rem 2 of
        0 -> white;
        _ -> black
    end.

rectangle({Column, Row}, SquareSize) ->
    {Column * SquareSize, Row * SquareSize, SquareSize, SquareSize}.

load_images() ->
    ImageFileNames = #{
        {black, rook}   => "black_rook.png",
        {black, knight} => "black_knight.png",
        {black, bishop} => "black_bishop.png",
        {black, queen}  => "black_queen.png",
        {black, king}   => "black_king.png",
        {black, pawn}   => "black_pawn.png",
        {white, rook}   => "white_rook.png",
        {white, knight} => "white_knight.png",
        {white, bishop} => "white_bishop.png",
        {white, queen}  => "white_queen.png",
        {white, king}   => "white_king.png",
        {white, pawn}   => "white_pawn.png"},
    maps:map(fun(_K, V) -> wxImage:new(
            filename:join("../images", V),
            [{type, ?wxBITMAP_TYPE_PNG}]) end,
        ImageFileNames).

init_board() ->
    Columns = lists:seq(0, 7),
    BlackPieces = [{black, rook}, {black, knight}, {black, bishop}, {black, queen},
                   {black, king}, {black, bishop}, {black, knight}, {black, rook}],
    WhitePieces = [{white, rook}, {white, knight}, {white, bishop}, {white, queen},
                   {white, king}, {white, bishop}, {white, knight}, {white, rook}],
    Row1 = [{{C, 1}, {black, pawn}} || C <- Columns ],
    Row6 = [{{C, 6}, {white, pawn}} || C <- Columns ],
    Row0 = [{{C, 0}, lists:nth(C + 1, BlackPieces)} || C <- Columns ],
    Row7 = [{{C, 7}, lists:nth(C + 1, WhitePieces)} || C <- Columns ],
    maps:from_list(Row0 ++ Row1 ++ Row6 ++ Row7).

paint_board(#{panel := Panel,
        layout := Layout,
        image_map := ImageMap,
        white_brush := WhiteBrush,
        black_brush := BlackBrush}) ->
    {W, H} = wxPanel:getSize(Panel),
    SquareSize = square_size(W, H),

    PaintSquare =
        fun(DC, C, R) ->
            Brush = case square_colour(C, R) of
                black -> BlackBrush;
                white -> WhiteBrush
            end,
            Rectangle = rectangle({C, R}, SquareSize),
            wxDC:setBrush(DC, Brush),
            wxDC:drawRectangle(DC, Rectangle),

            case maps:get({C, R}, Layout, none) of
                none -> ok;
                Piece ->
                    {X, Y, SW, SH} = Rectangle,
                    Image = wxImage:scale(maps:get(Piece, ImageMap), SW, SH),
                    PieceBitmap = wxBitmap:new(Image),
                    wxDC:drawBitmap(DC, PieceBitmap, {X, Y}),
                    wxImage:destroy(Image),
                    wxBitmap:destroy(PieceBitmap)
            end
        end,

    DC = wxPaintDC:new(Panel),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    Seq0to7 = lists:seq(0, 7),
    [PaintSquare(DC, C, R) || R <- Seq0to7, C <- Seq0to7],
    wxPaintDC:destroy(DC).

destroy_resources(#{image_map := ImageMap,
        white_brush := WhiteBrush,
        black_brush := BlackBrush}) ->
    [wxImage:destroy(I) || I <- maps:values(ImageMap)],
    wxBrush:destroy(WhiteBrush),
    wxBrush:destroy(BlackBrush).
