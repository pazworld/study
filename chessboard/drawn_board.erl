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
    ImageMap = load_images(),
    wxFrame:refresh(Frame),
    {Frame, #{panel => Panel, image_map => ImageMap,
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

terminate(_Reason, _) ->
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
        {white, rook}   => "white_rook.png",
        {white, knight} => "white_knight.png",
        {white, bishop} => "white_bishop.png",
        {white, queen}  => "white_queen.png",
        {white, king}   => "white_king.png"},
    maps:map(fun(_K, V) -> wxImage:new(
            filename:join("../images", V),
            [{type, ?wxBITMAP_TYPE_PNG}]) end,
        ImageFileNames).

paint_board(#{panel := Panel,
        white_brush := WhiteBrush,
        black_brush := BlackBrush}) ->
    {W, H} = wxPanel:getSize(Panel),
    SquareSize = square_size(W, H),

    PaintSquare =
        fun(DC, C, R) ->
            % io:format("~p~n", [square_colour(C, R)]),
            Brush = case square_colour(C, R) of
                black -> BlackBrush;
                white -> WhiteBrush
            end,
            Rectangle = rectangle({C, R}, SquareSize),
            wxDC:setBrush(DC, Brush),
            wxDC:drawRectangle(DC, Rectangle)
        end,

    DC = wxPaintDC:new(Panel),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    Seq0to7 = lists:seq(0, 7),
    [PaintSquare(DC, C, R) || R <- Seq0to7, C <- Seq0to7],
    wxPaintDC:destroy(DC).
