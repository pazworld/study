-module(chess_board).

-define(SQUARE, chess_square).
-define(UTILS, chess_utils).
-define(WHITE, {140, 220, 120}).
-define(BLACK, {80, 160, 60}).
-define(SELECTED_COLOUR, {238, 232, 170}).

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
    wxPanel:setBackgroundStyle(Panel, ?wxBG_STYLE_CUSTOM),
    % wxPanel:connect(Panel, paint, [callback]),
    % wxPanel:connect(Panel, erase_background, [callback]),
    % wxPanel:connect(Panel, left_down),
    wxFrame:connect(Frame, close_window),
    White = {140, 220, 120},
    Black = {80, 160, 60},

    Board = wxPanel:new(Frame, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxPanel:setBackgroundStyle(Board, ?wxBG_STYLE_CUSTOM),

    Grid = wxGridSizer:new(8, 8, 0, 0),
    wxPanel:setSizer(Board, Grid),

    WhiteBrush = wxBrush:new(?WHITE),
    BlackBrush = wxBrush:new(?BLACK),
    SelectedBrush = wxBrush:new(?SELECTED_COLOUR),

    MkSquare =
        fun(C, R) ->
            SquareColour = ?UTILS:square_colour(C, R),
            Brush = case SquareColour of
                white -> WhiteBrush;
                black -> BlackBrush
            end,
            Square = ?SQUARE:start_link(
                {C, R},
                self(),
                Board,
                Brush,
                SelectedBrush),
            {{C, R}, Square}
        end,

    SquareList = [MkSquare(C, R) || R <- lists:seq(0, 7), C <- lists:seq(0, 7)],
    [wxSizer:add(Grid, Square, [{flag, ?wxEXPAND}])
        || {_, Square} <- SquareList],

    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, Board, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxFrame:setSizer(Frame, MainSizer),
    wxFrame:show(Frame),

    State = #{panel => Panel,
        image_map => ?UTILS:load_images(),
        layout => ?UTILS:init_board(),
        white_brush => wxBrush:new(White),
        black_brush => wxBrush:new(Black),
        selected_brush => wxBrush:new({238, 232, 170}),
        background_brush => wxBrush:new(wxPanel:getBackgroundColour(Panel)),
        selected => none},
    wxFrame:refresh(Frame),
    {Frame, State}.

% ignore erase event
handle_sync_event(#wx{event = #wxErase{}}, _, _) ->
    ok;

% paint event
handle_sync_event(#wx{event = #wxPaint{}}, _, State) ->
    paint_board(State),
    ok.

% panel clicked
handle_event(#wx{event = #wxMouse{leftDown = true, x = X, y = Y}},
    State = #{panel := Panel,
        layout := Layout,
        selected := none}) ->  %% selecting a piece
    {C, R} = where(X, Y, Panel),
    case maps:get({C, R}, Layout, none) of
        none ->
            {noreply, State};
        _ ->
            wxPanel:refresh(Panel),
            {noreply, State#{selected => {C, R}}}
    end;

handle_event(#wx{event = #wxMouse{leftDown = true, x = X, y = Y}},
    State = #{panel := Panel,
        layout := Layout,
        selected := Selected}) ->  %% dropping a selected piece
    {C, R} = where(X, Y, Panel),
    Piece = maps:get(Selected, Layout),
    NewLayout = maps:put({C, R}, Piece, maps:remove(Selected, Layout)),
    wxPanel:refresh(Panel),
    {noreply, State#{layout => NewLayout, selected => none}};

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

where(X, Y, Panel) ->
    {W, H} = wxPanel:getSize(Panel),
    SquareSize = square_size(W, H),
    {X div SquareSize, Y div SquareSize}.

paint_board(#{panel := Panel,
        layout := Layout,
        image_map := ImageMap,
        white_brush := WhiteBrush,
        black_brush := BlackBrush,
        selected_brush := SelectedBrush,
        background_brush := BackgroundBrush,
        selected := Selected}) ->
    {W, H} = wxPanel:getSize(Panel),
    SquareSize = square_size(W, H),

    PaintSquare =
        fun(DC, C, R) ->
            Brush = case Selected of
                {C, R} ->
                    SelectedBrush;
                _ ->
                    case square_colour(C, R) of
                        black -> BlackBrush;
                        white -> WhiteBrush
                    end
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

    DC = wxBufferedPaintDC:new(Panel),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    wxDC:setBackground(DC, BackgroundBrush),
    wxDC:clear(DC),
    Seq0to7 = lists:seq(0, 7),
    [PaintSquare(DC, C, R) || R <- Seq0to7, C <- Seq0to7],
    wxBufferedPaintDC:destroy(DC).

destroy_resources(#{image_map := ImageMap,
        white_brush := WhiteBrush,
        black_brush := BlackBrush}) ->
    [wxImage:destroy(I) || I <- maps:values(ImageMap)],
    wxBrush:destroy(WhiteBrush),
    wxBrush:destroy(BlackBrush).
