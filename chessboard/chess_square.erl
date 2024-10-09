-module(chess_square).

-behaviour(wx_object).

-export([start_link/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        handle_event/2, handle_sync_event/3, terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

start_link(Location, BoardPid, Parent, Brush, SelectedBrush) ->
    wx_object:start_link(
        ?MODULE, [Location, BoardPid, Parent, Brush, SelectedBrush], []).

init([Location, BoardPid, Parent, Brush, SelectedBrush]) ->
    Panel = wxPanel:new(Parent, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxPanel:setBackgroundStyle(Panel, ?wxBG_STYLE_CUSTOM),
    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect(Panel, erase_background, [callback]),
    wxPanel:connect(Panel, left_down),
    State = #{
        location => Location,
        board_pid => BoardPid,
        square_panel => Panel,
        image => none,
        brush => Brush,
        selected_brush => SelectedBrush,
        selected => false
    },
    {Panel, State}.

% ignore erase event
handle_sync_event(#wx{event = #wxErase{}}, _, _) ->
    ok;

% paint event
handle_sync_event(#wx{event = #wxPaint{}}, _, State) ->
    paint_square(State),
    ok.

paint_square(#{square_panel := Panel,
               image := PieceImage,
               brush := Brush,
               selected_brush := SelectedBrush,
               selected := Selected}) ->
    Paint = fun(_DC, none) -> ok;
               (DC, Image) ->
                    {W, H} = wxPanel:getSize(Panel),
                    ScaledImage = wxImage:scale(Image, W, H),
                    PieceBitmap = wxBitmap:new(ScaledImage),
                    wxDC:drawBitmap(DC, PieceBitmap, {0, 0}),
                    wxImage:destroy(ScaledImage),
                    wxBitmap:destroy(PieceBitmap)
            end,

    DC = wxBufferedPaintDC:new(Panel),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    wxDC:setBackground(DC, case Selected of
                                true -> SelectedBrush;
                                false -> Brush
                            end),
    wxDC:clear(DC),
    Paint(DC, PieceImage),
    wxBufferedPaintDC:destroy(DC).

handle_call(Request, From, State) ->
    io:format("handle_call: Request: ~p, From: ~p~n", [Request, From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    io:format("handle_cast: Msg: ~p~n", [Msg]),
    {noreply, State}.

handle_info({Property, Value}, State = #{square_panel := Panel})
        when Property =:= selectable; Property =:= landable ->
    case Value of
        true -> wxWindow:setCursor(Panel, wxCursor:new(?wxCURSOR_HAND));
        false -> wxWindow:setCursor(Panel, ?wxNullCursor)
    end,
    {noreply, maps:put(Property, Value, State)};

handle_info({Property, Value}, State = #{square_panel := Panel}) ->
    wxWindow:refresh(Panel),
    {noreply, maps:put(Property, Value, State)};

handle_info(Info, State) ->
    io:format("handle_info: Info: ~p~n", [Info]),
    {noreply, State}.

% unselected square clicked
handle_event(#wx{event = #wxMouse{type = left_down}},
        State = #{square_panel := Panel,
                board_pid := BoardPid,
                selectable := true,
                selected := false,
                location := Location}) ->
    BoardPid ! {we_selected, Location},
    wxPanel:refresh(Panel),
    {noreply, State#{selected => true}};

% selected square clicked
handle_event(#wx{event = #wxMouse{type = left_down}}, State) ->
    {noreply, State};

handle_event(Event, State) ->
    io:format("handle_event: Event: ~p~n", [Event]),
    {noreply, State}.

terminate(_Reason, _State) ->
    wx:destroy().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
