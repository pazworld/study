-module(drawn_board).

-behaviour(wx_object).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        handle_event/2,
        handle_sync_event/3,
        terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

start_link() ->
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "chessboard"),
			% [{style, ?wxDEFAULT_FRAME_STYLE bor ?wxFULL_REPAINT_ON_RESIZE}]),
    Panel = wxPanel:new(Frame, [{size, {200, 200}}]),
    wxPanel:connect(Panel, paint, [callback]),
    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),
    wxFrame:refresh(Frame),
    {Frame, #{panel => Panel}}.

paint_board(#{panel := Panel}) ->
    DC = wxPaintDC:new(Panel),
    Black = {140, 220, 120},
    BlackBrush = wxBrush:new(Black),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    wxDC:setBrush(DC, BlackBrush),
    wxDC:drawRectangle(DC, {10, 10, 100, 100}),
    wxPaintDC:destroy(DC).

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
