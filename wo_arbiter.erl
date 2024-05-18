-module(wo_arbiter).

-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    handle_event/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    wx_object:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    wx:new(),
    %% Env = wx:get_env(),

    %% build and layout the GUI components
    Frame = wxFrame:new(wx:null(), 1, "Countdown"),
    Player1 = wo_player:start_link(player1, Frame, ?SERVER),
    Player2 = wo_player:start_link(player2, Frame, ?SERVER),
    %% Player1 = gen_server:call(player1, get_panel),
    %% Player2 = gen_server:call(player2, get_panel),

    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(MainSizer, Player1, [{proportion, 1}, {flag, ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, Player2, [{proportion, 1}, {flag, ?wxALL}, {border, 5}]),

    wxWindow:setSizer(Frame, MainSizer),
    wxSizer:setSizeHints(MainSizer, Frame),
    wxWindow:setMinSize(Frame, wxWindow:getSize(Frame)),

    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),
    player1 ! move,
    {Frame, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({reset, N}, State) ->
    player1 ! {reset, N},
    player2 ! {reset, N},
    {noreply, State};

handle_info({moved, player1}, State) ->
    player2 ! move,
    {noreply, State};
handle_info({moved, player2}, State) ->
    player1 ! move,
    {noreply, State};

handle_info({ilose, player1}, State) ->
    player2 ! youwin,
    {noreply, State};
handle_info({ilose, player2}, State) ->
    player1 ! youwin,
    {noreply, State};

handle_info(Msg, State) ->
    io:format("frame got unexpected message ~p~n", [Msg]),
    {noreply, State}.

handle_event(#wx{event = #wxClose{}}, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    %% sys:terminate(player1, Reason),
    %% sys:terminate(player2, Reason),
    wx:destroy(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
