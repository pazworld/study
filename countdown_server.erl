-module(countdown_server).

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

-define(SERVER, ?MODULE).

-record(state, {counter, button, counting_down, tref}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    print_pid("init"),
    wx:new(),
    Frame = wxFrame:new(wx:null(), 1, "Countdown"),

    %% build and layout the GUI components
    Label = wxStaticText:new(Frame, ?wxID_ANY, "Seconds remaining", [{style, ?wxALIGN_RIGHT}]),
    wxStaticText:wrap(Label, 100),
    Counter = wxTextCtrl:new(Frame, ?wxID_ANY, [{value, "42"}, {style, ?wxTE_RIGHT}]),
    Font = wxFont:new(42, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    wxTextCtrl:setFont(Counter, Font),
    Button = wxButton:new(Frame, ?wxID_ANY, [{label, "Start"}]),

    CounterSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(CounterSizer, Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
    wxSizer:add(CounterSizer, Counter, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, CounterSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, Button, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxWindow:setSizer(Frame, MainSizer),
    wxSizer:setSizeHints(MainSizer, Frame),
    wxWindow:setMinSize(Frame, wxWindow:getSize(Frame)),

    wxButton:connect(Button, command_button_clicked),
    wxFrame:connect(Frame, close_window),

    wxFrame:show(Frame),
    {ok, #state{counter = Counter, button = Button, counting_down = false}}.

handle_call(_Request, _From, State) ->
    print_pid("handle_call"),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    print_pid("handle_cast"),
    {noreply, State}.

%% Start button clicked
handle_info(
        #wx{obj = Button, event = #wxCommand{type = command_button_clicked}},
        #state{counter = Counter, counting_down = false} = State) ->
    print_pid("handle_info: counting_down=false"),
    case list_to_integer(wxTextCtrl:getValue(Counter)) of
        0 ->
            {noreply, State};
        _ ->
            wxTextCtrl:setEditable(Counter, false),
            wxButton:setLabel(Button, "Stop"),
            TRef = erlang:send_after(1000, self(), update_gui),
            {noreply, State#state{tref = TRef, counting_down = true}}
    end;

%% Stop button clicked
handle_info(
        #wx{obj = Button, event = #wxCommand{type = command_button_clicked}},
        #state{counter = Counter, counting_down = true, tref = TRef} = State) ->
    print_pid("handle_info: counting_down=true"),
    erlang:cancel_timer(TRef),
    wxTextCtrl:setEditable(Counter, true),
    wxButton:setLabel(Button, "Start"),
    {noreply, State#state{tref = undefined, counting_down = false}};

%% called every 1 second to count down
handle_info(
        update_gui,
        #state{button = Button, counter = Counter, counting_down = true} = State) ->
    print_pid("handle_info: update_gui"),
    Value = wxTextCtrl:getValue(Counter),
    case list_to_integer(Value) of
        1 ->
            wxTextCtrl:setValue(Counter, "0"),
            wxTextCtrl:setEditable(Counter, true),
            wxButton:setLabel(Button, "Start"),
            {noreply, State#state{counting_down = false}};
        N ->
            wxTextCtrl:setValue(Counter, integer_to_list(N-1)),
            TRef = erlang:send_after(1000, self(), update_gui),
            {noreply, State#state{tref = TRef}}
    end;

%% window closed
handle_info(#wx{event=#wxClose{}}, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    print_pid("terminate"),
    wx:destroy(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

print_pid(Message) ->
    io:format("~s: ~p~n", [Message, self()]).
