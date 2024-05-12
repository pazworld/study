-module(player).

-behaviour(gen_server).
-export([start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

-record(state, {panel, counter, button, tref, whoami, arbiter}).

start_link(Name, Env, Frame, Arbiter) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Env, Frame, Arbiter], []).

init([Name, Env, Frame, Arbiter]) ->
    wx:set_env(Env),
    Panel = wxPanel:new(Frame),

    %% build and layout the GUI components
    Label = wxStaticText:new(Panel, ?wxID_ANY, "Seconds remaining", [{style, ?wxALIGN_RIGHT}]),
    wxStaticText:wrap(Label, 100),
    Counter = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "42"}, {style, ?wxTE_RIGHT}]),
    Font = wxFont:new(42, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    wxTextCtrl:setFont(Counter, Font),
    wxTextCtrl:setEditable(Counter, false),

    Button = wxButton:new(Panel, ?wxID_ANY, [{label, "Moved"}]),
    wxButton:disable(Button),

    CounterSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(CounterSizer, Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
    wxSizer:add(CounterSizer, Counter, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, CounterSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, Button, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),

    wxWindow:setSizer(Panel, MainSizer),
    wxSizer:setSizeHints(MainSizer, Panel),
    wxWindow:setMinSize(Panel, wxWindow:getSize(Panel)),

    wxButton:connect(Button, command_button_clicked),

    {ok, #state{panel = Panel,
            counter = Counter,
            button = Button,
            whoami = Name,
            arbiter = Arbiter}}.

handle_call(get_panel, _From, #state{panel = Panel} = State) ->
    {reply, Panel, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({reset,N}, #state{counter = Counter, button = Button} = State) ->
    wxTextCtrl:setValue(Counter, integer_to_list(N)),
    wxButton:disable(Button),
    {noreply, State};

handle_info(youwin, #state{counter = Counter} = State) ->
    wxTextCtrl:setValue(Counter, "win"),
    {noreply, State};

handle_info(
        #wx{obj = Button, event = #wxCommand{type = command_button_clicked}},
        #state{tref = TRef, whoami = Name, arbiter = Arbiter} = State) ->
    erlang:cancel_timer(TRef),
    wxButton:disable(Button),
    Arbiter ! {moved, Name},
    {noreply, State#state{tref = undefined}};

handle_info(move, #state{button = Button} = State) ->
    wxButton:enable(Button),
    TRef = erlang:send_after(1000, self(), update_gui),
    {noreply, State#state{tref = TRef}};

handle_info(
        update_gui,
        #state{button = Button, counter = Counter, whoami = Name, arbiter = Arbiter} = State) ->
    Value = wxTextCtrl:getValue(Counter),
    case list_to_integer(Value) of
        1 ->
            wxTextCtrl:setValue(Counter, "0"),
            wxButton:disable(Button),
            Arbiter ! {ilose, Name},
            {noreply, State};
        N ->
            wxTextCtrl:setValue(Counter, integer_to_list(N-1)),
            TRef = erlang:send_after(1000, self(), update_gui),
            {noreply, State#state{tref = TRef}}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
