-module(ttt_game).
-author("Jonathon Mah").
-behaviour(gen_fsm).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, print_board/1, board_to_iolist/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

print_board(Board) ->
	io:fwrite(board_to_iolist(Board)).

board_to_iolist(Board) ->
	BoardChars = lists:flatmap(fun(Row) -> lists:map(fun board_elem_to_char/1, Row) end, Board),
	io_lib:format("~c|~c|~c~n"
		          "-+-+-~n"
				  "~c|~c|~c~n"
				  "-+-+-+~n"
				  "~c|~c|~c~n", BoardChars).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

-record(st, {o_ref, x_ref, board=[[u,u,u],[u,u,u],[u,u,u]]}).

init(_Args) ->
    {ok, initial_state_name, initial_state}.

state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    {reply, ok, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

board_elem_to_char(o) -> $O;
board_elem_to_char(x) -> $X;
board_elem_to_char(u) -> $ .
