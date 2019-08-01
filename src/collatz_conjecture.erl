-module(collatz_conjecture).
-ifdef(TEST).
    -compile(export_all).
-endif.
-export([steps/1, worker/0, calculate/2]).
-record(worker_state, {tasks = [], results = []}).

-spec steps(Num)    -> Result when
            Num     :: integer(),
            Result  :: pos_integer() | {error, string()}.

steps(N) when N =< 0 -> {error, "Only positive numbers are available"};
steps(N) -> steps_helper(N, 0).

-spec steps_helper(Num1, Num2) -> Result when
                  Num1        :: pos_integer(),
                  Num2        :: non_neg_integer(),
                  Result      :: pos_integer().

steps_helper(1, Steps) -> Steps;
steps_helper(N, Steps) when (N rem 2 =:= 0) -> steps_helper(N div 2, Steps + 1);
steps_helper(N, Steps) -> steps_helper(3 * N + 1, Steps + 1).

calculate(From, To) -> calculate(From, To, [spawn(collatz_conjecture, worker, []) || _X <- lists:seq(1, 5)], 0).

calculate(From, To, _Workers, LongestStep) when (From > To) -> LongestStep;

calculate(From, To, [H|T], LongestStep) ->
    H ! {self(), From},
    calculate(From + 1, To, T, LongestStep);


calculate(From, To, [], LongestStep) ->
    receive
        {WorkerPid, Result} ->
            calculate_message(From, To, [WorkerPid], max(Result, LongestStep))
    end.

worker() -> worker(#worker_state{}). % init state -> {worker_state,  [], []}
worker(State) ->  % head of function
    receive  % waiting for message
        Msg -> process_message(Msg, State)
    end.
-spec process_message(Input1, Input2) -> Result when
                       Input1          :: tuple(),
                       Input2          :: tuple(),
                       Result          :: list() | integer() | atom().

process_message({From, Task}, #worker_state{tasks = Tasks, results = Results} = State) when is_number(Task) ->  % accept message which is tuple
    Result = steps(Task),
    From ! {self(), Result}, % send message to 'From",(message include: Pid of current process, result of steps)
    worker(State#worker_state{tasks = [Task | Tasks], results = [Result | Results]}); % call itself again

process_message({From, get_all_tasks}, #worker_state{tasks = Tasks} = State) ->
    From ! Tasks,
    worker(State);

process_message({From, get_all_results}, #worker_state{results = Results} = State) ->
    From ! Results,
    worker(State);

process_message({From, get_last_result},#worker_state{results = Results} = State) when Results =:= [] ->
    From ! undefined,
    worker(State);

process_message({From, get_last_result}, #worker_state{results = Results} = State) ->
     From ! hd(Results),
     worker(State);

process_message(make_me_sia, _State) -> % accept message which is atom,
     ok.
