-module(collatz_conjecture).
-ifdef(TEST).
    -compile(export_all).
-endif.
-export([steps/1,worker/0]).

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
steps_helper(N, Steps) when (N rem 2 =:= 0) -> steps_helper(N div 2, Steps+1);
steps_helper(N, Steps) -> steps_helper(3*N + 1, Steps+1).


worker() -> worker([], []).
worker(Given_Num, Output_Num) ->  % head of function
    receive  % waiting for message
        Msg -> process_message(Msg, Given_Num, Output_Num)
    end.
-spec process_message(Input1, Input2, Input3) -> Result when
                      Input1          :: tuple(),
                      Input2          :: list(),
                      Input3          :: list(),
                      Result          :: list() | integer() | atom().

process_message({From, Number},Given_Num, Output_Num) when is_number(Number) ->  % accept message which is tuple
    io:format("~p", [Number]), % print the number from message
    Result = steps(Number),
    From ! {self(), Number, Result}, % send message to 'From",(message include: Pid of current process, result of steps)
    worker([Number|Given_Num], [Result|Output_Num]); % call itself again

process_message({From, get_all_tasks}, Given_Num, _Output_Num) ->
    From ! Given_Num,
    worker(Given_Num, _Output_Num);

process_message({From, get_all_results}, _Given_Num, Output_Num) ->
    From ! Output_Num,
    worker(_Given_Num, Output_Num);

process_message({From, get_last_result},_Given_Num, Output_Num) when Output_Num =:= [] ->
    From ! undefined,
    worker(_Given_Num, Output_Num);

process_message({From, get_last_result},_Given_Num, Output_Num) ->
     From ! hd(Output_Num),
     worker(_Given_Num, Output_Num);

process_message(make_me_sia,_Given_Num, _Output_Num) -> % accept message which is atom,
     ok.
