%%%---------------------------------------------------------------------------------------------------------------------
%%%  File:      collatz_conjecture.erl
%%%  @auther    Jie Lin < unilin328@gmail.com >
%%%
%%%  @doc
%%%  The Collatz conjecture is a conjecture in mathematics that concerns a sequence defined as follows: start with any
%%%  positive integer n. Then each term is obtained from the previous term as follows: if the previous term is even,
%%%  the next term is one half the previous term. If the previous term is odd, the next term is 3 times the previous
%%%  term plus 1. The conjecture is that no matter what value of n, the sequence will always reach 1.
%%%
%%%  @end
%%%
%%%

-module(collatz_conjecture). %define module name (module name must same as file name).
-ifdef(TEST). % if define test, then compile all which export.
    -compile(export_all).
-endif.
-export([steps/1, worker/0, calculate/2]).%export functions,functions is a list([name1/arity1,...]),name1 must be atom,
%arity must be integer.
-record(worker_state, {tasks = [], results = []}).% define record,record name and field must be atoms.

%%%  @doc verificate collatz_conjecture
-spec     steps(Num)     ->  Result  when %test steps
                 Num     ::  integer(),
                 Result  ::  pos_integer() | {error, string()}.

steps(N) when N =< 0 -> {error, "Only positive numbers are available"}; %when N is negtive integer
steps(N) -> steps_helper(N, 0).  %when N is positive integer,return to steps_helper.

-spec steps_helper(Num1, Num2)  ->  Result when %test steps_helper.
                 Num1           ::  pos_integer(),
                 Num2           ::  non_neg_integer(),
                 Result         ::  pos_integer().

steps_helper(1, Steps) -> Steps; %when N=1, get result "Steps".
steps_helper(N, Steps) when (N rem 2 =:= 0) -> steps_helper(N div 2, Steps + 1);% when N rem 2 =:=0.
steps_helper(N, Steps) -> steps_helper(3 * N + 1, Steps + 1). %when N cannot be divisible by 2.


%%%  @doc get longest step from all numbers
-spec   calculate(Num1, Num2) ->  Result when %test calculate
                 Num1         ::  pos_integer(),
                 Num2         ::  pos_integer(),
                 Result       ::  non_neg_integer().

calculate(From, To) -> calculate(From, To, [spawn(collatz_conjecture, worker, []) || _X <- lists:seq(1, 5)], 0).% spawn
%5 workers and put 5 pid (pid get from "(From, To)") to the list,

calculate(From, To, Workers, LongestStep) when (From > To) andalso length(Workers) =:= 5 -> LongestStep; %when From>To,
%return to the LongsStep.
calculate(From, To, Workers, LongestStep) when From > To -> %when From>To and worker "not full"
    receive
        {WorkerPid, Result} ->
            calculate(From, To, [WorkerPid | Workers], max(Result, LongestStep))
    end;

calculate(From, To, [H|T], LongestStep) -> % put 5 pid to the worker list,return the LongestStep
          H ! {self(), From},
          calculate(From + 1, To, T, LongestStep);

calculate(From, To, Workers, LongestStep) -> %get result, put pid back to worker and compare result and LongestStep
    receive
        {WorkerPid, Result} ->
            calculate(From, To, [WorkerPid | Workers], max(Result, LongestStep))
    end.


%%%  @doc spawn workers for everytime calculate and keep state
worker() -> worker(#worker_state{}). % init state -> {worker_state,  [], []}
worker(State) ->  % head of function
    receive  % waiting for message
        Msg -> process_message(Msg, State)
    end.

-spec process_message(Input1, Input2) -> Result when %test process_message
                       Input1          :: tuple(),
                       Input2          :: tuple(),
                       Result          :: list() | integer() | atom().

process_message({From, Task}, #worker_state{tasks = Tasks, results = Results} = State) when is_number(Task) ->  %accept
% message which is tuple
        Result = steps(Task),
        From ! {self(), Result}, % send message to 'From",(message include: Pid of current process, result of steps)
        worker(State#worker_state{tasks = [Task | Tasks], results = [Result | Results]}); % call itself again

process_message({From, get_all_tasks}, #worker_state{tasks = Tasks} = State) -> %get all tasks
        From ! Tasks,
        worker(State);

process_message({From, get_all_results}, #worker_state{results = Results} = State) -> %get all results
        From ! Results,
        worker(State);

process_message({From, get_last_result},#worker_state{results = Results} = State) when Results =:= [] -> %when result
% is empty list
        From ! undefined,
        worker(State);

process_message({From, get_last_result}, #worker_state{results = Results} = State) -> %get last result
        From ! hd(Results),
        worker(State);

process_message(make_me_sia, _State) -> % accept message which is atom,
         ok.
