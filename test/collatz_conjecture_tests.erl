-module(collatz_conjecture_tests).
-include_lib("eunit/include/eunit.hrl").


steps_test() -> %test steps.
    ?assertEqual(18, collatz_conjecture:steps(30)),
    ?assertEqual(6, collatz_conjecture:steps(10)),
    ?assertEqual({error,"Only positive numbers are available"}, collatz_conjecture:steps(-3)),
    ?assertEqual({error, "Only positive numbers are available"}, collatz_conjecture:steps(0)).

worker_test() ->
    WorkerPid=spawn(collatz_conjecture,worker,[]),% spawn worker and assign pid of worker to variable WorkerPid

    WorkerPid ! {self(), get_last_result},
    receive
        Empty ->
            ?assertEqual(undefined, Empty)
    end,

    WorkerPid ! {self(), get_all_tasks},
    receive
        Empty_task ->
            ?assertEqual([], Empty_task)
    end,

    WorkerPid ! {self(), 2}, % send message to WorkerPid(message include: pid of current process, number assign to worker
    receive  % waiting message
        {From1, Result1} -> % accept message which is tuple, assign value of first element of tupple to variable From1, assign second element of tupple to variable Result1
            ?assertEqual(1, Result1),%check result
            ?assertEqual(WorkerPid, From1); % check "From1" is "WorkerPid" or not
        _Another1 ->  % accept another message
            ?assert(false) % fail
    end, % end of waiting message

    WorkerPid ! {self(),10},
    receive
        {From2, Result2} ->
            ?assertEqual(6, Result2),
            ?assertEqual(WorkerPid, From2);
        _Another2 ->
            ?assert(false)
    end,

    WorkerPid ! {self(), 30},
    receive
        {From3, Result3} ->
            ?assertEqual(18, Result3),
            ?assertEqual(WorkerPid, From3);
        _Another3 ->
            ?assert(false)
    end,

    WorkerPid ! {self(), get_all_tasks},
    receive
        All_tasks when is_list(All_tasks) ->
            ?assertEqual([30,10,2], All_tasks);
        _Another4 ->
            ?assert(false)
    end,

    WorkerPid ! {self(), get_last_result},
    receive
        Head when is_integer(Head) ->
            ?assertEqual(18, Head);
        _Another5 ->
            ?assert(false)
    end,

    WorkerPid ! {self(), get_all_results},
    receive
        All_results when is_list(All_results) ->
            ?assertEqual([18,6,1], All_results);
        _Another6 ->
            ?assert(false)
    end,

    WorkerPid ! make_me_sia, % send message to WorkerPid( message is atom
    timer:sleep(10),
    ?assertNot(is_process_alive(WorkerPid)). % check process alive

calculate_test() ->
    ?assertEqual(7, collatz_conjecture:calculate(1, 3)),
    ?assertEqual(16, collatz_conjecture:calculate(1, 7)),
    ?assertEqual(20, collatz_conjecture:calculate(10, 20)),
    ?assertEqual(20, collatz_conjecture:calculate(15, 18)).

