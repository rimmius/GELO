Reminders: 
107> {ok, K7,_} = erl_scan:string("mod() -> 1+1.").     
{ok,[{atom,1,mod},
     {'(',1},
     {')',1},
     {'->',1},
     {integer,1,1},
     {'+',1},
     {integer,1,1},
     {dot,1}],
    1}
108> erl_parse:parse(K7).
{ok,{function,1,mod,0,
              [{clause,1,[],[],
                       [{op,1,'+',{integer,1,1},{integer,1,1}}]}]}}
