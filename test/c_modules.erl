-module(c_modules).

-compile(export_all).

fib_server() ->
    {c_module,
     [],
     {c_literal, [], fib_server},
     [{c_var, [], {calc, 1}},
      {c_var, [], {handle_call, 3}},
      {c_var, [], {handle_cast, 2}},
      {c_var, [], {init, 1}},
      {c_var, [], {module_info, 0}},
      {c_var, [], {module_info, 1}},
      {c_var, [], {start_link, 0}}],
     [{{c_literal, [{1, 1}], file}, {c_literal, [{1, 1}], [{"src/fib_server.erl", 1}]}},
      {{c_literal, [{3, 2}], behaviour}, {c_literal, [{3, 2}], [gen_server]}}],
     [{{c_var, [], {start_link, 0}},
       {c_fun,
        [{function, {start_link, 0}}, {8, 1}, {file, "src/fib_server.erl"}],
        [],
        {c_case,
         [{function, {start_link, 0}}, {8, 1}, {file, "src/fib_server.erl"}],
         {c_values, [{function, {start_link, 0}}, {8, 1}, {file, "src/fib_server.erl"}], []},
         [{c_clause,
           [{8, 1}, {file, "src/fib_server.erl"}],
           [],
           {c_literal, [], true},
           {c_call,
            [{8, 17}, {file, "src/fib_server.erl"}],
            {c_literal, [{8, 17}, {file, "src/fib_server.erl"}], gen_server},
            {c_literal, [{8, 28}, {file, "src/fib_server.erl"}], start_link},
            [{c_tuple,
              [{8, 39}, {file, "src/fib_server.erl"}],
              [{c_literal, [{8, 40}, {file, "src/fib_server.erl"}], local},
               {c_literal, [{8, 48}, {file, "src/fib_server.erl"}], fib_server}]},
             {c_literal, [{8, 58}, {file, "src/fib_server.erl"}], fib_server},
             {c_literal, [{8, 66}, {file, "src/fib_server.erl"}], []},
             {c_literal, [{8, 70}, {file, "src/fib_server.erl"}], []}]}}]}}},
      {{c_var, [], {calc, 1}},
       {c_fun,
        [{function, {calc, 1}}, {10, 1}, {file, "src/fib_server.erl"}],
        [{c_var, [{10, 1}, {file, "src/fib_server.erl"}], 0}],
        {c_case,
         [{function, {calc, 1}}, {10, 1}, {file, "src/fib_server.erl"}],
         {c_var, [{function, {calc, 1}}, {10, 1}, {file, "src/fib_server.erl"}], 0},
         [{c_clause,
           [{10, 1}, {file, "src/fib_server.erl"}],
           [{c_var, [{10, 6}, {file, "src/fib_server.erl"}], 'N'}],
           {c_literal, [], true},
           {c_call,
            [{10, 12}, {file, "src/fib_server.erl"}],
            {c_literal, [{10, 12}, {file, "src/fib_server.erl"}], gen_server},
            {c_literal, [{10, 23}, {file, "src/fib_server.erl"}], call},
            [{c_literal, [{10, 29}, {file, "src/fib_server.erl"}], fib_server},
             {c_tuple,
              [{10, 37}, {file, "src/fib_server.erl"}],
              [{c_literal, [{10, 38}, {file, "src/fib_server.erl"}], fib},
               {c_var, [{10, 43}, {file, "src/fib_server.erl"}], 'N'}]},
             {c_literal, [{10, 47}, {file, "src/fib_server.erl"}], 20000}]}}]}}},
      {{c_var, [], {init, 1}},
       {c_fun,
        [{function, {init, 1}}, {12, 1}, {file, "src/fib_server.erl"}],
        [{c_var, [{12, 1}, {file, "src/fib_server.erl"}], 0}],
        {c_case,
         [{function, {init, 1}}, {12, 1}, {file, "src/fib_server.erl"}],
         {c_var, [{function, {init, 1}}, {12, 1}, {file, "src/fib_server.erl"}], 0},
         [{c_clause,
           [{12, 1}, {file, "src/fib_server.erl"}],
           [{c_literal, [{12, 6}, {file, "src/fib_server.erl"}], []}],
           {c_literal, [], true},
           {c_seq,
            [],
            {c_call,
             [{13, 5}, {file, "src/fib_server.erl"}],
             {c_literal, [{13, 5}, {file, "src/fib_server.erl"}], io},
             {c_literal, [{13, 8}, {file, "src/fib_server.erl"}], format},
             [{c_literal, [{13, 15}, {file, "src/fib_server.erl"}], "~p starting~n"},
              {c_cons,
               [{13, 32}, {file, "src/fib_server.erl"}],
               {c_literal, [{13, 34}, {file, "src/fib_server.erl"}], fib_server},
               {c_literal, [{13, 40}, {file, "src/fib_server.erl"}], []}}]},
            {c_tuple,
             [{14, 5}, {file, "src/fib_server.erl"}],
             [{c_literal, [{14, 6}, {file, "src/fib_server.erl"}], ok},
              {c_literal, [{14, 10}, {file, "src/fib_server.erl"}], 0}]}}}]}}},
      {{c_var, [], {handle_call, 3}},
       {c_fun,
        [{function, {handle_call, 3}}, {16, 1}, {file, "src/fib_server.erl"}],
        [{c_var, [{16, 1}, {file, "src/fib_server.erl"}], 0},
         {c_var, [{16, 1}, {file, "src/fib_server.erl"}], 1},
         {c_var, [{16, 1}, {file, "src/fib_server.erl"}], 2}],
        {c_case,
         [{function, {handle_call, 3}}, {16, 1}, {file, "src/fib_server.erl"}],
         {c_values,
          [{function, {handle_call, 3}}, {16, 1}, {file, "src/fib_server.erl"}],
          [{c_var, [{16, 1}, {file, "src/fib_server.erl"}], 0},
           {c_var, [{16, 1}, {file, "src/fib_server.erl"}], 1},
           {c_var, [{16, 1}, {file, "src/fib_server.erl"}], 2}]},
         [{c_clause,
           [{16, 1}, {file, "src/fib_server.erl"}],
           [{c_tuple,
             [{16, 13}, {file, "src/fib_server.erl"}],
             [{c_literal, [{16, 14}, {file, "src/fib_server.erl"}], fib},
              {c_var, [{16, 19}, {file, "src/fib_server.erl"}], 'K'}]},
            {c_var, [{16, 23}, {file, "src/fib_server.erl"}], '_From'},
            {c_var, [{16, 30}, {file, "src/fib_server.erl"}], 'N'}],
           {c_literal, [], true},
           {c_let,
            [],
            [{c_var, [{16, 44}, {file, "src/fib_server.erl"}], 4}],
            {c_apply,
             [{16, 44}, {file, "src/fib_server.erl"}],
             {c_var, [{16, 44}, {file, "src/fib_server.erl"}], {fib, 1}},
             [{c_var, [{16, 48}, {file, "src/fib_server.erl"}], 'K'}]},
            {c_let,
             [],
             [{c_var, [{16, 54}, {file, "src/fib_server.erl"}], 3}],
             {c_call,
              [{16, 54}, {file, "src/fib_server.erl"}],
              {c_literal, [{16, 54}, {file, "src/fib_server.erl"}], erlang},
              {c_literal, [{16, 54}, {file, "src/fib_server.erl"}], '+'},
              [{c_var, [{16, 52}, {file, "src/fib_server.erl"}], 'N'},
               {c_literal, [{16, 56}, {file, "src/fib_server.erl"}], 1}]},
             {c_tuple,
              [{16, 36}, {file, "src/fib_server.erl"}],
              [{c_literal, [{16, 37}, {file, "src/fib_server.erl"}], reply},
               {c_var, [{16, 44}, {file, "src/fib_server.erl"}], 4},
               {c_var, [{16, 54}, {file, "src/fib_server.erl"}], 3}]}}}}]}}},
      {{c_var, [], {handle_cast, 2}},
       {c_fun,
        [{function, {handle_cast, 2}}, {18, 1}, {file, "src/fib_server.erl"}],
        [{c_var, [{18, 1}, {file, "src/fib_server.erl"}], 0},
         {c_var, [{18, 1}, {file, "src/fib_server.erl"}], 1}],
        {c_case,
         [{function, {handle_cast, 2}}, {18, 1}, {file, "src/fib_server.erl"}],
         {c_values,
          [{function, {handle_cast, 2}}, {18, 1}, {file, "src/fib_server.erl"}],
          [{c_var, [{18, 1}, {file, "src/fib_server.erl"}], 0},
           {c_var, [{18, 1}, {file, "src/fib_server.erl"}], 1}]},
         [{c_clause,
           [{18, 1}, {file, "src/fib_server.erl"}],
           [{c_var, [{18, 13}, {file, "src/fib_server.erl"}], '_Msg'},
            {c_var, [{18, 19}, {file, "src/fib_server.erl"}], 'N'}],
           {c_literal, [], true},
           {c_tuple,
            [{18, 25}, {file, "src/fib_server.erl"}],
            [{c_literal, [{18, 26}, {file, "src/fib_server.erl"}], noreply},
             {c_var, [{18, 35}, {file, "src/fib_server.erl"}], 'N'}]}}]}}},
      {{c_var, [], {fib, 1}},
       {c_fun,
        [{function, {fib, 1}}, {20, 1}, {file, "src/fib_server.erl"}],
        [{c_var, [{20, 1}, {file, "src/fib_server.erl"}], 0}],
        {c_case,
         [{function, {fib, 1}}, {20, 1}, {file, "src/fib_server.erl"}],
         {c_var, [{function, {fib, 1}}, {20, 1}, {file, "src/fib_server.erl"}], 0},
         [{c_clause,
           [{20, 1}, {file, "src/fib_server.erl"}],
           [{c_var, [{20, 5}, {file, "src/fib_server.erl"}], 'N'}],
           {c_call,
            [{20, 15}, {file, "src/fib_server.erl"}],
            {c_literal, [{20, 15}, {file, "src/fib_server.erl"}], erlang},
            {c_literal, [{20, 15}, {file, "src/fib_server.erl"}], '<'},
            [{c_var, [{20, 13}, {file, "src/fib_server.erl"}], 'N'},
             {c_literal, [{20, 17}, {file, "src/fib_server.erl"}], 0}]},
           {c_call,
            [{20, 22}, {file, "src/fib_server.erl"}],
            {c_literal, [{20, 22}, {file, "src/fib_server.erl"}], erlang},
            {c_literal, [{20, 22}, {file, "src/fib_server.erl"}], throw},
            [{c_literal, [{20, 28}, {file, "src/fib_server.erl"}], invalid_number}]}},
          {c_clause,
           [{21, 1}, {file, "src/fib_server.erl"}],
           [{c_literal, [{21, 5}, {file, "src/fib_server.erl"}], 0}],
           {c_literal, [], true},
           {c_literal, [{21, 11}, {file, "src/fib_server.erl"}], 0}},
          {c_clause,
           [{22, 1}, {file, "src/fib_server.erl"}],
           [{c_literal, [{22, 5}, {file, "src/fib_server.erl"}], 1}],
           {c_literal, [], true},
           {c_literal, [{22, 11}, {file, "src/fib_server.erl"}], 1}},
          {c_clause,
           [{23, 1}, {file, "src/fib_server.erl"}],
           [{c_var, [{23, 5}, {file, "src/fib_server.erl"}], 'N'}],
           {c_literal, [], true},
           {c_let,
            [],
            [{c_var, [{23, 17}, {file, "src/fib_server.erl"}], 3}],
            {c_call,
             [{23, 17}, {file, "src/fib_server.erl"}],
             {c_literal, [{23, 17}, {file, "src/fib_server.erl"}], erlang},
             {c_literal, [{23, 17}, {file, "src/fib_server.erl"}], '-'},
             [{c_var, [{23, 15}, {file, "src/fib_server.erl"}], 'N'},
              {c_literal, [{23, 19}, {file, "src/fib_server.erl"}], 1}]},
            {c_let,
             [],
             [{c_var, [{23, 11}, {file, "src/fib_server.erl"}], 4}],
             {c_apply,
              [{23, 11}, {file, "src/fib_server.erl"}],
              {c_var, [{23, 11}, {file, "src/fib_server.erl"}], {fib, 1}},
              [{c_var, [{23, 17}, {file, "src/fib_server.erl"}], 3}]},
             {c_let,
              [],
              [{c_var, [{23, 30}, {file, "src/fib_server.erl"}], 1}],
              {c_call,
               [{23, 30}, {file, "src/fib_server.erl"}],
               {c_literal, [{23, 30}, {file, "src/fib_server.erl"}], erlang},
               {c_literal, [{23, 30}, {file, "src/fib_server.erl"}], '-'},
               [{c_var, [{23, 28}, {file, "src/fib_server.erl"}], 'N'},
                {c_literal, [{23, 32}, {file, "src/fib_server.erl"}], 2}]},
              {c_let,
               [],
               [{c_var, [{23, 24}, {file, "src/fib_server.erl"}], 2}],
               {c_apply,
                [{23, 24}, {file, "src/fib_server.erl"}],
                {c_var, [{23, 24}, {file, "src/fib_server.erl"}], {fib, 1}},
                [{c_var, [{23, 30}, {file, "src/fib_server.erl"}], 1}]},
               {c_call,
                [{23, 22}, {file, "src/fib_server.erl"}],
                {c_literal, [{23, 22}, {file, "src/fib_server.erl"}], erlang},
                {c_literal, [{23, 22}, {file, "src/fib_server.erl"}], '+'},
                [{c_var, [{23, 11}, {file, "src/fib_server.erl"}], 4},
                 {c_var, [{23, 24}, {file, "src/fib_server.erl"}], 2}]}}}}}}]}}},
      {{c_var, [], {module_info, 0}},
       {c_fun,
        [{function, {module_info, 0}}, 0, {file, "src/fib_server.erl"}],
        [],
        {c_case,
         [{function, {module_info, 0}}, 0, {file, "src/fib_server.erl"}],
         {c_values, [{function, {module_info, 0}}, 0, {file, "src/fib_server.erl"}], []},
         [{c_clause,
           [0, {file, "src/fib_server.erl"}],
           [],
           {c_literal, [], true},
           {c_call,
            [0, {file, "src/fib_server.erl"}],
            {c_literal, [0, {file, "src/fib_server.erl"}], erlang},
            {c_literal, [0, {file, "src/fib_server.erl"}], get_module_info},
            [{c_literal, [0, {file, "src/fib_server.erl"}], fib_server}]}}]}}},
      {{c_var, [], {module_info, 1}},
       {c_fun,
        [{function, {module_info, 1}}, 0, {file, "src/fib_server.erl"}],
        [{c_var, [0, {file, "src/fib_server.erl"}], 0}],
        {c_case,
         [{function, {module_info, 1}}, 0, {file, "src/fib_server.erl"}],
         {c_var, [{function, {module_info, 1}}, 0, {file, "src/fib_server.erl"}], 0},
         [{c_clause,
           [0, {file, "src/fib_server.erl"}],
           [{c_var, [0, {file, "src/fib_server.erl"}], 'X'}],
           {c_literal, [], true},
           {c_call,
            [0, {file, "src/fib_server.erl"}],
            {c_literal, [0, {file, "src/fib_server.erl"}], erlang},
            {c_literal, [0, {file, "src/fib_server.erl"}], get_module_info},
            [{c_literal, [0, {file, "src/fib_server.erl"}], fib_server},
             {c_var, [0, {file, "src/fib_server.erl"}], 'X'}]}}]}}}]}.

foo() ->
    {c_module,
     [],
     {c_literal, [], foo},
     [{c_var, [], {foo, 0}}, {c_var, [], {module_info, 0}}, {c_var, [], {module_info, 1}}],
     [{{c_literal, [{1, 1}], file}, {c_literal, [{1, 1}], [{"src/foo.erl", 1}]}}],
     [{{c_var, [], {foo, 0}},
       {c_fun,
        [{function, {foo, 0}}, {5, 1}, {file, "src/foo.erl"}],
        [],
        {c_case,
         [{function, {foo, 0}}, {5, 1}, {file, "src/foo.erl"}],
         {c_values, [{function, {foo, 0}}, {5, 1}, {file, "src/foo.erl"}], []},
         [{c_clause,
           [{5, 1}, {file, "src/foo.erl"}],
           [],
           {c_literal, [], true},
           {c_literal, [{5, 10}, {file, "src/foo.erl"}], bar}}]}}},
      {{c_var, [], {module_info, 0}},
       {c_fun,
        [{function, {module_info, 0}}, 0, {file, "src/foo.erl"}],
        [],
        {c_case,
         [{function, {module_info, 0}}, 0, {file, "src/foo.erl"}],
         {c_values, [{function, {module_info, 0}}, 0, {file, "src/foo.erl"}], []},
         [{c_clause,
           [0, {file, "src/foo.erl"}],
           [],
           {c_literal, [], true},
           {c_call,
            [0, {file, "src/foo.erl"}],
            {c_literal, [0, {file, "src/foo.erl"}], erlang},
            {c_literal, [0, {file, "src/foo.erl"}], get_module_info},
            [{c_literal, [0, {file, "src/foo.erl"}], foo}]}}]}}},
      {{c_var, [], {module_info, 1}},
       {c_fun,
        [{function, {module_info, 1}}, 0, {file, "src/foo.erl"}],
        [{c_var, [0, {file, "src/foo.erl"}], 0}],
        {c_case,
         [{function, {module_info, 1}}, 0, {file, "src/foo.erl"}],
         {c_var, [{function, {module_info, 1}}, 0, {file, "src/foo.erl"}], 0},
         [{c_clause,
           [0, {file, "src/foo.erl"}],
           [{c_var, [0, {file, "src/foo.erl"}], 'X'}],
           {c_literal, [], true},
           {c_call,
            [0, {file, "src/foo.erl"}],
            {c_literal, [0, {file, "src/foo.erl"}], erlang},
            {c_literal, [0, {file, "src/foo.erl"}], get_module_info},
            [{c_literal, [0, {file, "src/foo.erl"}], foo},
             {c_var, [0, {file, "src/foo.erl"}], 'X'}]}}]}}}]}.

first_server() ->
    {c_module,
     [],
     {c_literal, [], first_server},
     [{c_var, [], {handle_call, 3}},
      {c_var, [], {handle_cast, 2}},
      {c_var, [], {init, 1}},
      {c_var, [], {module_info, 0}},
      {c_var, [], {module_info, 1}}],
     [{{c_literal, [{1, 1}], file}, {c_literal, [{1, 1}], [{"src/two/first_server.erl", 1}]}},
      {{c_literal, [{3, 2}], behaviour}, {c_literal, [{3, 2}], [gen_server]}}],
     [{{c_var, [], {init, 1}},
       {c_fun,
        [{function, {init, 1}}, {7, 1}, {file, "src/two/first_server.erl"}],
        [{c_var, [{7, 1}, {file, "src/two/first_server.erl"}], 0}],
        {c_case,
         [{function, {init, 1}}, {7, 1}, {file, "src/two/first_server.erl"}],
         {c_var, [{function, {init, 1}}, {7, 1}, {file, "src/two/first_server.erl"}], 0},
         [{c_clause,
           [{7, 1}, {file, "src/two/first_server.erl"}],
           [{c_literal, [{7, 6}, {file, "src/two/first_server.erl"}], []}],
           {c_literal, [], true},
           {c_tuple,
            [{7, 13}, {file, "src/two/first_server.erl"}],
            [{c_literal, [{7, 14}, {file, "src/two/first_server.erl"}], ok},
             {c_literal, [{7, 18}, {file, "src/two/first_server.erl"}], #{}}]}}]}}},
      {{c_var, [], {handle_call, 3}},
       {c_fun,
        [{function, {handle_call, 3}}, {9, 1}, {file, "src/two/first_server.erl"}],
        [{c_var, [{9, 1}, {file, "src/two/first_server.erl"}], 0},
         {c_var, [{9, 1}, {file, "src/two/first_server.erl"}], 1},
         {c_var, [{9, 1}, {file, "src/two/first_server.erl"}], 2}],
        {c_case,
         [{function, {handle_call, 3}}, {9, 1}, {file, "src/two/first_server.erl"}],
         {c_values,
          [{function, {handle_call, 3}}, {9, 1}, {file, "src/two/first_server.erl"}],
          [{c_var, [{9, 1}, {file, "src/two/first_server.erl"}], 0},
           {c_var, [{9, 1}, {file, "src/two/first_server.erl"}], 1},
           {c_var, [{9, 1}, {file, "src/two/first_server.erl"}], 2}]},
         [{c_clause,
           [{9, 1}, {file, "src/two/first_server.erl"}],
           [{c_var, [{9, 13}, {file, "src/two/first_server.erl"}], 'Request'},
            {c_var, [{9, 22}, {file, "src/two/first_server.erl"}], '_From'},
            {c_var, [{9, 29}, {file, "src/two/first_server.erl"}], 'State'}],
           {c_literal, [], true},
           {c_seq,
            [],
            {c_call,
             [{10, 5}, {file, "src/two/first_server.erl"}],
             {c_literal, [{10, 5}, {file, "src/two/first_server.erl"}], gen_server},
             {c_literal, [{10, 16}, {file, "src/two/first_server.erl"}], call},
             [{c_literal, [{10, 21}, {file, "src/two/first_server.erl"}], second_server},
              {c_var, [{10, 36}, {file, "src/two/first_server.erl"}], 'Request'}]},
            {c_tuple,
             [{11, 5}, {file, "src/two/first_server.erl"}],
             [{c_literal, [{11, 6}, {file, "src/two/first_server.erl"}], reply},
              {c_literal, [{11, 13}, {file, "src/two/first_server.erl"}], first},
              {c_var, [{11, 20}, {file, "src/two/first_server.erl"}], 'State'}]}}}]}}},
      {{c_var, [], {handle_cast, 2}},
       {c_fun,
        [{function, {handle_cast, 2}}, {13, 1}, {file, "src/two/first_server.erl"}],
        [{c_var, [{13, 1}, {file, "src/two/first_server.erl"}], 0},
         {c_var, [{13, 1}, {file, "src/two/first_server.erl"}], 1}],
        {c_case,
         [{function, {handle_cast, 2}}, {13, 1}, {file, "src/two/first_server.erl"}],
         {c_values,
          [{function, {handle_cast, 2}}, {13, 1}, {file, "src/two/first_server.erl"}],
          [{c_var, [{13, 1}, {file, "src/two/first_server.erl"}], 0},
           {c_var, [{13, 1}, {file, "src/two/first_server.erl"}], 1}]},
         [{c_clause,
           [{13, 1}, {file, "src/two/first_server.erl"}],
           [{c_var, [{13, 13}, {file, "src/two/first_server.erl"}], '_Request'},
            {c_var, [{13, 23}, {file, "src/two/first_server.erl"}], 'State'}],
           {c_literal, [], true},
           {c_tuple,
            [{13, 33}, {file, "src/two/first_server.erl"}],
            [{c_literal, [{13, 34}, {file, "src/two/first_server.erl"}], noreply},
             {c_var, [{13, 43}, {file, "src/two/first_server.erl"}], 'State'}]}}]}}},
      {{c_var, [], {module_info, 0}},
       {c_fun,
        [{function, {module_info, 0}}, 0, {file, "src/two/first_server.erl"}],
        [],
        {c_case,
         [{function, {module_info, 0}}, 0, {file, "src/two/first_server.erl"}],
         {c_values, [{function, {module_info, 0}}, 0, {file, "src/two/first_server.erl"}], []},
         [{c_clause,
           [0, {file, "src/two/first_server.erl"}],
           [],
           {c_literal, [], true},
           {c_call,
            [0, {file, "src/two/first_server.erl"}],
            {c_literal, [0, {file, "src/two/first_server.erl"}], erlang},
            {c_literal, [0, {file, "src/two/first_server.erl"}], get_module_info},
            [{c_literal, [0, {file, "src/two/first_server.erl"}], first_server}]}}]}}},
      {{c_var, [], {module_info, 1}},
       {c_fun,
        [{function, {module_info, 1}}, 0, {file, "src/two/first_server.erl"}],
        [{c_var, [0, {file, "src/two/first_server.erl"}], 0}],
        {c_case,
         [{function, {module_info, 1}}, 0, {file, "src/two/first_server.erl"}],
         {c_var, [{function, {module_info, 1}}, 0, {file, "src/two/first_server.erl"}], 0},
         [{c_clause,
           [0, {file, "src/two/first_server.erl"}],
           [{c_var, [0, {file, "src/two/first_server.erl"}], 'X'}],
           {c_literal, [], true},
           {c_call,
            [0, {file, "src/two/first_server.erl"}],
            {c_literal, [0, {file, "src/two/first_server.erl"}], erlang},
            {c_literal, [0, {file, "src/two/first_server.erl"}], get_module_info},
            [{c_literal, [0, {file, "src/two/first_server.erl"}], first_server},
             {c_var, [0, {file, "src/two/first_server.erl"}], 'X'}]}}]}}}]}.

second_server() ->
    {c_module,
     [],
     {c_literal, [], second_server},
     [{c_var, [], {handle_call, 3}},
      {c_var, [], {handle_cast, 2}},
      {c_var, [], {init, 1}},
      {c_var, [], {module_info, 0}},
      {c_var, [], {module_info, 1}}],
     [{{c_literal, [{1, 1}], file}, {c_literal, [{1, 1}], [{"src/two/second_server.erl", 1}]}},
      {{c_literal, [{3, 2}], behaviour}, {c_literal, [{3, 2}], [gen_server]}}],
     [{{c_var, [], {init, 1}},
       {c_fun,
        [{function, {init, 1}}, {7, 1}, {file, "src/two/second_server.erl"}],
        [{c_var, [{7, 1}, {file, "src/two/second_server.erl"}], 0}],
        {c_case,
         [{function, {init, 1}}, {7, 1}, {file, "src/two/second_server.erl"}],
         {c_var, [{function, {init, 1}}, {7, 1}, {file, "src/two/second_server.erl"}], 0},
         [{c_clause,
           [{7, 1}, {file, "src/two/second_server.erl"}],
           [{c_literal, [{7, 6}, {file, "src/two/second_server.erl"}], []}],
           {c_literal, [], true},
           {c_tuple,
            [{7, 13}, {file, "src/two/second_server.erl"}],
            [{c_literal, [{7, 14}, {file, "src/two/second_server.erl"}], ok},
             {c_literal, [{7, 18}, {file, "src/two/second_server.erl"}], #{}}]}}]}}},
      {{c_var, [], {handle_call, 3}},
       {c_fun,
        [{function, {handle_call, 3}}, {9, 1}, {file, "src/two/second_server.erl"}],
        [{c_var, [{9, 1}, {file, "src/two/second_server.erl"}], 0},
         {c_var, [{9, 1}, {file, "src/two/second_server.erl"}], 1},
         {c_var, [{9, 1}, {file, "src/two/second_server.erl"}], 2}],
        {c_case,
         [{function, {handle_call, 3}}, {9, 1}, {file, "src/two/second_server.erl"}],
         {c_values,
          [{function, {handle_call, 3}}, {9, 1}, {file, "src/two/second_server.erl"}],
          [{c_var, [{9, 1}, {file, "src/two/second_server.erl"}], 0},
           {c_var, [{9, 1}, {file, "src/two/second_server.erl"}], 1},
           {c_var, [{9, 1}, {file, "src/two/second_server.erl"}], 2}]},
         [{c_clause,
           [{9, 1}, {file, "src/two/second_server.erl"}],
           [{c_var, [{9, 13}, {file, "src/two/second_server.erl"}], 'Request'},
            {c_var, [{9, 22}, {file, "src/two/second_server.erl"}], '_From'},
            {c_var, [{9, 29}, {file, "src/two/second_server.erl"}], 'State'}],
           {c_literal, [], true},
           {c_seq,
            [],
            {c_call,
             [{10, 5}, {file, "src/two/second_server.erl"}],
             {c_literal, [{10, 5}, {file, "src/two/second_server.erl"}], gen_server},
             {c_literal, [{10, 16}, {file, "src/two/second_server.erl"}], call},
             [{c_literal, [{10, 21}, {file, "src/two/second_server.erl"}], third_server},
              {c_var, [{10, 35}, {file, "src/two/second_server.erl"}], 'Request'}]},
            {c_tuple,
             [{11, 5}, {file, "src/two/second_server.erl"}],
             [{c_literal, [{11, 6}, {file, "src/two/second_server.erl"}], reply},
              {c_literal, [{11, 13}, {file, "src/two/second_server.erl"}], first},
              {c_var, [{11, 20}, {file, "src/two/second_server.erl"}], 'State'}]}}}]}}},
      {{c_var, [], {handle_cast, 2}},
       {c_fun,
        [{function, {handle_cast, 2}}, {13, 1}, {file, "src/two/second_server.erl"}],
        [{c_var, [{13, 1}, {file, "src/two/second_server.erl"}], 0},
         {c_var, [{13, 1}, {file, "src/two/second_server.erl"}], 1}],
        {c_case,
         [{function, {handle_cast, 2}}, {13, 1}, {file, "src/two/second_server.erl"}],
         {c_values,
          [{function, {handle_cast, 2}}, {13, 1}, {file, "src/two/second_server.erl"}],
          [{c_var, [{13, 1}, {file, "src/two/second_server.erl"}], 0},
           {c_var, [{13, 1}, {file, "src/two/second_server.erl"}], 1}]},
         [{c_clause,
           [{13, 1}, {file, "src/two/second_server.erl"}],
           [{c_var, [{13, 13}, {file, "src/two/second_server.erl"}], '_Request'},
            {c_var, [{13, 23}, {file, "src/two/second_server.erl"}], 'State'}],
           {c_literal, [], true},
           {c_tuple,
            [{13, 33}, {file, "src/two/second_server.erl"}],
            [{c_literal, [{13, 34}, {file, "src/two/second_server.erl"}], noreply},
             {c_var, [{13, 43}, {file, "src/two/second_server.erl"}], 'State'}]}}]}}},
      {{c_var, [], {module_info, 0}},
       {c_fun,
        [{function, {module_info, 0}}, 0, {file, "src/two/second_server.erl"}],
        [],
        {c_case,
         [{function, {module_info, 0}}, 0, {file, "src/two/second_server.erl"}],
         {c_values, [{function, {module_info, 0}}, 0, {file, "src/two/second_server.erl"}], []},
         [{c_clause,
           [0, {file, "src/two/second_server.erl"}],
           [],
           {c_literal, [], true},
           {c_call,
            [0, {file, "src/two/second_server.erl"}],
            {c_literal, [0, {file, "src/two/second_server.erl"}], erlang},
            {c_literal, [0, {file, "src/two/second_server.erl"}], get_module_info},
            [{c_literal, [0, {file, "src/two/second_server.erl"}], second_server}]}}]}}},
      {{c_var, [], {module_info, 1}},
       {c_fun,
        [{function, {module_info, 1}}, 0, {file, "src/two/second_server.erl"}],
        [{c_var, [0, {file, "src/two/second_server.erl"}], 0}],
        {c_case,
         [{function, {module_info, 1}}, 0, {file, "src/two/second_server.erl"}],
         {c_var, [{function, {module_info, 1}}, 0, {file, "src/two/second_server.erl"}], 0},
         [{c_clause,
           [0, {file, "src/two/second_server.erl"}],
           [{c_var, [0, {file, "src/two/second_server.erl"}], 'X'}],
           {c_literal, [], true},
           {c_call,
            [0, {file, "src/two/second_server.erl"}],
            {c_literal, [0, {file, "src/two/second_server.erl"}], erlang},
            {c_literal, [0, {file, "src/two/second_server.erl"}], get_module_info},
            [{c_literal, [0, {file, "src/two/second_server.erl"}], second_server},
             {c_var, [0, {file, "src/two/second_server.erl"}], 'X'}]}}]}}}]}.

third_server() ->
    {c_module,
     [],
     {c_literal, [], third_server},
     [{c_var, [], {handle_call, 3}},
      {c_var, [], {handle_cast, 2}},
      {c_var, [], {init, 1}},
      {c_var, [], {module_info, 0}},
      {c_var, [], {module_info, 1}}],
     [{{c_literal, [{1, 1}], file}, {c_literal, [{1, 1}], [{"src/two/third_server.erl", 1}]}},
      {{c_literal, [{3, 2}], behaviour}, {c_literal, [{3, 2}], [gen_server]}}],
     [{{c_var, [], {init, 1}},
       {c_fun,
        [{function, {init, 1}}, {7, 1}, {file, "src/two/third_server.erl"}],
        [{c_var, [{7, 1}, {file, "src/two/third_server.erl"}], 0}],
        {c_case,
         [{function, {init, 1}}, {7, 1}, {file, "src/two/third_server.erl"}],
         {c_var, [{function, {init, 1}}, {7, 1}, {file, "src/two/third_server.erl"}], 0},
         [{c_clause,
           [{7, 1}, {file, "src/two/third_server.erl"}],
           [{c_literal, [{7, 6}, {file, "src/two/third_server.erl"}], []}],
           {c_literal, [], true},
           {c_tuple,
            [{7, 13}, {file, "src/two/third_server.erl"}],
            [{c_literal, [{7, 14}, {file, "src/two/third_server.erl"}], ok},
             {c_literal, [{7, 18}, {file, "src/two/third_server.erl"}], #{}}]}}]}}},
      {{c_var, [], {handle_call, 3}},
       {c_fun,
        [{function, {handle_call, 3}}, {9, 1}, {file, "src/two/third_server.erl"}],
        [{c_var, [{9, 1}, {file, "src/two/third_server.erl"}], 0},
         {c_var, [{9, 1}, {file, "src/two/third_server.erl"}], 1},
         {c_var, [{9, 1}, {file, "src/two/third_server.erl"}], 2}],
        {c_case,
         [{function, {handle_call, 3}}, {9, 1}, {file, "src/two/third_server.erl"}],
         {c_values,
          [{function, {handle_call, 3}}, {9, 1}, {file, "src/two/third_server.erl"}],
          [{c_var, [{9, 1}, {file, "src/two/third_server.erl"}], 0},
           {c_var, [{9, 1}, {file, "src/two/third_server.erl"}], 1},
           {c_var, [{9, 1}, {file, "src/two/third_server.erl"}], 2}]},
         [{c_clause,
           [{9, 1}, {file, "src/two/third_server.erl"}],
           [{c_var, [{9, 13}, {file, "src/two/third_server.erl"}], '_Request'},
            {c_var, [{9, 23}, {file, "src/two/third_server.erl"}], '_From'},
            {c_var, [{9, 30}, {file, "src/two/third_server.erl"}], 'State'}],
           {c_literal, [], true},
           {c_tuple,
            [{9, 40}, {file, "src/two/third_server.erl"}],
            [{c_literal, [{9, 41}, {file, "src/two/third_server.erl"}], reply},
             {c_literal, [{9, 48}, {file, "src/two/third_server.erl"}], first},
             {c_var, [{9, 55}, {file, "src/two/third_server.erl"}], 'State'}]}}]}}},
      {{c_var, [], {handle_cast, 2}},
       {c_fun,
        [{function, {handle_cast, 2}}, {11, 1}, {file, "src/two/third_server.erl"}],
        [{c_var, [{11, 1}, {file, "src/two/third_server.erl"}], 0},
         {c_var, [{11, 1}, {file, "src/two/third_server.erl"}], 1}],
        {c_case,
         [{function, {handle_cast, 2}}, {11, 1}, {file, "src/two/third_server.erl"}],
         {c_values,
          [{function, {handle_cast, 2}}, {11, 1}, {file, "src/two/third_server.erl"}],
          [{c_var, [{11, 1}, {file, "src/two/third_server.erl"}], 0},
           {c_var, [{11, 1}, {file, "src/two/third_server.erl"}], 1}]},
         [{c_clause,
           [{11, 1}, {file, "src/two/third_server.erl"}],
           [{c_var, [{11, 13}, {file, "src/two/third_server.erl"}], '_Request'},
            {c_var, [{11, 23}, {file, "src/two/third_server.erl"}], 'State'}],
           {c_literal, [], true},
           {c_tuple,
            [{11, 33}, {file, "src/two/third_server.erl"}],
            [{c_literal, [{11, 34}, {file, "src/two/third_server.erl"}], noreply},
             {c_var, [{11, 43}, {file, "src/two/third_server.erl"}], 'State'}]}}]}}},
      {{c_var, [], {module_info, 0}},
       {c_fun,
        [{function, {module_info, 0}}, 0, {file, "src/two/third_server.erl"}],
        [],
        {c_case,
         [{function, {module_info, 0}}, 0, {file, "src/two/third_server.erl"}],
         {c_values, [{function, {module_info, 0}}, 0, {file, "src/two/third_server.erl"}], []},
         [{c_clause,
           [0, {file, "src/two/third_server.erl"}],
           [],
           {c_literal, [], true},
           {c_call,
            [0, {file, "src/two/third_server.erl"}],
            {c_literal, [0, {file, "src/two/third_server.erl"}], erlang},
            {c_literal, [0, {file, "src/two/third_server.erl"}], get_module_info},
            [{c_literal, [0, {file, "src/two/third_server.erl"}], third_server}]}}]}}},
      {{c_var, [], {module_info, 1}},
       {c_fun,
        [{function, {module_info, 1}}, 0, {file, "src/two/third_server.erl"}],
        [{c_var, [0, {file, "src/two/third_server.erl"}], 0}],
        {c_case,
         [{function, {module_info, 1}}, 0, {file, "src/two/third_server.erl"}],
         {c_var, [{function, {module_info, 1}}, 0, {file, "src/two/third_server.erl"}], 0},
         [{c_clause,
           [0, {file, "src/two/third_server.erl"}],
           [{c_var, [0, {file, "src/two/third_server.erl"}], 'X'}],
           {c_literal, [], true},
           {c_call,
            [0, {file, "src/two/third_server.erl"}],
            {c_literal, [0, {file, "src/two/third_server.erl"}], erlang},
            {c_literal, [0, {file, "src/two/third_server.erl"}], get_module_info},
            [{c_literal, [0, {file, "src/two/third_server.erl"}], third_server},
             {c_var, [0, {file, "src/two/third_server.erl"}], 'X'}]}}]}}}]}.
