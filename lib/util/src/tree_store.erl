-module(tree_store).

%%% external exports
-export([new/0]).
-export([insert/2, insert/3]).
-export([lookup/2]).
-export([load_xml/2, load_xml/3]).
-export([format_error/1]).

%%% internal exports

%%% include files
-include_lib("util/include/log.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("xmerl/include/xmerl_xsd.hrl").

%%% constants

%%% records
-record(container, {
          name          :: name(),
          children = [] :: tree()
        }).

-record(leaf, {
          name  :: label(),
          value :: value()
        }).

%%% types

-type path() :: [name()].
-type name() :: label() | keys().
-type label() :: atom().
-type keys() :: tuple().
-type value() :: any().
-type multi_value() :: [value() | {name(), value()}]. 
-type convert_callback() ::
        fun((tree_store:path(), file:filename()) ->
                   {'ok', any()} | {'error', inet:posix()}).
-type tree() :: [#container{}|#leaf{}].
-type error_reason() ::
        {{'failed_schema_validation', [validation_error()]} | (Error :: any()),
         {'file', file:filename()},
         {'line', Line :: integer()},
         {'col', Col :: integer()}} |
        {'convert_failure', Value :: string(), {posix, inet:posix()}} |
        {'convert_failure', Value :: string(), Reason :: any()}.
-type validation_error() ::
        {'type', ErrorCause :: any(), Value :: string()} |
        {_, _, Error :: any()} |
        (Error :: any()).

%%%
%%% exported: new
%%%

-spec new() -> tree().

new() ->
    [].

%%%
%%% exported: insert
%%%

-spec insert(tree(), path(), value() | '$container') -> tree().

insert(Children, Path) ->
    insert(Children, Path, '$container').

insert(Children, [Name], '$container') ->
    %% insert new container
    lists:keysort(2, [#container{name = Name}|Children]);
insert(Children, [Name], Value) ->
    case lists:keysearch(Name, 2, Children) of
        %% leaf already exists (add one more)
        {value, Leaf} when is_record(Leaf, leaf) ->
            lists:keysort(2, [#leaf{name = Name, value = Value}|Children]);
        {value, Container} when is_record(Container, container) ->
            throw(bad_path);
        %% insert new leaf
        false ->
            lists:keysort(2, [#leaf{name = Name, value = Value}|Children])
    end;
insert(Children, [Name|Rest] = Path, Value) ->
    case lists:keysearch(Name, 2, Children) of
        %% step down in existing branch
        {value, #container{children = GrandChildren} = Container} ->
            UpdatedGrandChildren = insert(GrandChildren, Rest, Value),
            lists:keyreplace(
              Name, 2, Children,
              Container#container{children = UpdatedGrandChildren});
        {value, Leaf} when is_record(Leaf, leaf) ->
            throw(bad_path);
        %% create new path
        false ->
            lists:keysort(2, create(Children, Path, Value))
    end.

create(Children, [Name], '$container') ->
    lists:keysort(2, [#container{name = Name}|Children]);
create(Children, [Name], Value) ->
    lists:keysort(2, [#leaf{name = Name, value = Value}|Children]);
create(Children, [Name|Rest], Value) ->
    lists:keysort(2, [#container{name = Name,
                                 children = create([], Rest, Value)}|Children]).

%%%
%%% exported: lookup
%%%

-spec lookup(tree(), path()) -> multi_value().

lookup(Children, [Name]) ->
    case lists:keysearch(Name, 2, Children) of
        {value, #container{children = GrandChildren}} ->
            lists:reverse(child_values(GrandChildren));
        {value, #leaf{value = Value}} ->
            [Value];
        false ->
            []
    end;
lookup(Children, [Name|Rest]) ->
    case lists:keysearch(Name, 2, Children) of
        %% step down in branch
        {value, #container{children = GrandChildren}} ->
            lookup(GrandChildren, Rest);
        {value, Leaf} when is_record(Leaf, leaf) ->
            throw(bad_path);
        false ->
            throw(bad_path)
    end.

child_values([]) ->
    [];
child_values([#leaf{name = Name, value = Value}|Rest]) ->
    [{Name, Value}|child_values(Rest)];
child_values([#container{name = Name}|Rest]) ->
    [{Name, '$container'}|child_values(Rest)].

%%%
%%% exported: load_xml
%%%

-spec load_xml(SchemaFilename :: file:filename(),
               ConfigFilename :: file:filename(),
               convert_callback()) ->
                      {'ok', tree()} | {'error', error_reason()}.

load_xml(SchemaFilename, ConfigFilename) ->
    ConvertCallback = fun(_LeafPath, Value) -> Value end,
    load_xml(SchemaFilename, ConfigFilename, ConvertCallback).

load_xml(SchemaFilename, ConfigFilename, ConvertCallback) ->
    case catch xmerl_scan:file(ConfigFilename,
                               [{quiet, true},
                                {namespace_conformant, true},
                                {validation, schema},
                                {schemaLocation, [{"foo", SchemaFilename}]}]) of
        {'EXIT', {fatal, Reason}} ->
            {error, Reason};
        {error, Error} ->
            {error, Error};
        {ValidElement, []} ->
            create_tree(ConvertCallback, ValidElement)
    end.

create_tree(ConvertCallback, ValidElement) ->
    try
        {ok, create_tree(new(), ConvertCallback, [ValidElement], [])}
    catch
        throw:{convert_failure, Value, Reason} ->
            {error, {convert_failure, Value, Reason}}
    end.

create_tree(Tree, _ConvertCallback, [], _Path) ->
    Tree;
create_tree(
  Tree, ConvertCallback,
  [#xmlElement{name = Name, content = [#xmlText{value = Value}]}|Rest],
  Path) ->
    LeafPath = Path++[Name],
    case ConvertCallback(LeafPath, Value) of
        {ok, ConvertedValue} ->
            UpdatedTree = insert(Tree, LeafPath, ConvertedValue),
            create_tree(UpdatedTree, ConvertCallback, Rest, Path);
        {error, Reason} ->
            throw({convert_failure, Value, Reason})
    end;
create_tree(Tree, ConvertCallback, [Element|Rest], Path)
  when is_record(Element, xmlText) ->
    create_tree(Tree, ConvertCallback, Rest, Path);
create_tree(Tree, ConvertCallback,
            [#xmlElement{name = Name, content = Content}|Rest],
            Path) ->
    UpdatedTree = create_tree(Tree, ConvertCallback, Content, Path++[Name]),
    create_tree(UpdatedTree, ConvertCallback, Rest, Path).

%%%
%%% exported: format_error
%%%

-spec format_error(error_reason()) -> iolist().

format_error(
  {{failed_schema_validation,
    [{_, _, {undefined,
             {internal_error,
              {{badmatch,
                #xsd_state{errors = [{_ , _, {value_not_valid, Value,
                                              _SimpleTypes}}|_]}},
               _StackTrace}}}}|_]},
   {file, Filename},
   {line, _Line},
   {col, _Col}}) -> % to cater for a bug in xmerl
    io_lib:format("~s: bad value: ~s", [Filename, Value]);
format_error({{failed_schema_validation, [{type, ErrorCause, Value}|_]},
              {file, _Filename},
              {line, _Line},
              {col, _Col}}) ->
    io_lib:format("~w: ~s", [ErrorCause, Value]);
format_error({{failed_schema_validation, [{_, _, Error}|_]},
              {file, _Filename},
              {line, _Line},
              {col, _Col}}) ->
    xmerl_xsd:format_error(Error);
format_error({{failed_schema_validation, [Error|_]},
              {file, _Filename},
              {line, _Line},
              {col, _Col}}) ->
    xmerl_xsd:format_error(Error);
format_error({Error,
              {file, _Filename},
              {line, Line},
              {col, Col}}) ->
    io_lib:format("~w: ~w: syntax error: ~p", [Line, Col, Error]);
format_error({convert_failure, Value, {posix, Reason}}) ->
    io_lib:format("~s: ~s", [Value, inet:format_error(Reason)]);
format_error({convert_failure, Value, Reason}) ->
    io_lib:format("~s: ~p", [Value, Reason]);
format_error(Reason) ->
    case inet:format_error(Reason) of
        "unknown POSIX error" ->
            ?error_log(Reason),
            "internal error";
        String ->
            String
    end.
