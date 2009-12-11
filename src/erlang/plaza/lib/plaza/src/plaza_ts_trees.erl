-module(plaza_ts_trees) .

% TSTreeUsers = [blogs, posts, comments],
% TSTreeBlogs = [users, {posts, [comments]}]
% TSTreePosts = [users, comments, blogs]
% TSTreeComments = [users, posts]

%% TSWrite = [{users,
%%             [{blogs,
%%               [{posts,
%%                 [comments]}
%%               ]}
%%              ]},
%%            others
%%            ] .

-author("Antonio Garrote Hernandez") .

-include_lib("eunit/include/eunit.hrl").

-export([make/1, get/2, path/2, nodes/1, generate_trees/2, metaresource_path_uri/2, resource_path_uri/2]) .


%% @doc
%% Creates a new tree from a TSTree list expression.
make(TSTreeList) ->
    build_level(TSTreeList,gb_trees:empty()) .

build_level([], Tree) ->
    Tree ;
build_level([{Leaf, Node} | Leafs], Tree) ->
    TreeP = gb_trees:enter(Leaf, build_level(Node, gb_trees:empty()), Tree),
    build_level(Leafs,TreeP) ;
build_level([Leaf | Leafs], Tree) ->
    build_level(Leafs, gb_trees:enter(Leaf, leaf, Tree)) .


%% @doc
%% Returns the subtree in the tree for a given query path.
get([Node | Nodes], Tree) ->
    case gb_trees:lookup(Node, Tree) of
        {value, leaf}  ->
            case Nodes of
                []     -> {ok, leaf} ;
                _Other -> error
            end ;
        {value, TreeP} ->
            case Nodes of
                []     -> {ok, TreeP} ;
                _Other -> get(Nodes, TreeP)
            end ;
        none           -> error
    end .


%% @doc
%% returns the full list of nodes in a TSTree.
nodes(TSTreeList) ->
    Nodes = gb_trees:keys(TSTreeList),
    collect_nodes(Nodes, TSTreeList, []) .

collect_nodes([],_TSTree,Acum) -> Acum ;
collect_nodes([Node | Nodes],TSTree,Acum) ->
    case gb_trees:lookup(Node, TSTree) of
        {value, leaf}    -> collect_nodes(Nodes, TSTree, [Node | Acum]) ;
        {value, TSTreeP} -> AcumP = collect_nodes(gb_trees:keys(TSTreeP), TSTreeP, []),
                            collect_nodes(Nodes, TSTree, [Node | Acum] ++ AcumP)
    end .


%% @doc
%% Returns the path of nodes from the root of a TSTree
%% to one node, or error if the leaf node is not in the
%% tree
path(Dst, TSTreeList) ->
    Nodes = gb_trees:keys(TSTreeList),
    detect(Dst,Nodes,TSTreeList, []) .

detect(_Dst, [], _Tree, _Acum) -> error ;
detect(Dst,[Node | Nodes],Tree, Acum) ->
    case Dst =:= Node of
        true  -> lists:reverse([Node | Acum]) ;
        false -> case plaza_ts_trees:get(lists:reverse([Node]), Tree) of
                     {ok, leaf}  -> detect(Dst, Nodes, Tree, Acum) ;
                     {ok, TreeP} -> case detect(Dst, gb_trees:keys(TreeP), TreeP, [Node | Acum]) of
                                        error  -> detect(Dst, Nodes, Tree, Acum) ;
                                        Result -> Result
                                    end ;
                     error       -> error
                 end
    end .


%% @doc
%% Builds the path to a metaresource given as Dst from aTSTreList
metaresource_path_uri(Dst, TSTreeList) ->
    Path = lists:map(fun(C) -> atom_to_list(C) end, path(Dst, TSTreeList)),
    {_Last, Result} = lists:foldl(fun(C,{Last, Acum}) ->
                                          case Last of
                                              first ->
                                                  {C, Acum ++ "/" ++ C} ;
                                              L     ->
                                                  {C, Acum ++ "/:" ++ L ++ "_id" ++ "/" ++ C}
                                          end
                                  end,
                                  {first, ""},
                                  Path),
    Result .


%% @doc
%% Builds the path to a resource that is of the type of the given Dst from aTSTreList
resource_path_uri(Dst, TSTreeList) ->
    Path = lists:map(fun(C) -> atom_to_list(C) end, path(Dst, TSTreeList)),
    lists:foldl(fun(C,Acum) ->
                        Acum  ++ "/" ++ C ++ "/:" ++ C ++ "_id"
                end,
                "",
                Path) .

%%read_paths(Path, Src, Related, TSWriteTree) ->
%%     ResPath = clean_ids_from_path(Path),
%%     SubTree = plaza_ts_trees:get(ResPath, TSWriteTree),
%%     SubTreeNodes = plaza_ts_trees:nodes(SubTree),
%%     NotInSubtree = lists:filter(fun(N) ->
%%                                         lists:member(N,SubTreeNodes)
%%                                 end,
%%                                 SubTreeNodes),
%%     InSubtree = lists:filter(fun(N) ->
%%                                      not lists:member(N,SubTreeNodes)
%%                              end,
%%                              SubTreeNodes),


%% clean_ids_from_path(Path) ->
%%     do_clean_ids_from_path(Path,0,[]) .
%% do_clean_ids_from_path([],_,Acum) ->
%%     lists:reverse(Acum) ;
%% do_clean_ids_from_path([P|Ps], 0, Acum) ->
%%     do_clean_ids_from_path(Ps, 1, [P | Acum]) ;
%% do_clean_ids_from_path([_P|Ps], 1, Acum) ->
%%     do_clean_ids_from_path(Ps, 0, Acum) .


%% clean_resources_from_path(Path) ->
%%     do_clean_resources_from_path(Path,1,[]) .
%% do_clean_resources_from_path([],_,Acum) ->
%%     lists:reverse(Acum) ;
%% do_clean_resources_from_path([P|Ps], 0, Acum) ->
%%     do_clean_resources_from_path(Ps, 1, [P | Acum]) ;
%% do_clean_resources_from_path([_P|Ps], 1, Acum) ->
%%     do_clean_resources_from_path(Ps, 0, Acum) .

%% insert_ids_in_path(_Is, [], Acum) ->
%%     lists:reverse(Acum) ;
%% insert_ids_in_path([], _Ps, Acum) ->
%%     lists:reverse(Acum) ;
%% insert_ids_in_path([I|Is], [P|Ps], Acum) ->
%%     insert_ids_in_path(Is, Ps, [I | [ P | Acum]]) .


%% read_path_resource(Path, ResPath, TSReadTree, TSWriteTree, Acum) ->
%%     Nodes = gb_trees:keys(TSReadTree),
%%     Paths = lists:map(fun(Node) ->
%%                               case plaza_ts_trees:get(ResPath ++ [Node], TSWriteTree) of
%%                                   error ->  case Node =:= lists:nth(1,lists:reverse(ResPath)) of
%%                                                 false -> follow_path(gb_trees:lookup(Node, TSReadTree), [[Node]], [{[Node], "/" ++ atom_to_list(Node)}]) ;
%%                                                 true  -> Ids = clean_resources_from_path(Path),
%%                                                          UpTree = lists:reverse(lists:nthtail(1,lists:reverse(ResPath))),
%%                                                          follow_upstream_path(Ids,
%%                                                                               gb_trees:lookup(Node, TSReadTree),
%%                                                                               UpTree
%%                                                                               [{insert_ids_in_path(Ids,UpTree), }

%%                                             end ;
%%                                   _Subp ->  follow_sub_path(gb_trees:lookup(Node, TSReadTree), ResPath ++ ["*",Node], [{ResPath ++ ["*",Node], "/" ++ atom_to_list(Node)}])
%%                               end
%%                       end,
%%                       Nodes),
%%     undefined .


generate_trees(TSWriteTree, Tokens) ->
    Rs = lists:map(fun(N) -> generate_branches(N,"", [], Tokens) end,
                   TSWriteTree),
    lists:foldl(fun(L,Ac) -> Ac ++ L end, [], Rs) .


generate_branches({N, Subres}, Prefix, Acum, Tokens) ->
    Res = atom_to_list(N),
    Metaresource = Prefix ++ "/" ++ Res,
    {ok, Props} = dict:find(N,Tokens),
    AcumP = case proplists:lookup(metaresource,Props) of
                {metaresource, MetaMod} ->  [ {Metaresource, {resource, MetaMod}} | Acum] ;
                _OtherP                 ->  Acum
            end,
    case proplists:lookup(resource,Props) of
        {resource, Mod}  ->  Resource = Metaresource ++ "/:" ++ Res ++ "_id",
                             AcumPP = [{Resource, {resource, Mod}} | AcumP],
                             Branches = lists:map(fun(SR) -> generate_branches(SR, Resource, [], Tokens) end,
                                                  Subres),
                             lists:foldl(fun(L,Ac) -> Ac ++ L end, [], Branches) ++ AcumPP;
        _Other           ->  AcumP
    end ;

%%     {metaresource, MetaMod} = proplists:lookup(metaresource,Props),
%%     {resource, Mod} = proplists:lookup(resource,Props),
%%     Metaresource = Prefix ++ "/" ++ Res,
%%     Resource = Metaresource ++ "/:" ++ Res ++ "_id",
%%     AcumP = [{Resource, {resource, Mod}} |
%%              [ {Metaresource, {resource, MetaMod}} | Acum]],
%%     Branches = lists:map(fun(SR) -> generate_branches(SR, Resource, [], Tokens) end,
%%                          Subres),
%%     lists:foldl(fun(L,Ac) -> Ac ++ L end, [], Branches) ++ AcumP;

generate_branches(N, Prefix, Acum, Tokens) ->
    Res = atom_to_list(N),
    Metaresource = Prefix ++ "/" ++ Res,
    {ok, Props} = dict:find(N,Tokens),
    AcumP = case proplists:lookup(metaresource,Props) of
                {metaresource, MetaMod} ->  [ {Metaresource, {resource, MetaMod}} | Acum] ;
                _OtherP                 ->  Acum
            end,
    AcumPP = case proplists:lookup(resource,Props) of
                 {resource, Mod}  ->  Resource = Metaresource ++ "/:" ++ Res ++ "_id",
                                      [{Resource, {resource, Mod}} | AcumP] ;
                 _Other           ->  AcumP
             end,
    AcumPP .
%%     {metaresource, MetaMod} = proplists:lookup(metaresource,Props),
%%     {resource, Mod} = proplists:lookup(resource,Props),
%%     Metaresource = Prefix ++ "/" ++ Res,
%%     Resource = Metaresource ++ "/:" ++ Res ++ "_id",
%%     [{Resource, {resource, Mod}} |
%%      [ {Metaresource, {resource, MetaMod}} | Acum]] .


%% Tests


get_test() ->
    TSBlogs = plaza_ts_trees:make([users, {posts, [comments]}]),
    ?assertEqual({ok,leaf},
                 plaza_ts_trees:get([users], TSBlogs)),
    {ok, TSPosts} = plaza_ts_trees:get([posts], TSBlogs),
    ?assertEqual({ok, leaf},
                plaza_ts_trees:get([comments], TSPosts)),
    ?assertEqual({ok,leaf},
                 plaza_ts_trees:get([posts, comments], TSBlogs)) .

nodes_test() ->
    TSBlogs = plaza_ts_trees:make([users, {posts, [comments]}]),
    ?assertEqual([users,posts,comments],
                 plaza_ts_trees:nodes(TSBlogs)) .

path_test() ->
    TSBlogs = plaza_ts_trees:make([users, {posts, [comments]}]),
    ?assertEqual([posts,comments],
                 path(comments,TSBlogs)),
    ?assertEqual([posts],
                 path(posts,TSBlogs)),
    ?assertEqual([users],
                 path(users,TSBlogs)),
    ?assertEqual(error,
                 path(non_existent,TSBlogs)) .




generate_trees_test() ->
    Dict = dict:from_list([{blogs, [{metaresource, blogs}, {resource, blog}]},
                           {comments, [{metaresource, comments},{resource, comment}]},
                           {posts, [{metaresource, posts},{resource, post}]},
                           {users, [{metaresource, users},{resource, user}]}]),
    TSWrite = [{users,
                [{blogs,
                  [{posts,
                    [comments]}
                  ]}
                ]}
              ],
    ?assertEqual([{"/users/:users_id/blogs/:blogs_id/posts/:posts_id/comments/:comments_id",{resource,comment}},
                  {"/users/:users_id/blogs/:blogs_id/posts/:posts_id/comments",{resource,comments}},
                  {"/users/:users_id/blogs/:blogs_id/posts/:posts_id",{resource,post}},
                  {"/users/:users_id/blogs/:blogs_id/posts",{resource,posts}},
                  {"/users/:users_id/blogs/:blogs_id",{resource,blog}},
                  {"/users/:users_id/blogs",{resource,blogs}},
                  {"/users/:users_id",{resource,user}},
                  {"/users",{resource,users}}],
                 generate_trees(TSWrite, Dict)) .
