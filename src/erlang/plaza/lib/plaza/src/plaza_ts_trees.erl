-module(plaza_ts_trees) .

% TSTreeUsers = [blogs, posts, comments],
% TSTreeBlogs = [users, {posts, [comments]}]
% TSTreePosts = [users, comments, blogs]
% TSTreeComments = [users, posts]

-author("Antonio Garrote Hernandez") .

-include_lib("eunit/include/eunit.hrl").

-export([make/1, get/2, path/2, nodes/1]) .


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
        false -> case get(lists:reverse([Node | Acum]), Tree) of
                     {ok, leaf}  -> detect(Dst, Nodes, Tree, Acum) ;
                     {ok, TreeP} -> case detect(Dst, gb_trees:keys(TreeP), TreeP, [Node | Acum]) of
                                        error  -> detect(Dst, Nodes, Tree, Acum) ;
                                        Result -> Result
                                    end ;
                     error       -> error
                 end
    end .


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
