# Graphs: Uses and Implementation

Original idea:
* Nodes are uniquely identifiable objects
* Edges are, possibly labeled, connections between them

+ Types get condesed to Gr eType nType, where both edges and nodes are hashable
- id and label conflated, not neccesarily true
- mutation becomes... problematic

u/tikhonjelvis comment:
* Node labels are often not unique, e.g. colors
* Node ids are structural, while labels are simply annotations
* User shouldn't have to deal with ids in depth, make them abstract

+ Node label does not have to be hashable or unique
+ Label mutation becomes easy
+ Edges become more space efficient
- ids become added complexity for both user and author
- edges lose information about other node (not terrible because lookups are so
  fast)
- Have to provide some sort of unique key generator

u/runeks comment:
* Nodes can be simple unique objects
* Edges can carry most of the information
* Edges can be logically splittable
* Possibly allow for arbitrary edge types

+ More expressive in terms of already existing types
- More complex in terms of type classes
- Must expose more of the internals permanently



Nodes:
* Unique
* Unique core (perhaps name+phone of employee that also has dept., salary, etc.)
* Not unique (key must be generated for it)

    Alternatively:
    * Immutible
    * Immutible core
    * Mutable

Edges:
* Must point to Node id
* Half edges must then be hashed by other node id and label


-------------------------------------------------------------
Conal Elliot's Denotational Design

API Draft

  type Graph
  type Node
  type Edge


Meanings Try 2 (Adjacency List / FGL)

  TotalGraph
  -- Need to enforce Semigroup Edge for most operations
  Opt 2: μ :: Graph -> (Node -> Node -> Edge)

  Partial Graph
  -- Need to distinguish when not there vs. mempty
                                       (Nice to think of it as (Node -> (Node -> Edge))
                                       (Partial graphs will be more like (Node -> Maybe (Node -> Edge)))
    Node :: Seems to mostly have an Eq constraint
    Edge :: Best to think of this as the type of the combination of two edges
        i.e. Any is unlabeled or [e] just is a list of all edges

  -- Generic update function for edges between two nodes
  updateWith :: (Eq Node) => (Edge -> Edge -> Edge) -> Node -> Node -> Edge -> Graph -> Graph
  μ (updateWith f n n' e g) =
    λsrc dst ->
      if (n,n') ≡ (src,dst)
          then f (μ g n n') e
          else μ g n n'
  -- You get update and removal if you can provide the functions "+" and "-"
  update :: (Eq Node, Semigroup Edge) => Node -> Node -> Edge -> Graph -> Graph
  μ (update src dst e g) ≡ updateWith (<>)
  remove :: Node -> Node -> Node -> Edge -> Graph -> Graph
  μ (remove src dst e g) = updateWith (Magic (-) func)
  -- Find a nodes neighbors
  sample :: Node -> Graph -> (Node -> Bool)
  μ (sample n g) ≡ μ g n
  -- Return all Nodes
  nodes :: Graph Node Edge -> {Node}
  μ (nodes g1) ≡ { k | k ∈ Node }

  instance Semigroup Edge => Semigroup (Graph Node Edge) where
    Free for total as long as range is a Semigroup
    Partial instance free as long as Edge is a Semigroup

  instance Monoid Edge => Monoid (Graph Node Edge) where
    Free for total as long as range is a Monoid
    Partial instance free as long as Edge is a Semigroup (if modeled by Node -> Maybe (Node -> Edge))

  instance Functor (Graph Node) where
    Free for functions, same for applicative and monad (But the functions we
                                                        want will be a little different)
    -- For total
    fmap :: (a -> b) -> Graph Node a -> Graph Node b
    fmap = fmap . fmap
    -- For Partial
    fmap = fmap . fmap . fmap -- if modeled by (Node -> Maybe (Node -> Edge))


  -- Requires:
      nodes :: Graph Node Edge -> [Node]
  instance Foldable (Graph Node) where
    -- We run into something strange here. We can only find values if we apply
    -- the graph to all node pairs. In the infinite case, this doesn't make a
    -- ton of sense. In the finite case, we have to know how to pass all nodes.

    -- For Partial
    foldMap f g = foldMap f cartProd
      where
        cartProd = g <$> nodes <*> nodes


Meanings Try 3 (John) (pair s,t :: Edge -> Node)
